from pyTSFoil.environment.basic import MultiBumpModificationAction, FigureState
from pyTSFoil.environment.prompt import DescriptionActionMultiBump, DescriptionStateFigure, PromptForLLM
import os
import tiktoken
from typing import Dict, Any, List, Union
from pyTSFoil.environment.prompt import DescriptionActionBumpMod, DescriptionStateFigure, DescriptionTask, DescriptionHistory
import json
import numpy as np
import random
from utils import create_env
import re
from typing import defaultdict

def count_tokens(text: str) -> int:
    '''
    Count the number of tokens in the text.
    '''
    # Initialize tokenizer for a specific model, e.g., GPT-3.5 or GPT-4
    # Compatible with OpenAI GPT models
    tokenizer = tiktoken.get_encoding("cl100k_base")  

    # Tokenize the text and get the number of tokens
    num_tokens = len(tokenizer.encode(text))

    return num_tokens





class Environment:
    def __init__(self, data_file_path: str):
        self.action = MultiBumpModificationAction()
        self.state = FigureState()
        self.description_action = DescriptionActionMultiBump(self.action)
        self.description_state = DescriptionStateFigure(self.state)
        self.foil_env = create_env()
        self.data_file_path = data_file_path
        self.data_files = os.listdir(self.data_file_path)
        self.all_data = [json.load(open(os.path.join(self.data_file_path, file))) for file in self.data_files]

    def get_env_reward(self, actions: np.ndarray, initial_airfoil: np.ndarray) -> float:
        '''
        Get the reward of the environment.
        '''
        self.foil_env.reset(airfoil_coordinates=initial_airfoil)
        for action in actions:
            self.foil_env.step(action)
        reward = self.foil_env.reward
        self.foil_env.close()

        return reward
    
    def generate_prompt(self, random_step: bool = True):
        # 默认是找一个随机的step生成一条数据
        trajectory_data = random.choice(self.all_data)
        prompt = ""
        prompt += DescriptionTask.get_general_description()
        prompt += DescriptionTask.get_design_knowledge()
        prompt += self.description_state.get_general_description()
        prompt += self.description_action.get_general_description()
        
        state_names = list(self.state.state_dict.keys())
        action_names = list(self.action.action_dict.keys())

        # Initialize description components
        description_parts = []
        description_parts.append("**Airfoil Design History:**\n")

        # get a random step
        random_step = random.randint(1, len(trajectory_data)-2)  # 最多只能到倒数第二个step，此时给出倒数第二个action，并转移到最终state和reward？
        trajectory_data = trajectory_data[:random_step]

        for step in trajectory_data:
            
            # Extract data from step
            prev_state = np.array(step.get('previous_state', []))
            action = np.array(step.get('action', []))
            reward = step.get('reward', 0.0)
            next_state = np.array(step.get('next_state', []))
            i_current_step = int(step['info']['i_current_step'])
            i_reference_step = int(step['info']['i_reference_step'])
            is_current_step_valid = bool(step['info']['is_current_step_valid'])
            
            # Check if this is the initial state (first step with zero action)
            is_initial_state = i_current_step == 0
            
            if is_initial_state:
                description_parts.append(f"\n**Initial Airfoil:**")
                description_parts.append(f"\n- Initial state:")
            else:
                description_parts.append(f"\n**Step {i_current_step}:**")
                description_parts.append(f"\n- Reference step to be modified: {i_reference_step}")
                description_parts.append(f"\n- Reference state:")
            
            # Describe the reference state (key aerodynamic parameters)
            for i in range(len(state_names)):
                description_parts.append(f" {state_names[i]}= {prev_state[i]:.4f}")
                
            if is_initial_state:
                description_parts.append("\n")
                continue

            description_parts.append(f"\n- Action:")
            for i in range(len(action_names)):
                description_parts.append(f" {action_names[i]}= {action[i]:.4f}")
                
            description_parts.append(f"\n- Next state:")
            for i in range(len(state_names)):
                description_parts.append(f" {state_names[i]}= {next_state[i]:.4f}")
            
            description_parts.append(f"\n- Reward: {reward:.4f}")
            description_parts.append(f"\n- Is current step valid: {is_current_step_valid}")
            description_parts.append("\n")
        

        # 给出当前的状态+动作的next state，让LLM继续推断动作
        description_parts.append("Now, please infer the next action based on the last state and history.\n")


        prompt += "".join(description_parts)
        prompt += self.description_action.instruction_for_action_output()
        initial_airfoil = np.array(trajectory_data[0]['info']['airfoil_coordinates'])
        history_actions = [np.array(action['action']) for action in trajectory_data[1:random_step]]
        return prompt, initial_airfoil, history_actions

    def verify_score(self, completion: str, initial_airfoil: np.ndarray, history_actions: np.ndarray) -> float:
        '''
        Verify the score of the completion.
        '''

        # 先把LLM的输出转化成action
        action = self.parse_action(completion)
        self.foil_env.reset(airfoil_coordinates=initial_airfoil)
        for action in history_actions + [action]:
            self.foil_env.step(action)
        reward = self.foil_env.reward
        self.foil_env.close()
        return reward

    def parse_action(self, completion: str) -> np.ndarray:
        '''
        Parse the action from the completion.
        '''

        # 只取 ### Answer 之后的内容
        answer_part = completion.split("### Answer", 1)[-1]

        # 匹配 \boxed{upper|lower bump i (deviation|height): value}
        pattern = re.compile(
            r'''\\boxed\{\s*
                (?P<section>upper|lower)\s+bump\s+(?P<idx>\d+)\s+
                (?P<key>deviation|height)\s*:\s*
                (?P<val>[+-]?(?:\d+(?:\.\d*)?|\.\d+)(?:[eE][+-]?\d+)?)\s*
                \}
            ''',
            re.VERBOSE
        )

        # 固定输出顺序：upper 0..4 (deviation, height), 然后 lower 0..4 (deviation, height)
        order = []
        for sec in ('upper', 'lower'):
            for i in range(5):
                order.append((sec, i, 'deviation'))
                order.append((sec, i, 'height'))

        pos_map = {triplet: k for k, triplet in enumerate(order)}

        # 初始化 1x20，默认全 0
        arr = np.zeros((1, len(order)), dtype=float)

        # 写入已出现的值；未出现的保持 0
        for m in pattern.finditer(answer_part):
            sec = m.group('section')
            idx = int(m.group('idx'))
            key = m.group('key')
            val = float(m.group('val'))
            t = (sec, idx, key)
            if t in pos_map:              # 只填入我们关心的 20 个槽位
                arr[0, pos_map[t]] = val  # 缺失的不动 -> 仍为 0
        arr = arr.squeeze(0)
        return arr



    
    def count_tokens(self, description: str) -> int:
        '''
        Count the number of tokens in the text.
        '''
        # Initialize tokenizer for a specific model, e.g., GPT-3.5 or GPT-4
        # Compatible with OpenAI GPT models
        tokenizer = tiktoken.get_encoding("cl100k_base")  
        
        # Tokenize the text and get the number of tokens
        num_tokens = len(tokenizer.encode(description))
        
        return num_tokens
    
    def save_description(self, description: str, output_path: str):
        '''
        Save the description to a markdown file.
        '''
        with open(output_path, 'w') as f:
            f.write(description)
            
    def save_description_with_tokens(self, description: str, output_path: str):
        '''
        Save the description to a markdown file with the number of tokens.
        '''
        description += f"\n\nNumber of tokens: {self.count_tokens(description)}"
        self.save_description(description, output_path)



if __name__ == "__main__":
    
    trajectory_json_path = 'example/env_FigState_MultiBump/trajectory.json'
    os.makedirs('example/prompt/descriptions', exist_ok=True)
    env = Environment()
    prompt, initial_airfoil, history_actions = env.generate_prompt(trajectory_json_path)
    print(prompt)

    with open(trajectory_json_path, 'r') as f:
        trajectory_data = json.load(f)
        
    initial_airfoil = np.array(trajectory_data[0]['info']['airfoil_coordinates'])

    for i in range(1, 4):
        
        actions = [np.array(action['action']) for action in trajectory_data[1:i+1]]  # 历史actions，第0步没有记录action所以从第1步开始
    
        reward = env.get_env_reward(actions = actions, initial_airfoil=initial_airfoil)
        
        print(f"Step {env.foil_env.i_current_step}: Reward = {reward:.3f}; {trajectory_data[i]['reward']:.3f}")
