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
    def __init__(self):
        self.action = MultiBumpModificationAction()
        self.state = FigureState()
        self.description_action = DescriptionActionMultiBump(self.action)
        self.description_state = DescriptionStateFigure(self.state)
        self.foil_env = create_env()

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
    
    def generate_prompt(self, trajectory_json_path: str, random_step: bool = True):
        # 默认是找一个随机的step生成一条数据
        
        prompt = ""
        prompt += DescriptionTask.get_general_description()
        prompt += DescriptionTask.get_design_knowledge()
        prompt += self.description_state.get_general_description()
        prompt += self.description_action.get_general_description()
        try:
            with open(trajectory_json_path, 'r') as f:
                trajectory_data = json.load(f)
        except FileNotFoundError:
            return f"Error: Trajectory file '{trajectory_json_path}' not found."
        except json.JSONDecodeError:
            return f"Error: Invalid JSON format in file '{trajectory_json_path}'."
        
        if not trajectory_data:
            return "Empty trajectory - no design steps recorded."
        
        state_names = list(self.state.state_dict.keys())
        action_names = list(self.action.action_dict.keys())

        # Initialize description components
        description_parts = []
        description_parts.append("**Airfoil Design History:**\n")

        # get a random step
        random_step = random.randint(0, len(trajectory_data)-1)
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

        prompt += "".join(description_parts)
        prompt += self.description_action.instruction_for_action_output()
        return prompt
    
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
    
    # trajectory_json_path = '/data1/xwn/projects/llm_grpo_foil/pyTSFoil/example/env_FigState_MultiBump/trajectory.json'
    trajectory_json_path = 'example/env_FigState_MultiBump/trajectory.json'
    os.makedirs('example/prompt/descriptions', exist_ok=True)
    env = Environment()
    prompt = env.generate_prompt(trajectory_json_path)

    with open(trajectory_json_path, 'r') as f:
        trajectory_data = json.load(f)
        
    initial_airfoil = np.array(trajectory_data[0]['info']['airfoil_coordinates'])

    for i in range(1, 4):
        
        actions = [np.array(action['action']) for action in trajectory_data[1:i+1]]
    
        reward = env.get_env_reward(actions = actions, initial_airfoil=initial_airfoil)
        
        print(f"Step {env.foil_env.i_current_step}: Reward = {reward:.3f}; {trajectory_data[i]['reward']:.3f}")


