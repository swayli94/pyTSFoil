'''
Print the prompt for LLM.

The prompt is saved to the markdown file.

Usage:
    python example/prompt/print_prompt_for_LLM.py

'''

from pyTSFoil.environment.basic import MultiBumpModificationAction, FigureState
from pyTSFoil.environment.prompt import DescriptionActionMultiBump, DescriptionStateFigure, PromptForLLM
import os
import tiktoken


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


if __name__ == "__main__":
    
    trajectory_json_path = 'pyTSFoil/example/env_FigState_MultiBump/trajectory.json'
    os.makedirs('example/prompt/descriptions', exist_ok=True)
    
    action = MultiBumpModificationAction()
    state = FigureState()
    
    description_action = DescriptionActionMultiBump(action)
    description_state = DescriptionStateFigure(state)
    
    prompt_for_LLM = PromptForLLM(
        description_action=description_action,
        description_state=description_state,
        )
    
    description = prompt_for_LLM.generate_prompt(
        environment_trajectory=trajectory_json_path,
        state_dict=state.state_dict,
        action_dict=action.action_dict,
        )
    
    num_tokens = count_tokens(description)
    print(f"Number of tokens: {num_tokens}")
    
    description += f"\n\nNumber of tokens: {num_tokens}"
    
    # Save description to markdown file
    output_path = 'example/prompt/descriptions/prompt_for_LLM.md'
    with open(output_path, 'w') as f:
        f.write(description)
