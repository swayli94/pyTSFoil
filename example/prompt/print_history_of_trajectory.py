'''
Print the history of the trajectory.

The trajectory is read from the json file.
The description is saved to the markdown file.

Usage:
    python example/prompt/print_history_of_trajectory.py

This script demonstrates how to use the DescriptionHistory class to convert
environment trajectories into human-readable descriptions. The trajectory 
contains state-action-reward sequences from airfoil design optimization.

Input: JSON file with trajectory data containing:
- previous_state: airfoil state parameters (17 values)
- action: modification parameters (6 for bump, 11 for global)
- reward: optimization reward
- next_state: resulting airfoil state
- done: episode completion flag
- info: additional metadata

Output: Markdown file with step-by-step design history including:
- Initial aerodynamic conditions
- Applied modifications
- Resulting changes
- Overall progress summary
'''

import os
import sys

path = os.path.dirname(os.path.abspath(__file__))
# Add project root to Python path for multi-branch development
project_root = os.path.abspath(os.path.join(path, '..', '..'))
if project_root not in sys.path:
    sys.path.insert(0, project_root)

from pyTSFoil.environment.prompt import DescriptionHistory
from pyTSFoil.environment.basic import MultiBumpModificationAction, FigureState


if __name__ == "__main__":
    
    trajectory_json_path = 'pyTSFoil/example/env_FigState_MultiBump/trajectory.json'
    os.makedirs('example/prompt/descriptions', exist_ok=True)
    
    print(f"Processing trajectory file: {trajectory_json_path}")
    
    # Convert trajectory to description
    description = DescriptionHistory.convert_to_description(
                    trajectory_json_path,
                    state_dict=FigureState().state_dict,
                    action_dict=MultiBumpModificationAction().action_dict,
                    )
    
    # Save description to markdown file
    output_path = 'pyTSFoil/example/prompt/descriptions/history_of_trajectory.md'
    with open(output_path, 'w') as f:
        f.write(description)
    