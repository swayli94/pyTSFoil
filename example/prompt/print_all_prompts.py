#!/usr/bin/env python3
"""
Example script to demonstrate and save all descriptions from pyTSFoil description classes.

This script creates instances of all description classes, prints their general descriptions,
and saves each description to a separate file for easy reference.
"""

import os
import sys

path = os.path.dirname(os.path.abspath(__file__))
# Add project root to Python path for multi-branch development
project_root = os.path.abspath(os.path.join(path, '..', '..'))
if project_root not in sys.path:
    sys.path.insert(0, project_root)

from pyTSFoil.environment.prompt import (
    DescriptionActionBumpMod,
    DescriptionActionGlobalMod, 
    DescriptionActionMultiBump,
    DescriptionStateFigure,
    DescriptionTask
)
from pyTSFoil.environment.basic import (
    BumpModificationAction,
    GlobalModificationAction,
    MultiBumpModificationAction,
    FigureState
)


def create_output_directory():
    """Create output directory for description files."""
    output_dir = os.path.join(path, 'descriptions')
    os.makedirs(output_dir, exist_ok=True)
    return output_dir


def save_description_to_file(description: str, filename: str, output_dir: str):
    """Save a description to a file."""
    filepath = os.path.join(output_dir, filename)
    with open(filepath, 'w', encoding='utf-8') as f:
        f.write(description)
    print(f"✓ Saved to: {filepath}")


def main():
    """Main function to generate and save all descriptions."""
    
    print("=" * 80)
    print("PyTSFoil Description Classes Demo")
    print("=" * 80)
    
    # Create output directory
    output_dir = create_output_directory()
    print(f"Output directory: {output_dir}\n")
    
    # 1. Bump Modification Action Description
    # Create the underlying action instance for realistic parameters
    bump_action = BumpModificationAction()
    desc_bump = DescriptionActionBumpMod(bump_action)
    
    bump_description = desc_bump.get_general_description()
    save_description_to_file(bump_description, 'bump_modification_description.md', output_dir)
    
    # 2. Global Modification Action Description  
    # Create the underlying action instance for realistic parameters
    global_action = GlobalModificationAction()
    desc_global = DescriptionActionGlobalMod(global_action)
    
    global_description = desc_global.get_general_description()
    save_description_to_file(global_description, 'global_modification_description.md', output_dir)
    
    # 3. Multi-Bump Modification Action Description
    multi_bump_action = MultiBumpModificationAction()
    desc_multi_bump = DescriptionActionMultiBump(multi_bump_action)
    
    multi_bump_description = desc_multi_bump.get_general_description()
    save_description_to_file(multi_bump_description, 'multi_bump_modification_description.md', output_dir)
    
    # 4. Figure State Description
    # Create the underlying state instance for realistic parameters
    figure_state = FigureState()
    desc_state = DescriptionStateFigure(figure_state)
    
    state_description = desc_state.get_general_description()
    save_description_to_file(state_description, 'figure_state_description.md', output_dir)
    
    # 5. Action Output Instruction
    bump_output_instruction = desc_bump.instruction_for_action_output()
    save_description_to_file(bump_output_instruction, 'bump_action_output_instruction.md', output_dir)
    
    multi_bump_output_instruction = desc_multi_bump.instruction_for_action_output()
    save_description_to_file(multi_bump_output_instruction, 'multi_bump_action_output_instruction.md', output_dir)
    
    # 6. Task Descriptions
    # 6a. General Task Description
    task_general = DescriptionTask.get_general_description()
    save_description_to_file(task_general, 'task_general_description.md', output_dir)
    
    # 6b. Design Knowledge
    design_knowledge = DescriptionTask.get_design_knowledge()
    save_description_to_file(design_knowledge, 'task_design_knowledge.md', output_dir)

    print("=" * 80)
    print("All descriptions have been generated and saved successfully!")
    print("=" * 80)


if __name__ == "__main__":
    main()
