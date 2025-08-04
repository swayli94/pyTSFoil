#!/usr/bin/env python3
"""
Example script to demonstrate and save all descriptions from pyTSFoil description classes.

This script creates instances of all description classes, prints their general descriptions,
and saves each description to a separate file for easy reference.
"""

import os
import sys

# Add the parent directory to the path so we can import pyTSFoil
sys.path.append(os.path.join(os.path.dirname(__file__), '..', '..'))

from pyTSFoil.environment.prompt import (
    DescriptionActionBumpMod,
    DescriptionActionGlobalMod, 
    DescriptionStateFigure,
    DescriptionTask
)
from pyTSFoil.environment.basic import (
    BumpModificationAction,
    GlobalModificationAction,
    FigureState
)


def create_output_directory():
    """Create output directory for description files."""
    output_dir = os.path.join(os.path.dirname(__file__), 'descriptions')
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
    
    # 3. Figure State Description
    # Create the underlying state instance for realistic parameters
    figure_state = FigureState()
    desc_state = DescriptionStateFigure(figure_state)
    
    state_description = desc_state.get_general_description()
    save_description_to_file(state_description, 'figure_state_description.md', output_dir)
    
    # 4. Bump Action Extraction Instruction
    bump_extraction_instruction = desc_bump.instruction_for_action_extraction()
    save_description_to_file(bump_extraction_instruction, 'bump_action_extraction_instruction.md', output_dir)
    
    # 5. Task Descriptions
    # 5a. General Task Description
    task_general = DescriptionTask.get_general_description()
    save_description_to_file(task_general, 'task_general_description.md', output_dir)
    
    # 5b. Design Knowledge
    design_knowledge = DescriptionTask.get_design_knowledge()
    save_description_to_file(design_knowledge, 'task_design_knowledge.md', output_dir)
    
    # 5c. Bump Action Output Instruction
    bump_output_instruction = DescriptionTask.instruction_for_bump_action_output()
    save_description_to_file(bump_output_instruction, 'task_bump_action_output_instruction.md', output_dir)

    print("=" * 80)
    print("All descriptions have been generated and saved successfully!")
    print("=" * 80)


if __name__ == "__main__":
    main()
