'''
Read the trajectory.json file and repeat the trajectory.

Check whether the trajectory and reward are the same.
'''

import os
import sys

path = os.path.dirname(os.path.abspath(__file__))
# Add project root to Python path for multi-branch development
project_root = os.path.abspath(os.path.join(path, '..', '..'))
if project_root not in sys.path:
    sys.path.insert(0, project_root)

import numpy as np
import json

from pyTSFoil.environment.basic import MultiBumpModificationAction
from pyTSFoil.environment.utils import TSFoilEnv_FigState_MultiBumpAction


if __name__ == "__main__":

    print('path: ', path)

    with open(os.path.join(path, 'trajectory.json'), 'r') as f:
        trajectory_data = json.load(f)

    # Create the environment
    airfoil_coordinates = np.array(trajectory_data[0]['info']['airfoil_coordinates'])
    initial_airfoil = airfoil_coordinates.copy()
    initial_airfoil[:,1] = initial_airfoil[:,1] * 0.8

    action_class = MultiBumpModificationAction()
    
    env = TSFoilEnv_FigState_MultiBumpAction(
            initial_airfoil=initial_airfoil,
            output_dir=path,
            render_mode='none',
            path_save_fig_of_observation=None,
            action_class=action_class,
            )
    
    # Run the environment
    env.reset(airfoil_coordinates=airfoil_coordinates)   

    for i in range(1, len(trajectory_data)):
        
        action = np.array(trajectory_data[i]['action'])
        
        env.step(action)
        
    env.save_trajectory(os.path.join(path, 'trajectory-repeated.json'))
    env.close()
    
    # Compare the trajectory files
    with open(os.path.join(path, 'trajectory-repeated.json'), 'r') as f:
        trajectory_data_repeated = json.load(f)
        
    for i in range(len(trajectory_data)):
        if trajectory_data[i] != trajectory_data_repeated[i]:
            print(f'Step {i} is different')
            print(trajectory_data[i])
            print(trajectory_data_repeated[i])
            break
            
    print('All steps are the same')
