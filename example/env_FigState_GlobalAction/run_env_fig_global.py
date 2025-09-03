

import os
import sys

path = os.path.dirname(os.path.abspath(__file__))
# Add project root to Python path for multi-branch development
project_root = os.path.abspath(os.path.join(path, '..', '..'))
if project_root not in sys.path:
    sys.path.insert(0, project_root)

import numpy as np
import matplotlib.pyplot as plt

from pyTSFoil.environment.utils import TSFoilEnv_FigState_GlobalAction


if __name__ == "__main__":
    
    np.set_printoptions(formatter={'float': '{:8.4f}'.format}, linewidth=np.inf)

    path = os.path.dirname(os.path.abspath(__file__))
    print('path: ', path)

    # Create the environment
    x, y = np.loadtxt(os.path.join(path, 'rae2822.dat'), skiprows=1).T
    airfoil_coordinates = np.column_stack((x, y))
    
    env = TSFoilEnv_FigState_GlobalAction(
            initial_airfoil=airfoil_coordinates,
            output_dir=path,
            render_mode='save',
            path_save_fig_of_observation=os.path.join(path, 'fig_of_observation.png'),
            )
    
    # Run the environment
    env.reset()   
    
    for i in range(5):
        
        action = env.action_class.random_action(scale=1.0)
        
        env.step(action)
        
    env.render()
    env.save_trajectory(os.path.join(path, 'trajectory.json'))
    env.close()
    


