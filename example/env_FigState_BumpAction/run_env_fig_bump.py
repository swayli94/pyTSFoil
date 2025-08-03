
import os
import numpy as np

from pyTSFoil.environment.utils import TSFoilEnv_FigState_BumpAction


if __name__ == "__main__":

    path = os.path.dirname(os.path.abspath(__file__))
    print('path: ', path)

    # Create the environment
    x, y = np.loadtxt(os.path.join(path, 'rae2822.dat'), skiprows=1).T
    airfoil_coordinates = np.column_stack((x, y))
    
    env = TSFoilEnv_FigState_BumpAction(
            airfoil_coordinates=airfoil_coordinates,
            output_dir=path,
            render_mode='save',
            path_save_fig_of_observation=os.path.join(path, 'fig_of_observation.png'),
            )
    
    # Run the environment
    env.reset()   

    for i in range(5):
        
        action = env.Action.random_action(scale=1.0)
        
        env.step(action)
        
    env.render()
    env.save_trajectory(os.path.join(path, 'trajectory.json'))
    env.close()
    


