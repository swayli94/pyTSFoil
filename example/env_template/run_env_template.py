
import os
import numpy as np

from pyTSFoil.environment.env_template import TSFoilEnv_Template


if __name__ == "__main__":

    path = os.path.dirname(os.path.abspath(__file__))
    print('path: ', path)

    # Create the environment
    x, y = np.loadtxt(os.path.join(path, 'rae2822.dat'), skiprows=1).T
    airfoil_coordinates = np.column_stack((x, y))
    
    env = TSFoilEnv_Template(
            airfoil_coordinates=airfoil_coordinates,
            output_dir=path,
            render_mode='save',
            )
    
    # Run the environment
    env.reset()   
    env.render()
    
    for i in range(2):
        
        action = np.array([1.05])
        env.step(action)
        env.render()
        
    env.close()
    


