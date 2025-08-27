import os
import matplotlib.pyplot as plt
from pyTSFoil.environment.utils import TSFoilEnv_FigState_GlobalAction
from pyTSFoil.environment.basic import GlobalModificationAction
from model.database import AirfoilDatabase
# get the path of the file
path = os.path.dirname(os.path.abspath(__file__))
print(path)

def create_env(render_mode='none', 
                        n_max_step=10,
                        show_airfoil=False):
    '''
    Factory function to create a new environment instance with unique worker ID
    
    This function is called by each worker process to create its own
    isolated environment instance. Each worker gets completely separate
    PyTSFoil/Fortran data to avoid conflicts.
    
    Args:
        worker_id: Unique worker ID for creating separate output directories
        render_mode: Rendering mode ('none', 'both', etc.)
        n_max_step: Maximum number of steps per episode
    '''
    # Create sample airfoil
    database = AirfoilDatabase(fname_database=os.path.join(path, 'assets/selected-airfoils-cst.dat'))
    
    # Custom action class
    action_class = GlobalModificationAction()

    # Create unique output directory for each worker to avoid file conflicts
    # if worker_id is not None:
    #     worker_output_dir = os.path.join(path, 'temp', f'worker_{worker_id}')
    #     os.makedirs(worker_output_dir, exist_ok=True)
        
    #     # Plot airfoil geometry
    #     if show_airfoil:
    #         os.makedirs(os.path.join(path, 'temp_geometry'), exist_ok=True)
    #         plt.plot(airfoil_coordinates[:, 0], airfoil_coordinates[:, 1], 'k')
    #         plt.xlim(-0.1, 1.1)
    #         plt.ylim(-0.1, 0.1)
    #         plt.savefig(os.path.join(path, 'temp_geometry', f'airfoil_{worker_id}.png'))
    #         plt.close()

    # else:
    #     worker_output_dir = path
        
    #     # Plot airfoil geometry
    #     if show_airfoil:
    #         os.makedirs(os.path.join(path, 'temp_geometry'), exist_ok=True)
    #         plt.plot(airfoil_coordinates[:, 0], airfoil_coordinates[:, 1], 'k')
    #         plt.xlim(-0.1, 1.1)
    #         plt.ylim(-0.1, 0.1)
    #         plt.savefig(os.path.join(path, 'temp_geometry', 'airfoil.png'))
    #         plt.close()

    # Create environment instance
    return TSFoilEnv_FigState_GlobalAction(
        database=database,
        # output_dir=worker_output_dir,
        render_mode=render_mode,
        action_class=action_class,
        n_max_step=n_max_step
    )
