import os
import matplotlib.pyplot as plt
from pyTSFoil.environment.utils import TSFoilEnv_FigState_MultiBumpAction
from pyTSFoil.environment.basic import MultiBumpModificationAction
from model.database import AirfoilDatabase
# get the path of the file
path = os.path.dirname(os.path.abspath(__file__))
print(path)

def create_env(render_mode='none', n_max_step=10):
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
    action_class = MultiBumpModificationAction()

    # Create environment instance
    return TSFoilEnv_FigState_MultiBumpAction(
        database=database,
        initial_airfoil='random-selected',
        output_dir=None,
        render_mode=render_mode,
        action_class=action_class,
        n_max_step=n_max_step
    )

