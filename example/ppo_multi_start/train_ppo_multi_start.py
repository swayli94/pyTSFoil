'''
Example usage of PPO with TSFoilEnv_FigState_BumpAction_MultiEnv

This script demonstrates training PPO with parallel environments using the 
refactored multiprocessing implementation. The new implementation uses 
multiprocessing.Pool for better reliability and automatic resource management.

Key improvements:
- Simpler and more reliable multiprocessing using Pool
- Automatic cleanup (no need for manual shutdown)
- Better error isolation between workers
- More consistent performance across different systems
'''
import os
import numpy as np
import torch
import multiprocessing as mp
from typing import List, Tuple
import matplotlib.pyplot as plt

# Set multiprocessing start method to 'spawn' to avoid CUDA forking issues
mp.set_start_method('spawn', force=True)

# Import the classes
from pyTSFoil.environment.utils import TSFoilEnv_FigState_BumpAction
from pyTSFoil.environment.basic import BumpModificationAction, FigureState, cst_foil, check_validity
from model.ppo_mp import PPO_FigState_BumpAction_MultiEnv

path = os.path.dirname(os.path.abspath(__file__))


class AirfoilDatabase():
    '''
    Airfoil database for XFoil environment
    '''
    def __init__(self, fname_database='reference-airfoils-cst.dat', n_cst=10, n_point_geo=201):
        
        self.fname_database = fname_database
        self.n_cst = n_cst
        self.n_point_geo = n_point_geo
        
        self.airfoils : List[dict] = []
        
        self.read_database()
        
    def read_database(self) -> None:
        
        with open(self.fname_database, 'r') as f:
            lines = f.readlines()

        for line in lines:
            
            if line[0] == '#' or len(line) <= 0:
                continue
            
            data = line.split()
            
            airfoil = {
                'name' : data[0],
                'tmax' : float(data[1]),
                'tail' : float(data[2]),
                'angle-TE' : float(data[3]),
                'cst_u' : np.array([float(x) for x in data[4:4+self.n_cst]]),
                'cst_l' : np.array([float(x) for x in data[4+self.n_cst:4+2*self.n_cst]]),
            }
            
            self.airfoils.append(airfoil)

    def get_airfoil_coordinates(self, ID: int) -> Tuple[np.ndarray, np.ndarray, np.ndarray, np.ndarray]:
        
        cst_u = self.airfoils[ID]['cst_u']
        cst_l = self.airfoils[ID]['cst_l']
        
        x, yu, yl, _, _ = cst_foil(self.n_point_geo, cst_u, cst_l)
        
        xx = np.concatenate((x[::-1], x[1:]))
        yy = np.concatenate((yu[::-1], yl[1:]))
        
        return np.column_stack((xx, yy)), x, yu, yl

    def get_random_airfoil_coordinates(self) -> Tuple[np.ndarray, np.ndarray, np.ndarray, np.ndarray]:
        
        is_action_valid = False
        n_try = 0
        
        while not is_action_valid:

            id0 = np.random.randint(0, len(self.airfoils))
            id1 = np.random.randint(0, len(self.airfoils))
            alpha = np.random.rand()
            
            airfoil_0, x, yu0, yl0 = self.get_airfoil_coordinates(id0)
            airfoil_1, _, yu1, yl1 = self.get_airfoil_coordinates(id1)
            
            airfoil_coordinates = alpha*airfoil_0 + (1-alpha)*airfoil_1
            
            yu_new = alpha*yu0 + (1-alpha)*yu1
            yl_new = alpha*yl0 + (1-alpha)*yl1
            
            is_action_valid = check_validity(x, yu_new, yl_new)
            n_try += 1
            
            if n_try > 100:
                raise ValueError('Failed to generate valid airfoil coordinates')
        
        return airfoil_coordinates, x, yu_new, yl_new


def create_env_with_id(worker_id=None, render_mode='none', 
                        n_max_step=5,
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
    database = AirfoilDatabase(fname_database=os.path.join(path, 'reference-airfoils-cst.dat'))
    
    if worker_id is not None:
        airfoil_coordinates, _, _, _ = database.get_random_airfoil_coordinates()
    else:
        airfoil_coordinates, _, _, _ = database.get_airfoil_coordinates(11)  # RAE2822
    
    # Custom action class
    action_class = BumpModificationAction()
    
    action_class.action_dict['UBL']['bound'] = [0.05, 0.8]
    action_class.action_dict['UBH']['bound'] = [-0.003, 0.003]
    action_class.action_dict['UBH']['min_increment'] = 0.001
    action_class.action_dict['UBW']['bound'] = [0.4, 0.8]
    
    action_class.action_dict['LBL']['bound'] = [0.05, 0.8]
    action_class.action_dict['LBH']['bound'] = [-0.003, 0.003]
    action_class.action_dict['LBH']['min_increment'] = 0.001
    action_class.action_dict['LBW']['bound'] = [0.4, 0.8]
    
    action_class._update_action_bounds(action_class.action_dict)

    # Create unique output directory for each worker to avoid file conflicts
    if worker_id is not None:
        worker_output_dir = os.path.join(path, 'temp', f'worker_{worker_id}')
        os.makedirs(worker_output_dir, exist_ok=True)
        
        # Plot airfoil geometry
        if show_airfoil:
            os.makedirs(os.path.join(path, 'temp_geometry'), exist_ok=True)
            plt.plot(airfoil_coordinates[:, 0], airfoil_coordinates[:, 1], 'k')
            plt.xlim(-0.1, 1.1)
            plt.ylim(-0.1, 0.1)
            plt.savefig(os.path.join(path, 'temp_geometry', f'airfoil_{worker_id}.png'))
            plt.close()

    else:
        worker_output_dir = path
        
        # Plot airfoil geometry
        if show_airfoil:
            os.makedirs(os.path.join(path, 'temp_geometry'), exist_ok=True)
            plt.plot(airfoil_coordinates[:, 0], airfoil_coordinates[:, 1], 'k')
            plt.xlim(-0.1, 1.1)
            plt.ylim(-0.1, 0.1)
            plt.savefig(os.path.join(path, 'temp_geometry', 'airfoil.png'))
            plt.close()

    # Create environment instance
    return TSFoilEnv_FigState_BumpAction(
        airfoil_coordinates=airfoil_coordinates,
        output_dir=worker_output_dir,
        render_mode=render_mode,
        action_class=action_class,
        n_max_step=n_max_step
    )


class EnvFactory:
    '''
    Picklable environment factory functions for each worker
    '''
    def __init__(self, worker_id):
        self.worker_id = worker_id
    
    def __call__(self):
        return create_env_with_id(self.worker_id)


def main(device='auto'):
    '''Main training loop using refactored multiprocessing implementation'''
    
    # Number of parallel environments (can be increased with new reliable implementation)
    n_envs = 50
    
    # Create list of environment factory functions with unique worker IDs
    env_fns = [EnvFactory(i) for i in range(n_envs)]
    
    eval_env = create_env_with_id(
        worker_id=None,  # No worker ID for evaluation (uses main output directory)
        render_mode='save',  # Enable rendering for evaluation
    )
    
    print(f"Creating PPO agent with {n_envs} parallel environments...")
    
    # Create specialized PPO agent with multiple environments
    ppo_agent = PPO_FigState_BumpAction_MultiEnv(
        env_fns=env_fns,
        env_eval=eval_env,
        lr=1e-6,
        gamma=0.99,
        gae_lambda=0.95,
        clip_epsilon=0.95,
        value_loss_coef=0.1,
        entropy_coef=0.01,
        max_grad_norm=0.5,
        n_epochs=20, 
        batch_size=500,
        n_steps=5,
        dim_latent=64,
        dim_hidden=1024,
        n_interp_points=101,
        device=device
    )
    
    # Train the agent
    try:
        ppo_agent.train(
            total_time_steps=100000,  # Increased for new reliable implementation
            log_interval=1,
            save_interval=10,
            eval_interval=10,
            save_path=os.path.join(path, 'ppo_fig_bump_model.pt'),
            plot_training=True,
            plot_path=os.path.join(path, 'training_progress.png'),
            use_entropy_decay=True
        )
    except Exception as e:
        print(f"Training failed with error: {e}")
        raise
    
    # Clean up CUDA tensors and force garbage collection
    if torch.cuda.is_available():
        torch.cuda.empty_cache()
        torch.cuda.synchronize()
    
    # Optional: Clean up worker directories
    cleanup_choice = 'y'
    if cleanup_choice in ['y', 'yes']:
        import shutil
        for i in range(n_envs):
            worker_dir = os.path.join(path, 'temp', f'worker_{i}')
            if os.path.exists(worker_dir):
                try:
                    shutil.rmtree(worker_dir)
                    # print(f"Cleaned up {worker_dir}")
                except Exception as e:
                    print(f"Could not clean up {worker_dir}: {e}")
        print("Worker directory cleanup completed!")


if __name__ == "__main__":
    
    np.set_printoptions(formatter={'float': '{:8.4f}'.format})
    
    print('path: ', path)
    
    GPU_ID = 1
    device = f'cuda:{GPU_ID}' if torch.cuda.is_available() else 'cpu'
    
    main(device=device)
    