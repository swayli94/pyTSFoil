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

# Set multiprocessing start method to 'spawn' to avoid CUDA forking issues
mp.set_start_method('spawn', force=True)

# Import the classes
from pyTSFoil.environment.utils import TSFoilEnv_FigState_BumpAction
from pyTSFoil.environment.basic import BumpModificationAction
from model.ppo_mp import PPO_FigState_MultiEnv

path = os.path.dirname(os.path.abspath(__file__))

def create_env_with_id(worker_id=None, render_mode='none', n_max_step=10):
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
    x, y = np.loadtxt(os.path.join(path, 'rae2822.dat'), skiprows=1).T
    airfoil_coordinates = np.column_stack((x, y))
    
    # Custom action class
    action_class = BumpModificationAction()
    
    action_class.action_dict['UBL']['bound'] = [0.05, 0.9]
    action_class.action_dict['UBH']['bound'] = [-0.005, 0.005]
    action_class.action_dict['UBH']['min_increment'] = 0.001
    action_class.action_dict['UBW']['bound'] = [0.6, 1.0]
    
    action_class.action_dict['LBL']['bound'] = [0.05, 0.9]
    action_class.action_dict['LBH']['bound'] = [-0.005, 0.005]
    action_class.action_dict['LBH']['min_increment'] = 0.001
    action_class.action_dict['LBW']['bound'] = [0.6, 1.0]
    
    action_class._update_action_bounds(action_class.action_dict)

    # Create unique output directory for each worker to avoid file conflicts
    if worker_id is not None:
        worker_output_dir = os.path.join(path, 'temp', f'worker_{worker_id}')
        os.makedirs(worker_output_dir, exist_ok=True)
    else:
        worker_output_dir = path

    # Create environment instance
    return TSFoilEnv_FigState_BumpAction(
        initial_airfoil=airfoil_coordinates,
        output_dir=worker_output_dir,
        render_mode=render_mode,
        action_class=action_class,
        n_max_step=n_max_step
    )

# Picklable environment factory functions for each worker
class EnvFactory:
    def __init__(self, worker_id):
        self.worker_id = worker_id
    
    def __call__(self):
        return create_env_with_id(self.worker_id)


def main(device='auto', resume=False):
    '''Main training loop using refactored multiprocessing implementation'''
    
    # Number of parallel environments (can be increased with new reliable implementation)
    n_envs = 100
    
    # Create list of environment factory functions with unique worker IDs
    env_fns = [EnvFactory(i) for i in range(n_envs)]
    
    eval_env = create_env_with_id(
        worker_id=None,  # No worker ID for evaluation (uses main output directory)
        render_mode='save',  # Enable rendering for evaluation
    )
    
    print(f"Creating PPO agent with {n_envs} parallel environments...")
    
    # Create specialized PPO agent with multiple environments
    ppo_agent = PPO_FigState_MultiEnv(
        env_fns=env_fns,
        env_eval=eval_env,
        lr=1e-4,
        gamma=0.99,
        gae_lambda=0.95,
        clip_epsilon=0.2,
        value_loss_coef=0.5,
        entropy_coef=0.0001,
        max_grad_norm=0.5,
        n_epochs=4,
        batch_size=200,
        n_steps=10,
        dim_latent=64,
        dim_hidden=512,
        n_interp_points=101,
        initial_action_std=0.2,
        device=device,
        max_processes=50
    )
    
    save_path = os.path.join(path, 'ppo_fig_bump_model.pt')
    
    if resume and os.path.exists(save_path):
        ppo_agent.load_model(save_path)
        
        ppo_agent.actor_critic.actor_log_std.data = torch.ones_like(
            ppo_agent.actor_critic.actor_log_std, device=device
        ) * np.log(ppo_agent.initial_action_std)
    
    # Train the agent
    try:
        ppo_agent.train(
            total_time_steps=int(1e6),  # Increased for new reliable implementation
            log_interval=1,
            save_interval=10,
            eval_interval=10,
            save_path=save_path,
            plot_training=True,
            plot_path=os.path.join(path, 'training_progress.png'),
            use_action_std_decay=False
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
    
    GPU_ID = 0
    device = f'cuda:{GPU_ID}' if torch.cuda.is_available() else 'cpu'
    
    main(device=device, resume=True)
    