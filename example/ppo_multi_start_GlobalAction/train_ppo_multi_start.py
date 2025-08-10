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
import sys
sys.path.append(os.path.dirname(__file__))

import numpy as np
import torch
import torch.nn as nn
import multiprocessing as mp
from typing import List, Tuple
import matplotlib.pyplot as plt

# Set multiprocessing start method to 'spawn' to avoid CUDA forking issues
mp.set_start_method('spawn', force=True)

# Import the classes
from pyTSFoil.environment.utils import TSFoilEnv_FigState_GlobalAction
from pyTSFoil.environment.basic import GlobalModificationAction
from model.database import AirfoilDatabase
from model.ppo import ActorCritic
from model.ppo_mp import PPO_FigState_MultiEnv

path = os.path.dirname(os.path.abspath(__file__))


def create_env_with_id(worker_id=None, render_mode='none', 
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
    database = AirfoilDatabase(fname_database=os.path.join(path, 'selected-airfoils-cst.dat'))
    
    if worker_id is not None:
        # airfoil_coordinates, _, _, _ = database.get_random_airfoil_coordinates()
        airfoil_coordinates, _, _, _ = database.get_airfoil_coordinates(worker_id)
    else:
        airfoil_coordinates, _, _, _ = database.get_airfoil_coordinates(10)  # RAE2822
    
    # Custom action class
    action_class = GlobalModificationAction()

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
    return TSFoilEnv_FigState_GlobalAction(
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


class ActorCritic_Custom(ActorCritic):
    '''
    Actor-Critic network for PPO
    '''
    def __init__(self, 
                    dim_state: int, 
                    dim_action: int, 
                    dim_latent: int = 64,
                    dim_hidden: int = 256,
                    n_interp_points: int = 100,
                    initial_std: float = 0.1):
        
        super(ActorCritic, self).__init__()

        self.dim_state = dim_state
        self.dim_action = dim_action
        self.dim_latent = dim_latent
        self.dim_hidden = dim_hidden
        self.n_interp_points = n_interp_points
        self.initial_std = initial_std
        
        # Encode state_array [dim_state] into latent space
        self.state_encoder = nn.Sequential(
            nn.Linear(dim_state, 32),
            nn.ReLU(),
            nn.Linear(32, 256),
            nn.ReLU(),
            nn.Linear(256, dim_latent),
            nn.ReLU()
        )
        
        # Encode figure_array (yu, yl, mwu, mwl) [n_interp_points, 4] into latent space (with CNN)
        # Input needs to be transposed to [batch_size, 4, n_interp_points] for Conv1d
        self.figure_encoder = nn.Sequential(
            nn.Conv1d(4, 16, kernel_size=3, stride=1, padding=1),
            nn.ReLU(),
            nn.Conv1d(16, 32, kernel_size=3, stride=1, padding=1),
            nn.ReLU(),
            nn.Conv1d(32, 64, kernel_size=3, stride=1, padding=1),
            nn.ReLU(),
            nn.AdaptiveAvgPool1d(1),  # Global average pooling instead of flatten
            nn.Flatten(),
            nn.Linear(64, dim_latent),
            nn.ReLU()
        )
        
        # Shared feature extractor (combines both latent representations)
        self.shared_net = nn.Sequential(
            nn.Linear(2 * dim_latent, dim_hidden),  # Combined latent features
            nn.ReLU(),
            nn.Linear(dim_hidden, dim_hidden),
            nn.ReLU(),
        )
        
        # Actor head (policy network)
        self.actor_mean = nn.Sequential(
            nn.Linear(dim_hidden, dim_hidden),
            nn.ReLU(),
            nn.Linear(dim_hidden, dim_action),
            nn.Tanh()  # Output in [-1, 1]
        )
        
        # Actor std (learnable std deviation)
        self.actor_log_std = nn.Parameter(torch.ones(dim_action) * np.log(initial_std))
        
        # Critic head (value network)
        self.critic = nn.Sequential(
            nn.Linear(dim_hidden, dim_hidden),
            nn.ReLU(),
            nn.Linear(dim_hidden, 32),
            nn.ReLU(),
            nn.Linear(32, 1)
        )


def main(device='auto', resume=False):
    '''Main training loop using refactored multiprocessing implementation'''
    
    # Number of parallel environments (can be increased with new reliable implementation)
    n_envs = 200
    
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
        lr=4e-4,
        gamma=0.99,
        gae_lambda=0.95,
        clip_epsilon=0.15,
        value_loss_coef=0.5,
        entropy_coef=0.01,
        max_grad_norm=0.5,
        n_epochs=10,
        batch_size=400,
        n_steps=10,
        dim_latent=64,
        dim_hidden=512,
        n_interp_points=101,
        initial_action_std=0.3,
        device=device,
        max_processes=50,
        actor_critic_class_fn=ActorCritic_Custom
    )
    
    # ppo_agent.reward_range_for_plot = [-5, 5]
    
    save_path = os.path.join(path, 'ppo_fig_global_model.pt')
    
    if resume and os.path.exists(save_path):
        ppo_agent.load_model(save_path)
        
        ppo_agent.actor_critic.actor_log_std.data = torch.ones_like(
            ppo_agent.actor_critic.actor_log_std, device=device
        ) * np.log(ppo_agent.initial_action_std)
    
    # Train the agent
    try:
        ppo_agent.train(
            total_time_steps=int(1e6),
            log_interval=1,
            save_interval=10,
            eval_interval=10,
            save_path=os.path.join(path, 'ppo_fig_global_model.pt'),
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
    
    GPU_ID = 0
    device = f'cuda:{GPU_ID}' if torch.cuda.is_available() else 'cpu'

    main(device=device, resume=True)
    