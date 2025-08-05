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
from pyTSFoil.environment.basic import BumpModificationAction, FigureState
from model.ppo_mp import PPO_FigState_BumpAction_MultiEnv

path = os.path.dirname(os.path.abspath(__file__))

def create_env_with_id(worker_id=None):
    '''
    Factory function to create a new environment instance with unique worker ID
    
    This function is called by each worker process to create its own
    isolated environment instance. Each worker gets completely separate
    PyTSFoil/Fortran data to avoid conflicts.
    '''
    # Create sample airfoil
    x, y = np.loadtxt(os.path.join(path, 'rae2822.dat'), skiprows=1).T
    airfoil_coordinates = np.column_stack((x, y))
    
    # Custom action class
    action_class = BumpModificationAction()
    
    action_class.action_dict['UBL']['bound'] = [0.01, 0.8]
    action_class.action_dict['UBH']['bound'] = [-0.002, 0.002]
    action_class.action_dict['UBH']['min_increment'] = 0.001
    action_class.action_dict['UBW']['bound'] = [0.4, 0.8]
    
    action_class.action_dict['LBL']['bound'] = [0.01, 0.8]
    action_class.action_dict['LBH']['bound'] = [-0.002, 0.002]
    action_class.action_dict['LBH']['min_increment'] = 0.001
    action_class.action_dict['LBW']['bound'] = [0.4, 0.8]
    
    action_class._update_action_bounds(action_class.action_dict)

    # Create unique output directory for each worker to avoid file conflicts
    if worker_id is not None:
        worker_output_dir = os.path.join(path, 'temp', f'worker_{worker_id}')
        os.makedirs(worker_output_dir, exist_ok=True)
    else:
        worker_output_dir = path

    # Create environment instance
    return TSFoilEnv_FigState_BumpAction(
        airfoil_coordinates=airfoil_coordinates,
        output_dir=worker_output_dir,
        render_mode='none',  # No rendering for parallel environments
        action_class=action_class,
        n_max_step=10,
        critical_reward=0.0,
    )

# Picklable environment factory functions for each worker
class EnvFactory:
    def __init__(self, worker_id):
        self.worker_id = worker_id
    
    def __call__(self):
        return create_env_with_id(self.worker_id)

def main():
    '''Main training loop using refactored multiprocessing implementation'''
    
    # Number of parallel environments (can be increased with new reliable implementation)
    n_envs = 50
    
    # Create list of environment factory functions with unique worker IDs
    env_fns = [EnvFactory(i) for i in range(n_envs)]
    
    print(f"Creating PPO agent with {n_envs} parallel environments...")
    print("Using new pool-based multiprocessing implementation for better reliability")
    
    # Create specialized PPO agent with multiple environments
    ppo_agent = PPO_FigState_BumpAction_MultiEnv(
        env_fns=env_fns,
        lr=3e-4,
        gamma=0.99,
        gae_lambda=0.95,
        clip_epsilon=0.2,
        value_loss_coef=0.5,
        entropy_coef=0.01,
        max_grad_norm=0.5,
        n_epochs=10,
        batch_size=64,
        n_steps=10,
        dim_latent=64,
        dim_hidden=256,
        n_interp_points=101,
        device='auto'
    )
    
    # Train the agent
    print("Starting training...")
    try:
        ppo_agent.train(
            total_time_steps=20000,  # Increased for new reliable implementation
            log_interval=1,
            save_interval=10,
            save_path=os.path.join(path, 'ppo_fig_bump_model.pt'),
            plot_training=True,
            plot_path=os.path.join(path, 'training_progress.png')
        )
        print("Training completed successfully!")
    except Exception as e:
        print(f"Training failed with error: {e}")
        raise
    
    # Create a separate environment for evaluation (with rendering enabled)
    x, y = np.loadtxt(os.path.join(path, 'rae2822.dat'), skiprows=1).T
    airfoil_coordinates = np.column_stack((x, y))
    
    # Create evaluation environment with rendering enabled
    action_class = BumpModificationAction()
    action_class.action_dict['UBL']['bound'] = [0.01, 0.8]
    action_class.action_dict['UBH']['bound'] = [-0.002, 0.002]
    action_class.action_dict['UBH']['min_increment'] = 0.001
    action_class.action_dict['UBW']['bound'] = [0.4, 0.8]
    action_class.action_dict['LBL']['bound'] = [0.01, 0.8]
    action_class.action_dict['LBH']['bound'] = [-0.002, 0.002]
    action_class.action_dict['LBH']['min_increment'] = 0.001
    action_class.action_dict['LBW']['bound'] = [0.4, 0.8]
    action_class._update_action_bounds(action_class.action_dict)
    
    eval_env = TSFoilEnv_FigState_BumpAction(
        airfoil_coordinates=airfoil_coordinates,
        output_dir=path,
        render_mode='both',  # Enable rendering for evaluation
        action_class=action_class
    )
    
    # Evaluate the trained agent
    print("\nEvaluating trained agent...")
    n_eval_episodes = 3
    episode_rewards = []
    episode_lengths = []
    final_cl_values = []
    final_cd_values = []
    
    for episode in range(n_eval_episodes):
        eval_env.reset()
        state_array, figure_array = eval_env._get_observation_for_RL(n_interp_points=ppo_agent.n_interp_points)
        
        episode_reward = 0
        episode_length = 0
        done = False
        
        print(f"\nEpisode {episode + 1}:")
        while not done:
            # Get action from trained policy
            action_scaled = ppo_agent.get_action(state_array, figure_array, deterministic=True)
            
            # Step environment
            obs, reward, done, info = eval_env.step(action_scaled)
            state_array, figure_array = eval_env._get_observation_for_RL(n_interp_points=ppo_agent.n_interp_points)
            
            episode_reward += reward
            episode_length += 1
            
            print(f"  Step {episode_length}: Action = {action_scaled}, Reward = {reward:.4f}")
            
            if done:
                final_cl_values.append(info.get('cl', 0.0))
                final_cd_values.append(info.get('cd', 0.001))
                episode_rewards.append(episode_reward)
                episode_lengths.append(episode_length)
                eval_env.render()
                print(f"  Episode finished: Total reward = {episode_reward:.4f}, Length = {episode_length}")
                print(f"  Final CL = {final_cl_values[-1]:.4f}, CD = {final_cd_values[-1]:.6f}")
    
    print("\nEvaluation Results:")
    print(f"Mean Reward: {np.mean(episode_rewards):.4f} ± {np.std(episode_rewards):.4f}")
    print(f"Mean Length: {np.mean(episode_lengths):.2f} ± {np.std(episode_lengths):.2f}")
    print(f"Mean CL: {np.mean(final_cl_values):.4f} ± {np.std(final_cl_values):.4f}")
    print(f"Mean CD: {np.mean(final_cd_values):.6f} ± {np.std(final_cd_values):.6f}")
    
    # Example of using the trained policy directly
    print("\nExample direct policy usage:")
    eval_env.reset()
    state_array, figure_array = eval_env._get_observation_for_RL(n_interp_points=ppo_agent.n_interp_points)
    
    print(f"State array shape: {state_array.shape}")
    print(f"Figure array shape: {figure_array.shape}")
    print(f"State features: {state_array[:5]}...")  # First 5 features
    
    # Get action from policy
    action_scaled = ppo_agent.get_action(state_array, figure_array, deterministic=True)
    print(f"Action (scaled): {action_scaled}")
    print(f"Action names: {eval_env.action_class.action_name}")
    
    # Step environment with action
    next_obs, reward, done, info = eval_env.step(action_scaled)
    print(f"Reward: {reward:.4f}")
    print(f"CL: {info.get('cl', 0.0):.4f}, CD: {info.get('cd', 0.0):.6f}")
    
    eval_env.close()
    
    # Clean up CUDA tensors and force garbage collection
    if torch.cuda.is_available():
        torch.cuda.empty_cache()
        torch.cuda.synchronize()
    
    print("\nTraining and evaluation completed!")
    print("Note: Multiprocessing cleanup is automatic with the new pool-based implementation")
    print("Temporary worker directories created for parallel execution can be cleaned up if needed")
    
    # Optional: Clean up worker directories
    # cleanup_choice = input("\nClean up temporary worker directories? (y/n): ").lower().strip()
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
    
    main()
    