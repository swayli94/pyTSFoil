'''
Example usage of PPO with TSFoilEnv_FigState_BumpAction_MultiEnv
'''
import os
import numpy as np
import torch

# Import the classes
from pyTSFoil.environment.utils import TSFoilEnv_FigState_BumpAction
from pyTSFoil.environment.basic import BumpModificationAction, FigureState
from model.ppo_mp import PPO_FigState_BumpAction_MultiEnv

path = os.path.dirname(os.path.abspath(__file__))
print('path: ', path)

np.set_printoptions(formatter={'float': '{:8.4f}'.format})

def create_env():
    '''Factory function to create a new environment instance'''
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

    # Create environment instance
    return TSFoilEnv_FigState_BumpAction(
        airfoil_coordinates=airfoil_coordinates,
        output_dir=path,
        render_mode='none',  # No rendering for parallel environments
        action_class=action_class
    )

def main():
    '''Main training loop'''
    
    # Number of parallel environments
    n_envs = 4
    
    # Create list of environment factory functions
    env_fns = [create_env for _ in range(n_envs)]
    
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
        n_steps=20,  # Increased steps since we have multiple envs
        dim_latent=64,
        dim_hidden=256,
        n_interp_points=101,
        device='auto'
    )
    
    # Train the agent
    ppo_agent.train(
        total_time_steps=100,  # Small number for testing
        log_interval=5,
        save_interval=50,
        save_path=os.path.join(path, 'ppo_fig_bump_model.pt'),
        plot_training=True,
        plot_path=os.path.join(path, 'training_progress.png')
    )
    
    # Create a separate environment for evaluation (with rendering enabled)
    x, y = np.loadtxt(os.path.join(path, 'rae2822.dat'), skiprows=1).T
    airfoil_coordinates = np.column_stack((x, y))
    
    eval_env = TSFoilEnv_FigState_BumpAction(
        airfoil_coordinates=airfoil_coordinates,
        output_dir=path,
        render_mode='both',  # Enable rendering for evaluation
        action_class=ppo_agent.action_class
    )
    
    # Manual evaluation since multiprocessing version doesn't have built-in evaluate method
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
    
    # Explicitly shutdown the multiprocessing environments
    ppo_agent.shutdown()

if __name__ == "__main__":
    main()
    