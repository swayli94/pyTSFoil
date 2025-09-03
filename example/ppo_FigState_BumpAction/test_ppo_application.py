'''
This script is used to test the trained PPO agent.
'''

import os
import sys

path = os.path.dirname(os.path.abspath(__file__))
# Add project root to Python path for multi-branch development
project_root = os.path.abspath(os.path.join(path, '..', '..'))
if project_root not in sys.path:
    sys.path.insert(0, project_root)
    
import numpy as np
import torch

from model.ppo_mp import PPO_FigState_MultiEnv
from train_ppo_mp import create_env_with_id, EnvFactory


def load_trained_agent(model_path, device='auto'):
    '''
    Load the trained PPO agent from saved model file
    
    Args:
        model_path: Path to the saved model file
        device: Device to load the model on ('auto', 'cpu', or 'cuda:0')
    
    Returns:
        Loaded PPO agent ready for evaluation
    '''
    print(f"Loading trained agent from: {model_path}")
    
    # Determine device
    if device == 'auto':
        device = 'cuda:0' if torch.cuda.is_available() else 'cpu'
    
    # Create PPO agent with same parameters as training (but without env_fns for loading)
    # We'll create a dummy env_fn list just for initialization
    env_fns = [EnvFactory(0)]
    
    ppo_agent = PPO_FigState_MultiEnv(
        env_fns=env_fns,  # Dummy function, not used for evaluation
        lr=1e-5,
        gamma=0.99,
        gae_lambda=0.95,
        clip_epsilon=0.1,
        value_loss_coef=0.1,
        entropy_coef=0.01,
        max_grad_norm=0.5,
        n_epochs=20, 
        batch_size=500,
        n_steps=5,
        dim_latent=32,
        dim_hidden=512,
        n_interp_points=101,
        device=device
    )
    
    # Load the saved model weights
    if os.path.exists(model_path):
        # Set weights_only=False to handle collections.deque in the saved model
        # This is safe since we trust our own trained model
        checkpoint = torch.load(model_path, map_location=device, weights_only=False)
        ppo_agent.actor_critic.load_state_dict(checkpoint['actor_critic_state_dict'])
        print("Successfully loaded trained model weights!")
    else:
        raise FileNotFoundError(f"Model file not found: {model_path}")
    
    return ppo_agent


def evaluate_trained_agent(ppo_agent, n_eval_episodes=3, fig_name='tsfoil_gym_render_test'):
    '''
    Evaluate the trained PPO agent using a separate environment with rendering enabled
    
    Args:
        ppo_agent: Trained PPO agent
        n_eval_episodes: Number of evaluation episodes to run
        
    Returns:
        dict: Evaluation results containing mean rewards, lengths, CL, and CD values
    '''
    print("\nEvaluating trained agent...")
    
    # Create evaluation environment with rendering enabled using the factory function
    eval_env = create_env_with_id(
        worker_id=None,  # No worker ID for evaluation (uses main output directory)
        render_mode='save',  # Enable rendering for evaluation
    )
    
    episode_rewards = []
    episode_lengths = []
    final_cl_values = []
    final_cd_values = []
    
    for episode in range(n_eval_episodes):
        
        eval_env.reset()
        state_array, figure_array = eval_env._get_observation_for_RL(n_interp_points=ppo_agent.n_interp_points)
        
        done = False
        deterministic = episode == 0
        
        print(f"\nEpisode {episode + 1}:")
        i_step = 0
        while not done:
            # Get action from trained policy
            action_unscaled = ppo_agent.get_action(state_array, figure_array, deterministic=deterministic)
            
            # Step environment
            obs, reward, done, info = eval_env.step(action_unscaled)
            state_array, figure_array = eval_env._get_observation_for_RL(n_interp_points=ppo_agent.n_interp_points)
            
            i_step += 1
            print(f"  Step {i_step}: Action = {action_unscaled}, Reward = {reward:.4f}, CL = {info.get('cl', 0.0):.4f}, CD = {info.get('cd', 0.001):.6f}")
            
            if done:
                final_cl_values.append(info.get('cl', 0.0))
                final_cd_values.append(info.get('cd', 0.001))
                episode_rewards.append(eval_env.total_reward)
                episode_lengths.append(eval_env.get_trajectory_length())
                
                eval_env.render_fig_fname = f'{fig_name}_{episode}.png'
                eval_env.render()
                
                print(f"  Episode finished: Total reward = {eval_env.total_reward:.4f}, Length = {eval_env.get_trajectory_length()}")
                print(f"  Final CL = {final_cl_values[-1]:.4f}, CD = {final_cd_values[-1]:.6f}")
    
    # Print evaluation results
    print("\nEvaluation Results:")
    print(f"Mean Reward: {np.mean(episode_rewards):.4f} ± {np.std(episode_rewards):.4f}")
    print(f"Mean Length: {np.mean(episode_lengths):.2f} ± {np.std(episode_lengths):.2f}")
    print(f"Mean CL: {np.mean(final_cl_values):.4f} ± {np.std(final_cl_values):.4f}")
    print(f"Mean CD: {np.mean(final_cd_values):.6f} ± {np.std(final_cd_values):.6f}")
        
    eval_env.close()
    
    # Return evaluation results
    return {
        'episode_rewards': episode_rewards,
        'episode_lengths': episode_lengths,
        'final_cl_values': final_cl_values,
        'final_cd_values': final_cd_values,
        'mean_reward': np.mean(episode_rewards),
        'mean_length': np.mean(episode_lengths),
        'mean_cl': np.mean(final_cl_values),
        'mean_cd': np.mean(final_cd_values)
    }


def main():
    '''Main function to test the trained PPO agent'''
    
    # Set random seed for reproducibility
    np.random.seed(42)
    torch.manual_seed(42)
    
    # Path to the saved model
    model_path = os.path.join(path, 'ppo_fig_bump_model.pt')
    
    # Determine device
    GPU_ID = 0
    device = f'cuda:{GPU_ID}' if torch.cuda.is_available() else 'cpu'
    print(f"Using device: {device}")
    
    try:
        # Load the trained agent
        ppo_agent = load_trained_agent(model_path, device=device)
        
        # Evaluate the trained agent
        print("\nStarting evaluation of trained PPO agent...")
        eval_results = evaluate_trained_agent(ppo_agent, n_eval_episodes=3)
        
        # Print summary of results
        print("\n" + "="*50)
        print("EVALUATION SUMMARY")
        print("="*50)
        print(f"Episodes evaluated: 3")
        print(f"Mean reward: {eval_results['mean_reward']:.4f}")
        print(f"Mean episode length: {eval_results['mean_length']:.2f}")
        print(f"Mean CL: {eval_results['mean_cl']:.4f}")
        print(f"Mean CD: {eval_results['mean_cd']:.6f}")
        print(f"Mean CL/CD ratio: {eval_results['mean_cl']/eval_results['mean_cd']:.2f}")
        print("="*50)
        
    except FileNotFoundError as e:
        print(f"Error: {e}")
        print("Please make sure you have trained the model first by running train_ppo_mp.py")
    except Exception as e:
        print(f"An error occurred during evaluation: {e}")
        raise

if __name__ == "__main__":
    
    np.set_printoptions(formatter={'float': '{:8.4f}'.format})
    
    main()
