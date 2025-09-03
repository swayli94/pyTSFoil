'''
This script is used to test the trained PPO agent on all reference airfoils.

The script evaluates the trained PPO agent on all 26 reference airfoils from the 
database, generating one environment per airfoil. Each airfoil is evaluated for 
one episode and results are collected, analyzed, and saved.

Key features:
- Loads trained PPO model
- Evaluates on all reference airfoils (26 airfoils)
- Generates rendered figures for each evaluation
- Provides comprehensive performance analysis
- Ranks airfoils by CL/CD ratio performance
- Saves detailed results to file
- Organizes all output files in 'test_ppo' folder
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
from train_ppo_multi_start import create_env_with_id, EnvFactory, AirfoilDatabase, ActorCritic_Custom


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
    
    eval_env = create_env_with_id(
        worker_id=None,  # No worker ID for evaluation (uses main output directory)
        render_mode='save',  # Enable rendering for evaluation
    )
    
    ppo_agent = PPO_FigState_MultiEnv(
        env_fns=env_fns,
        env_eval=eval_env,
        lr=1e-6,
        gamma=0.99,
        gae_lambda=0.95,
        clip_epsilon=0.2,
        value_loss_coef=0.2,
        entropy_coef=0.01,
        max_grad_norm=0.5,
        n_epochs=10,
        batch_size=2000,
        n_steps=10,
        dim_latent=128,
        dim_hidden=1024,
        n_interp_points=101,
        initial_action_std=0.2,
        device=device,
        max_processes=50,
        actor_critic_class_fn=ActorCritic_Custom
    )
    
    # Load the saved model weights
    if os.path.exists(model_path):
        ppo_agent.load_model(model_path)
        print("Successfully loaded trained model weights!")
    else:
        raise FileNotFoundError(f"Model file not found: {model_path}")
    
    return ppo_agent


def evaluate_all_airfoils(ppo_agent, n_eval_episodes=1, fig_name_prefix='airfoil'):
    '''
    Evaluate the trained PPO agent on all airfoils
    
    Args:
        ppo_agent: Trained PPO agent
        n_eval_episodes: Number of evaluation episodes to run per airfoil
        fig_name_prefix: Prefix for saved figure filenames
        
    Returns:
        dict: Evaluation results for all airfoils
    '''
    print("\nEvaluating trained agent on all airfoils...")
    
    # Create test_ppo directory for output files
    test_ppo_dir = os.path.join(path, 'test_ppo')
    os.makedirs(test_ppo_dir, exist_ok=True)
    print(f"All figures and results will be saved to: {test_ppo_dir}")
    
    # Load airfoil database
    database = AirfoilDatabase(fname_database=os.path.join(path, 'selected-airfoils-cst.dat'))
    n_airfoils = len(database.airfoils)
    
    print(f"Found {n_airfoils} airfoils in database")
    
    # Storage for all results
    all_results = {}
    summary_results = {
        'airfoil_names': [],
        'airfoil_ids': [],
        'mean_rewards': [],
        'mean_lengths': [],
        'mean_cl_values': [],
        'mean_cd_values': [],
        'cl_cd_ratios': []
    }
    
    for airfoil_id in range(n_airfoils):
        airfoil_name = database.airfoils[airfoil_id]['name']
        print(f"\n{'='*60}")
        print(f"Evaluating Airfoil {airfoil_id}: {airfoil_name}")
        print(f"{'='*60}")
        
        # Create evaluation environment for this specific airfoil
        eval_env = create_env_with_id(
            worker_id=airfoil_id,  # Use airfoil ID as worker ID
            render_mode='save',     # Enable rendering for evaluation
        )
        
        # Storage for this airfoil's episodes
        episode_rewards = []
        episode_lengths = []
        final_cl_values = []
        final_cd_values = []
        
        for episode in range(n_eval_episodes):
            eval_env.reset()
            state_array, figure_array = eval_env._get_observation_for_RL(n_interp_points=ppo_agent.n_interp_points)
            
            done = False
            deterministic = True  # Use deterministic policy for evaluation
            
            print(f"  Episode {episode + 1}:")
            i_step = 0
            while not done:
                # Get action from trained policy
                action_unscaled = ppo_agent.get_action(state_array, figure_array, deterministic=deterministic)
                
                # Step environment
                obs, reward, done, info = eval_env.step(action_unscaled)
                state_array, figure_array = eval_env._get_observation_for_RL(n_interp_points=ppo_agent.n_interp_points)
                
                i_step += 1
                print(f"    Step {i_step}: Action = {action_unscaled}, Reward = {reward:.4f}, CL = {info.get('cl', 0.0):.4f}, CD = {info.get('cd', 0.001):.6f}")
                
                if done:
                    final_cl_values.append(info.get('cl', 0.0))
                    final_cd_values.append(info.get('cd', 0.001))
                    episode_rewards.append(eval_env.total_reward)
                    episode_lengths.append(eval_env.get_trajectory_length())
                    
                    # Save rendered figure with unique name
                    eval_env.render_fig_fname = os.path.join(test_ppo_dir, f'{fig_name_prefix}_{airfoil_id:02d}_{episode}.png')
                    eval_env.render()
                    
                    print(f"    Episode finished: Total reward = {eval_env.total_reward:.4f}, Length = {eval_env.get_trajectory_length()}")
                    print(f"    Final CL = {final_cl_values[-1]:.4f}, CD = {final_cd_values[-1]:.6f}")
        
        eval_env.close()
        
        # Calculate statistics for this airfoil
        mean_reward = np.mean(episode_rewards)
        mean_length = np.mean(episode_lengths)
        mean_cl = np.mean(final_cl_values)
        mean_cd = np.mean(final_cd_values)
        cl_cd_ratio = mean_cl / mean_cd if mean_cd > 0 else 0.0
        
        # Store detailed results
        all_results[airfoil_name] = {
            'airfoil_id': airfoil_id,
            'episode_rewards': episode_rewards,
            'episode_lengths': episode_lengths,
            'final_cl_values': final_cl_values,
            'final_cd_values': final_cd_values,
            'mean_reward': mean_reward,
            'mean_length': mean_length,
            'mean_cl': mean_cl,
            'mean_cd': mean_cd,
            'cl_cd_ratio': cl_cd_ratio
        }
        
        # Store summary results
        summary_results['airfoil_names'].append(airfoil_name)
        summary_results['airfoil_ids'].append(airfoil_id)
        summary_results['mean_rewards'].append(mean_reward)
        summary_results['mean_lengths'].append(mean_length)
        summary_results['mean_cl_values'].append(mean_cl)
        summary_results['mean_cd_values'].append(mean_cd)
        summary_results['cl_cd_ratios'].append(cl_cd_ratio)
        
        # Print summary for this airfoil
        print(f"  Results for {airfoil_name}:")
        print(f"    Mean Reward: {mean_reward:.4f}")
        print(f"    Mean Length: {mean_length:.2f}")
        print(f"    Mean CL: {mean_cl:.4f}")
        print(f"    Mean CD: {mean_cd:.6f}")
        print(f"    CL/CD Ratio: {cl_cd_ratio:.2f}")
    
    # Print overall summary
    print(f"\n{'='*80}")
    print("OVERALL EVALUATION SUMMARY")
    print(f"{'='*80}")
    print(f"Evaluated {n_airfoils} reference airfoils with {n_eval_episodes} episode(s) each")
    print(f"Total episodes: {n_airfoils * n_eval_episodes}")
    print()
    
    # Sort by CL/CD ratio for better presentation
    sorted_indices = np.argsort(summary_results['cl_cd_ratios'])[::-1]  # Descending order
    
    print(f"{'Rank':<4} {'ID':<3} {'Airfoil Name':<15} {'Reward':<8} {'Length':<6} {'CL':<7} {'CD':<8} {'CL/CD':<7}")
    print("-" * 80)
    
    for rank, idx in enumerate(sorted_indices, 1):
        print(f"{rank:<4} {summary_results['airfoil_ids'][idx]:<3} {summary_results['airfoil_names'][idx]:<15} "
              f"{summary_results['mean_rewards'][idx]:<8.4f} {summary_results['mean_lengths'][idx]:<6.2f} "
              f"{summary_results['mean_cl_values'][idx]:<7.4f} {summary_results['mean_cd_values'][idx]:<8.6f} "
              f"{summary_results['cl_cd_ratios'][idx]:<7.2f}")
    
    print()
    print(f"Best performing airfoil (highest CL/CD): {summary_results['airfoil_names'][sorted_indices[0]]} "
          f"(CL/CD = {summary_results['cl_cd_ratios'][sorted_indices[0]]:.2f})")
    print(f"Overall mean CL/CD ratio: {np.mean(summary_results['cl_cd_ratios']):.2f} ± {np.std(summary_results['cl_cd_ratios']):.2f}")
    print(f"{'='*80}")
    
    return {
        'detailed_results': all_results,
        'summary_results': summary_results
    }


def main():
    '''Main function to test the trained PPO agent'''
    
    # Set random seed for reproducibility
    np.random.seed(42)
    torch.manual_seed(42)
    
    # Path to the saved model
    model_path = os.path.join(path, 'ppo_fig_global_model.pt')
    
    # Determine device
    GPU_ID = 0
    device = f'cuda:{GPU_ID}' if torch.cuda.is_available() else 'cpu'
    print(f"Using device: {device}")
    
    try:
        # Load the trained agent
        ppo_agent = load_trained_agent(model_path, device=device)
        
        # Evaluate the trained agent on all airfoils
        print("\nStarting evaluation of trained PPO agent on all airfoils...")
        all_eval_results = evaluate_all_airfoils(
            ppo_agent, 
            n_eval_episodes=1,  # 1 episode per airfoil for comprehensive evaluation
            fig_name_prefix='airfoil'
        )
        
        # Additional analysis can be done with the returned results
        summary_results = all_eval_results['summary_results']
        detailed_results = all_eval_results['detailed_results']
        
        # Save results to file for further analysis
        test_ppo_dir = os.path.join(path, 'test_ppo')
        os.makedirs(test_ppo_dir, exist_ok=True)
        results_file = os.path.join(test_ppo_dir, 'evaluation_results.txt')
        with open(results_file, 'w') as f:
            f.write("Evaluation Results for All Airfoils\n")
            f.write("=" * 50 + "\n\n")
            
            for i, airfoil_name in enumerate(summary_results['airfoil_names']):
                f.write(f"Airfoil: {airfoil_name} (ID: {summary_results['airfoil_ids'][i]})\n")
                f.write(f"  Mean Reward: {summary_results['mean_rewards'][i]:.4f}\n")
                f.write(f"  Mean Length: {summary_results['mean_lengths'][i]:.2f}\n")
                f.write(f"  Mean CL: {summary_results['mean_cl_values'][i]:.4f}\n")
                f.write(f"  Mean CD: {summary_results['mean_cd_values'][i]:.6f}\n")
                f.write(f"  CL/CD Ratio: {summary_results['cl_cd_ratios'][i]:.2f}\n\n")
        
        print(f"\nDetailed results saved to: {results_file}")
        
    except FileNotFoundError as e:
        print(f"Error: {e}")
        print("Please make sure you have trained the model first by running train_ppo_mp.py")
    except Exception as e:
        print(f"An error occurred during evaluation: {e}")
        raise


if __name__ == "__main__":
    
    np.set_printoptions(formatter={'float': '{:8.4f}'.format}, linewidth=np.inf)
    
    main()
