'''
This script is used to test the trained PPO agent.
'''

import os
import sys
import numpy as np
import torch

from pyTSFoil.environment.utils import TSFoilEnv_FigState_BumpAction
from pyTSFoil.environment.basic import BumpModificationAction, FigureState
from model.ppo_mp import PPO_FigState_BumpAction_MultiEnv
from train_ppo_mp import evaluate_trained_agent, EnvFactory

path = os.path.dirname(os.path.abspath(__file__))

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
    
    ppo_agent = PPO_FigState_BumpAction_MultiEnv(
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
        eval_results = evaluate_trained_agent(ppo_agent, n_eval_episodes=3, fig_name='tsfoil_gym_render_test')
        
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
    print("Testing Trained PPO Agent")
    print("="*30)
    main()
