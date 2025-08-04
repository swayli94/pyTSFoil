'''
Example usage of PPO with TSFoilEnv_FigState_BumpAction
'''
import os
import numpy as np
import torch

# Import the classes
from pyTSFoil.environment.utils import TSFoilEnv_FigState_BumpAction
from pyTSFoil.environment.basic import BumpModificationAction, FigureState
from model.ppo import PPO_FigState_BumpAction

path = os.path.dirname(os.path.abspath(__file__))
print('path: ', path)

np.set_printoptions(formatter={'float': '{:.4f}'.format})

def main():
    '''Main training loop'''
    
    # Create sample airfoil
    x, y = np.loadtxt(os.path.join(path, 'rae2822.dat'), skiprows=1).T
    airfoil_coordinates = np.column_stack((x, y))
    
    # Create environment
    env = TSFoilEnv_FigState_BumpAction(
        airfoil_coordinates=airfoil_coordinates,
        output_dir=path,
        render_mode='both',  # or 'display' or 'both'
    )
    
    # Create specialized PPO agent
    ppo_agent = PPO_FigState_BumpAction(
        env=env,
        lr=3e-4,
        gamma=0.99,
        gae_lambda=0.95,
        clip_epsilon=0.2,
        value_loss_coef=0.5,
        entropy_coef=0.01,
        max_grad_norm=0.5,
        n_epochs=10,
        batch_size=64,
        n_steps=5,
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
        save_path=os.path.join(path, 'ppo_figstate_bump_model.pt'),
        plot_training=True
    )
    
    # Evaluate the trained agent
    print("\nEvaluating trained agent...")
    eval_results = ppo_agent.evaluate(n_episodes=5, render=True)
    
    print("\nEvaluation Results:")
    print(f"Mean Reward: {eval_results['mean_reward']:.4f} ± {eval_results['std_reward']:.4f}")
    print(f"Mean Length: {eval_results['mean_length']:.2f} ± {eval_results['std_length']:.2f}")
    print(f"Mean CL: {eval_results['mean_cl']:.4f} ± {eval_results['std_cl']:.4f}")
    print(f"Mean CD: {eval_results['mean_cd']:.6f} ± {eval_results['std_cd']:.6f}")
    print(f"Mean L/D: {eval_results['mean_ld_ratio']:.2f} ± {eval_results['std_ld_ratio']:.2f}")
    
    # Example of using the trained policy directly
    print("\nExample direct policy usage:")
    obs = env.reset()
    state_array, figure_array = ppo_agent._get_state_from_env()
    
    print(f"State array shape: {state_array.shape}")
    print(f"Figure array shape: {figure_array.shape}")
    print(f"State features: {state_array[:5]}...")  # First 5 features
    
    # Get action from policy
    action_scaled = ppo_agent.get_action(state_array, figure_array, deterministic=True)
    print(f"Action (scaled): {action_scaled}")
    print(f"Action names: {env.Action.action_name}")
    
    # Step environment with action
    next_obs, reward, done, info = env.step(action_scaled)
    print(f"Reward: {reward:.4f}")
    print(f"CL: {info.get('cl', 0.0):.4f}, CD: {info.get('cd', 0.0):.6f}")
    
    env.close()

if __name__ == "__main__":
    main()
    