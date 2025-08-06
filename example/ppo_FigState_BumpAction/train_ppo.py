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

np.set_printoptions(formatter={'float': '{:8.4f}'.format})

def main():
    '''Main training loop'''
    
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

    # Create environment
    env = TSFoilEnv_FigState_BumpAction(
        airfoil_coordinates=airfoil_coordinates,
        output_dir=path,
        render_mode='both',  # or 'display' or 'both',
        action_class=action_class
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
        save_path=os.path.join(path, 'ppo_fig_bump_model.pt'),
        plot_training=True,
        plot_path=os.path.join(path, 'training_progress.png')
    )
    
    # Evaluate the trained agent
    print("\nEvaluating trained agent...")
    eval_results = ppo_agent.evaluate(n_episodes=1, render=True)
    
    print("\nEvaluation Results:")
    print(f"Mean Reward: {eval_results['mean_reward']:.4f} ± {eval_results['std_reward']:.4f}")
    print(f"Mean Length: {eval_results['mean_length']:.2f} ± {eval_results['std_length']:.2f}")
    print(f"Mean CL: {eval_results['mean_cl']:.4f} ± {eval_results['std_cl']:.4f}")
    print(f"Mean CD: {eval_results['mean_cd']:.6f} ± {eval_results['std_cd']:.6f}")
    print(f"Mean L/D: {eval_results['mean_ld_ratio']:.2f} ± {eval_results['std_ld_ratio']:.2f}")

if __name__ == "__main__":
    main()
    