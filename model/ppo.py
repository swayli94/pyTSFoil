'''
Proximal Policy Optimization (PPO) algorithm for airfoil design
'''

import torch
import torch.nn as nn
import torch.optim as optim
from torch.distributions import Normal
import numpy as np
import matplotlib.pyplot as plt
from typing import Tuple, Optional, List
from collections import deque

from pyTSFoil.environment.utils import TSFoilEnv_FigState_BumpAction


class ActorCritic(nn.Module):
    '''
    Actor-Critic network for PPO
    
    Actor: outputs mean and std for continuous action distribution
    Critic: outputs state value
    
    The FigureState contains two parts:
    - state_array: state variables
    - figure_array: figure array, [yu, yl, mwu, mwl]
    
    Parameters:
    -----------
    dim_state: int
        dimension of the state space
    dim_action: int
        dimension of the action space
    dim_latent: int
        dimension of the latent space
    dim_hidden: int
        dimension of the hidden layers
    n_interp_points: int
        number of interpolation points for the figure array
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
            nn.Linear(32, dim_latent),
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
            nn.Linear(dim_hidden, 1)
        )
        
    def forward(self, state_array: torch.Tensor, figure_array: torch.Tensor) -> Tuple[torch.Tensor, torch.Tensor, torch.Tensor]:
        """
        Forward pass of the Actor-Critic network
        
        Parameters:
        -----------
        state_array: torch.Tensor [batch_size, dim_state]
            parametric state features
        figure_array: torch.Tensor [batch_size, n_interp_points, 4]
            airfoil geometry and Mach data
            
        Returns:
        --------
        action_mean: torch.Tensor [batch_size, dim_action]
            mean of the action distribution
        action_std: torch.Tensor [batch_size, dim_action]
            standard deviation of the action distribution
        value: torch.Tensor [batch_size, 1]
            value function estimate
        """
        # Encode state array
        state_latent = self.state_encoder(state_array)
        
        # Encode figure array (need to transpose for Conv1d)
        # figure_array: [batch_size, n_interp_points, 4] -> [batch_size, 4, n_interp_points]
        figure_transposed = figure_array.transpose(1, 2)
        figure_latent = self.figure_encoder(figure_transposed)
        
        # Combine latent representations
        combined_latent = torch.cat([state_latent, figure_latent], dim=1)
        shared_features = self.shared_net(combined_latent)
        
        # Actor outputs
        action_mean = self.actor_mean(shared_features)
        action_std = torch.exp(self.actor_log_std.expand_as(action_mean))
        
        # Critic output
        value = self.critic(shared_features)
        
        return action_mean, action_std, value
    
    def get_action_and_value(self, state_array: torch.Tensor, figure_array: torch.Tensor, action: Optional[torch.Tensor] = None) -> Tuple[torch.Tensor, torch.Tensor, torch.Tensor, torch.Tensor]:
        '''
        Get action and value from the Actor-Critic network
        
        Parameters:
        -----------
        state_array: torch.Tensor [batch_size, dim_state]
            parametric state features
        figure_array: torch.Tensor [batch_size, n_interp_points, 4]
            airfoil geometry and Mach data
        action: Optional[torch.Tensor] [batch_size, dim_action]
            action to be taken
            
        Returns:
        --------
        action: torch.Tensor [batch_size, dim_action]
            action to be taken
        log_prob: torch.Tensor [batch_size, 1]
            log probability of the action
        entropy: torch.Tensor [batch_size, 1]
            entropy of the action distribution
        value: torch.Tensor [batch_size, 1]
            value function estimate
        '''
        action_mean, action_std, value = self.forward(state_array, figure_array)
        dist = Normal(action_mean, action_std)
        
        if action is None:
            action = dist.sample()
        
        log_prob = dist.log_prob(action).sum(dim=-1)
        entropy = dist.entropy().sum(dim=-1)
        
        return action, log_prob, entropy, value


class PPO_FigState_BumpAction():
    '''
    Proximal Policy Optimization (PPO) algorithm for airfoil design
    
    This class is specifically designed to work with:
    - TSFoilEnv_FigState_BumpAction environment
    - FigureState for state representation (parametric + visual)
    - BumpModificationAction for airfoil modifications
    
    '''
    def __init__(self, 
                 env: TSFoilEnv_FigState_BumpAction,
                 lr: float = 3e-4,
                 gamma: float = 0.99,
                 gae_lambda: float = 0.95,
                 clip_epsilon: float = 0.2,
                 value_loss_coef: float = 0.5,
                 entropy_coef: float = 0.01,
                 max_grad_norm: float = 0.5,
                 n_epochs: int = 10,
                 batch_size: int = 64,
                 n_steps: int = 2048,
                 dim_latent: int = 64,
                 dim_hidden: int = 256,
                 n_interp_points: int = 101,
                 initial_action_std: float = 0.1,
                 device: str = 'auto'):
        
        self.env = env
        self.action_class = env.action_class
        self.state_class = env.state_class
        
        # Hyperparameters
        self.lr = lr
        self.gamma = gamma
        self.gae_lambda = gae_lambda
        self.clip_epsilon = clip_epsilon
        self.value_loss_coef = value_loss_coef
        self.entropy_coef = entropy_coef
        self.initial_entropy_coef = entropy_coef  # Store initial value for decay
        self.max_grad_norm = max_grad_norm
        self.n_epochs = n_epochs
        self.batch_size = batch_size
        self.n_steps = n_steps
        self.n_interp_points = n_interp_points
        self.initial_action_std = initial_action_std
        
        # Device setup
        if device == 'auto':
            self.device = torch.device('cuda' if torch.cuda.is_available() else 'cpu')
        else:
            self.device = torch.device(device)
        
        # Network setup
        self.dim_state = env.observation_space.shape[0]
        self.dim_action = env.action_space.shape[0]
        self.dim_latent = dim_latent
        self.dim_hidden = dim_hidden
        
        self.actor_critic = ActorCritic(
            dim_state=self.dim_state,
            dim_action=self.dim_action,
            dim_latent=dim_latent,
            dim_hidden=dim_hidden,
            n_interp_points=n_interp_points,
            initial_std=initial_action_std
        ).to(self.device)
        
        self.optimizer = optim.Adam(self.actor_critic.parameters(), lr=lr)
        
        # Training statistics
        self.training_stats = {
            'episode_rewards': deque(maxlen=100),
            'episode_lengths': deque(maxlen=100),
            'policy_losses': [],
            'value_losses': [],
            'entropies': [],
            'clip_fractions': [],
            'learning_rates': []
        }
        
        # Per-update episode statistics for plotting
        self.episode_rewards_per_update = []
        self.episode_lengths_per_update = []
        self.current_update_rewards = []
        self.current_update_lengths = []
        
        # Storage for rollouts
        self.reset_storage()
        
        print(f"PPO_FigState_BumpAction initialized:")
        print(f"  - State array dim: {self.dim_state} (FigureState parametric features)")
        print(f"  - Figure array dim: [{n_interp_points}, 4] (yu, yl, mwu, mwl)")
        print(f"  - Action array dim: {self.dim_action} (BumpModificationAction)")
        print(f"  - Action bounds: {env.action_class.action_lower_bound} to {env.action_class.action_upper_bound}")
        print(f"  - Device: {self.device}")

    def reset_storage(self) -> None:
        '''
        Reset storage for collecting rollouts
        '''
        self.state_arrays = []
        self.figure_arrays = []
        self.actions = []
        self.rewards = []
        self.values = []
        self.log_probs = []
        self.dones = []
        self.advantages = []
        self.returns = []
    
    def collect_rollouts(self, n_steps: Optional[int] = None) -> dict:
        '''
        Collect rollouts from FigureState environment
        
        Parameters:
        -----------
        n_steps: Optional[int]
            Number of steps to collect rollouts
        
        Returns:
        --------
        rollout_data: dict
            Dictionary containing all rollout data with proper state splitting
        '''
        if n_steps is None:
            n_steps = self.n_steps
            
        self.reset_storage()
        
        self.env.reset()
        state_array, figure_array = self.env._get_observation_for_RL(n_interp_points=self.n_interp_points)

        for step in range(n_steps):
            
            # Convert the current state to tensors
            current_state_array_tensor = torch.FloatTensor(state_array).unsqueeze(0).to(self.device)
            current_figure_array_tensor = torch.FloatTensor(figure_array).unsqueeze(0).to(self.device)
            
            # Get action, value, log_prob of the current state
            with torch.no_grad():
                action, log_prob, _, value = self.actor_critic.get_action_and_value(
                    current_state_array_tensor, current_figure_array_tensor
                )
            
            # Recover action from [-1, 1] to actual action values
            action_np = action.squeeze().cpu().numpy()
            action_unscaled = self.action_class.recover_action(action_np)
            
            # Store transition BEFORE taking the step (state that generated the action)
            self.state_arrays.append(state_array.copy())      # Store CURRENT state
            self.figure_arrays.append(figure_array.copy())    # Store CURRENT state
            self.actions.append(action_np)                     # Store action from CURRENT state
            self.values.append(value.squeeze().cpu().numpy())
            self.log_probs.append(log_prob.cpu().numpy())
                        
            # Take environment step with unscaled action
            next_obs, reward, done, info = self.env.step(action_unscaled)
            self.rewards.append(reward)
            self.dones.append(done)
            
            # Roll-back treatment
            # If the current step is invalid, use the value of the previous step
            flag_roll_back = info.get('is_current_step_valid', False)
            if flag_roll_back and len(self.values) > 1:
                self.values[-1] = self.values[-2]
            
            # Get the next state for next iteration
            state_array, figure_array = self.env._get_observation_for_RL(n_interp_points=self.n_interp_points)

            print(f"step {step:02d} | action: {action_unscaled} | reward: {reward:.2e}")
            
            if done:
                break
        
        # Episode finished
        episode_length = self.env.get_trajectory_length()
        episode_reward = self.env.total_reward
        
        self.training_stats['episode_rewards'].append(episode_reward)
        self.training_stats['episode_lengths'].append(episode_length)
        
        # Also collect for current update statistics
        self.current_update_rewards.append(episode_reward)
        self.current_update_lengths.append(episode_length)
        
        # Compute advantages and returns using final state
        state_array_tensor = torch.FloatTensor(state_array).unsqueeze(0).to(self.device)
        figure_array_tensor = torch.FloatTensor(figure_array).unsqueeze(0).to(self.device)
        self.compute_gae(state_array_tensor, figure_array_tensor)
        
        # Prepare rollout data
        rollout_data = {
            'state_arrays': np.array(self.state_arrays),
            'figure_arrays': np.array(self.figure_arrays),
            'actions': np.array(self.actions),
            'rewards': np.array(self.rewards),
            'values': np.array(self.values),
            'log_probs': np.array(self.log_probs),
            'dones': np.array(self.dones),
            'advantages': np.array(self.advantages),
            'returns': np.array(self.returns)
        }
        
        return rollout_data
    
    def decay_entropy_coef(self, update_count: int, total_updates: int, 
                          decay_type: str = 'linear', min_entropy_coef: float = 0.001) -> None:
        '''
        Decay entropy coefficient during training to reduce exploration over time
        
        Parameters:
        -----------
        update_count: int
            Current update number
        total_updates: int  
            Total number of updates planned
        decay_type: str
            Type of decay ('linear', 'exponential')
        min_entropy_coef: float
            Minimum entropy coefficient value
        '''
        if decay_type == 'linear':
            decay_factor = max(0, 1 - update_count / total_updates)
        elif decay_type == 'exponential':
            decay_factor = 0.99 ** update_count
        else:
            decay_factor = 1.0
            
        self.entropy_coef = max(min_entropy_coef, 
                               self.initial_entropy_coef * decay_factor)
    
    def compute_gae(self, next_state_array: torch.Tensor, next_figure_array: torch.Tensor) -> None:
        '''
        Compute Generalized Advantage Estimation (GAE)
        
        Parameters:
        -----------
        next_state_array: torch.Tensor [batch_size, dim_state]
            parametric state features
        next_figure_array: torch.Tensor [batch_size, n_interp_points, 4]
            airfoil geometry and Mach data
        
        '''
        # Get final value estimate
        with torch.no_grad():
            _, _, _, next_value = self.actor_critic.get_action_and_value(next_state_array, next_figure_array)
            next_value = next_value.squeeze().cpu().numpy()
        
        # Compute advantages using GAE
        advantages = []
        returns = []
        
        gae = 0
        for i in reversed(range(len(self.rewards))):
            if i == len(self.rewards) - 1:
                next_non_terminal = 1.0 - self.dones[i]
                next_val = next_value
            else:
                next_non_terminal = 1.0 - self.dones[i]
                next_val = self.values[i + 1]
            
            delta = self.rewards[i] + self.gamma * next_val * next_non_terminal - self.values[i]
            gae = delta + self.gamma * self.gae_lambda * next_non_terminal * gae
            advantages.insert(0, gae)
            returns.insert(0, gae + self.values[i])
        
        self.advantages = advantages
        self.returns = returns
    
    def update_policy(self, rollout_data: dict) -> dict:
        '''
        Update policy using PPO loss
        
        Parameters:
        -----------
        rollout_data: dict
            Dictionary containing all rollout data
            
        Returns:
        --------
        update_info: dict
            Dictionary containing update information
        '''
        # Convert to tensors
        state_arrays = torch.FloatTensor(rollout_data['state_arrays']).to(self.device)
        figure_arrays = torch.FloatTensor(rollout_data['figure_arrays']).to(self.device)
        actions = torch.FloatTensor(rollout_data['actions']).to(self.device)
        old_log_probs = torch.FloatTensor(rollout_data['log_probs']).to(self.device)
        advantages = torch.FloatTensor(rollout_data['advantages']).to(self.device)
        returns = torch.FloatTensor(rollout_data['returns']).to(self.device)
        old_values = torch.FloatTensor(rollout_data['values']).to(self.device)
        
        # Normalize advantages
        advantages = (advantages - advantages.mean()) / (advantages.std() + 1e-8)
        
        # Training statistics
        total_policy_loss = 0
        total_value_loss = 0
        total_entropy = 0
        total_clip_fraction = 0
        n_updates = 0
        
        # Multiple epochs of optimization
        for epoch in range(self.n_epochs):
            # Create random minibatches
            indices = torch.randperm(len(state_arrays))
            
            for start in range(0, len(state_arrays), self.batch_size):
                end = start + self.batch_size
                batch_indices = indices[start:end]
                
                # Get batch data
                batch_state_arrays = state_arrays[batch_indices]
                batch_figure_arrays = figure_arrays[batch_indices]
                batch_actions = actions[batch_indices]
                batch_old_log_probs = old_log_probs[batch_indices]
                batch_advantages = advantages[batch_indices]
                batch_returns = returns[batch_indices]
                batch_old_values = old_values[batch_indices]
                
                # Forward pass
                _, new_log_probs, entropy, new_values = self.actor_critic.get_action_and_value(
                    batch_state_arrays, batch_figure_arrays, batch_actions
                )
                new_values = new_values.squeeze()
                
                # Policy loss (PPO clipped objective)
                ratio = torch.exp(new_log_probs - batch_old_log_probs)
                surr1 = ratio * batch_advantages
                surr2 = torch.clamp(ratio, 1.0 - self.clip_epsilon, 1.0 + self.clip_epsilon) * batch_advantages
                policy_loss = -torch.min(surr1, surr2).mean()
                
                # Value loss (clipped)
                value_pred_clipped = batch_old_values + torch.clamp(
                    new_values - batch_old_values, -self.clip_epsilon, self.clip_epsilon
                )
                value_loss1 = (new_values - batch_returns) ** 2
                value_loss2 = (value_pred_clipped - batch_returns) ** 2
                value_loss = 0.5 * torch.max(value_loss1, value_loss2).mean()
                
                # Entropy bonus
                entropy_loss = -entropy.mean()
                
                # Total loss
                total_loss = (policy_loss + 
                            self.value_loss_coef * value_loss + 
                            self.entropy_coef * entropy_loss)
                
                # Backward pass
                self.optimizer.zero_grad()
                total_loss.backward()
                nn.utils.clip_grad_norm_(self.actor_critic.parameters(), self.max_grad_norm)
                self.optimizer.step()
                
                # Statistics
                with torch.no_grad():
                    clip_fraction = ((ratio - 1.0).abs() > self.clip_epsilon).float().mean()
                    
                total_policy_loss += policy_loss.item()
                total_value_loss += value_loss.item()
                total_entropy += entropy.mean().item()
                total_clip_fraction += clip_fraction.item()
                n_updates += 1
        
        # Store training statistics
        self.training_stats['policy_losses'].append(total_policy_loss / n_updates)
        self.training_stats['value_losses'].append(total_value_loss / n_updates)
        self.training_stats['entropies'].append(total_entropy / n_updates)
        self.training_stats['clip_fractions'].append(total_clip_fraction / n_updates)
        self.training_stats['learning_rates'].append(self.lr)
        
        return {
            'policy_loss': total_policy_loss / n_updates,
            'value_loss': total_value_loss / n_updates,
            'entropy': total_entropy / n_updates,
            'clip_fraction': total_clip_fraction / n_updates
        }
    
    def train(self, total_time_steps: int, 
              log_interval: int = 10, save_interval: int = 100, eval_interval: int = 100,
              save_path: str = 'ppo_model.pt', plot_training: bool = True,
              plot_path: str = 'training_progress.png',
              use_entropy_decay: bool = False, entropy_decay_type: str = 'linear') -> None:
        '''
        Train the PPO agent
        
        Parameters:
        -----------
        total_time_steps: int
            Total number of time_steps to train
        log_interval: int
            Log progress every N updates
        save_interval: int
            Save model every N updates
        save_path: str
            Path to save the model
        plot_training: bool
            Whether to plot training progress
            
        Returns:
        --------
        None
        '''
        print(f"Starting PPO training for {total_time_steps} time_steps")

        time_steps_so_far = 0
        update_count = 0
        estimated_total_updates = total_time_steps // self.n_steps + 1
        
        while time_steps_so_far < total_time_steps:
            
            print(f"Step {time_steps_so_far} / {total_time_steps} | Update {update_count}")
            
            # Collect rollouts
            rollout_data = self.collect_rollouts()
            time_steps_so_far += len(rollout_data['state_arrays'])
            
            # Update policy
            update_info = self.update_policy(rollout_data)
            update_count += 1
            
            # Apply entropy decay if enabled
            if use_entropy_decay:
                self.decay_entropy_coef(update_count, estimated_total_updates, entropy_decay_type)
            
            # Save episode statistics for this update period
            self.episode_rewards_per_update.append(self.current_update_rewards.copy())
            self.episode_lengths_per_update.append(self.current_update_lengths.copy())
            # Reset for next update
            self.current_update_rewards.clear()
            self.current_update_lengths.clear()
            
            # Logging
            if update_count % log_interval == 0:
                avg_reward = np.mean(self.training_stats['episode_rewards']) if self.training_stats['episode_rewards'] else 0
                avg_length = np.mean(self.training_stats['episode_lengths']) if self.training_stats['episode_lengths'] else 0
                
                print(f"  Update {update_count:3d} | "
                      f"Timesteps {time_steps_so_far:5d} | "
                      f"Avg Reward {avg_reward:.2f} | "
                      f"Avg Length {avg_length:.1f} | "
                      f"Policy Loss {update_info['policy_loss']:.4f} | "
                      f"Value Loss {update_info['value_loss']:.4f} | "
                      f"Entropy {update_info['entropy']:.1f} (coef={self.entropy_coef:.4f}) -> std {np.mean(np.exp(self.actor_critic.actor_log_std.detach().cpu().numpy())):.4f} | "
                      f"Actual action std: {np.mean(np.std(rollout_data['actions'], axis=0)):.4f}")
            
                if plot_training:
                    self.plot_training_progress(save_path=plot_path)
            
            if update_count % save_interval == 0:
                self.save_model(save_path)
                
            if update_count % eval_interval == 0:
                self.plot_eval_results()
        
        print("Training completed!")
        self.save_model(save_path)
        if plot_training:
            self.plot_training_progress(save_path=plot_path)

    def evaluate(self, n_episodes: int = 10, n_steps: int = 10, render: bool = False) -> dict:
        '''
        Evaluate the trained policy on FigureState environment
        
        Parameters:
        -----------
        n_episodes: int
            Number of episodes to evaluate
        n_steps: int
            Number of steps to evaluate
        render: bool
            Whether to render the episodes
            
        Returns:
        --------
        results: dict
            Evaluation results including episode rewards and airfoil performance
        '''
        episode_rewards = []
        episode_lengths = []
        final_cl_values = []
        final_cd_values = []
        final_ld_ratios = []
               
        for episode in range(n_episodes):
            
            self.env.reset()
            state_array, figure_array = self.env._get_observation_for_RL(n_interp_points=self.n_interp_points)
            
            done = False
            deterministic = episode == 0
            
            for step in range(n_steps):

                action_unscaled = self.get_action(state_array, figure_array, deterministic=deterministic)
                
                obs, reward, done, info = self.env.step(action_unscaled)
                state_array, figure_array = self.env._get_observation_for_RL(n_interp_points=self.n_interp_points)
                
                if done:
                    break
                
            if render:
                self.env.render()
            
            # Store episode statistics
            episode_reward = self.env.total_reward
            episode_length = self.env.get_trajectory_length()
            episode_rewards.append(episode_reward)
            episode_lengths.append(episode_length)
            
            # Store final aerodynamic performance
            final_cl_values.append(info.get('cl', 0.0))
            final_cd_values.append(info.get('cd', 0.001))
            final_ld_ratios.append(info.get('cl', 0.0) / max(info.get('cd', 0.0001), 0.0001))
            
            print(f"  Episode {episode+1:3d}: Total Reward = {episode_reward:8.2f}, "
                  f"Length = {episode_length:4d}, CL = {final_cl_values[-1]:.4f}, "
                  f"CD = {final_cd_values[-1]:.6f}, L/D = {final_ld_ratios[-1]:.2f}")
                
        results = {
            'mean_reward': np.mean(episode_rewards),
            'std_reward': np.std(episode_rewards), 
            'mean_length': np.mean(episode_lengths),
            'std_length': np.std(episode_lengths),
            'mean_cl': np.mean(final_cl_values),
            'std_cl': np.std(final_cl_values),
            'mean_cd': np.mean(final_cd_values),
            'std_cd': np.std(final_cd_values),
            'mean_ld_ratio': np.mean(final_ld_ratios),
            'std_ld_ratio': np.std(final_ld_ratios),
            'episode_rewards': episode_rewards,
            'episode_lengths': episode_lengths,
            'final_cl_values': final_cl_values,
            'final_cd_values': final_cd_values,
            'final_ld_ratios': final_ld_ratios
        }
        
        return results
    
    def save_model(self, path: str):
        '''Save the model and training statistics'''
        torch.save({
            'actor_critic_state_dict': self.actor_critic.state_dict(),
            'optimizer_state_dict': self.optimizer.state_dict(),
            'training_stats': dict(self.training_stats),
            'episode_rewards_per_update': self.episode_rewards_per_update,
            'episode_lengths_per_update': self.episode_lengths_per_update,
            'hyperparameters': {
                'lr': self.lr,
                'gamma': self.gamma,
                'gae_lambda': self.gae_lambda,
                'clip_epsilon': self.clip_epsilon,
                'value_loss_coef': self.value_loss_coef,
                'entropy_coef': self.entropy_coef,
                'max_grad_norm': self.max_grad_norm,
                'n_epochs': self.n_epochs,
                'batch_size': self.batch_size,
                'n_steps': self.n_steps,
                'dim_state': self.dim_state,
                'dim_action': self.dim_action
            }
        }, path)
    
    def load_model(self, path: str):
        '''Load a saved model'''
        checkpoint = torch.load(path, map_location=self.device, weights_only=False)
        self.actor_critic.load_state_dict(checkpoint['actor_critic_state_dict'])
        self.optimizer.load_state_dict(checkpoint['optimizer_state_dict'])
        self.training_stats.update(checkpoint['training_stats'])
        
        # Load per-update statistics if available (for backward compatibility)
        if 'episode_rewards_per_update' in checkpoint:
            self.episode_rewards_per_update = checkpoint['episode_rewards_per_update']
        if 'episode_lengths_per_update' in checkpoint:
            self.episode_lengths_per_update = checkpoint['episode_lengths_per_update']
            
        print(f"Model loaded from {path}")
    
    def plot_training_progress(self, save_path: str = 'training_progress.png'):
        '''Plot training progress'''
        fig, axes = plt.subplots(2, 3, figsize=(15, 10))
        fig.suptitle('PPO Training Progress', fontsize=16)
        
        # Episode reward statistics per update
        if hasattr(self, 'episode_rewards_per_update') and self.episode_rewards_per_update:
            updates = list(range(len(self.episode_rewards_per_update)))
            mean_rewards = [np.mean(rewards) if rewards else 0 for rewards in self.episode_rewards_per_update]
            min_rewards = [np.min(rewards) if rewards else 0 for rewards in self.episode_rewards_per_update]
            max_rewards = [np.max(rewards) if rewards else 0 for rewards in self.episode_rewards_per_update]
            
            axes[0, 0].plot(updates, mean_rewards, 'b-', label='Mean', linewidth=2)
            axes[0, 0].bar(updates, 
                          np.array(max_rewards) - np.array(min_rewards),
                          bottom=min_rewards,
                          alpha=0.3, color='blue', label='Min-Max Range')
            axes[0, 0].set_title('Episode Reward Statistics')
            axes[0, 0].set_xlabel('Update')
            axes[0, 0].set_ylabel('Reward')
            axes[0, 0].legend()
        
        # Episode length statistics per update
        if hasattr(self, 'episode_lengths_per_update') and self.episode_lengths_per_update:
            updates = list(range(len(self.episode_lengths_per_update)))
            mean_lengths = [np.mean(lengths) if lengths else 0 for lengths in self.episode_lengths_per_update]
            min_lengths = [np.min(lengths) if lengths else 0 for lengths in self.episode_lengths_per_update]
            max_lengths = [np.max(lengths) if lengths else 0 for lengths in self.episode_lengths_per_update]
            
            axes[0, 1].plot(updates, mean_lengths, 'g-', label='Mean', linewidth=2)
            axes[0, 1].bar(updates,
                          np.array(max_lengths) - np.array(min_lengths),
                          bottom=min_lengths,
                          alpha=0.3, color='green', label='Min-Max Range')
            axes[0, 1].set_title('Episode Length Statistics')
            axes[0, 1].set_xlabel('Update')
            axes[0, 1].set_ylabel('Length')
            axes[0, 1].legend()
        
        # Policy loss
        if self.training_stats['policy_losses']:
            # Remove outliers from policy losses
            filtered_policy_losses, valid_indices = self._remove_outliers(
                self.training_stats['policy_losses'], method='iqr', iqr_factor=1.5
            )
            
            axes[0, 2].plot(valid_indices, filtered_policy_losses, 'b-', alpha=0.8)
            axes[0, 2].set_title(f'Policy Loss')
            axes[0, 2].set_xlabel('Update')
            axes[0, 2].set_ylabel('Loss')
            axes[0, 2].grid(True, alpha=0.3)
        
        # Value loss
        if self.training_stats['value_losses']:
            # Remove outliers from value losses
            filtered_value_losses, valid_indices = self._remove_outliers(
                self.training_stats['value_losses'], method='iqr', iqr_factor=1.5
            )
            
            axes[1, 0].plot(valid_indices, filtered_value_losses, 'r-', alpha=0.8)
            axes[1, 0].set_title(f'Value Loss')
            axes[1, 0].set_xlabel('Update')
            axes[1, 0].set_ylabel('Loss')
            axes[1, 0].grid(True, alpha=0.3)
        
        # Entropy
        if self.training_stats['entropies']:
            axes[1, 1].plot(self.training_stats['entropies'])
            axes[1, 1].set_title('Entropy')
            axes[1, 1].set_xlabel('Update')
            axes[1, 1].set_ylabel('Entropy')
        
        # Clip fraction
        if self.training_stats['clip_fractions']:
            axes[1, 2].plot(self.training_stats['clip_fractions'])
            axes[1, 2].set_title('Clip Fraction')
            axes[1, 2].set_xlabel('Update')
            axes[1, 2].set_ylabel('Fraction')
        
        plt.tight_layout()
        plt.savefig(save_path, dpi=300, bbox_inches='tight')
        plt.close()
    
    def plot_eval_results(self):
        '''Plot evaluation progress'''
        self.evaluate(n_episodes=1, n_steps=10, render=True)
    
    def _remove_outliers(self, data: List[float], method: str = 'iqr', 
                        iqr_factor: float = 1.5, std_factor: float = 2.5) -> Tuple[List[float], List[int]]:
        '''
        Remove outliers from data using statistical methods
        
        Parameters:
        -----------
        data: List[float]
            Input data to filter
        method: str
            Method to use: 'iqr' (Interquartile Range) or 'std' (Standard Deviation)
        iqr_factor: float
            Factor for IQR method (typical: 1.5 for outliers, 3.0 for extreme outliers)
        std_factor: float
            Factor for std method (typical: 2.0-3.0)
            
        Returns:
        --------
        filtered_data: List[float]
            Data with outliers removed
        valid_indices: List[int]
            Indices of non-outlier points
        '''
        if not data or len(data) < 4:  # Need minimum data points
            return data, list(range(len(data)))
            
        data_array = np.array(data)
        
        if method == 'iqr':
            Q1 = np.percentile(data_array, 25)
            Q3 = np.percentile(data_array, 75)
            IQR = Q3 - Q1
            lower_bound = Q1 - iqr_factor * IQR
            upper_bound = Q3 + iqr_factor * IQR
            
        elif method == 'std':
            mean = np.mean(data_array)
            std = np.std(data_array)
            lower_bound = mean - std_factor * std
            upper_bound = mean + std_factor * std
            
        else:
            raise ValueError(f"Unknown method: {method}")
        
        # Find valid indices (non-outliers)
        valid_mask = (data_array >= lower_bound) & (data_array <= upper_bound)
        valid_indices = np.where(valid_mask)[0].tolist()
        filtered_data = data_array[valid_mask].tolist()
        
        return filtered_data, valid_indices
    
    def get_action(self, state_array: np.ndarray, figure_array: np.ndarray, 
                   deterministic: bool = False) -> np.ndarray:
        '''
        Get action from the policy for FigureState input
        
        Parameters:
        -----------
        state_array: np.ndarray [dim_state]
            parametric state features from FigureState
        figure_array: np.ndarray [n_interp_points, 4]
            airfoil geometry and Mach data [yu, yl, mwu, mwl]
        deterministic: bool
            If True, use mean of policy distribution
            
        Returns:
        --------
        action_unscaled: np.ndarray [dim_action]    
            Unscaled action ready for environment
        '''
        self.actor_critic.eval()
        
        # Convert to tensors
        state_array_tensor = torch.FloatTensor(state_array).unsqueeze(0).to(self.device)
        figure_array_tensor = torch.FloatTensor(figure_array).unsqueeze(0).to(self.device)
        
        with torch.no_grad():
            if deterministic:
                action_mean, _, _ = self.actor_critic(state_array_tensor, figure_array_tensor)
                action = action_mean.squeeze().cpu().numpy()
            else:
                action, _, _, _ = self.actor_critic.get_action_and_value(
                    state_array_tensor, figure_array_tensor)
                action = action.squeeze().cpu().numpy()
        
        self.actor_critic.train()
        
        # Scale action from [-1, 1] to actual action bounds
        action_unscaled = self.action_class.recover_action(np.clip(action, -1.0, 1.0))
        
        return action_unscaled
    
    def diagnose_entropy_behavior(self) -> dict:
        '''
        Diagnose entropy behavior to understand if the current trend is problematic
        
        Returns:
        --------
        diagnosis: dict
            Dictionary with entropy analysis and recommendations
        '''
        if len(self.training_stats['entropies']) < 10:
            return {'status': 'insufficient_data', 'message': 'Need more training updates to analyze'}
        
        entropies = np.array(self.training_stats['entropies'])
        recent_entropies = entropies[-10:]  # Last 10 updates
        
        # Check if entropy is increasing, decreasing, or stable
        trend_slope = np.polyfit(range(len(recent_entropies)), recent_entropies, 1)[0]
        
        # Get current action standard deviations
        current_std = np.exp(self.actor_critic.actor_log_std.detach().cpu().numpy())
        mean_std = np.mean(current_std)
        
        # Get current entropy coefficient
        current_entropy_coef = self.entropy_coef
        
        diagnosis = {
            'entropy_trend_slope': trend_slope,
            'mean_recent_entropy': np.mean(recent_entropies),
            'current_action_std': current_std,
            'mean_action_std': mean_std,
            'entropy_coefficient': current_entropy_coef,
            'status': 'unknown',
            'recommendations': []
        }
        
        # Analyze the behavior
        if trend_slope > 0.1:  # Strongly increasing
            if mean_std > 1.0:  # Very high exploration
                diagnosis['status'] = 'problematic_high_entropy'
                diagnosis['recommendations'].extend([
                    f"Reduce entropy_coef from {current_entropy_coef:.4f} to {current_entropy_coef*0.5:.4f}",
                    "Consider using entropy decay: use_entropy_decay=True",
                    "Check if learning rate is too high causing instability"
                ])
            else:
                diagnosis['status'] = 'exploration_phase'
                diagnosis['recommendations'].append("Normal exploration behavior, monitor for convergence")
                
        elif trend_slope < -0.1:  # Strongly decreasing
            diagnosis['status'] = 'converging'
            diagnosis['recommendations'].append("Good: entropy decreasing, policy converging")
            
        else:  # Stable
            if mean_std > 0.8:
                diagnosis['status'] = 'high_stable_entropy'
                diagnosis['recommendations'].extend([
                    "High but stable entropy - may need entropy decay",
                    f"Consider reducing entropy_coef from {current_entropy_coef:.4f}"
                ])
            else:
                diagnosis['status'] = 'healthy'
                diagnosis['recommendations'].append("Healthy entropy behavior")
        
        return diagnosis
