'''
Proximal Policy Optimization (PPO) algorithm for airfoil design

With multiprocessing, we can run multiple environments in parallel.
'''
import torch
import numpy as np
from typing import List, Tuple, Optional, Callable

import copy
import multiprocessing as mp
import time
from typing import Dict, Any

from model.ppo import PPO_FigState_BumpAction, ActorCritic
from pyTSFoil.environment.utils import TSFoilEnv_FigState_BumpAction


def collect_rollout_worker(params: Dict[str, Any]) -> Dict[str, Any]:
    """
    Worker function to collect rollout data from a single environment.
    Each worker runs independently and collects a portion of the total rollout.
    
    Parameters:
    -----------
    params: Dict[str, Any]
        Dictionary containing:
        - 'env_fn': Function that creates environment instance
        - 'worker_id': Unique identifier for this worker
        - 'n_steps': Number of steps this worker should collect
        - 'n_interp_points': Number of interpolation points for figure array
        - 'actor_critic_state': State dict of the actor-critic model
        - 'device': Device to run model on
        - 'action_class': Action class for converting actions
        - 'dim_state': State space dimension
        - 'dim_action': Action space dimension
        - 'dim_latent': Latent space dimension
        - 'dim_hidden': Hidden layer dimension
        
    Returns:
    --------
    Dict[str, Any]: Rollout data collected by this worker
    """
    try:
        import torch
        import numpy as np
        
        # Extract parameters
        env_fn = params['env_fn']
        worker_id = params['worker_id'] 
        n_steps = params['n_steps']
        n_interp_points = params['n_interp_points']
        actor_critic_state = params['actor_critic_state']
        device = params['device']
        action_class = params['action_class']
        
        # print(f"Worker {worker_id}: Starting rollout collection for {n_steps} steps")
        
        # Create environment in this process
        env = env_fn()
        
        # Use passed dimensions instead of inferring them
        actor_critic = ActorCritic(
            dim_state=params['dim_state'],
            dim_action=params['dim_action'],
            dim_latent=params.get('dim_latent', 64),
            dim_hidden=params.get('dim_hidden', 256),
            n_interp_points=n_interp_points
        ).to(device)
        
        actor_critic.load_state_dict(actor_critic_state)
        actor_critic.eval()
        
        # Reset environment and get initial state
        env.reset()
        state_array, figure_array = env._get_observation_for_RL(n_interp_points=n_interp_points)
        
        # Storage for rollout data
        state_arrays = []
        figure_arrays = []
        actions = []
        rewards = []
        values = []
        log_probs = []
        dones = []
        episode_rewards = []
        episode_lengths = []
        
        for step in range(n_steps):
            
            # Convert to tensors
            state_array_tensor = torch.FloatTensor(state_array).unsqueeze(0).to(device)
            figure_array_tensor = torch.FloatTensor(figure_array).unsqueeze(0).to(device)
            
            # Get action from policy
            with torch.no_grad():
                action, log_prob, _, value = actor_critic.get_action_and_value(
                    state_array_tensor, figure_array_tensor
                )
            
            # Convert action
            action_np = action.squeeze().cpu().numpy()
            action_unscaled = action_class.recover_action(action_np)
            
            # Store transition data
            state_arrays.append(state_array.copy())
            figure_arrays.append(figure_array.copy())
            actions.append(action_np)
            values.append(value.squeeze().cpu().numpy())
            log_probs.append(log_prob.cpu().numpy())
            
            # Take environment step
            next_obs, reward, done, info = env.step(action_unscaled)
            
            # Store step results
            rewards.append(reward)
            dones.append(done)
            
            # Roll-back treatment
            # If the current step is invalid, use the value of the previous step
            flag_roll_back = info.get('is_current_step_valid', False)
            if flag_roll_back and len(values) > 1:
                values[-1] = values[-2]
            
            # Get next state
            state_array, figure_array = env._get_observation_for_RL(n_interp_points=n_interp_points)
            
            # np.set_printoptions(formatter={'float': '{:8.4f}'.format})
            # print(f"step {step:02d} | action: {action_unscaled} | reward: {reward:.2e}")
            
            if done:
                break
                
        # Episode finished
        episode_rewards.append(np.sum(rewards))
        episode_lengths.append(env.get_trajectory_length())
        
        return {
            'worker_id': worker_id,
            'success': True,
            'state_arrays': np.array(state_arrays),
            'figure_arrays': np.array(figure_arrays), 
            'actions': np.array(actions),
            'rewards': np.array(rewards),
            'values': np.array(values),
            'log_probs': np.array(log_probs),
            'dones': np.array(dones),
            'episode_rewards': episode_rewards,
            'episode_lengths': episode_lengths,
            'final_state_array': state_array,
            'final_figure_array': figure_array
        }
        
    except Exception as e:
        print(f"Worker {worker_id} error: {str(e)}")
        return {
            'worker_id': worker_id,
            'success': False,
            'error': str(e)
        }


class PPO_FigState_BumpAction_MultiEnv(PPO_FigState_BumpAction):
    '''
    Proximal Policy Optimization (PPO) algorithm for airfoil design
    
    This class is specifically designed to work with:
    - TSFoilEnv_FigState_BumpAction environments
    - FigureState for state representation (parametric + visual)
    - BumpModificationAction for airfoil modifications
    
    The 'MultiEnv' version uses multiprocessing.Pool to collect rollouts
    in parallel from multiple environment instances. This approach is 
    simpler and more reliable than the queue-based approach.
    '''

    def __init__(self, 
                 env_fns: List[callable],
                 env_eval: TSFoilEnv_FigState_BumpAction|None = None,
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
                 device: str = 'auto',
                 max_processes: Optional[int] = None):
        '''
        Initialize the PPO_FigState_BumpAction_MultiEnv class
        
        Parameters:
        -----------
        env_fns: List[callable]
            List of functions that create environment instances
            Each function should return a TSFoilEnv_FigState_BumpAction instance
        max_processes: Optional[int]
            Maximum number of processes to use for parallel rollout collection.
            If None, uses the number of environments (len(env_fns)).
            If specified, will use min(max_processes, len(env_fns), cpu_count()).
        '''
        # Create a temporary environment to get dimensions
        if env_eval is None:
            env_eval = copy.deepcopy(env_fns[0]())
            env_eval.render_mode = 'none'

        super().__init__(env_eval, lr, gamma, gae_lambda, clip_epsilon, 
                            value_loss_coef, entropy_coef, max_grad_norm, 
                            n_epochs, batch_size, n_steps, 
                            dim_latent, dim_hidden, n_interp_points, device)

        self.env_fns = env_fns
        self.n_envs = len(env_fns)
        
        # Set maximum number of processes to use
        if max_processes is None:
            self.max_processes = self.n_envs
        else:
            self.max_processes = min(max_processes, self.n_envs)
            
        # Actual number of processes to use for multiprocessing
        self.n_processes = min(self.max_processes, mp.cpu_count())
        
    def collect_rollouts(self, n_steps: Optional[int] = None) -> dict:
        '''
        Collect rollouts from multiple environments in parallel using multiprocessing.Pool
        
        Parameters:
        -----------
        n_steps: Optional[int]
            Number of steps to collect rollouts across all environments
        
        Returns:
        --------
        rollout_data: dict
            Dictionary containing all rollout data with proper state splitting
        '''
        if n_steps is None:
            n_steps = self.n_steps
            
        self.reset_storage()
        
        # Prepare parameters for each worker
        worker_params = []
        for worker_id in range(self.n_envs):
            params = {
                'env_fn': self.env_fns[worker_id],
                'worker_id': worker_id,
                'n_steps': n_steps,
                'n_interp_points': self.n_interp_points,
                'actor_critic_state': self.actor_critic.state_dict(),
                'device': 'cpu',  # Use CPU for workers to avoid GPU conflicts
                'action_class': self.action_class,
                'dim_latent': self.dim_latent,
                'dim_hidden': self.dim_hidden,
                # Pass model dimensions directly to avoid reconstructing in each worker
                'dim_state': self.dim_state,
                'dim_action': self.dim_action
            }
            worker_params.append(params)
        
        # Run rollout collection in parallel using Pool
        start_time = time.time()
        
        with mp.Pool(processes=self.n_processes) as pool:
            worker_results = pool.map(collect_rollout_worker, worker_params)
        
        elapsed_time = time.time() - start_time

        # Process results from workers
        successful_results = [r for r in worker_results if r['success']]
        failed_results = [r for r in worker_results if not r['success']]
        
        if failed_results:
            print(f"Warning: {len(failed_results)} workers failed:")
            for result in failed_results:
                print(f"  Worker {result['worker_id']}: {result['error']}")
        
        if not successful_results:
            raise RuntimeError("All workers failed to collect rollouts")

        # Combine data from all successful workers
        combined_state_arrays = []
        combined_figure_arrays = []
        combined_actions = []
        combined_rewards = []
        combined_values = []
        combined_log_probs = []
        combined_dones = []
        
        for result in successful_results:
            combined_state_arrays.append(result['state_arrays'])
            combined_figure_arrays.append(result['figure_arrays'])
            combined_actions.append(result['actions'])
            combined_rewards.append(result['rewards'])
            combined_values.append(result['values'])
            combined_log_probs.append(result['log_probs'])
            combined_dones.append(result['dones'])
            
            # Update training stats
            self.training_stats['episode_rewards'].extend(result['episode_rewards'])
            self.training_stats['episode_lengths'].extend(result['episode_lengths'])
            
            # Also collect for current update statistics  
            self.current_update_rewards.extend(result['episode_rewards'])
            self.current_update_lengths.extend(result['episode_lengths'])
        
        # Concatenate all arrays
        self.state_arrays = np.concatenate(combined_state_arrays, axis=0)
        self.figure_arrays = np.concatenate(combined_figure_arrays, axis=0)
        self.actions = np.concatenate(combined_actions, axis=0)
        self.rewards = np.concatenate(combined_rewards, axis=0)
        self.values = np.concatenate(combined_values, axis=0)
        self.log_probs = np.concatenate(combined_log_probs, axis=0)
        self.dones = np.concatenate(combined_dones, axis=0)
        
        print(f"  Collect rollout data ({len(self.state_arrays)} steps) from {len(successful_results)}/{self.n_envs} environments using {self.n_processes} processes in {elapsed_time:.2f}s")
        
        # Use final state from first successful worker for bootstrapping GAE
        final_result = successful_results[0]
        bootstrap_state_tensor = torch.FloatTensor(final_result['final_state_array']).unsqueeze(0).to(self.device)
        bootstrap_figure_tensor = torch.FloatTensor(final_result['final_figure_array']).unsqueeze(0).to(self.device)
        
        self.compute_gae(bootstrap_state_tensor, bootstrap_figure_tensor)
        
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

    def plot_eval_results(self):
        '''Plot evaluation results'''
        self.evaluate(n_episodes=1, n_steps=10, render=True)
