'''
Proximal Policy Optimization (PPO) algorithm for airfoil design

With multiprocessing, we can run multiple environments in parallel.
'''
import torch
import numpy as np
from typing import List, Tuple, Optional, Callable

import multiprocessing as mp
import queue
import time

from model.ppo import PPO_FigState_BumpAction
from pyTSFoil.environment.utils import TSFoilEnv_FigState_BumpAction


def env_worker(env_fn, env_id, action_queue, result_queue, n_interp_points):
    """
    Worker function for running environment in separate process
    
    Parameters:
    -----------
    env_fn: callable
        Function that creates and returns the environment
    env_id: int
        Unique identifier for this environment worker
    action_queue: mp.Queue
        Queue to receive actions from main process
    result_queue: mp.Queue  
        Queue to send results back to main process
    n_interp_points: int
        Number of interpolation points for figure array
    """
    try:
        # Create environment in this process
        env = env_fn()
        
        # Reset environment and get initial state
        env.reset()
        state_array, figure_array = env._get_observation_for_RL(n_interp_points=n_interp_points)
        
        # Send initial state
        result_queue.put({
            'env_id': env_id,
            'type': 'state',
            'state_array': state_array,
            'figure_array': figure_array,
            'episode_reward': 0,
            'episode_length': 0
        })
        
        episode_reward = 0
        episode_length = 0
        
        while True:
            try:
                # Wait for action from main process
                action_data = action_queue.get(timeout=10.0)
                
                if action_data is None:  # Shutdown signal
                    break
                    
                if action_data['env_id'] != env_id:
                    continue  # Not for this environment
                
                action = action_data['action']
                
                # Take environment step
                next_obs, reward, done, info = env.step(action)
                episode_reward += reward
                episode_length += 1
                
                # Get next state
                next_state_array, next_figure_array = env._get_observation_for_RL(n_interp_points=n_interp_points)
                
                # Send step result
                result_queue.put({
                    'env_id': env_id,
                    'type': 'step',
                    'reward': reward,
                    'done': done,
                    'info': info,
                    'state_array': next_state_array,
                    'figure_array': next_figure_array,
                    'episode_reward': episode_reward,
                    'episode_length': episode_length
                })
                
                if done:
                    # Episode finished, reset
                    env.reset()
                    reset_state_array, reset_figure_array = env._get_observation_for_RL(n_interp_points=n_interp_points)
                    episode_reward = 0
                    episode_length = 0
                    
                    # Send reset state
                    result_queue.put({
                        'env_id': env_id,
                        'type': 'reset',
                        'state_array': reset_state_array,
                        'figure_array': reset_figure_array,
                        'episode_reward': episode_reward,
                        'episode_length': episode_length
                    })
                    
            except queue.Empty:
                # Timeout waiting for action, continue
                continue
                
    except Exception as e:
        result_queue.put({
            'env_id': env_id,
            'type': 'error',
            'error': str(e)
        })


class PPO_FigState_BumpAction_MultiEnv(PPO_FigState_BumpAction):
    '''
    Proximal Policy Optimization (PPO) algorithm for airfoil design
    
    This class is specifically designed to work with:
    - TSFoilEnv_FigState_BumpAction environments
    - FigureState for state representation (parametric + visual)
    - BumpModificationAction for airfoil modifications
    
    The 'MultiEnv' version of PPO_FigState_BumpAction is designed to
    generate multiple episodes in parallel (in `collect_rollouts`).
    Each episode is run in a separate environment, and the rollouts
    are collected in a single batch. Each environment is run in a 
    separate process, because the environment is not thread-safe.
    '''

    def __init__(self, 
                 env_fns: List[callable],
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
                 device: str = 'auto'):
        '''
        Initialize the PPO_FigState_BumpAction_MultiEnv class
        
        Parameters:
        -----------
        env_fns: List[callable]
            List of functions that create environment instances
            Each function should return a TSFoilEnv_FigState_BumpAction instance
        '''
        # Create a temporary environment to get dimensions
        temp_env = env_fns[0]()
        super().__init__(temp_env, lr, gamma, gae_lambda, clip_epsilon, 
                            value_loss_coef, entropy_coef, max_grad_norm, 
                            n_epochs, batch_size, n_steps, 
                            dim_latent, dim_hidden, n_interp_points, device)

        self.env_fns = env_fns
        self.n_envs = len(env_fns)
        
        # Multiprocessing setup
        self.processes = []
        self.action_queues = []
        self.result_queues = []
        self.env_states = {}
        
        # Start worker processes
        self._start_workers()
        
        print(f"PPO_FigState_BumpAction_MultiEnv initialized with {self.n_envs} parallel environments")

    def _start_workers(self):
        '''Start worker processes for each environment'''
        for env_id in range(self.n_envs):
            # Create queues for communication
            action_queue = mp.Queue()
            result_queue = mp.Queue()
            
            # Create and start process
            process = mp.Process(
                target=env_worker,
                args=(self.env_fns[env_id], env_id, action_queue, result_queue, self.n_interp_points)
            )
            process.start()
            
            self.processes.append(process)
            self.action_queues.append(action_queue)
            self.result_queues.append(result_queue)
        
        # Wait for initial states from all environments
        print("Waiting for initial states from all environments...")
        for env_id in range(self.n_envs):
            while True:
                try:
                    result = self.result_queues[env_id].get(timeout=5.0)
                    if result['type'] == 'state':
                        self.env_states[env_id] = {
                            'state_array': result['state_array'],
                            'figure_array': result['figure_array'],
                            'episode_reward': result['episode_reward'],
                            'episode_length': result['episode_length']
                        }
                        break
                    elif result['type'] == 'error':
                        raise RuntimeError(f"Environment {env_id} error: {result['error']}")
                except queue.Empty:
                    raise RuntimeError(f"Environment {env_id} failed to respond")
        
        print("All environments initialized successfully")

    def _stop_workers(self):
        '''Stop all worker processes'''
        # Send shutdown signal to all workers
        for action_queue in self.action_queues:
            action_queue.put(None)
        
        # Wait for all processes to finish
        for process in self.processes:
            process.join(timeout=5.0)
            if process.is_alive():
                process.terminate()
                process.join()
        
        # Clear resources
        self.processes.clear()
        self.action_queues.clear()
        self.result_queues.clear()
        self.env_states.clear()

    def __del__(self):
        '''Cleanup when object is destroyed'''
        if hasattr(self, 'processes') and self.processes:
            self._stop_workers()

    def collect_rollouts(self, n_steps: Optional[int] = None) -> dict:
        '''
        Collect rollouts from multiple environments in parallel using multiprocessing
        
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
        
        steps_collected = 0
        active_envs = set(range(self.n_envs))  # Track which environments are active
        
        while steps_collected < n_steps and active_envs:
            # Collect actions for all active environments
            env_actions = {}
            
            for env_id in active_envs:
                # Get current state for this environment
                state_array = self.env_states[env_id]['state_array']
                figure_array = self.env_states[env_id]['figure_array']
                
                # Convert to tensors
                state_array_tensor = torch.FloatTensor(state_array).unsqueeze(0).to(self.device)
                figure_array_tensor = torch.FloatTensor(figure_array).unsqueeze(0).to(self.device)
                
                # Get action from policy
                with torch.no_grad():
                    action, log_prob, _, value = self.actor_critic.get_action_and_value(
                        state_array_tensor, figure_array_tensor
                    )
                
                # Store action info for this environment
                action_np = action.squeeze().cpu().numpy()
                action_unscaled = self.action_class.recover_action(action_np)
                
                env_actions[env_id] = {
                    'action_np': action_np,
                    'action_unscaled': action_unscaled,
                    'log_prob': log_prob.cpu().numpy(),
                    'value': value.squeeze().cpu().numpy()
                }
                
                # Store transition data (state that generated the action)
                self.state_arrays.append(state_array.copy())
                self.figure_arrays.append(figure_array.copy())
                self.actions.append(action_np)
                self.values.append(env_actions[env_id]['value'])
                self.log_probs.append(env_actions[env_id]['log_prob'])
            
            # Send actions to all environments
            for env_id in active_envs:
                self.action_queues[env_id].put({
                    'env_id': env_id,
                    'action': env_actions[env_id]['action_unscaled']
                })
            
            # Collect results from all environments
            for env_id in active_envs.copy():  # Use copy to safely modify during iteration
                try:
                    result = self.result_queues[env_id].get(timeout=10.0)
                    
                    if result['type'] == 'step':
                        # Store step results
                        self.rewards.append(result['reward'])
                        self.dones.append(result['done'])
                        
                        # Update environment state
                        self.env_states[env_id] = {
                            'state_array': result['state_array'],
                            'figure_array': result['figure_array'],
                            'episode_reward': result['episode_reward'],
                            'episode_length': result['episode_length']
                        }
                        
                        steps_collected += 1
                        
                        print(f"step {steps_collected:02d} (env {env_id}) | "
                              f"action: {env_actions[env_id]['action_unscaled']} | "
                              f"reward: {result['reward']:.2e}")
                        
                        if result['done']:
                            # Episode finished
                            self.training_stats['episode_rewards'].append(result['episode_reward'])
                            self.training_stats['episode_lengths'].append(result['episode_length'])
                            
                            # Wait for reset state
                            reset_result = self.result_queues[env_id].get(timeout=5.0)
                            if reset_result['type'] == 'reset':
                                self.env_states[env_id] = {
                                    'state_array': reset_result['state_array'],
                                    'figure_array': reset_result['figure_array'],
                                    'episode_reward': reset_result['episode_reward'],
                                    'episode_length': reset_result['episode_length']
                                }
                    
                    elif result['type'] == 'error':
                        print(f"Environment {env_id} error: {result['error']}")
                        active_envs.remove(env_id)
                        
                except queue.Empty:
                    print(f"Environment {env_id} timeout")
                    active_envs.remove(env_id)
                except Exception as e:
                    print(f"Environment {env_id} exception: {e}")
                    active_envs.remove(env_id)
        
        if not active_envs:
            raise RuntimeError("All environments failed")
        
        # Use the first available environment's current state for bootstrapping GAE
        bootstrap_env_id = next(iter(active_envs))
        bootstrap_state = self.env_states[bootstrap_env_id]
        
        bootstrap_state_tensor = torch.FloatTensor(bootstrap_state['state_array']).unsqueeze(0).to(self.device)
        bootstrap_figure_tensor = torch.FloatTensor(bootstrap_state['figure_array']).unsqueeze(0).to(self.device)
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
    
    def _get_state_from_env(self, env_idx: int = 0) -> Tuple[np.ndarray, np.ndarray]:
        '''
        Get state from a specific environment for RL training
        This method is not used in the multiprocessing version as states are 
        communicated through queues, but kept for compatibility.
        
        Parameters:
        -----------
        env_idx: int
            Index of the environment to get state from
        
        Returns:
        --------
        state_array: np.ndarray [dim_state]
            parametric state features
        figure_array: np.ndarray [n_interp_points, 4]  
            airfoil geometry and Mach data [yu, yl, mwu, mwl]
        '''
        if env_idx in self.env_states:
            return self.env_states[env_idx]['state_array'], self.env_states[env_idx]['figure_array']
        else:
            raise RuntimeError(f"Environment {env_idx} state not available")

    def train(self, total_time_steps: int, log_interval: int = 10, save_interval: int = 100,
              save_path: str = 'ppo_model.pt', plot_training: bool = True,
              plot_path: str = 'training_progress.png') -> None:
        '''
        Train the PPO agent with proper multiprocessing cleanup
        '''
        try:
            # Call parent train method
            super().train(total_time_steps, log_interval, save_interval, save_path, plot_training, plot_path)
        finally:
            # Ensure workers are stopped even if training fails
            self._stop_workers()

    def shutdown(self):
        '''Explicitly shutdown all worker processes'''
        self._stop_workers()

