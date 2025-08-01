'''
Template gym environment for reinforcement learning.
'''
import os
from typing import List, Dict, Tuple, Any

import gymnasium as gym
from gymnasium import spaces
import numpy as np
from pyTSFoil.pytsfoil import PyTSFoil
import matplotlib.pyplot as plt


class TSFoilEnv_Template(gym.Env):
    '''
    Template gym environment for reinforcement learning.
    
    Special settings:
    - The action is applied to the airfoil geometry of the reference step.
    - The reference step is not necessarily the previous step.
    - Only when the reward is greater than the critical reward, the reference step is updated.

    The settings in the template are:
    - The action is left empty.
    - The observation is left empty.
    - The reward is the lift to drag ratio. (default)
    - The done is the step number is greater than the maximum step number. (default)
    - The info is the information of the current step. (default)
    '''
    def __init__(self, 
            airfoil_coordinates: np.ndarray,
            angle_of_attack : float = 0.5,
            mach_infinity : float = 0.75,
            output_dir : str|None = None,
            render_mode: str = 'both',  # 'display', 'save', 'both'
            n_max_step: int = 100,
            critical_reward: float = 0.0,
            ) -> None:
        
        super(TSFoilEnv_Template, self).__init__()
        
        self.name = 'TSFoilEnv_Template'
        self.ID = 0
        
        self.dim_action = 1
        self.dim_observation = 1

        self.action_space = spaces.Box(low=-1.0, high=1.0, shape=(self.dim_action,))
        self.observation_space = spaces.Box(low=0.0, high=1.0, shape=(self.dim_observation,))

        self.observation = np.zeros(self.dim_observation)
        self.reward = 0.0
        self.total_reward = 0.0
        self.done = False
        self.info = {}

        self.i_current_step = 0
        self.i_reference_step = 0
        self.airfoil_coordinates = airfoil_coordinates.copy()
        self.airfoil_coordinates_initial = airfoil_coordinates.copy()

        self.render_mode = render_mode
        
        self.n_max_step = n_max_step
        self.critical_reward_to_update_reference_step = critical_reward
        
        if output_dir is None:
            self.output_dir = os.path.dirname(__file__)
        else:
            self.output_dir = output_dir
                
        self.pytsfoil = PyTSFoil(
            airfoil_coordinates=self.airfoil_coordinates,
            work_dir=None,
            output_dir=self.output_dir
        )
        
        self.pytsfoil.set_config(
            ALPHA=angle_of_attack,
            EMACH=mach_infinity,
            MAXIT=9999,
            NWDGE=0,
            n_point_x=200,
            n_point_y=80,
            n_point_airfoil=100,
            EPS=0.2,
            CVERGE=1e-6,
            flag_output_solve=False,
            flag_output_summary=False,
            flag_output_shock=False,
            flag_output_field=False,
            flag_print_info=False,
        )
        
        self.pytsfoil.initialize_data()
        
        # Initialize trajectory data storage
        self._init_trajectory()
        
        # Initialize rendering components
        self._init_render()

    def step(self, action: np.ndarray) -> tuple[np.ndarray, float, bool, dict]:
        
        self.i_current_step += 1
        
        previous_state = self._get_state_from_reference_step()
        self._apply_action_to_reference_step(action)
        self._run_simulation()
        self._get_observation()
        self._get_reward()
        self._get_done()
        self._get_info()
        
        # Store trajectory data (previous_state, action, reward, next_state) + all step info
        self._store_trajectory_data(previous_state, action, self.reward, self.observation)
        
        # Update reference step for the next step
        self._update_reference_step()
        
        return self.observation, self.reward, self.done, self.info

    def reset(self) -> np.ndarray:
        '''
        Reset the environment.
        '''
        self.pytsfoil.airfoil['coordinates'] = self.airfoil_coordinates_initial
        
        self._run_simulation()
        
        action = np.zeros(self.dim_action)
        self._get_observation()
        self.reward = 0.0
        self.total_reward = 0.0
        self.done = False
        self._get_info()

        self.i_current_step = 0
        self.i_reference_step = 0
        
        # Clear trajectory data
        self._init_trajectory()
        self._store_trajectory_data(self.observation, action, self.reward, self.observation)
        
        return self.observation

    def render(self) -> None:
        '''
        Render the environment.
        '''
        # Update the plots
        self._update_plots()
        
        # Handle display/saving based on render mode
        if self.render_mode in ['display', 'both']:
            self._print_state() # Print current state information
            plt.draw()
            plt.pause(0.01)  # Small pause to allow display update
        
        if self.render_mode in ['save', 'both']:
            self._save_current_frame()

    def close(self) -> None:
        '''
        Close the environment and clean up resources.
        '''
        if hasattr(self, 'fig') and self.fig is not None:
            plt.close(self.fig)
            self.fig = None

    def _run_simulation(self) -> None:
        '''
        Run the pytsfoil.
        '''
        self.pytsfoil.set_airfoil()
        
        self.pytsfoil.set_mesh()
        
        self.pytsfoil.compute_mesh_indices()

        self.pytsfoil.run_fortran_solver()
        
        self.pytsfoil.compute_data_summary()
        
        self.pytsfoil.print_summary()

    def _get_state_from_reference_step(self) -> np.ndarray:
        '''
        Get the state from the reference step.
        '''
        return self.trajectory[self.i_reference_step]['next_state'].copy()

    def _apply_action_to_reference_step(self, action: np.ndarray) -> None:
        '''
        Apply action to the airfoil of the reference step, update the airfoil coordinates.
        
        In this template, the action is scaling the airfoil thickness.
        The x-coordinate of the reference airfoil is kept unchanged.
        '''
        ref_airfoil_coordinates = self.trajectory[self.i_reference_step]['info']['airfoil_coordinates']
        
        self.airfoil_coordinates[:,0] = ref_airfoil_coordinates[:,0]
        self.airfoil_coordinates[:,1] = ref_airfoil_coordinates[:,1] * action[0]
        
        self.pytsfoil.airfoil['coordinates'] = self.airfoil_coordinates

    def _get_observation(self) -> np.ndarray:
        '''
        Get the observation.
        '''
        self.observation = np.zeros(self.dim_observation)
        
        return self.observation

    def _get_reward(self) -> float:
        '''
        Get the reward.
        '''
        cl = self.pytsfoil.data_summary['cl']
        cd = self.pytsfoil.data_summary['cd']
        
        self.reward = cl / cd
        
        return self.reward

    def _get_done(self) -> bool:
        '''
        Get the done.
        '''
        self.done = self.i_current_step >= self.n_max_step
        return self.done

    def _get_info(self) -> dict:
        '''
        Get the info.
        '''
        self.info = {
            
            'i_current_step': self.i_current_step,
            'i_reference_step': self.i_reference_step,
            
            'airfoil_coordinates': self.airfoil_coordinates,
            
            'alpha': self.pytsfoil.data_summary['alpha'],
            'mach': self.pytsfoil.data_summary['mach'],

            'cl': self.pytsfoil.data_summary['cl'],
            'cd': self.pytsfoil.data_summary['cd'],
            'cd_int': self.pytsfoil.data_summary['cd_int'],
            'cd_wave': self.pytsfoil.data_summary['cd_wave'],
            'cd_body': self.pytsfoil.data_summary['cd_body'],
            
            'xx': self.pytsfoil.mesh['xx'],
            'cpu': self.pytsfoil.data_summary['cpu'],
            'cpl': self.pytsfoil.data_summary['cpl'],
            'mau': self.pytsfoil.data_summary['mau'],
            'mal': self.pytsfoil.data_summary['mal'],

            }
        
        return self.info

    def _update_reference_step(self) -> None:
        '''
        Update the reference step.
        '''
        if self.reward > self.critical_reward_to_update_reference_step:
            self.i_reference_step = self.i_current_step
            self.total_reward += self.reward

    def _init_trajectory(self) -> None:
        '''
        Initialize trajectory storage for all data.
        The trajectory is a list of dictionaries containing:
        - step: the step number
        - previous_state: the previous state
        - action: the action
        - reward: the reward
        - next_state: the next state
        - done: the done
        - info: the info
        '''
        # Initialize trajectory storage: List of dictionaries containing 
        # (previous_state, action, reward, next_state) + all step info
        self.trajectory: List[Dict[str, Any]] = []

    def _store_trajectory_data(self, previous_state: np.ndarray, action: np.ndarray, 
                             reward: float, next_state: np.ndarray) -> None:
        '''
        Store trajectory data including (previous_state, action, reward, next_state) 
        and all related information.
        '''
        trajectory_entry = {
            'step': self.i_current_step,
            'reference_step': self.i_reference_step,
            'previous_state': previous_state.copy(),
            'action': action.copy() if hasattr(action, 'copy') else action,
            'reward': reward,
            'next_state': next_state.copy(),
            'done': self.done,
            'info': self.info.copy() if isinstance(self.info, dict) else self.info
        }
        
        self.trajectory.append(trajectory_entry)
    
    def get_trajectory_as_tuples(self) -> List[Tuple[np.ndarray, Any, float, np.ndarray]]:
        '''
        Get trajectory data as list of tuples (previous_state, action, reward, next_state).
        
        Returns:
            List of tuples containing (previous_state, action, reward, next_state)
        '''
        return [(entry['previous_state'], entry['action'], entry['reward'], entry['next_state']) 
                for entry in self.trajectory]
    
    def save_trajectory(self, filepath: str) -> None:
        '''
        Save trajectory data to a file.
        
        Args:
            filepath: Path to save the trajectory data
        '''
        import pickle
        with open(filepath, 'wb') as f:
            pickle.dump(self.trajectory, f)
    
    def load_trajectory(self, filepath: str) -> None:
        '''
        Load trajectory data from a file.
        
        Args:
            filepath: Path to load the trajectory data from
        '''
        import pickle
        with open(filepath, 'rb') as f:
            self.trajectory = pickle.load(f)
    
    def get_trajectory_history(self) -> Dict[str, List]:
        '''
        Extract history-like data structure from trajectory for compatibility.
        
        Returns:
            Dictionary with historical data extracted from trajectory
        '''
        if not self.trajectory:
            return {
                'airfoil_coordinates': [],
                'step_numbers': [],
                'cl': [],
                'cd': [],
                'ld_ratio': [],
                'xx': [],
                'mau': [],
                'mal': [],
            }
        
        history = {
            'airfoil_coordinates': [],
            'step_numbers': [],
            'cl': [],
            'cd': [],
            'ld_ratio': [],
            'xx': [],
            'mau': [],
            'mal': [],
        }
        
        for entry in self.trajectory:
            history['step_numbers'].append(entry['step'])
            history['airfoil_coordinates'].append(entry['history_snapshot']['airfoil_coordinates'])
            history['cl'].append(entry['history_snapshot']['cl'])
            history['cd'].append(entry['history_snapshot']['cd'])
            history['ld_ratio'].append(entry['history_snapshot']['ld_ratio'])
            history['xx'].append(entry['history_snapshot']['xx'])
            history['mau'].append(entry['history_snapshot']['mau'])
            history['mal'].append(entry['history_snapshot']['mal'])
        
        return history

    def _init_render(self) -> None:
        '''
        Initialize rendering components.
        '''
        # Set up matplotlib for interactive mode if displaying
        if self.render_mode in ['display', 'both']:
            plt.ion()  # Turn on interactive mode
        
        # Create figure and axes - now with 3 subplots for historical plots
        self.fig, self.axes = plt.subplots(2, 2, figsize=(16, 10))
        self.fig.suptitle('pyTSFOIL gym environment - Historical View', fontsize=16, fontweight='bold')
        
        # Flatten axes for easier access
        self.axes = self.axes.flatten()
        
        # Initialize empty lines for real-time updating (these will be cleared and redrawn)
        self.geometry_lines = []  # Will hold lines for all airfoil geometries
        self.mach_lines = []      # Will hold lines for all Mach distributions
        
        # Add sonic reference line
        self.sonic_line = self.axes[1].axhline(y=1.0, color='k', linestyle='--', alpha=0.5, label='Sonic (M=1)')
        
        # Set up axes properties
        self._setup_axes()
        
        plt.tight_layout()

    def _setup_axes(self) -> None:
        '''
        Set up axes properties.
        '''
        # Top left: Airfoil geometry evolution
        self.axes[0].set_xlim([-0.1, 1.1])
        self.axes[0].set_ylim([-0.11, 0.11])
        self.axes[0].set_xlabel('X/c')
        self.axes[0].set_ylabel('Y/c')
        self.axes[0].set_title('Airfoil Geometry Evolution')
        self.axes[0].grid(True, alpha=0.3)
        # self.axes[0].set_aspect('equal')
        
        # Top right: Mach distribution evolution
        self.axes[1].set_xlabel('X/c')
        self.axes[1].set_ylabel('Mach Number')
        self.axes[1].set_title('Mach Number Distribution Evolution')
        self.axes[1].grid(True, alpha=0.3)
        self.axes[1].set_xlim([-0.2, 1.2])
        self.axes[1].set_ylim([-0.1, 1.5])
        self.axes[1].legend()
        
        # Bottom left: Performance metrics over time
        self.axes[2].set_xlabel('Step')
        self.axes[2].set_ylabel('Coefficient Value')
        self.axes[2].set_title('Performance Metrics History')
        self.axes[2].grid(True, alpha=0.3)
        
        # Bottom right: L/D ratio over time
        self.axes[3].set_xlabel('Step')
        self.axes[3].set_ylabel('L/D Ratio')
        self.axes[3].set_title('L/D Ratio History')
        self.axes[3].grid(True, alpha=0.3)

    def _print_state(self) -> None:
        '''
        Print current state information.
        '''
        if hasattr(self, 'info') and self.info:
            print(f"\n--- Step {self.i_current_step:3d} ---")
            print(f"Lift Coeff (CL): {self.info.get('cl', 0.0):.4f}")
            print(f"Drag Coeff (CD): {self.info.get('cd', 0.0):.6f}")
            print(f"L/D Ratio:       {self.reward:.2f}")
            print(f"Done:            {self.done}")

    def _update_plots(self) -> None:
        '''
        Update the existing plots with all historical data.
        '''
        # Clear existing plot lines
        for ax in self.axes:
            # Keep the grid and labels, but clear the data lines
            for line in ax.get_lines():
                if line != self.sonic_line:  # Don't remove the sonic reference line
                    line.remove()
        
        # Only plot if we have trajectory data
        if not hasattr(self, 'trajectory') or not self.trajectory:
            return
        
        # Generate colors for different steps (use colormap for smooth transition)
        n_steps = len(self.trajectory)
        colors = plt.cm.viridis(np.linspace(0, 1, n_steps))
        
        # Plot 1: Airfoil geometry evolution
        for i, entry in enumerate(self.trajectory):
            coords = entry['info']['airfoil_coordinates']
            alpha = 0.3 + 0.7 * (i / max(1, n_steps - 1))  # Fade older steps, highlight recent ones
            step_num = entry['step']
            label = f'Step {step_num}' if i == n_steps - 1 else None  # Only label the latest
            self.axes[0].plot(coords[:, 0], coords[:, 1], 
                            color=colors[i], alpha=alpha, linewidth=1, label=label)
        
        # Plot 2: Mach number distribution evolution
        for i, entry in enumerate(self.trajectory):
            xx = entry['info']['xx']
            mau = entry['info']['mau']
            mal = entry['info']['mal']
            if len(xx) > 0:  # Only plot if we have data
                alpha = 0.3 + 0.7 * (i / max(1, n_steps - 1))
                step_num = entry['step']
                label_upper = f'Step {step_num} (upper)' if i == n_steps - 1 else None
                label_lower = f'Step {step_num} (lower)' if i == n_steps - 1 else None
                
                self.axes[1].plot(xx, mau, color=colors[i], alpha=alpha, 
                                linewidth=1, linestyle='-', label=label_upper)
                self.axes[1].plot(xx, mal, color=colors[i], alpha=alpha, 
                                linewidth=1, linestyle='-', label=label_lower)
        
        # Plot 3: Performance metrics history
        steps = [entry['step'] for entry in self.trajectory]
        cl_values = [entry['info']['cl'] for entry in self.trajectory]
        cd_values = [entry['info']['cd'] for entry in self.trajectory]
        
        if cl_values and cd_values:
            self.axes[2].plot(steps, cl_values, 'b-o', label='CL', markersize=4)
            self.axes[2].plot(steps, cd_values, 'r-s', label='CD', markersize=4)
            self.axes[2].legend()
        
        # Plot 4: L/D ratio history
        ld_values = [entry['info']['ld_ratio'] for entry in self.trajectory]
        if ld_values:
            self.axes[3].plot(steps, ld_values, 'g-^', label='L/D Ratio', markersize=4)
            self.axes[3].legend()
        
        # Update legends for geometry and mach plots
        if n_steps > 0:
            self.axes[0].legend()
            self.axes[1].legend()
        
        # Update overall title with current step information
        title = f'pyTSFOIL gym environment - Historical View (Current Step: {self.i_current_step})'
        self.fig.suptitle(title, fontsize=16, fontweight='bold')

    def _save_current_frame(self) -> None:
        '''
        Save the current frame to disk.
        '''
        if self.render_mode in ['save', 'both']:
            # Save current state render
            render_path = os.path.join(self.output_dir, 'tsfoil_gym_render.png')
            self.fig.savefig(render_path, dpi=300, bbox_inches='tight')

