'''
Template gym environment for reinforcement learning.
'''
import os
import json
from typing import List, Dict, Tuple, Any

import gymnasium as gym
from gymnasium import spaces
import numpy as np
import matplotlib.pyplot as plt

from pyTSFoil.pytsfoil import PyTSFoil
from pyTSFoil.environment.basic import Reward, Action, FigureState


def dist_clustcos(nn: int, a0=0.0079, a1=0.96, beta=1.0) -> np.ndarray:
    '''
    Point distribution on x-axis [0, 1]. (More points at both ends)

    Parameters
    ----------
    nn: int
        total amount of points
        
    a0: float
        Parameter for distributing points near x=0.
        Smaller a0, more points near x=0.
        
    a1: float
        Parameter for distributing points near x=1.
        Larger a1, more points near x=1.
        
    beta: float
        Parameter for distribution points.
    
    Examples
    ---------
    >>> xx = dist_clustcos(n, a0, a1, beta)

    '''
    aa = np.power((1-np.cos(a0*np.pi))/2.0, beta)
    dd = np.power((1-np.cos(a1*np.pi))/2.0, beta) - aa
    yt = np.linspace(0.0, 1.0, num=nn)
    a  = np.pi*(a0*(1-yt)+a1*yt)
    xx = (np.power((1-np.cos(a))/2.0,beta)-aa)/dd

    return xx


class NumpyArrayEncoder(json.JSONEncoder):
    '''
    Convert ndarray to list and numpy scalars to Python types for json dump
    '''
    def default(self, o: Any) -> Any:
        if isinstance(o, np.ndarray):
            return o.tolist()
        elif isinstance(o, np.integer):
            return int(o)
        elif isinstance(o, np.floating):
            return float(o)
        elif isinstance(o, np.bool_):
            return bool(o)
        return super().default(o)


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
            cl_target : float|None = None,
            output_dir : str|None = None,
            render_mode: str = 'both',  # 'display', 'save', 'both', 'none'
            state_class: FigureState = None,
            action_class: Action = None,
            n_max_step: int = 10,
            reward_class: Reward|None = None,
            ) -> None:
        
        super(TSFoilEnv_Template, self).__init__()
        
        self.name = 'TSFoilEnv_Template'
        self.ID = 0
        
        self.angle_of_attack = angle_of_attack
        self.mach_infinity = mach_infinity
        self.cl_target_input = cl_target
        self.cl_target = None
        
        self.state_class = state_class if state_class is not None else FigureState()
        self.action_class = action_class if action_class is not None else Action(dim_action=1)
        
        if reward_class is None:
            self.reward_class = Reward()
        else:
            self.reward_class = reward_class
        
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
        self.is_current_step_valid = True
        self.is_action_valid = True
        
        self.n_airfoil_points = 101 # Number of points in the airfoil surfaces
        self.x_airfoil_surface = dist_clustcos(self.n_airfoil_points)
        
        interp_coordinates = self._preprocess_airfoil(airfoil_coordinates)
        
        self.airfoil_coordinates = interp_coordinates.copy()
        self.airfoil_coordinates_initial = interp_coordinates.copy()

        self.render_mode = render_mode
        
        self.n_max_step = n_max_step
        
        if output_dir is None:
            self.output_dir = os.path.dirname(__file__)
        else:
            self.output_dir = output_dir
                
        self.pytsfoil = PyTSFoil(
            airfoil_coordinates=self.airfoil_coordinates,
            work_dir=None,
            output_dir=self.output_dir
        )
        
        # Initialize trajectory data storage
        self._init_trajectory()
        
        # Initialize rendering components
        self._init_render()
        
        self.render_fig_fname = 'tsfoil_gym_render.png'

    def step(self, action: np.ndarray) -> tuple[np.ndarray, float, bool, dict]:
        
        self.i_current_step += 1
        
        previous_state = self._get_state_from_reference_step()
        self._apply_action_to_reference_step(action)
        self._run_simulation()
        self._get_reward_and_validity()
        self._get_done()
        self._get_info()
        self._get_observation()
        
        # Store trajectory data (previous_state, action, reward, next_state) + all step info
        self._store_trajectory_data(previous_state, action, self.reward, self.observation)
        
        # Update reference step for the next step
        self._update_reference_step()
        
        return self.observation, self.reward, self.done, self.info

    def reset(self) -> np.ndarray:
        '''
        Reset the environment.
        '''
        self.airfoil_coordinates = self.airfoil_coordinates_initial.copy()
        self.pytsfoil.airfoil['coordinates'] = self.airfoil_coordinates_initial.copy()
        
        self._run_simulation()

        self.i_current_step = 0
        self.i_reference_step = 0
        self.is_current_step_valid = True
        self.is_action_valid = True
        
        action = np.zeros(self.dim_action)
        self.reward = 0.0
        self.total_reward = 0.0
        self.done = False
        self._get_info()
        self._get_observation()
        
        if self.cl_target_input is None:
            self.cl_target = self.pytsfoil.data_summary['cl']
        else:
            self.cl_target = self.cl_target_input
            
        # Clear trajectory data
        self._init_trajectory()
        self._store_trajectory_data(self.observation, action, self.reward, self.observation)
        self._init_render()
        
        return self.observation

    def render(self) -> None:
        '''
        Render the environment.
        '''
        if self.render_mode in ['none']:
            return
        
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

    def _preprocess_airfoil(self, airfoil_coordinates: np.ndarray) -> np.ndarray:
        '''
        Preprocess the airfoil coordinates.
        
        Interpolate the airfoil upper and lower surfaces to 101 points,
        and concatenate them to form a new airfoil with 201 points.
        
        Parameters
        ----------
        airfoil_coordinates: np.ndarray
            The airfoil coordinates to be preprocessed
            
        Returns
        -------
        interp_coordinates: np.ndarray
            The preprocessed airfoil coordinates
        '''
        x = airfoil_coordinates[:,0]
        y = airfoil_coordinates[:,1]
        
        le_pos = x.argmin()
        xu = x[:le_pos+1][::-1]
        yu = y[:le_pos+1][::-1]
        xl = x[le_pos:]
        yl = y[le_pos:]

        # Interpolate the airfoil
        yu_interp = np.interp(self.x_airfoil_surface, xu, yu)
        yl_interp = np.interp(self.x_airfoil_surface, xl, yl)
        
        interp_coordinates = np.zeros((2*self.n_airfoil_points-1, 2))
        interp_coordinates[:,0] = np.concatenate((self.x_airfoil_surface[::-1], self.x_airfoil_surface[1:]))
        interp_coordinates[:,1] = np.concatenate((yu_interp[::-1], yl_interp[1:]))

        return interp_coordinates

    def _run_simulation(self) -> None:
        '''
        Run the pytsfoil, the airfoil coordinates are defined outside.
        '''
        self.pytsfoil.set_config(
            ALPHA=self.angle_of_attack,
            EMACH=self.mach_infinity,
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
        
        self.pytsfoil.run()

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
        
        self.is_action_valid = True

    def _get_observation(self) -> np.ndarray:
        '''
        Get the observation.
        '''
        self.observation = np.zeros(self.dim_observation)
        
        return self.observation

    def _get_observation_for_RL(self, *args, **kwargs) -> Any:
        '''
        Get the observation for RL.
        '''
        return self.observation

    def _get_reward_and_validity(self) -> float:
        '''
        Get the reward, total reward, and validity.
        
        When the reward is greater than the critical reward, the current step is considered as valid.
        '''
        cl = self.pytsfoil.data_summary['cl']
        cd = self.pytsfoil.data_summary['cd']
        
        cd_old = self.get_data_from_trajectory(self.i_reference_step, 'cd')
        
        self.reward = self.reward_class.calculate_reward(cl, cd, cd_old, self.cl_target)
        
        #* Check if the current step is valid
        if self.reward > self.reward_class.critical_reward_to_update_reference_step:
            self.total_reward += self.reward
            self.is_current_step_valid = True
        else:
            self.is_current_step_valid = False
            
        if not self.is_action_valid:
            self.reward = self.reward_class.invalid_action_penalty_reward
            self.is_current_step_valid = False
        
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
            'is_current_step_valid': self.is_current_step_valid,
            'is_action_valid': self.is_action_valid,
            
            'airfoil_coordinates': self.airfoil_coordinates.copy(),
            
            'total_reward': self.total_reward,
            
            'alpha': self.pytsfoil.data_summary['alpha'],
            'mach': self.pytsfoil.data_summary['mach'],

            'cl': self.pytsfoil.data_summary['cl'],
            'cm': self.pytsfoil.data_summary['cm'],
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
        When the current step is valid, the reference step is updated.
        '''
        if self.is_current_step_valid:
            self.i_reference_step = self.i_current_step

    #* Undo related functions

    def undo_last_step(self) -> None:
        '''
        Undo the last step. 
        
        Note: 
        - Run before `step()` is called.
        - When the `step()` has not been called, the `i_current_step` is actually the last step.
        - The action is always applied to the reference step.
        - The reference step is not updated when the previous step is invalid.
        - So, usually, there is no need to manually undo the last step.
        '''
        # Check if there's anything to undo
        if self.i_current_step <= 0 or len(self.trajectory) == 0:
            return  # Nothing to undo (at initial state)
        
        # Mark the current step as invalid if it exists in trajectory
        if len(self.trajectory) > self.i_current_step:
            self.trajectory[self.i_current_step]['info']['is_current_step_valid'] = False
            self.is_current_step_valid = False
        
        # Find the most recent valid step to use as new reference
        new_reference_step = 0  # Default to initial step
        for i in range(self.i_current_step - 1, -1, -1):
            if i < len(self.trajectory) and self.trajectory[i]['info']['is_current_step_valid']:
                new_reference_step = i
                break
        
        # Update reference step
        self.i_reference_step = new_reference_step
        
        # Restore airfoil coordinates from the reference step
        if self.i_reference_step < len(self.trajectory):
            
            ref_airfoil_coordinates = self.trajectory[self.i_reference_step]['info']['airfoil_coordinates']
            self.airfoil_coordinates = ref_airfoil_coordinates.copy()
            self.pytsfoil.airfoil['coordinates'] = self.airfoil_coordinates

            ref_entry = self.trajectory[self.i_reference_step]
            self.observation = ref_entry['next_state'].copy()
            self.reward = ref_entry['reward']
            self.done = ref_entry['done']
            self.info = ref_entry['info'].copy()
            self.total_reward = ref_entry['info']['total_reward']
            
        else:
            raise ValueError(f"No valid reference step found, {self.i_reference_step}")
    
    #* Trajectory related functions
    
    def _init_trajectory(self) -> None:
        '''
        Initialize trajectory storage for all data.
        
        The trajectory is a list of dictionaries containing:
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
            'previous_state': previous_state.copy(),
            'action': action.copy() if hasattr(action, 'copy') else action,
            'reward': reward,
            'next_state': next_state.copy(),
            'done': self.done,
            'info': self.info.copy() if isinstance(self.info, dict) else self.info
        }
        
        self.trajectory.append(trajectory_entry)
    
    def get_data_from_trajectory(self, step: int, key: str) -> Any:
        '''
        Get data from the trajectory.
        
        Parameters
        ----------
        step: int
            The step number
        key: str
            The key of the data
        '''
        if len(self.trajectory) == 0:
            raise ValueError("Trajectory is empty")
        
        if step < 0 or step >= len(self.trajectory):
            raise ValueError(f"Step {step} is out of range")
        
        if key in self.trajectory[step].keys():
            return self.trajectory[step][key]
        elif key in self.trajectory[step]['info'].keys():
            return self.trajectory[step]['info'][key]
        else:
            raise ValueError(f"Key {key} not found in trajectory")

    def get_trajectory_as_tuples(self) -> List[Tuple[np.ndarray, Any, float, np.ndarray]]:
        '''
        Get trajectory data as list of tuples (previous_state, action, reward, next_state).
        
        Note, only the valid steps are included.
        
        Returns:
        --------
        trajectory: List[Tuple[np.ndarray, Any, float, np.ndarray]]
            List of tuples containing (previous_state, action, reward, next_state)
        '''
        trajectory = []
        
        for entry in self.trajectory:
            
            if entry['info']['is_current_step_valid']:
                trajectory.append((entry['previous_state'], entry['action'], entry['reward'], entry['next_state']))
        
        return trajectory
    
    def save_trajectory(self, fname_json: str) -> None:
        '''
        Save trajectory data to a file.
        
        Parameters
        ----------
        fname_json: str
            Path to save the trajectory data
        '''
        with open(fname_json, 'w') as f:
            json.dump(self.trajectory, f, indent=4, cls=NumpyArrayEncoder)
    
    def load_trajectory(self, fname_json: str) -> None:
        '''
        Load trajectory data from a file and convert lists back to numpy arrays.
        
        Parameters
        ----------
        fname_json: str
            Path to load the trajectory data from
        '''
        with open(fname_json, 'r') as file:
            self.trajectory = json.load(file)
        
        # Convert specific fields back to numpy arrays
        array_fields_trajectory = ['previous_state', 'action', 'next_state']
        array_fields_info = ['airfoil_coordinates', 'xx', 'cpu', 'cpl', 'mau', 'mal']
        
        for entry in self.trajectory:
            # Convert trajectory level arrays
            for field in array_fields_trajectory:
                if field in entry and isinstance(entry[field], list):
                    entry[field] = np.array(entry[field])
            
            # Convert info level arrays
            if 'info' in entry and isinstance(entry['info'], dict):
                for field in array_fields_info:
                    if field in entry['info'] and isinstance(entry['info'][field], list):
                        entry['info'][field] = np.array(entry['info'][field])
    
    def get_trajectory_length(self) -> int:
        '''
        Get the length of the trajectory (number of valid steps).
        '''
        return sum(1 for entry in self.trajectory if entry['info']['is_current_step_valid'])
    
    #* Render related functions
    
    def _init_render(self) -> None:
        '''
        Initialize rendering components.
        '''
        if self.render_mode in ['none']:
            return
        
        # Close the existing figure
        if hasattr(self, 'fig') and self.fig is not None:
            plt.close(self.fig)
            self.fig = None
        
        # Set up matplotlib for interactive mode if displaying
        if self.render_mode in ['display', 'both']:
            plt.ion()  # Turn on interactive mode
        
        # Create figure and axes - now with 3 subplots for historical plots
        self.fig, self.axes = plt.subplots(2, 2, figsize=(16, 10))
        self.fig.suptitle('pyTSFoil gym environment - Historical View', fontsize=16, fontweight='bold')
        
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
        self.axes[3].set_ylabel('Reward')
        self.axes[3].set_title('Reward History')
        self.axes[3].grid(True, alpha=0.3)

    def _print_state(self) -> None:
        '''
        Print current state information.
        '''
        if hasattr(self, 'info') and self.info:
            print(f"\n--- Step {self.i_current_step:3d} ---")
            print(f"Lift Coeff (CL): {self.info.get('cl', 0.0):.4f}")
            print(f"Drag Coeff (CD): {self.info.get('cd', 0.0):.6f}")
            print(f"Reward:          {self.reward:.2f}")
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
            linewidth = 3.0 if i==0 else 1.0
            step_num = entry['info']['i_current_step']
            is_valid = entry['info'].get('is_current_step_valid', True)
            linestyle = '-' if is_valid else '--'  # Dashed line for invalid steps
            label = f'Step {step_num}'  # Show all step numbers in legend
            self.axes[0].plot(coords[:, 0], coords[:, 1], 
                            color=colors[i], alpha=alpha, linewidth=linewidth, 
                            linestyle=linestyle, label=label)
        
        # Plot 2: Mach number distribution evolution
        for i, entry in enumerate(self.trajectory):
            xx = entry['info']['xx']
            mau = entry['info']['mau']
            mal = entry['info']['mal']
            if len(xx) > 0:  # Only plot if we have data
                alpha = 0.3 + 0.7 * (i / max(1, n_steps - 1))
                step_num = entry['info']['i_current_step']
                is_valid = entry['info'].get('is_current_step_valid', True)
                linewidth = 3.0 if i==0 else 1.0
                linestyle = '-' if is_valid else '--'  # Dashed line for invalid steps
                label_upper = f'Step {step_num}'
                
                self.axes[1].plot(xx, mau, color=colors[i], alpha=alpha, 
                                linewidth=linewidth, linestyle=linestyle, label=label_upper)
                self.axes[1].plot(xx, mal, color=colors[i], alpha=alpha, 
                                linewidth=linewidth, linestyle=linestyle, label=None)
        
        # Plot 3: Performance metrics history
        steps = [entry['info']['i_current_step'] for entry in self.trajectory]
        cl_values = [entry['info']['cl'] for entry in self.trajectory]
        cd_values = [entry['info']['cd'] * 100 for entry in self.trajectory]
        
        if cl_values and cd_values:
            self.axes[2].plot(steps, cl_values, 'b-o', label='CL', markersize=4)
            self.axes[2].plot(steps, cd_values, 'r-s', label='CD (*100)', markersize=4)
            self.axes[2].legend()
        
        # Plot 4: Reward history
        reward_values = [entry['reward'] for entry in self.trajectory]
        total_reward_values = [entry['info']['total_reward'] for entry in self.trajectory]
        if reward_values:
            self.axes[3].plot(steps, reward_values, 'g-^', label='Reward', markersize=4)
            self.axes[3].plot(steps, total_reward_values, 'r-o', label='Total Reward', markersize=4)
            self.axes[3].legend()
        
        # Update legends for geometry and mach plots
        if n_steps > 0:
            self.axes[0].legend()
            self.axes[1].legend()
        
        # Update overall title with current step information
        title = f'pyTSFoil gym environment - Historical View'
        self.fig.suptitle(title, fontsize=16, fontweight='bold')

    def _save_current_frame(self) -> None:
        '''
        Save the current frame to disk.
        '''
        if self.render_mode in ['save', 'both']:
            # Save current state render
            render_path = os.path.join(self.output_dir, self.render_fig_fname)
            self.fig.savefig(render_path, dpi=300, bbox_inches='tight')

