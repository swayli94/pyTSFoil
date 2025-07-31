'''
Empty gym environment for reinforcement learning.
'''
import os
import sys

path = os.path.dirname(os.path.abspath(__file__))
if path not in sys.path:
    sys.path.append(path)
    
import gymnasium as gym
from gymnasium import spaces
import numpy as np
from pytsfoil import PyTSFoil
import matplotlib.pyplot as plt


class TSFoilEnv_Empty(gym.Env):
    '''
    Empty gym environment for reinforcement learning.

    The action is left empty.

    The observation is the lift to drag ratio.
    '''
    def __init__(self, 
            airfoil_coordinates: np.ndarray,
            angle_of_attack : float = 0.5,
            mach_infinity : float = 0.75,
            output_dir : str|None = None,
            render_mode: str = 'both',  # 'display', 'save', 'both'
            ) -> None:
        
        super(TSFoilEnv_Empty, self).__init__()
        
        self.dim_action = 1
        self.dim_observation = 1

        self.action_space = spaces.Box(low=0.0, high=1.0, shape=(self.dim_action,))
        self.observation_space = spaces.Box(low=0.0, high=1.0, shape=(self.dim_observation,))

        self.observation = 0.0
        self.reward = 0.0
        self.done = False
        self.info = {}

        self.i_step = 0
        self.i_max_step = 100
        self.airfoil_coordinates = airfoil_coordinates.copy()
        
        # Rendering configuration
        self.render_mode = render_mode
        self.frame_count = 0
        
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
        
        # Initialize storage for historical data
        self._init_history()
        
        # Initialize rendering components
        self._init_render()

    def step(self, action) -> tuple[float, float, bool, dict]:
        
        self.i_step += 1
        
        self._apply_action(action)
        self._run_pytsfoil()
        self._get_observation()
        self._get_reward()
        self._get_done()
        self._get_info()
        
        # Store step data for historical plotting
        self._store_step_data()
        
        return self.observation, self.reward, self.done, self.info

    def reset(self) -> np.ndarray:
        
        self.i_step = 0
        self.frame_count = 0
        
        # Clear historical data
        self._init_history()
        
        self.pytsfoil.airfoil['coordinates'] = self.airfoil_coordinates
        
        self._run_pytsfoil()
        self._get_observation()
        self._get_reward()
        self._get_info()
        
        # Store initial state
        self._store_step_data()
        
        self.done = False
        
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
        
        self.frame_count += 1

    def close(self) -> None:
        '''
        Close the environment and clean up resources.
        '''
        if hasattr(self, 'fig') and self.fig is not None:
            plt.close(self.fig)
            self.fig = None

    def _run_pytsfoil(self) -> None:
        '''
        Run the pytsfoil.
        '''
        self.pytsfoil.set_airfoil()
        
        self.pytsfoil.set_mesh()
        
        self.pytsfoil.compute_mesh_indices()

        self.pytsfoil.run_fortran_solver()
        
        self.pytsfoil.compute_data_summary()
        
        self.pytsfoil.print_summary()

    def _apply_action(self, action: np.ndarray) -> None:
        '''
        Apply the action, update the airfoil coordinates.
        '''
        self.airfoil_coordinates[:,1] = self.airfoil_coordinates[:,1] * action[0]
        
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
        self.done = self.i_step >= self.i_max_step
        return self.done

    def _get_info(self) -> dict:
        '''
        Get the info.
        '''
        self.info = {
            
            'i_step': self.i_step,
            
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

    def _init_history(self) -> None:
        '''
        Initialize storage for historical data.
        '''
        self.history = {
            'airfoil_coordinates': [],
            'step_numbers': [],
            'cl': [],
            'cd': [],
            'ld_ratio': [],
            'xx': [],
            'mau': [],
            'mal': [],
        }

    def _store_step_data(self) -> None:
        '''
        Store current step data in history.
        '''
        # Store airfoil coordinates
        self.history['airfoil_coordinates'].append(self.airfoil_coordinates.copy())
        
        # Store step number
        self.history['step_numbers'].append(self.i_step)
        
        # Store performance metrics
        if hasattr(self, 'info') and self.info:
            self.history['cl'].append(self.info.get('cl', 0.0))
            self.history['cd'].append(self.info.get('cd', 0.0))
            self.history['ld_ratio'].append(self.reward)
            
            # Store Mach distribution data if available
            if 'xx' in self.info:
                self.history['xx'].append(self.info['xx'].copy())
                self.history['mau'].append(self.info['mau'].copy())
                self.history['mal'].append(self.info['mal'].copy())
            else:
                # Store empty arrays if no data available
                self.history['xx'].append(np.array([]))
                self.history['mau'].append(np.array([]))
                self.history['mal'].append(np.array([]))

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
            print(f"\n--- Step {self.i_step:3d} ---")
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
        
        # Only plot if we have historical data
        if not hasattr(self, 'history') or not self.history['step_numbers']:
            return
        
        # Generate colors for different steps (use colormap for smooth transition)
        n_steps = len(self.history['step_numbers'])
        colors = plt.cm.viridis(np.linspace(0, 1, n_steps))
        
        # Plot 1: Airfoil geometry evolution
        for i, coords in enumerate(self.history['airfoil_coordinates']):
            alpha = 0.3 + 0.7 * (i / max(1, n_steps - 1))  # Fade older steps, highlight recent ones
            step_num = self.history['step_numbers'][i]
            label = f'Step {step_num}' if i == n_steps - 1 else None  # Only label the latest
            self.axes[0].plot(coords[:, 0], coords[:, 1], 
                            color=colors[i], alpha=alpha, linewidth=1, label=label)
        
        # Plot 2: Mach number distribution evolution
        for i, (xx, mau, mal) in enumerate(zip(self.history['xx'], self.history['mau'], self.history['mal'])):
            if len(xx) > 0:  # Only plot if we have data
                alpha = 0.3 + 0.7 * (i / max(1, n_steps - 1))
                step_num = self.history['step_numbers'][i]
                label_upper = f'Step {step_num} (upper)' if i == n_steps - 1 else None
                label_lower = f'Step {step_num} (lower)' if i == n_steps - 1 else None
                
                self.axes[1].plot(xx, mau, color=colors[i], alpha=alpha, 
                                linewidth=1, linestyle='-', label=label_upper)
                self.axes[1].plot(xx, mal, color=colors[i], alpha=alpha, 
                                linewidth=1, linestyle='-', label=label_lower)
        
        # Plot 3: Performance metrics history
        if self.history['cl'] and self.history['cd']:
            steps = self.history['step_numbers']
            self.axes[2].plot(steps, self.history['cl'], 'b-o', label='CL', markersize=4)
            self.axes[2].plot(steps, self.history['cd'], 'r-s', label='CD', markersize=4)
            self.axes[2].legend()
        
        # Plot 4: L/D ratio history
        if self.history['ld_ratio']:
            steps = self.history['step_numbers']
            self.axes[3].plot(steps, self.history['ld_ratio'], 'g-^', label='L/D Ratio', markersize=4)
            self.axes[3].legend()
        
        # Update legends for geometry and mach plots
        if n_steps > 0:
            self.axes[0].legend()
            self.axes[1].legend()
        
        # Update overall title with current step information
        title = f'pyTSFOIL gym environment - Historical View (Current Step: {self.i_step})'
        self.fig.suptitle(title, fontsize=16, fontweight='bold')

    def _save_current_frame(self) -> None:
        '''
        Save the current frame to disk.
        '''
        if self.render_mode in ['save', 'both']:
            # Save current state render
            render_path = os.path.join(self.output_dir, 'tsfoil_gym_render.png')
            self.fig.savefig(render_path, dpi=300, bbox_inches='tight')


if __name__ == "__main__":
    '''
    Main function to test the environment.
    '''
    path = os.path.join('example', 'env_empty')
    
    # Create the environment
    x, y = np.loadtxt(os.path.join(path, 'rae2822.dat'), skiprows=1).T
    airfoil_coordinates = np.column_stack((x, y))
    
    env = TSFoilEnv_Empty(
            airfoil_coordinates=airfoil_coordinates,
            output_dir=path,
            render_mode='save',
            )
    
    # Run the environment
    env.reset()   
    env.render()
    
    for i in range(2):
        
        action = np.array([1.05])
        env.step(action)
        env.render()
        
    env.close()
    
