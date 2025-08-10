'''
Gym environment with CST-modeling tools.
'''
import numpy as np
from typing import Tuple
from gymnasium import spaces

from pyTSFoil.environment.env_template import TSFoilEnv_Template
from pyTSFoil.environment.basic import FigureState, GlobalModificationAction, BumpModificationAction, Reward


class TSFoilEnv_FigState_GlobalAction(TSFoilEnv_Template):
    '''
    Gym environment with FigureState and GlobalModificationAction.
    '''
    def __init__(self, 
            airfoil_coordinates: np.ndarray,
            angle_of_attack : float = 0.5,
            mach_infinity : float = 0.75,
            cl_target : float|None = None,
            output_dir : str|None = None,
            render_mode: str = 'both',  # 'display', 'save', 'both'
            path_save_fig_of_observation: str = None,
            state_class: FigureState = None,
            action_class: GlobalModificationAction = None,
            n_max_step: int = 10,
            reward_class: Reward|None = None,
            ) -> None:

        super().__init__(
            airfoil_coordinates=airfoil_coordinates,
            angle_of_attack=angle_of_attack,
            mach_infinity=mach_infinity,
            cl_target=cl_target,
            output_dir=output_dir,
            render_mode=render_mode,
            n_max_step=n_max_step,
            reward_class=reward_class,
        )

        self.state_class = state_class if state_class is not None else FigureState()
        self.action_class = action_class if action_class is not None else GlobalModificationAction()

        self.dim_action = self.action_class.dim_action
        self.dim_observation = self.state_class.dim_state

        self.action_space = spaces.Box(
            low=self.action_class.action_lower_bound.astype(np.float32), 
            high=self.action_class.action_upper_bound.astype(np.float32), 
            shape=(self.dim_action,), dtype=np.float32)
        self.observation_space = spaces.Box(
            low=self.state_class.state_lower_bound.astype(np.float32), 
            high=self.state_class.state_upper_bound.astype(np.float32), 
            shape=(self.dim_observation,), dtype=np.float32)
        
        self.observation = np.zeros(self.dim_observation)
        
        self.path_save_fig_of_observation = path_save_fig_of_observation

    def _apply_action_to_reference_step(self, action: np.ndarray) -> None:
        '''
        Apply the action, update the airfoil coordinates.
        
        Parameters
        ----------
        action: np.ndarray
            The unscaled action to be applied
        '''
        ref_airfoil_coordinates = self.trajectory[self.i_reference_step]['info']['airfoil_coordinates']
        
        # Split airfoil into upper and lower surfaces
        yu = ref_airfoil_coordinates[:,1][:self.n_airfoil_points][::-1]
        yl = ref_airfoil_coordinates[:,1][self.n_airfoil_points-1:]

        _, _, yu_new, yl_new, self.is_action_valid = self.action_class.apply_action(action, self.x_airfoil_surface, yu, yl)
        
        if self.is_action_valid:
            self.airfoil_coordinates[:,0] = ref_airfoil_coordinates[:,0]
            self.airfoil_coordinates[:,1] = np.concatenate((yu_new[::-1], yl_new[1:]))
            
        self.pytsfoil.airfoil['coordinates'] = self.airfoil_coordinates.copy()

    def _get_observation(self) -> Tuple[np.ndarray, str]:
        '''
        Get the observation.
        '''
        state_array, figure_base64 = self.state_class.calculate_state(
            x=self.x_airfoil_surface,
            yu=self.airfoil_coordinates[:,1][:self.n_airfoil_points][::-1],
            yl=self.airfoil_coordinates[:,1][self.n_airfoil_points-1:],
            xxu=self.info['xx'],
            xxl=self.info['xx'],
            mwu=self.info['mau'],
            mwl=self.info['mal'],
            Cl=self.info['cl'],
            Cd_wave=self.info['cd_wave'],
            Cm=self.info['cm'],
            save_fig_path=self.path_save_fig_of_observation
        )
        
        self.observation = state_array
        
        return self.observation, figure_base64
    
    def _get_observation_for_RL(self, n_interp_points: int = 101) -> Tuple[np.ndarray, np.ndarray]:
        '''
        Get the observation for RL.
        '''
        state_array, figure_array = self.state_class.calculate_state_for_RL(
            x=self.x_airfoil_surface,
            yu=self.airfoil_coordinates[:,1][:self.n_airfoil_points][::-1],
            yl=self.airfoil_coordinates[:,1][self.n_airfoil_points-1:],
            xxu=self.info['xx'],
            xxl=self.info['xx'],
            mwu=self.info['mau'],
            mwl=self.info['mal'],
            Cl=self.info['cl'],
            Cd_wave=self.info['cd_wave'],
            Cm=self.info['cm'],
            n_interp_points=n_interp_points
        )

        self.observation = state_array
        
        return self.observation, figure_array


class TSFoilEnv_FigState_BumpAction(TSFoilEnv_Template):
    '''
    Gym environment with FigureState and BumpModificationAction.
    '''
    def __init__(self, 
            airfoil_coordinates: np.ndarray,
            angle_of_attack : float = 0.5,
            mach_infinity : float = 0.75,
            cl_target : float|None = None,
            output_dir : str|None = None,
            render_mode: str = 'both',  # 'display', 'save', 'both'
            path_save_fig_of_observation: str = None,
            state_class: FigureState = None,
            action_class: BumpModificationAction = None,
            n_max_step: int = 10,
            reward_class: Reward|None = None,
            ) -> None:
        
        super().__init__(
            airfoil_coordinates=airfoil_coordinates,
            angle_of_attack=angle_of_attack,
            mach_infinity=mach_infinity,
            cl_target=cl_target,
            output_dir=output_dir,
            render_mode=render_mode,
            n_max_step=n_max_step,
            reward_class=reward_class,
        )
        
        self.state_class = state_class if state_class is not None else FigureState()
        self.action_class = action_class if action_class is not None else BumpModificationAction()
        
        self.dim_action = self.action_class.dim_action
        self.dim_observation = self.state_class.dim_state
        
        self.action_space = spaces.Box(
            low=self.action_class.action_lower_bound.astype(np.float32), 
            high=self.action_class.action_upper_bound.astype(np.float32), 
            shape=(self.dim_action,), dtype=np.float32)
        self.observation_space = spaces.Box(
            low=self.state_class.state_lower_bound.astype(np.float32), 
            high=self.state_class.state_upper_bound.astype(np.float32), 
            shape=(self.dim_observation,), dtype=np.float32)
        
        self.observation = np.zeros(self.dim_observation)
        
        self.path_save_fig_of_observation = path_save_fig_of_observation
        
    def _apply_action_to_reference_step(self, action: np.ndarray) -> None:
        '''
        Apply the action, update the airfoil coordinates.
        '''
        ref_airfoil_coordinates = self.trajectory[self.i_reference_step]['info']['airfoil_coordinates']
        
        # Split airfoil into upper and lower surfaces
        yu = ref_airfoil_coordinates[:,1][:self.n_airfoil_points][::-1]
        yl = ref_airfoil_coordinates[:,1][self.n_airfoil_points-1:]

        _, _, yu_new, yl_new, self.is_action_valid = self.action_class.apply_action(action, self.x_airfoil_surface, yu, yl)
        
        if self.is_action_valid:
            self.airfoil_coordinates[:,0] = ref_airfoil_coordinates[:,0]
            self.airfoil_coordinates[:,1] = np.concatenate((yu_new[::-1], yl_new[1:]))
            
        self.pytsfoil.airfoil['coordinates'] = self.airfoil_coordinates.copy()
        
    def _get_observation(self) -> Tuple[np.ndarray, str]:
        '''
        Get the observation.
        '''
        state_array, figure_base64 = self.state_class.calculate_state(
            x=self.x_airfoil_surface,
            yu=self.airfoil_coordinates[:,1][:self.n_airfoil_points][::-1],
            yl=self.airfoil_coordinates[:,1][self.n_airfoil_points-1:],
            xxu=self.info['xx'],
            xxl=self.info['xx'],
            mwu=self.info['mau'],
            mwl=self.info['mal'],
            Cl=self.info['cl'],
            Cd_wave=self.info['cd_wave'],
            Cm=self.info['cm'],
            save_fig_path=self.path_save_fig_of_observation
        )
        
        self.observation = state_array
        
        return self.observation, figure_base64

    def _get_observation_for_RL(self, n_interp_points: int = 101) -> Tuple[np.ndarray, np.ndarray]:
        '''
        Get the observation for RL.
        '''
        state_array, figure_array = self.state_class.calculate_state_for_RL(
            x=self.x_airfoil_surface,
            yu=self.airfoil_coordinates[:,1][:self.n_airfoil_points][::-1],
            yl=self.airfoil_coordinates[:,1][self.n_airfoil_points-1:],
            xxu=self.info['xx'],
            xxl=self.info['xx'],
            mwu=self.info['mau'],
            mwl=self.info['mal'],
            Cl=self.info['cl'],
            Cd_wave=self.info['cd_wave'],
            Cm=self.info['cm'],
            n_interp_points=n_interp_points
        )

        self.observation = state_array
        
        return self.observation, figure_array

