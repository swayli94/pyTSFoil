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
            path_save_fig_of_observation=path_save_fig_of_observation,
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
            path_save_fig_of_observation=path_save_fig_of_observation,
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
