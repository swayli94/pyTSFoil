'''
Gym environment with CST-modeling tools.
'''
import numpy as np

from gymnasium import spaces

from env_empty import TSFoilEnv_Empty


class TSFoilEnv_CST(TSFoilEnv_Empty):
    '''
    Gym environment with CST-modeling tools.
    '''
    def __init__(self, 
            airfoil_coordinates: np.ndarray,
            angle_of_attack : float = 0.5,
            mach_infinity : float = 0.75,
            output_dir : str|None = None,
            render_mode: str = 'both',  # 'display', 'save', 'both'
            ) -> None:
        
        super().__init__(
            airfoil_coordinates=airfoil_coordinates,
            angle_of_attack=angle_of_attack,
            mach_infinity=mach_infinity,
            output_dir=output_dir,
            render_mode=render_mode,
        )

        self.State = AirfoilState()
        self.Action = GlobalModificationAction()

        self.dim_action = self.Action.dim_action
        self.dim_observation = self.State.dim_state

        self.action_space = spaces.Box(low=0.0, high=1.0, shape=(self.dim_action,))
        self.observation_space = spaces.Box(low=0.0, high=1.0, shape=(self.dim_observation,))
        
        self.action = np.zeros(self.dim_action)
        self.observation = np.zeros(self.dim_observation)

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
    
    def _get_done(self) -> bool:
        '''
        Get the done.
        '''
        self.done = self.i_step >= self.i_max_step
        return self.done
    
    