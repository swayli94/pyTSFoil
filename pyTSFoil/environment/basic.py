'''
Basic classes for environment, state, action
'''
import json
import numpy as np
from typing import List, Any

import numpy as np
from typing import Tuple, List
from scipy.interpolate import interp1d

from cst_modeling.section import bump_function, cst_foil_fit, cst_foil
from cst_modeling.foil import FoilGeoFeatures, FoilModification


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


class Action():
    '''
    Action of airfoil geometric modifications
    
    Parameters
    ------------------
    dim_action: int
        dimension of action
        
    action_upper_bound, action_lower_bound: ndarray [dim_action], or None
        upper and lower bounds of the action vector
        
    EPSILON: float
        small value to avoid zero in the denominator
    
    Attributes
    ------------------
    action_name: list
        name of the action components
        
    action_upper_bound, action_lower_bound: ndarray [dim_action]
        upper and lower bounds of the action vector
    
    '''
    def __init__(self, dim_action: int, action_upper_bound=None, action_lower_bound=None,
                    action_name: List[str]=None, EPSILON=1E-20) -> None:
        
        self.dim_action = dim_action
        
        if action_name is None:
            self.action_name : List[str] = [None for _ in range(dim_action)]
        else:  
            self.action_name = action_name
        
        if action_upper_bound is None:
            self.action_upper_bound = np.zeros(dim_action)
        else:
            self.action_upper_bound = action_upper_bound
            
        if action_lower_bound is None:
            self.action_lower_bound = np.ones(dim_action)
        else:
            self.action_lower_bound = action_lower_bound
            
        for i in range(self.dim_action):
            self.action_upper_bound[i] = max(self.action_upper_bound[i], self.action_lower_bound[i]+EPSILON)

    def apply_bounds(self, action_array: np.ndarray) -> bool:
        '''
        Apply bounds to the action

        Parameters
        --------------
        action_array: ndarray
            unscaled action vector

        Example
        --------------
        >>> within_bounds = apply_bounds(action_array)
        '''
        within_bounds = True

        for i in range(self.dim_action):

            if action_array[i] > self.action_upper_bound[i]:
                action_array[i] = self.action_upper_bound[i]
                within_bounds = False

            elif action_array[i] < self.action_lower_bound[i]:
                action_array[i] = self.action_lower_bound[i]
                within_bounds = False

        return within_bounds

    def scale_action(self, action_array: np.ndarray) -> np.ndarray:
        '''
        Scale action to [0,1]
        '''
        return (action_array - self.action_lower_bound)/(self.action_upper_bound-self.action_lower_bound)

    def recover_action(self, scaled_action_array: np.ndarray) -> np.ndarray:
        '''
        Recover action from [0,1]
        '''
        return scaled_action_array*(self.action_upper_bound-self.action_lower_bound) + self.action_lower_bound

    def random_action(self) -> np.ndarray:
        '''
        Take a random action
        
        Returns
        ---------------
        action_array: ndarray
            an unscaled action vector
        '''
        action_array = np.random.rand(self.dim_action)
        
        action_array = self.recover_action(action_array)

        return action_array

    def apply_action(self, scaled_action_array: np.ndarray, *args):
        '''
        Apply action
        
        Parameters
        ---------------
        scaled_action_array: ndarray
            scaled action vector
        '''
        raise NotImplementedError

    @staticmethod
    def within_0_and_1(value: float) -> bool:
        return value >= 0.0 and value <= 1.0


class State():
    '''
    State variables
    '''
    def __init__(self) -> None:
        
        self.state_name : List[str] = []
        
        self.state_array : np.ndarray = None
        
    @property
    def dim_state(self) -> int:
        '''
        Dimension of the state vector.
        '''
        return len(self.state_name)

    def __call__(self, key=None) -> float:
        '''
        Fetch the state variable (vector).
        
        Parameters
        ---------------
        key: str
            key string for the state variable
        '''
        return self.fetch(key)
        
    def fetch(self, key=None) -> float:
        '''
        Fetch the state variable (vector).
        
        Parameters
        ---------------
        key: str
            key string for the state variable
        '''
        if key is None:
            return self.state_array
        
        elif isinstance(key, str):
            
            ii = self.state_name.index(key)
            return self.state_array[ii]
        
        else:
            print('Error [State.fetch()]: ')
            print('    the input [key] must be None or a key string in [state_name]', key)
            print()
            raise Exception
        
    def update(self):
        raise NotImplementedError


class ParameterState(State):
    '''
    Parametric state of airfoils:
    
    - 't_max'           (maximum thickness, thickness = yu - yl)
    - 'x_t'             (x-coordinate of t_max)
    - 'c_f60'           (average camber of front 60% of the airfoil)
    - 'c_r40'           (average camber of rear 40% of the airfoil)
    - 'r_le'            (leading edge radius)
    - 'wedge_angle'     (angle of the wedge formed by the airfoil upper and lower surfaces)
    - 'slope_angle_le'  (angle of the slope of the airfoil upper surface at leading edge)
    - 'slope_angle_te'  (angle of the slope of the airfoil upper surface at trailing edge)
    - 'Cl'              (lift coefficient)
    - 'Cd_wave'         (wave drag coefficient)
    - 'Cm'              (moment coefficient)
    - 'Cl_aft'          (aft loading, i.e., the lift produced in the airfoil aft region)
    - 'Mw_le'           (suction peak, i.e., maximum wall Mach number near leading edge)
    '''
    def __init__(self) -> None:
        
        super().__init__()
        
        self.state_name = [ 't_max', 'x_t', 'c_f60', 'c_r40',
                            'r_le', 'wedge_angle', 'slope_angle_le', 'slope_angle_te',
                            'Cl', 'Cd_wave', 'Cm',
                            'Cl_aft', 'Mw_le']
    
    def update(self, x: np.ndarray, yu: np.ndarray, yl: np.ndarray,
                xxu: np.ndarray, xxl: np.ndarray,
                cpu: np.ndarray, cpl: np.ndarray,
                mwu: np.ndarray, mwl: np.ndarray,
                Cl: float, Cd_wave: float, Cm: float) -> np.ndarray:
        '''
        Update the state variables
        
        Parameters
        ---------------
        x: np.ndarray
            x-coordinates of the airfoil geometry
        yu, yl: np.ndarray
            upper/lower surface coordinates of the airfoil geometry
        xxu, xxl: np.ndarray
            x-coordinates of the upper/lower surface mesh
        cpu, cpl: np.ndarray
            pressure coefficient of the upper/lower surface
        mwu, mwl: np.ndarray
            wall Mach number of the upper/lower surface
        Cl, Cd_wave, Cm: float
            lift/wave drag/moment coefficients
        '''

        #* Extract features
        geo = FoilGeoFeatures(x, yu, yl)
        
        t_max, x_t, _ = geo.get_maximum_thickness()
        
        Cl_aft = self.calculate_aft_loading(Cp)
        
        Cp_sp = self.calculate_suction_peak(Cp)
        
        kCp_u = self.calculate_average_pressure_slope(Cp)
        
        #* Assemble state variables
        self.state_array = np.zeros(self.dim_state)
        
        self.state_array[0] = t_max
        self.state_array[1] = x_t
        self.state_array[2] = geo.get_average_camber_front_60p()
        self.state_array[3] = geo.get_average_camber_rear_40p()
        self.state_array[4] = geo.get_leading_edge_radius()
        self.state_array[5] = geo.get_trailing_edge_wedge_angle()
        self.state_array[6] = geo.get_leading_edge_slope_angle()
        self.state_array[7] = geo.get_trailing_edge_slope_angle()
        self.state_array[8] = Cl_aft
        self.state_array[9] = abs(Cp_sp)  #* Note: this is the absolute value of Cp
        self.state_array[10] = kCp_u
        
        return self.state_array
        


class FigureState(object):
    '''
    
    '''


class BumpModificationAction(Action):
    '''
    Action of bump modification to the upper and lower surfaces of an airfoil
    
    Parameters
    --------------
    dim_action: int
        dimension of action
    
    action_name: list
        name of the action components
    
    action_upper_bound, action_lower_bound: ndarray [dim_action], or None
        upper and lower bounds of the action vector
    
    n_cst: int
        number of CST parameters in the upper/lower airfoil surface
        
    keep_airfoil_tmax: bool
        whether keep the maximum airfoil thickness the same during modification
    '''
    def __init__(self, dim_action=6, 
                    action_upper_bound=[1.0,  0.005, 0.8, 1.0,  0.01, 0.8], 
                    action_lower_bound=[0.0, -0.005, 0.2, 0.0, -0.01, 0.2],
                    critical_h_for_no_bump=1E-6,
                    n_cst=10, keep_airfoil_tmax=False) -> None:
        
        super().__init__(dim_action, np.array(action_upper_bound), np.array(action_lower_bound))

        self.action_name = ['x_u', 'h_u', 's_u', 'x_l', 'h_l', 's_l']
        self.n_cst = n_cst
        self.keep_airfoil_tmax = keep_airfoil_tmax
        
        self.critical_h_for_no_bump = critical_h_for_no_bump
        
        self.I_XU = 0
        self.I_HU = 1
        self.I_SU = 2
        self.I_XL = 3
        self.I_HL = 4
        self.I_SL = 5
        
    def apply_action(self, scaled_action_array: np.ndarray, x: np.ndarray, yu: np.ndarray, yl: np.ndarray) \
                    -> Tuple[np.ndarray, np.ndarray, np.ndarray, np.ndarray]:
        '''
        Modify airfoil geometry based on a given action

        Parameters
        -------------
        scaled_action_array: ndarray [dim_action]
            scaled action vector
        
        x, yu, yl: ndarray
            coordinates of the airfoil

        Returns
        -------------
        cst_u, cst_l: ndarray
            CST parameters
            
        yu_new, yl_new: ndarray
            new airfoil geometry
        '''
        action_array = self.recover_action(scaled_action_array)
        
        self.apply_bounds(action_array)

        if self.keep_airfoil_tmax:
            tmax = np.max(yu-yl)
        else:
            tmax = None

        if abs(action_array[self.I_HU]) > self.critical_h_for_no_bump:
        
            yu_new = yu + bump_function(x, xc=action_array[self.I_XU], h=action_array[self.I_HU], s=action_array[self.I_SU], kind='H')
            
        else:
            
            yu_new = yu
        
        if abs(action_array[self.I_HL]) > self.critical_h_for_no_bump:
            
            yl_new = yl + bump_function(x, xc=action_array[self.I_XL], h=action_array[self.I_HL], s=action_array[self.I_SL], kind='H')
            
        else:
            
            yl_new = yl

        cst_u, cst_l = cst_foil_fit(x, yu_new, x, yl_new, n_cst=self.n_cst)
        
        _, yu_new, yl_new, _, _ = cst_foil(x.shape[0], cst_u, cst_l, x=x, t=tmax)

        return cst_u, cst_l, yu_new, yl_new


class GlobalModificationAction(Action):
    '''
    Global modification of airfoil geometry
    
    Action include modifications to the following geometric features:
    
    1. `t_a`: airfoil thickness
    2. `c_a`: airfoil camber
    3. `x_t`: maximum airfoil thickness location
    4. `c_f60`: average camber of front 60% of the airfoil
    5. `c_r40`: average camber of rear 40% of the airfoil
    6. `r_le`: leading edge radius
    7. `a_sle`: leading edge slope angle (degree)
    8. `a_wte`: trailing edge wedge angle (degree)
    9. `a_ste`: trailing edge slope angle (degree)
    10. `t_20`: thickness at 20% chord
    11. `t_70`: thickness at 70% chord
    '''
    def __init__(self, n_cst=10) -> None:

        # action name, upper bound (lower bound is the negative of upper bound)
        
        self.action_list = {
            'Dt_a':      0.001,
            'Dc_a':      0.002,
            'Dx_t':      0.050,
            'Dc_f60':    0.002,
            'Dc_r40':    0.001,
            'Dr_le':     0.002,
            'Da_sle':    0.500,
            'Da_wte':    0.500,
            'Da_ste':    0.500,
            'Dt_20':     0.001,
            'Dt_70':     0.001
        }
        
        action_name = list(self.action_list.keys())
        
        super().__init__(dim_action=len(self.action_list), 
                action_upper_bound = np.array([ self.action_list[key] for key in action_name]), 
                action_lower_bound = np.array([-self.action_list[key] for key in action_name]),
                action_name = action_name)
    
        self.n_cst = n_cst

    def recover_action(self, scaled_action_array: np.ndarray) -> np.ndarray:
        '''
        Recover action from [0,1]
        '''
        action = scaled_action_array*(self.action_upper_bound-self.action_lower_bound) + self.action_lower_bound
        
        for i in range(len(action)):
            if not self.within_0_and_1(scaled_action_array[i]):
                action[i] = 0.0
                
        return action

    def apply_action(self, scaled_action_array: np.ndarray, x: np.ndarray, yu: np.ndarray, yl: np.ndarray) \
                    -> Tuple[np.ndarray, np.ndarray, np.ndarray, np.ndarray]:
        '''
        Modify airfoil geometry based on a given action

        Parameters
        -------------
        scaled_action_array: ndarray [dim_action]
            scaled action vector, in [0,1].
            If the value is negative, the corresponding action is not applied.
        
        x, yu, yl: ndarray
            coordinates of the airfoil

        Returns
        -------------
        cst_u, cst_l: ndarray
            CST parameters
            
        yu_new, yl_new: ndarray
            new airfoil geometry
        '''
        action_array = self.recover_action(scaled_action_array)
        
        geo_old = FoilGeoFeatures(x, yu, yl)
        modify = FoilModification(x, yu, yl, self.n_cst)
        
        t_max_old, x_t_old, _ = geo_old.get_maximum_thickness()
        
        
        #* airfoil thickness
        if self.within_0_and_1(scaled_action_array[0]):
        
            modify.set_thickness(action_array[0]+t_max_old)
        
        
        #* airfoil camber
        if self.within_0_and_1(scaled_action_array[1]):
        
            c_old = geo_old.get_average_camber()
            modify.set_camber(action_array[1]+c_old)
        
        
        #* maximum airfoil thickness location
        if self.within_0_and_1(scaled_action_array[2]):
            
            modify.set_maximum_thickness_location(action_array[2]+x_t_old)
            
        
        #* average camber of front 60% of the airfoil
        if self.within_0_and_1(scaled_action_array[3]):
        
            c_old = geo_old.get_average_camber_front_60p()
            modify.set_camber_front(action_array[3]+c_old)


        #* average camber of rear 40% of the airfoil
        if self.within_0_and_1(scaled_action_array[4]):
        
            c_old = geo_old.get_average_camber_rear_40p()
            modify.set_camber_rear(action_array[4]+c_old, width_bump=0.8)
        
        
        #* leading edge radius
        if self.within_0_and_1(scaled_action_array[5]):
        
            r_old = geo_old.get_leading_edge_radius()
            modify.set_leading_edge_radius(action_array[5]+r_old)
        
        
        #* leading edge slope angle (degree)
        if self.within_0_and_1(scaled_action_array[6]):
        
            a_old = geo_old.get_leading_edge_slope_angle()
            modify.set_leading_edge_slope_angle(action_array[6]+a_old)
        
        
        #* trailing edge wedge angle (degree)
        if self.within_0_and_1(scaled_action_array[7]):
        
            a_old = geo_old.get_trailing_edge_wedge_angle()
            modify.set_trailing_edge_wedge_angle(action_array[7]+a_old)
        
        
        #* trailing edge slope angle (degree)
        if self.within_0_and_1(scaled_action_array[8]):
        
            a_old = geo_old.get_trailing_edge_slope_angle()
            modify.set_trailing_edge_slope_angle(action_array[8]+a_old)
        
        
        #* thickness at 20% chord
        if self.within_0_and_1(scaled_action_array[9]):
        
            t_old = geo_old.get_thickness_at(0.2)
            modify.set_thickness_at(0.2, action_array[9]+t_old, width_bump=0.6)
        
        #* thickness at 70% chord
        if self.within_0_and_1(scaled_action_array[10]):
        
            t_old = geo_old.get_thickness_at(0.7)
            modify.set_thickness_at(0.7, action_array[10]+t_old, width_bump=0.6)


        cst_u, cst_l = modify.get_cst_coefficients()
        
        _, yu_new, yl_new, _, _ = cst_foil(x.shape[0], cst_u, cst_l, x=x)

        return cst_u, cst_l, yu_new, yl_new



