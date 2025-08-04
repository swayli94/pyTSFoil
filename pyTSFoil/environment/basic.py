'''
Basic classes for environment, state, action
'''
import io
import base64
import numpy as np

from typing import List, Any, Tuple
import matplotlib.pyplot as plt

from cst_modeling.section import bump_function, cst_foil_fit, cst_foil
from cst_modeling.foil import FoilGeoFeatures, FoilModification


class Action():
    '''
    Action of airfoil geometric modifications.
    The action is scaled to [-1,1], where 0 indicates no modification.
    
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
    '''
    def __init__(self, dim_action: int, 
                    action_upper_bound : np.ndarray=None, 
                    action_lower_bound : np.ndarray=None,
                    EPSILON: float=1E-20) -> None:
        
        self.dim_action = dim_action
        self.action_name : List[str] = ['action_'+str(i) for i in range(dim_action)]

        if isinstance(action_upper_bound, np.ndarray):
            self.action_upper_bound = action_upper_bound
        else:
            self.action_upper_bound = np.ones(dim_action)
            
        if isinstance(action_lower_bound, np.ndarray):
            self.action_lower_bound = action_lower_bound
        else:
            self.action_lower_bound = - np.ones(dim_action)
                        
        for i in range(self.dim_action):
            self.action_upper_bound[i] = max(self.action_upper_bound[i], self.action_lower_bound[i]+EPSILON)

    def scale_action(self, action_array: np.ndarray, clip: bool=True) -> np.ndarray:
        '''
        Scale action to [-1,1]
        
        Parameters
        --------------
        action_array: ndarray
            unscaled action vector
            
        clip: bool
            whether to clip the action to [-1,1]
        '''
        scaled_action_array = 2*(action_array - self.action_lower_bound)/(self.action_upper_bound-self.action_lower_bound) - 1
        
        if clip:
            scaled_action_array = np.clip(scaled_action_array, -1, 1)
        
        return scaled_action_array

    def recover_action(self, scaled_action_array: np.ndarray, clip: bool=True) -> np.ndarray:
        '''
        Recover action from [-1,1]
        
        Parameters
        --------------
        scaled_action_array: ndarray
            scaled action vector
            
        clip: bool
            whether to clip the scaled action to [-1,1] before recovering the action
        '''
        if clip:
            scaled_action_array = np.clip(scaled_action_array, -1, 1)
        
        action_array = (scaled_action_array + 1)/2*(self.action_upper_bound-self.action_lower_bound) + self.action_lower_bound
        
        return action_array

    def random_action(self, scale: float=1.0) -> np.ndarray:
        '''
        Take a random action by sampling from a uniform distribution in [-1,1],
        scale it to the range of [-scale, scale],
        then recover the action to the original range.
        
        Parameters
        ---------------
        scale: float
            scale the action to the range of [-scale, scale]
            
        Returns
        ---------------
        action_array: ndarray
            an unscaled action vector
        '''
        return self.recover_action((np.random.rand(self.dim_action)*2-1)*scale)

    def apply_action(self, scaled_action_array: np.ndarray, *args):
        '''
        Apply action
        
        Parameters
        ---------------
        scaled_action_array: ndarray
            scaled action vector
        '''
        raise NotImplementedError


class BumpModificationAction(Action):
    '''
    Action of bump modification to the upper and lower surfaces of an airfoil
    
    Parameters
    --------------
    action_upper_bound, action_lower_bound: ndarray [6]
        upper and lower bounds of the action vector
        
    critical_height_for_no_bump: float
        critical height for no bump
    
    n_cst: int
        number of CST parameters in the upper/lower airfoil surface
        
    keep_airfoil_tmax: bool
        whether keep the maximum airfoil thickness the same during modification
    '''
    def __init__(self,
                    critical_height_for_no_bump=1E-3,
                    n_cst=10, 
                    keep_airfoil_tmax=False) -> None:
        
        self.action_dict = {
            'UBL': {'bound': [ 0.0,   1.0],   'min_increment': 0.002,  'meaning': 'upper bump location'},
            'UBH': {'bound': [-0.005, 0.005], 'min_increment': 0.0005, 'meaning': 'upper bump height'},
            'UBW': {'bound': [ 0.2,   0.8],   'min_increment': 0.01,   'meaning': 'upper bump width'},
            'LBL': {'bound': [ 0.0,   1.0],   'min_increment': 0.002,  'meaning': 'lower bump location'},
            'LBH': {'bound': [-0.01,  0.01],  'min_increment': 0.0005, 'meaning': 'lower bump height'},
            'LBW': {'bound': [ 0.2,   0.8],   'min_increment': 0.01,   'meaning': 'lower bump width'},
        }
        
        super().__init__(dim_action=len(self.action_dict), 
                    action_upper_bound=np.array([self.action_dict[key]['bound'][1] for key in self.action_dict.keys()]), 
                    action_lower_bound=np.array([self.action_dict[key]['bound'][0] for key in self.action_dict.keys()]))

        self.action_name = list(self.action_dict.keys())
        
        self.n_cst = n_cst
        self.keep_airfoil_tmax = keep_airfoil_tmax
        
        self.critical_height_for_no_bump = critical_height_for_no_bump
        
    def apply_action(self, action_array: np.ndarray, x: np.ndarray, yu: np.ndarray, yl: np.ndarray) \
                    -> Tuple[np.ndarray, np.ndarray, np.ndarray, np.ndarray]:
        '''
        Modify airfoil geometry based on a given action

        Parameters
        -------------
        action_array: ndarray [dim_action]
            unscaled action vector
        
        x, yu, yl: ndarray
            coordinates of the airfoil

        Returns
        -------------
        cst_u, cst_l: ndarray
            CST parameters
            
        yu_new, yl_new: ndarray
            new airfoil geometry
        '''
        if self.keep_airfoil_tmax:
            tmax = np.max(yu-yl)
        else:
            tmax = None

        if abs(action_array[1]) > self.critical_height_for_no_bump:
        
            yu_new = yu + bump_function(x, xc=action_array[0], h=action_array[1], s=action_array[2], kind='H')
            
        else:
            
            yu_new = yu
        
        if abs(action_array[4]) > self.critical_height_for_no_bump:
            
            yl_new = yl + bump_function(x, xc=action_array[3], h=action_array[4], s=action_array[5], kind='H')
            
        else:
            
            yl_new = yl

        cst_u, cst_l = cst_foil_fit(x, yu_new, x, yl_new, n_cst=self.n_cst)
        
        _, yu_new, yl_new, _, _ = cst_foil(x.shape[0], cst_u, cst_l, x=x, t=tmax)

        return cst_u, cst_l, yu_new, yl_new


class GlobalModificationAction(Action):
    '''
    Global modification of airfoil geometry
    '''
    def __init__(self) -> None:

        # action name, absolute bound, min increment
        self.action_dict = {
            'dTHK': {'bound': 0.002, 'min_increment': 0.0002, 'meaning': 'airfoil THickness'},
            'dCAM': {'bound': 0.002, 'min_increment': 0.0002, 'meaning': 'airfoil CAMber'},
            'dMTL': {'bound': 0.050, 'min_increment':   0.01, 'meaning': 'Maximum airfoil Thickness Location'},
            'dCF6': {'bound': 0.002, 'min_increment': 0.0005, 'meaning': 'average Camber of Front 60 percent of the airfoil'},
            'dCR4': {'bound': 0.001, 'min_increment': 0.0005, 'meaning': 'average Camber of Rear 40 percent of the airfoil'},
            'dLER': {'bound': 0.002, 'min_increment': 0.0005, 'meaning': 'Leading Edge Radius'},
            'dLES': {'bound': 0.500, 'min_increment':    0.1, 'meaning': 'Leading Edge Slope angle (degree)'},
            'dTEW': {'bound': 0.500, 'min_increment':    0.1, 'meaning': 'Trailing Edge Wedge angle (degree)'},
            'dTES': {'bound': 0.500, 'min_increment':    0.1, 'meaning': 'Trailing Edge Slope angle (degree)'},
            'dTH2': {'bound': 0.001, 'min_increment': 0.0002, 'meaning': 'THickness at 20 percent chord'},
            'dTH7': {'bound': 0.001, 'min_increment': 0.0002, 'meaning': 'THickness at 70 percent chord'},
            }
        
        super().__init__(dim_action=len(self.action_dict), 
                action_upper_bound = np.array([ self.action_dict[key]['bound'] for key in self.action_dict.keys()]), 
                action_lower_bound = np.array([-self.action_dict[key]['bound'] for key in self.action_dict.keys()]))
        
        self.action_name = list(self.action_dict.keys())
    
        self.n_cst = 10
        
    def id(self, key: str) -> int:
        '''
        Get the index of the action
        '''
        return self.action_name.index(key)

    def is_action_noticeable(self, key: str, action_array: np.ndarray) -> bool:
        '''
        Check if the action is noticeable
        '''
        return abs(action_array[self.id(key)]) > self.action_dict[key]['min_increment']

    def apply_action(self, action_array: np.ndarray, x: np.ndarray, yu: np.ndarray, yl: np.ndarray) \
                    -> Tuple[np.ndarray, np.ndarray, np.ndarray, np.ndarray]:
        '''
        Modify airfoil geometry based on a given action

        Parameters
        -------------
        action_array: ndarray [dim_action]
            unscaled action vector
        
        x, yu, yl: ndarray
            coordinates of the airfoil

        Returns
        -------------
        cst_u, cst_l: ndarray
            CST parameters
            
        yu_new, yl_new: ndarray
            new airfoil geometry
        '''
        geo_old = FoilGeoFeatures(x, yu, yl)
        modify = FoilModification(x, yu, yl, self.n_cst)
        
        t_max_old, x_t_old, _ = geo_old.get_maximum_thickness()

        #* airfoil thickness (dTHK)
        if self.is_action_noticeable('dTHK', action_array):
            modify.set_thickness(action_array[self.id('dTHK')]+t_max_old)
        
        #* airfoil camber
        if self.is_action_noticeable('dCAM', action_array):
            c_old = geo_old.get_average_camber()
            modify.set_camber(action_array[self.id('dCAM')]+c_old)
        
        #* maximum airfoil thickness location
        if self.is_action_noticeable('dMTL', action_array):
            modify.set_maximum_thickness_location(action_array[self.id('dMTL')]+x_t_old)
            
        #* average camber of front 60% of the airfoil
        if self.is_action_noticeable('dCF6', action_array):
            c_old = geo_old.get_average_camber_front_60p()
            modify.set_camber_front(action_array[self.id('dCF6')]+c_old)
            
        #* average camber of rear 40% of the airfoil
        if self.is_action_noticeable('dCR4', action_array):
            c_old = geo_old.get_average_camber_rear_40p()
            modify.set_camber_rear(action_array[self.id('dCR4')]+c_old, width_bump=0.8)
        
        #* leading edge radius
        if self.is_action_noticeable('dLER', action_array):
            r_old = geo_old.get_leading_edge_radius()
            modify.set_leading_edge_radius(action_array[self.id('dLER')]+r_old)
        
        #* leading edge slope angle (degree)
        if self.is_action_noticeable('dLES', action_array): 
            a_old = geo_old.get_leading_edge_slope_angle()
            modify.set_leading_edge_slope_angle(action_array[self.id('dLES')]+a_old)
        
        #* trailing edge wedge angle (degree)
        if self.is_action_noticeable('dTEW', action_array): 
            a_old = geo_old.get_trailing_edge_wedge_angle()
            modify.set_trailing_edge_wedge_angle(action_array[self.id('dTEW')]+a_old)
        
        #* trailing edge slope angle (degree)
        if self.is_action_noticeable('dTES', action_array):
            a_old = geo_old.get_trailing_edge_slope_angle()
            modify.set_trailing_edge_slope_angle(action_array[self.id('dTES')]+a_old)
        
        #* thickness at 20% chord
        if self.is_action_noticeable('dTH2', action_array):
            t_old = geo_old.get_thickness_at(0.2)
            modify.set_thickness_at(0.2, action_array[self.id('dTH2')]+t_old, width_bump=0.6)
        
        #* thickness at 70% chord
        if self.is_action_noticeable('dTH7', action_array):    
            t_old = geo_old.get_thickness_at(0.7)
            modify.set_thickness_at(0.7, action_array[self.id('dTH7')]+t_old, width_bump=0.6)

        #* get new CST coefficients and airfoil geometry
        cst_u, cst_l = modify.get_cst_coefficients()
        
        _, yu_new, yl_new, _, _ = cst_foil(x.shape[0], cst_u, cst_l, x=x)

        return cst_u, cst_l, yu_new, yl_new


class FigureState():
    '''
    Use the figure of wall Mach number distribution and parameters to represent the state of an airfoil.
    
    '''
    def __init__(self) -> None:
        
        self.state_dict = {
            't_max': {'bound': [0.00, 0.20], 'meaning': 'maximum thickness'},
            'x_t_max': {'bound': [0.2, 0.8], 'meaning': 'x-coordinate of maximum thickness'},
            'volume': {'bound': [0.0, 0.2], 'meaning': 'volume'},
            'r_LE': {'bound': [0.0, 0.05], 'meaning': 'leading edge radius'},
            'a_LE': {'bound': [0.0, 90.0], 'meaning': 'leading edge slope angle (degree)'},
            'a_TEW': {'bound': [0.0, 90.0], 'meaning': 'trailing edge wedge angle (degree)'},
            'a_TES': {'bound': [0.0, 90.0], 'meaning': 'trailing edge slope angle (degree)'},
            't_20p': {'bound': [0.0, 0.10], 'meaning': 'thickness at 20 percent chord'},
            't_70p': {'bound': [0.0, 0.10], 'meaning': 'thickness at 70 percent chord'},
            'c_avg': {'bound': [0.0, 0.10], 'meaning': 'average camber'},
            'x_u_crest': {'bound': [0.2, 0.80], 'meaning': 'x-coordinate of upper crest point'},
            'y_u_crest': {'bound': [0.0, 0.10], 'meaning': 'y-coordinate of upper crest point'},
            'x_l_crest': {'bound': [0.2, 0.80], 'meaning': 'x-coordinate of lower crest point'},
            'y_l_crest': {'bound': [-0.1, 0.0], 'meaning': 'y-coordinate of lower crest point'},
            'Cl': {'bound': [0.0, 1.0], 'meaning': 'lift coefficient'},
            'Cd_wave': {'bound': [0.0, 0.05], 'meaning': 'wave drag coefficient'},
            'Cm': {'bound': [-0.5, 0.5], 'meaning': 'moment coefficient'},
        }

        # Crest point is the point where the upper/lower surface is tangent to the flow direction.
        # For simplicity, we consider a zero degree angle of attack,
        # so the crest point is the point with the maximum abs(y)
        
        self.dim_state = len(self.state_dict)
        
        self.state_lower_bound = np.array([self.state_dict[key]['bound'][0] for key in self.state_dict.keys()])
        self.state_upper_bound = np.array([self.state_dict[key]['bound'][1] for key in self.state_dict.keys()])

    def calculate_state(self, 
                x: np.ndarray, yu: np.ndarray, yl: np.ndarray,
                xxu: np.ndarray, xxl: np.ndarray,
                mwu: np.ndarray, mwl: np.ndarray,
                Cl: float, Cd_wave: float, Cm: float,
                save_fig_path: str = None) -> Tuple[np.ndarray, str]:
        '''
        Calculate the state variables and create figure for LLM.
        
        Parameters
        --------------
        x, yu, yl: np.ndarray
            x-coordinates, upper/lower surface coordinates of the airfoil geometry
        xxu, xxl: np.ndarray
            x-coordinates of the upper/lower surface mesh
        mwu, mwl: np.ndarray
            wall Mach number of the upper/lower surface
        Cl, Cd_wave, Cm: float
            lift/wave drag/moment coefficients
        save_fig_path: str, optional
            file path to save the figure as PNG for debugging (default: None)

        Returns
        --------------
        state_array: np.ndarray
            state variables
        figure_base64: str
            base64 encoded PNG image of the wall Mach number distribution
        '''
        
        state_array = self._calculate_parametric_state(x, yu, yl, Cl, Cd_wave, Cm)
        
        figure_base64 = self._calculate_figure_state(x, yu, yl, xxu, xxl, mwu, mwl, save_fig_path)
    
        return state_array, figure_base64
    
    def calculate_state_for_RL(self, 
                x: np.ndarray, yu: np.ndarray, yl: np.ndarray,
                xxu: np.ndarray, xxl: np.ndarray,
                mwu: np.ndarray, mwl: np.ndarray,
                Cl: float, Cd_wave: float, Cm: float,
                n_interp_points: int = 101) -> Tuple[np.ndarray, np.ndarray]:
        '''
        Calculate the state variables for RL.
        The figure originally used for LLMs is not used for RL.
        The airfoil geometry and wal Mach number distribution in the figure are interpolated to the same number of points. Then, used as the state for RL.
        
        Parameters
        --------------
        x, yu, yl: np.ndarray
            x-coordinates, upper/lower surface coordinates of the airfoil geometry
        xxu, xxl: np.ndarray
            x-coordinates of the upper/lower surface mesh
        mwu, mwl: np.ndarray
            wall Mach number of the upper/lower surface
        Cl, Cd_wave, Cm: float
            lift/wave drag/moment coefficients
        n_interp_points: int
            number of interpolation points for the wall Mach number distribution and the airfoil geometry
            
        Returns
        --------------
        state_array: np.ndarray
            state variables
        figure_array: np.ndarray [n_interp_points, 4]
            figure array, [yu, yl, mwu, mwl]
        '''
        state_array = self._calculate_parametric_state(x, yu, yl, Cl, Cd_wave, Cm)
        
        x_interp = np.linspace(0, 1, n_interp_points)
        
        yu_interp = np.interp(x_interp, x, yu)
        yl_interp = np.interp(x_interp, x, yl)
        mwu_interp = np.interp(x_interp, xxu, mwu)
        mwl_interp = np.interp(x_interp, xxl, mwl)
        
        figure_array = np.column_stack((yu_interp, yl_interp, mwu_interp, mwl_interp))

        return state_array, figure_array
    
    def _calculate_parametric_state(self, 
                x: np.ndarray, yu: np.ndarray, yl: np.ndarray,
                Cl: float, Cd_wave: float, Cm: float) -> np.ndarray:
        
        state_array = np.zeros(self.dim_state)
        
        geo_features = FoilGeoFeatures(x, yu, yl)
        
        state_array[0], state_array[1], _ = geo_features.get_maximum_thickness()
        state_array[2] = geo_features.get_volume()
        state_array[3] = geo_features.get_leading_edge_radius()
        state_array[4] = geo_features.get_leading_edge_slope_angle()
        state_array[5] = geo_features.get_trailing_edge_wedge_angle()
        state_array[6] = geo_features.get_trailing_edge_slope_angle()
        state_array[7] = geo_features.get_thickness_at(0.2)
        state_array[8] = geo_features.get_thickness_at(0.7)
        state_array[9] = geo_features.get_average_camber()
        state_array[10], state_array[11], _ = geo_features.get_upper_crest_point()
        state_array[12], state_array[13], _ = geo_features.get_lower_crest_point()
        state_array[14] = Cl
        state_array[15] = Cd_wave
        state_array[16] = Cm

        return state_array
            
    def _calculate_figure_state(self, 
                x: np.ndarray, yu: np.ndarray, yl: np.ndarray,
                xxu: np.ndarray, xxl: np.ndarray,
                mwu: np.ndarray, mwl: np.ndarray,
                save_fig_path: str = None) -> str:
        
        # Create figure for LLM consumption
        fig, ax = plt.subplots(1, 2, figsize=(12, 5))
        
        # Plot airfoil geometry in first subplot
        ax[0].plot(x, yu, 'b-', linewidth=2, label='Upper Surface')
        ax[0].plot(x, yl, 'r-', linewidth=2, label='Lower Surface')
        ax[0].set_xlabel('X/c', fontsize=12)
        ax[0].set_ylabel('Y/c', fontsize=12)
        ax[0].set_title('Airfoil Geometry', fontsize=14)
        ax[0].grid(True, alpha=0.3)
        ax[0].legend()
        ax[0].set_xlim([-0.1, 1.1])
        ax[0].set_ylim([-0.1, 0.1])
        
        # Filter Mach number data to x in [0,1] range
        mask_u = (xxu >= 0) & (xxu <= 1)
        mask_l = (xxl >= 0) & (xxl <= 1)
        
        # Plot wall Mach number distribution in second subplot (following existing pattern from pytsfoil.py)
        ax[1].plot(xxu[mask_u], mwu[mask_u], 'b-', linewidth=2, label='Upper Surface')
        ax[1].plot(xxl[mask_l], mwl[mask_l], 'r-', linewidth=2, label='Lower Surface')
        
        # Add reference line for sonic condition
        ax[1].axhline(y=1.0, color='k', linestyle='--', alpha=0.5, label='Sonic (M=1)')
        
        # Set plot properties
        ax[1].set_xlabel('X/c', fontsize=12)
        ax[1].set_ylabel('Wall Mach Number', fontsize=12)
        ax[1].set_title('Wall Mach Number Distribution', fontsize=14)
        ax[1].grid(True, alpha=0.3)
        ax[1].legend()
        ax[1].set_xlim([-0.1, 1.1])
        ax[1].set_ylim([-0.1, 1.6])
        
        plt.tight_layout()
        
        # Save figure to file if path is provided (for debugging)
        if save_fig_path is not None:
            plt.savefig(save_fig_path, format='png', dpi=100, bbox_inches='tight')
        
        # Convert figure to base64 string for LLM consumption
        buffer = io.BytesIO()
        plt.savefig(buffer, format='png', dpi=150, bbox_inches='tight')
        buffer.seek(0)
        figure_base64 = base64.b64encode(buffer.getvalue()).decode('utf-8')
        buffer.close()
        
        # Close the figure to free memory
        plt.close(fig)
        
        return figure_base64
    
