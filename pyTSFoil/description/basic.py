'''
Language description of actions
'''
import json
import numpy as np
from typing import List, Dict, Any, Union
from pyTSFoil.environment.basic import BumpModificationAction, GlobalModificationAction, FigureState


class DescriptionActionBumpMod():
    '''
    Bump modification of airfoil geometry
    '''
    def __init__(self, bump_action: BumpModificationAction = None):
        # Get action parameters from actual BumpModificationAction instance
        if bump_action is None:
            bump_action = BumpModificationAction()  # Use default parameters
            
        self.action_names = bump_action.action_name
        self.action_bounds = {
            'upper_bound': bump_action.action_upper_bound.tolist(),
            'lower_bound': bump_action.action_lower_bound.tolist()
        }
        self.critical_height_for_no_bump = bump_action.critical_height_for_no_bump
        self.n_cst = bump_action.n_cst
        self.keep_airfoil_tmax = bump_action.keep_airfoil_tmax
        
    def get_general_description(self):
        """
        Returns a comprehensive description of the BumpModificationAction for LLMs.
        """
        
        # Build parameter descriptions dynamically
        param_descriptions = []
        for i, name in enumerate(self.action_names):
            lower = self.action_bounds['lower_bound'][i]
            upper = self.action_bounds['upper_bound'][i]
            
            # Determine parameter type and surface based on parameter name suffix
            if name.endswith('L'):  # Location parameters (UBL, LBL)
                param_type = 'chordwise location'
                param_detail = 'Position along chord: 0.0 = leading edge, 1.0 = trailing edge'
                units = 'chord fraction'
            elif name.endswith('H'):  # Height parameters (UBH, LBH)
                param_type = 'bump height'
                param_detail = 'Positive = outward bump, negative = inward bump'
                units = 'chord fraction'
            elif name.endswith('W'):  # Width parameters (UBW, LBW)
                param_type = 'bump width'
                param_detail = 'Controls bump spread: larger values = wider, more gradual bumps'
                units = 'dimensionless'
            else:
                param_type = 'unknown parameter'
                param_detail = 'Parameter type not recognized'
                units = 'unknown'
            
            surface = 'upper' if name.startswith('U') else 'lower'
            
            param_descriptions.append(f"""
{i+1}. **{name}** (range: {lower} to {upper})
   - Controls the {param_type} of the bump on the {surface} surface
   - {param_detail}
   - Units: {units}""")
        
        description = f"""
**Airfoil Bump Modification Strategy:**
This action adds localized bumps to both the upper and lower surfaces of the airfoil. You must specify the location, height, and width characteristics for these bumps to achieve desired aerodynamic properties.

**Physical Understanding:**
- **Upper Surface Bumps:** 
  * Positive height → increases local thickness and curvature (outward bump)
  * Negative height → decreases local thickness and curvature (inward bump)
- **Lower Surface Bumps:**
  * Positive height → decreases local thickness and curvature (outward bump)  
  * Negative height → increases local thickness and curvature (inward bump)

**Action Parameters:**
The modification consists of {len(self.action_names)} parameters:{''.join(param_descriptions)}

**Technical Implementation:**
- Uses Hicks-Henne bump functions for smooth, aerodynamically-reasonable modifications
- Bumps are only applied if their height exceeds the critical threshold ({self.critical_height_for_no_bump}) to avoid insignificant changes
- {'Maintains' if self.keep_airfoil_tmax else 'Does not maintain'} original maximum thickness during modification

"""
        return description
    
    def get_action_description(self, action_array: np.ndarray):
        '''
        Get the description of the action
        '''
        description = ''
        for i, action in enumerate(action_array):
            description += f'{i+1}. {self.action_names[i]}: {action}\n'
        return description
    
    @staticmethod
    def instruction_for_action_extraction():
        '''
        Get the instruction for action extraction
        '''
        description = """
You are reading text to extract parameters of bump modifications, and convert them to python variables in the format of list. 

For example: '''upper = [location, height, width] \n lower = [location, height, width]'''. 
You should replace the parameters location, height, width with according values.

If the text indicates no bump is added to the upper or lower surface, then the parameters of the corresponding surface are zero. You should only reply python code in plain text without any other content.
"""
        return description
    

class DescriptionActionGlobalMod():
    '''
    Global modification of airfoil geometry

    '''
    def __init__(self, global_action: GlobalModificationAction = None):
        # Get action dictionary from actual GlobalModificationAction instance
        if global_action is None:
            global_action = GlobalModificationAction()  # Use default parameters
            
        self.action_dict = global_action.action_dict
        self.action_names = global_action.action_name
        self.n_cst = global_action.n_cst
        self.action_upper_bound = global_action.action_upper_bound.tolist()
        self.action_lower_bound = global_action.action_lower_bound.tolist()
        
    def get_general_description(self):
        """
        Returns a comprehensive description of the GlobalModificationAction for LLMs.
        """
        
        # Categorize parameters for better organization
        thickness_params = ['dTHK', 'dTH2', 'dTH7', 'dMTL']
        camber_params = ['dCAM', 'dCF6', 'dCR4']
        edge_params = ['dLER', 'dLES', 'dTEW', 'dTES']
        
        # Build parameter descriptions dynamically with better categorization
        param_descriptions = []
        for i, (key, value) in enumerate(self.action_dict.items()):
            bound = value['bound']
            min_inc = value['min_increment']
            meaning = value['meaning']
            
            # Determine units and parameter category
            if 'angle' in meaning.lower():
                units = 'degrees'
                param_range = f"±{bound}"
            else:
                units = 'chord fraction'
                param_range = f"±{bound}"
            
            # Get category for parameter
            if key in thickness_params:
                category = 'Thickness Control'
            elif key in camber_params:
                category = 'Camber Control'
            elif key in edge_params:
                category = 'Edge Geometry'
            else:
                category = 'General'
            
            param_descriptions.append(f"""
{i+1}. **{key}** - {meaning}
   - Range: {param_range} ({units})
   - Minimum increment: {min_inc}
   - Category: {category}
   - Effect: {self._get_parameter_detail(key, meaning)}""")
        
        description = f"""
**Global Geometric Modification Strategy:**
This action performs global modifications to airfoil geometry by adjusting fundamental shape characteristics. Unlike local bump modifications, these changes affect the overall airfoil shape through systematic parameter adjustments that maintain aerodynamic smoothness and physical realizability.

**Physical Understanding:**
Global modifications work by adjusting the underlying mathematical representation of the airfoil:
- **Thickness Parameters:** Control the overall thickness distribution and maximum thickness location
- **Camber Parameters:** Adjust the mean line curvature and camber distribution
- **Edge Parameters:** Modify leading and trailing edge characteristics for suction peak and separation control
- **Localized Thickness:** Fine-tune thickness at specific chord locations for pressure distribution control

**Action Parameters:**
The modification consists of {len(self.action_dict)} parameters organized by function:{''.join(param_descriptions)}

**Technical Implementation:**
- Actions are only applied if they exceed their minimum increment threshold ({min([v['min_increment'] for v in self.action_dict.values()])} minimum) to avoid imperceptible changes
- Modifications are relative to the current airfoil geometry (additive/delta changes)

"""
        return description
    
    def _get_parameter_detail(self, key, meaning):
        """Get detailed explanation for each parameter with aerodynamic context."""
        details = {
            'dTHK': 'Changes overall maximum thickness - affects structural capacity, drag characteristics, and internal volume. Positive increases thickness, negative decreases thickness.',
            'dCAM': 'Modifies overall camber (mean line curvature) - directly impacts lift generation and pitching moment. Positive increases camber, negative decreases camber.',
            'dMTL': 'Shifts chordwise position of maximum thickness - affects pressure distribution, transition location, and structural efficiency. Positive moves aft, negative moves forward.',
            'dCF6': 'Adjusts average camber of front 60% of airfoil - controls early flow acceleration and nose-down pitching moment. Critical for lift coefficient and stall characteristics.',
            'dCR4': 'Modifies average camber of rear 40% of airfoil - influences flow recovery and trailing edge pressure. Affects lift effectiveness and moment characteristics.',
            'dLER': 'Changes leading edge radius - affects stagnation point size, pressure gradient, and boundary layer behavior. Larger radius improves high-angle performance.',
            'dLES': 'Adjusts leading edge slope angle - controls flow acceleration around the nose and transition behavior. Steeper angles increase acceleration.',
            'dTEW': 'Modifies trailing edge wedge angle - affects wake formation, base pressure, and overall drag. Smaller wedge angles typically reduce pressure drag.',
            'dTES': 'Changes trailing edge slope angle - influences flow separation point and wake characteristics. Affects pressure recovery in trailing edge region.',
            'dTH2': 'Adjusts thickness at 20% chord - controls forward loading and pressure distribution. Affects boundary layer development and transition location.',
            'dTH7': 'Modifies thickness at 70% chord - influences aft loading and pressure recovery. Critical for adverse pressure gradient management and separation control.',
        }
        return details.get(key, meaning)
    
    def get_action_description(self, action_array: np.ndarray):
        '''
        Get the description of the action
        '''
        description = ''
        for i, action in enumerate(action_array):
            description += f'{i+1}. {self.action_names[i]}: {action}\n'
        return description


class DescriptionStateFigure():
    '''
    Description of the state of the airfoil based on figure of wall Mach number distribution
    '''
    def __init__(self, figure_state: FigureState = None):
        # Get state dictionary from actual FigureState instance
        if figure_state is None:
            figure_state = FigureState()  # Use default parameters
            
        self.state_dict = figure_state.state_dict
        self.dim_state = figure_state.dim_state
        self.state_lower_bound = figure_state.state_lower_bound.tolist()
        self.state_upper_bound = figure_state.state_upper_bound.tolist()
        self.state_names = list(self.state_dict.keys())

    def get_general_description(self):
        '''
        Get the general description of the state
        '''
        # Build state parameter descriptions dynamically
        param_descriptions = []
        for i, (key, value) in enumerate(self.state_dict.items()):
            lower_bound = value['bound'][0]
            upper_bound = value['bound'][1]
            meaning = value['meaning']
            param_descriptions.append(f"""
{i+1}. **{key}** - {meaning}
   - Range: {lower_bound} to {upper_bound}
   - Units: {'dimensionless' if upper_bound <= 1.0 and 'degree' not in meaning else 'degrees' if 'degree' in meaning else 'chord fraction'}""")
        
        description = f"""
**FigureState: Comprehensive Airfoil State Representation**

This state representation combines parametric geometric features, aerodynamic coefficients, and visual information through wall Mach number distribution plots. The state consists of {self.dim_state} parameters that comprehensively describe airfoil characteristics:

**State Parameters:**{''.join(param_descriptions)}

**Key Features:**
- Combines geometric parameters (thickness, camber, leading/trailing edge characteristics)
- Includes aerodynamic performance metrics (lift, drag, moment coefficients)
- Provides visual representation through geometry and wall Mach number distribution plots
- Wall Mach number distribution is another representation of the pressure coefficient distribution
- The area of the wall Mach number distribution indicates the lift
- The area of the wall Mach number distribution in the rear region indicates the magnitude of the pitching moment
- Sonic line (M=1) indicates the supersonic region (M>1) and subsonic region (M<1)
- The abrupt change of the wall Mach number distribution from M<1 to M>1 indicates the shock wave
- The change of wall Mach number in the shock wave indicates the wave drag (Cd_wave), which is a major source of drag that can be reduced.

"""
        return description
    
    def get_state_description(self, state_array: np.ndarray):
        '''
        Get the description of a specific state
        
        Parameters
        ----------
        state_array : np.ndarray
            Array of state values corresponding to the state_dict parameters
            
        Returns
        -------
        str
            Formatted description of the state values
        '''
        if len(state_array) != self.dim_state:
            raise ValueError(f"State array length ({len(state_array)}) must match state dimension ({self.dim_state})")
            
        description = "**Current Airfoil State:**\n\n"
        
        # Group parameters by category for better organization
        geometric_params = ['t_max', 'x_t_max', 'volume', 'r_LE', 't_20p', 't_70p', 'c_avg']
        angle_params = ['a_LE', 'a_TEW', 'a_TES']
        crest_params = ['x_u_crest', 'y_u_crest', 'x_l_crest', 'y_l_crest']
        aero_params = ['Cl', 'Cd_wave', 'Cm']
        
        # Geometric characteristics
        description += "**Geometric Characteristics:**\n"
        for key in geometric_params:
            if key in self.state_names:
                idx = self.state_names.index(key)
                value = state_array[idx]
                meaning = self.state_dict[key]['meaning']
                description += f"  - {key}: {value:.6f} ({meaning})\n"
        
        # Angular characteristics
        description += "\n**Angular Characteristics:**\n"
        for key in angle_params:
            if key in self.state_names:
                idx = self.state_names.index(key)
                value = state_array[idx]
                meaning = self.state_dict[key]['meaning']
                description += f"  - {key}: {value:.2f}° ({meaning})\n"
        
        # Crest point analysis
        description += "\n**Flow Crest Points:**\n"
        for key in crest_params:
            if key in self.state_names:
                idx = self.state_names.index(key)
                value = state_array[idx]
                meaning = self.state_dict[key]['meaning']
                description += f"  - {key}: {value:.6f} ({meaning})\n"
        
        # Aerodynamic performance
        description += "\n**Aerodynamic Performance:**\n"
        for key in aero_params:
            if key in self.state_names:
                idx = self.state_names.index(key)
                value = state_array[idx]
                meaning = self.state_dict[key]['meaning']
                description += f"  - {key}: {value:.6f} ({meaning})\n"
        
        return description


class DescriptionTask():
    '''
    Description of the task
    '''
        
    @staticmethod
    def get_general_description():
        '''
        Get the general description of the task
        '''
        description = """
**Airfoil Design via Geometry Modification**

**Purpose:**
You are an airfoil designer tasked with modifying airfoil geometry. The design goal is to **minimize the drag of the airfoil**, meanwhile keeping the **lift and maximum thickness not to decrease**.

This is typically part of a multi-step design process.You'll receive the design history, where the modification was withdrawn when the drag was increased or the lift was significantly decreased. In other words, the modification is only kept when it is beneficial to the airfoil design.

In the current step, you will receive the airfoil geometry, wall Mach number distribution, the lift and drag coefficients and other state features. You need to determine the optimal modifications for the current step. 

"""
        return description

    @staticmethod
    def get_design_knowledge():
        '''
        Get the design rules
        '''
        description = """
There are some rules for airfoil design:

1) increasing the maximum thickness usually increases the drag;
2) changing the location of the maximum thickness will change the suction plateau and the shock wave location;
3) increasing the average camber of the front 60 percentage of the airfoil usually increases the lift;
4) increasing the average camber of the rear 40 percentage of the airfoil usually increases the aft loading, which usually increases the lift of the airfoil and increase the magnitude of the pitching moment (Cm);
5) increasing the leading edge radius usually reduces the suction peak;
6) changing the trailing edge wedge angle usually changes the suction peak;
7) changing the leading edge slope angle usually changes the suction peak;
8) changing the trailing edge slope angle usually changes the aft loading;
9) reducing the suction peak usually reduces the airfoil drag;
    
These rules are not always correct for different airfoils, but they can guide you in modifying the airfoil to reduce drag.

"""
        return description

    @staticmethod
    def instruction_for_bump_action_output():
        '''
        Get the instruction for action output
        '''
        description = """
At the very end of your response, you **must** tell me the decision in the following format: 
    upper = [location, height, width]
    lower = [location, height, width] 

For example: 
    upper = [0.2, 0.001, 0.5] 
    lower = [0.8, -0.001, 0.5]

No more content after the decision in this format is given. 

If the text indicates no bump is added to the upper or lower surface, then the parameters of the corresponding surface are zero. You should only reply python code in plain text without any other content.
"""
        return description


class DescriptionHistory():
    '''
    Convert the environment trajectory to a short description.
    
    The [state, action, reward, next_state, is_valid] in each step is converted to description.
    Only the float values in the state and action are converted to description.
    The figure is not converted to description.
    '''
    
    @staticmethod
    def convert_to_description(
            environment_trajectory: Union[List[Dict[str, Any]], str],
            state_dict: Dict[str, Dict[str, Any]] = None,
            action_dict: Dict[str, Dict[str, Any]] = None,
            ) -> str:
        '''
        Convert the environment trajectory to a description
        
        Parameters
        ----------
        environment_trajectory : Union[List[Dict[str, Any]], str]
            Either a list of trajectory steps, each containing:
            - previous_state: np.ndarray/list
            - action: np.ndarray/list  
            - reward: float
            - next_state: np.ndarray/list
            - done: bool
            - info: dict
            Or a string path to a JSON file containing such trajectory data
        state_dict : Dict[str, Dict[str, Any]]
            The dictionary of the state parameters
        action_dict : Dict[str, Dict[str, Any]]
            The dictionary of the action parameters
            
        Note
        ----
        The state_dict and action_dict are used to get the names of the state and action parameters.
        If not provided, the default state_dict and action_dict will be used.
        
        Returns
        -------
        str
            Human-readable description of the trajectory
        '''
        # Handle JSON file path input
        if isinstance(environment_trajectory, str):
            try:
                with open(environment_trajectory, 'r') as f:
                    trajectory_data = json.load(f)
            except FileNotFoundError:
                return f"Error: Trajectory file '{environment_trajectory}' not found."
            except json.JSONDecodeError:
                return f"Error: Invalid JSON format in file '{environment_trajectory}'."
        else:
            trajectory_data = environment_trajectory
        
        if not trajectory_data:
            return "Empty trajectory - no design steps recorded."
        
        state_names = list(state_dict.keys())
        action_names = list(action_dict.keys())

        # Initialize description components
        description_parts = []
        description_parts.append("**Airfoil Design History:**\n")

        for step in trajectory_data:
            
            # Extract data from step
            prev_state = np.array(step.get('previous_state', []))
            action = np.array(step.get('action', []))
            reward = step.get('reward', 0.0)
            next_state = np.array(step.get('next_state', []))
            i_current_step = int(step['info']['i_current_step'])
            i_reference_step = int(step['info']['i_reference_step'])
            is_current_step_valid = bool(step['info']['is_current_step_valid'])
            
            # Check if this is the initial state (first step with zero action)
            is_initial_state = i_current_step == 0
            
            if is_initial_state:
                description_parts.append(f"\n**Initial Airfoil:**")
                description_parts.append(f"\n- Initial state:")
            else:
                description_parts.append(f"\n**Step {i_current_step}:**")
                description_parts.append(f"\n- Reference step to be modified: {i_reference_step}")
                description_parts.append(f"\n- Reference state:")
            
            # Describe the reference state (key aerodynamic parameters)
            for i in range(len(state_names)):
                description_parts.append(f" {state_names[i]}= {prev_state[i]:.4f}")
                
            if is_initial_state:
                description_parts.append("\n")
                continue

            description_parts.append(f"\n- Action:")
            for i in range(len(action_names)):
                description_parts.append(f" {action_names[i]}= {action[i]:.4f}")
                
            description_parts.append(f"\n- Next state:")
            for i in range(len(state_names)):
                description_parts.append(f" {state_names[i]}= {next_state[i]:.4f}")
            
            description_parts.append(f"\n- Reward: {reward:.4f}")
            description_parts.append(f"\n- Is current step valid: {is_current_step_valid}")
            description_parts.append("\n")

        return "".join(description_parts)
    

class PromptForLLM():
    '''
    Generate the prompt for LLM.
    
    In this class, we consider `DescriptionActionBumpMod` and `DescriptionStateFigure` as the action and state description.
    
    The prompt contains description of:
    - the task
    - the design knowledge
    - the state
    - the action
    - the history of the trajectory
    - the instruction for action output
    '''
    def __init__(self,
                description_action: DescriptionActionBumpMod = None,
                description_state: DescriptionStateFigure = None,
                ):
        if description_action is None:
            description_action = DescriptionActionBumpMod()
        if description_state is None:
            description_state = DescriptionStateFigure()
            
        self.description_action = description_action
        self.description_state = description_state
        
    def generate_prompt(self,
                        environment_trajectory: Union[List[Dict[str, Any]], str],
                        state_dict: Dict[str, Dict[str, Any]] = None,
                        action_dict: Dict[str, Dict[str, Any]] = None,
                        ) -> str:
        '''
        Get the prompt for LLM.
        
        Parameters
        ----------
        environment_trajectory : Union[List[Dict[str, Any]], str]
            The environment trajectory
        state_dict : Dict[str, Dict[str, Any]]
            The dictionary of the state parameters
        action_dict : Dict[str, Dict[str, Any]]
            The dictionary of the action parameters
        '''
        prompt = ""
        prompt += DescriptionTask.get_general_description()
        prompt += DescriptionTask.get_design_knowledge()
        prompt += self.description_state.get_general_description()
        prompt += self.description_action.get_general_description()
        prompt += DescriptionHistory.convert_to_description(
            environment_trajectory,
            state_dict,
            action_dict,
            )
        prompt += DescriptionTask.instruction_for_bump_action_output()
        return prompt

