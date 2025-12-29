"""
TSFoil core data management module

Contains the core class for all shared data, responsible for:
- Data storage and management
- Configuration parameter settings
- Fortran module initialization
- Working directory management
"""

import math
import sys
import os
from pathlib import Path
import numpy as np

from ._fortran import tsf


class TSFoilCore:
    """
    TSFoil core data class that stores all shared data and configuration information.
    
    This class is the data center for all functional modules, through which you can access:
    - Configuration parameters (config)
    - Airfoil data (airfoil) 
    - Mesh data (mesh)
    - Computation results (data_summary)
    """
    
    def __init__(self, airfoil_coordinates: np.ndarray|None = None,
                 airfoil_file: str|None = None,
                 work_dir: str|None = None,
                 output_dir: str|None = None):
        """
        Initialize core data object
        
        Parameters
        ----------
        airfoil_coordinates: ndarray [n_points, 2] | None
            Airfoil coordinate data
        airfoil_file: str | None  
            Airfoil geometry file path
        work_dir: str | None
            Working directory
        output_dir: str | None
            Output directory
        """
        # Core data structures
        self.config = {}
        self.airfoil = {'file': airfoil_file, 'coordinates': airfoil_coordinates}
        self.mesh = {}
        self.data_summary = {}

        # Setup working directories
        self._setup_directories(work_dir, output_dir)
        
        # Setup default configuration
        self._default_config()
    
    def _setup_directories(self, work_dir: str|None, output_dir: str|None) -> None:
        """Setup working directory and output directory"""
        # Fortran output file directory (smry.out, tsfoil2.out)
        if work_dir is None:
            script_dir = Path(__file__).parent
            parent_dir = script_dir.parent
            os.chdir(parent_dir)
        else:
            os.chdir(work_dir)
                        
        self.work_dir = os.getcwd()
        
        # Python output file directory (cpxs.dat, field.dat)
        if output_dir is None:
            self.output_dir = self.work_dir
        else:
            self.output_dir = output_dir
    
    def _default_config(self) -> None:
        """Setup default configuration parameters"""
        # Default configuration parameters (user input parameters, corresponding to namelist /INP/ in Fortran)
        # NU, NL, DELTA are set in set_airfoil()
        # IMAXI, JMAXI are set in set_mesh()
        self.config = {
            'AK': 0.0,              # Free stream similarity parameter
            'ALPHA': 0.0,           # Angle of attack
            'BCTYPE': 1,            # Boundary condition type (1 = free air, 2 = wind tunnel)
            'CVERGE': 0.00001,      # Convergence criterion
            'DVERGE': 10.0,         # Divergence criterion  
            'EMACH': 0.75,          # Mach number
            'EPS': 0.2,             # Convergence tolerance
            'FCR': 1,               # Whether to use fully conservative difference equations
            'IPRTER': 100,          # Convergence history print interval
            'KUTTA': 1,             # Whether to enforce Kutta condition
            'MAXIT': 1000,          # Maximum number of iterations
            'PHYS': 1,              # Physical coordinates vs similarity coordinates
            'POR': 0.0,             # Porosity
            'RIGF': 0.0,            # Rigidity factor for transonic effects
            'SIMDEF': 3,            # Similarity scaling (1 = Cole, 2 = Spreiter, 3 = Krupp)
            'WCIRC': 1.0,           # Weight for circulation jump at trailing edge (0.0-1.0)
            'WE': [1.8, 1.9, 1.95], # SOR relaxation factors
            'NWDGE': 0,             # Viscous wedge parameters (0 = no wedge, 1 = Murman wedge, 2 = Yoshihara wedge)
            'REYNLD': 4.0E6,        # Reynolds number
            'WCONST': 4.0,          # Wall constant for Murman wedge
            'IFLAP': 0,             # Flap flag
            'DELFLP': 0.0,          # Flap deflection angle
            'FLPLOC': 0.77,         # Flap location
            'n_point_x': 81,        # Grid points in x-direction (IMAXI)
            'n_point_y': 60,        # Grid points in y-direction (JMAXI)
            'n_point_airfoil': 51,  # Number of points on airfoil surface
            
            'flag_output': True,          # Write solver process to tsfoil2.out
            'flag_output_summary': True,  # smry.out
            'flag_output_shock': True,    # cpxs.dat
            'flag_output_field': True,    # field.dat
            
            'flag_print_info': True,
        }
        
        # Default parameters
        self.skiprows: int = 1
        self.x_scale: float = 5.0
        self.y_scale: float = 4.0
    
    def set_config(self, **kwargs) -> None:
        """
        Set configuration parameters
        
        Parameters
        ----------
        **kwargs
            Configuration parameter key-value pairs
        """
        for key, value in kwargs.items():
            if key in self.config:
                self.config[key] = value
            else:
                raise ValueError(f"Invalid configuration parameter: {key}")
    
    def initialize_data(self) -> None:
        """Initialize data in Fortran module and Python module"""
        # Initialize common data
        tsf.common_data.initialize_common()
        tsf.solver_data.initialize_solver_data()

        # Check parameters
        if self.config['EMACH'] < 0.5 or self.config['EMACH'] > 2.0:
            raise ValueError("EMACH must be between 0.5 and 2.0")
        if self.config['ALPHA'] < -9.0 or self.config['ALPHA'] > 9.0:
            raise ValueError("ALPHA must be between -9.0 and 9.0")
        if self.config['NWDGE'] > 0 and self.config['EMACH'] > 1.0:
            raise ValueError("NWDGE must be 0 if EMACH <= 1.0")
        
        # Set AK=0 for physical coordinates
        if self.config['PHYS'] == 1:
            self.config['AK'] = 0.0
            
        # Constants
        self.n_mesh_points = tsf.common_data.n_mesh_points
        self.nmp_plus1 = tsf.common_data.nmp_plus1
        self.nmp_plus2 = tsf.common_data.nmp_plus2
        
        # Apply self.config to common data
        for key, value in self.config.items():
            setattr(tsf.common_data, key.lower(), value)
        
        # Print working directory and output directory
        if self.config['flag_print_info']:
            print(f"pyTSFoil working directory: {self.work_dir}")
            print(f"pyTSFoil output directory: {self.output_dir}")
            print()

    def emach1(self, u: float, delta: float) -> float:
        """
        Compute local Mach number.
        
        Parameters
        ----------
        u : float
            Local velocity
        delta : float
            Maximum thickness of airfoil
            
        Returns
        -------
        float
            Local Mach number or similarity parameter
        """
        ak = tsf.common_data.ak
        gam1 = tsf.common_data.gam1
        phys = tsf.common_data.phys
        simdef = tsf.common_data.simdef
        emach = tsf.common_data.emach
        
        # Compute similarity parameter based on local velocity
        ak1 = ak - gam1 * u
        
        if not phys:
            # Return value of local similarity parameter
            return ak1
        else:
            # Compute value of local Mach number and return
            delrt2 = delta ** (2.0 / 3.0)
            
            if simdef == 1:  # Cole scaling
                arg = 1.0 - delrt2 * ak1
            elif simdef == 2:  # Spreiter scaling
                arg = 1.0 - delrt2 * ak1 * emach ** (4.0 / 3.0)
            elif simdef == 3:  # Krupp scaling
                arg = 1.0 - delrt2 * ak1 * emach
            else:
                raise ValueError(f"SIMDEF {simdef} not supported in emach1")
            
            if arg > 0.0:
                return math.sqrt(arg)
            return 0.0

    def px(self, i: int, j: int) -> float:
        """
        Compute U = DP/DX at point I,J
        
        Parameters
        ----------
        i : int
            X index (1-based Fortran indexing)
        j : int
            Y index (1-based Fortran indexing)
        
        Returns
        -------
        float
            Velocity component dP/dX at point (i,j)
        """
        imin = tsf.common_data.imin
        imax = tsf.common_data.imax
        xdiff = tsf.common_data.xdiff
        p_arr = tsf.solver_data.p
        
        # Convert to 0-based indexing for array access
        i0 = i - 1
        j0 = j - 1
        
        if i == imin:
            # Upstream boundary
            return (1.5 * xdiff[i] * (p_arr[j0, i0 + 1] - p_arr[j0, i0]) -
                    0.5 * xdiff[i + 1] * (p_arr[j0, i0 + 2] - p_arr[j0, i0 + 1]))
        elif i == imax:
            # Downstream boundary
            return (1.5 * xdiff[i0] * (p_arr[j0, i0] - p_arr[j0, i0 - 1]) -
                    0.5 * xdiff[i0 - 1] * (p_arr[j0, i0 - 1] - p_arr[j0, i0 - 2]))
        else:
            # Interior mesh point
            pji = p_arr[j0, i0]
            return 0.5 * (xdiff[i] * (p_arr[j0, i0 + 1] - pji) + 
                         xdiff[i0] * (pji - p_arr[j0, i0 - 1]))

    def py(self, i: int, j: int) -> float:
        """
        Compute V = DP/DY at point I,J
        
        Parameters
        ----------
        i : int
            X index (1-based Fortran indexing)
        j : int
            Y index (1-based Fortran indexing)
        
        Returns
        -------
        float
            Velocity component dP/dY at point (i,j)
        """
        jmin = tsf.common_data.jmin
        jmax = tsf.common_data.jmax
        jup = tsf.common_data.jup
        jlow = tsf.common_data.jlow
        ile = tsf.common_data.ile
        ite = tsf.common_data.ite
        ydiff = tsf.common_data.ydiff
        alpha = tsf.common_data.alpha
        fxu = tsf.common_data.fxu
        fxl = tsf.common_data.fxl
        p_arr = tsf.solver_data.p
        pjump = tsf.solver_data.pjump
        
        # Convert to 0-based indexing for array access
        i0 = i - 1
        j0 = j - 1
        jup0 = jup - 1
        jlow0 = jlow - 1
        
        if j == jmin:
            # I,J is on lower boundary. Use one sided derivative
            return (1.5 * ydiff[j] * (p_arr[j0 + 1, i0] - p_arr[j0, i0]) -
                    0.5 * ydiff[j + 1] * (p_arr[j0 + 2, i0] - p_arr[j0 + 1, i0]))
        
        elif j == jlow:
            # I,J is on row of mesh points below airfoil
            vminus = ydiff[j0] * (p_arr[j0, i0] - p_arr[j0 - 1, i0])
            
            if i < ile:
                # I,J is ahead of airfoil
                return 0.5 * ((p_arr[jup0, i0] - p_arr[jlow0, i0]) * ydiff[jup0] + vminus)
            elif i > ite:
                # I,J is behind airfoil
                return 0.5 * ((p_arr[jup0, i0] - pjump[i0] - p_arr[jlow0, i0]) * ydiff[jup0] + vminus)
            else:
                # I,J is under airfoil. Use derivative boundary condition
                ic = i - ile  # 0-based index for fxl
                return 0.5 * (fxl[ic] - alpha + vminus)
        
        elif j == jup:
            # I,J is on row of mesh points above airfoil
            vplus = ydiff[j] * (p_arr[j0 + 1, i0] - p_arr[j0, i0])
            
            if i < ile:
                # I,J is ahead of airfoil
                return 0.5 * ((p_arr[jup0, i0] - p_arr[jlow0, i0]) * ydiff[jup0] + vplus)
            elif i > ite:
                # I,J is behind airfoil
                return 0.5 * ((p_arr[jup0, i0] - pjump[i0] - p_arr[jlow0, i0]) * ydiff[jup0] + vplus)
            else:
                # I,J is over airfoil. Use derivative boundary condition
                ic = i - ile  # 0-based index for fxu
                return 0.5 * (vplus + fxu[ic] - alpha)
        
        elif j == jmax:
            # I,J is on top row of mesh points. Use one sided formula
            return (1.5 * ydiff[j0] * (p_arr[j0, i0] - p_arr[j0 - 1, i0]) -
                    0.5 * ydiff[j0 - 1] * (p_arr[j0 - 1, i0] - p_arr[j0 - 2, i0]))
        
        else:
            # I,J is an interior point
            pji = p_arr[j0, i0]
            return 0.5 * (ydiff[j] * (p_arr[j0 + 1, i0] - pji) + 
                         ydiff[j0] * (pji - p_arr[j0 - 1, i0]))

