'''
This is a python interface for TSFOIL.

The wind tunnel option is not implemented.

Note
------
Data security:
- CRITICAL: All PyTSFoil instances in the same Python process share the same 
  underlying Fortran module data (tsf.common_data, tsf.solver_data, etc.)
- Creating multiple PyTSFoil objects (e.g., pytsfoil1 = PyTSFoil(), pytsfoil2 = PyTSFoil()) 
  will result in shared state - changes made by one instance affect all others
- This can lead to data corruption, incorrect results, and unpredictable behavior

Safe usage patterns:
1. Single instance per process: Use only one PyTSFoil instance at a time in each Python process
2. Sequential analysis: Complete one analysis before starting another
3. Multiprocessing: For parallel analyses, use multiprocessing.Pool where each process 
   gets its own isolated copy of the Fortran data
4. Process isolation: Each subprocess will have independent Fortran module variables

Example of UNSAFE usage (same process):
    pytsfoil1 = PyTSFoil()  # ⚠️  These share the same
    pytsfoil2 = PyTSFoil()  # ⚠️  Fortran data!

Example of SAFE usage (multiprocessing):
    import multiprocessing as mp
    def run_analysis(params):
        pytsfoil = PyTSFoil()  # ✅ Each process gets its own data
        # ... run analysis
    with mp.Pool() as pool:
        pool.map(run_analysis, case_list)

'''

import sys
import os
from pathlib import Path
import numpy as np
from scipy.interpolate import CubicSpline
from scipy import integrate
import matplotlib.pyplot as plt
from typing import Dict, Any

try:
    from .leading_edge import (solve_inner_problem, apply_composite_correction,
                               compute_surface_corrections, compute_phi_s_2d,
                               apply_step_d_le_closure)
except ImportError:
    solve_inner_problem = None
    apply_composite_correction = None
    compute_surface_corrections = None
    compute_phi_s_2d = None
    apply_step_d_le_closure = None

try:
    import tsfoil_fortran as tsf
except ImportError as e:
    print("ERROR: Could not import tsfoil_fortran module!")
    print(f"Import error: {e}")
    print()
    print("Make sure you have compiled the Fortran modules with f2py:")
    print("  python3 pyTSFoil/compile_f2py.py")
    sys.exit(1)


def calculate_cp_isentropic(ma: np.ndarray, minf: float, gamma: float = 1.4) -> np.ndarray:
    '''
    Calculate the isentropic pressure coefficient from local Mach number and freestream Mach number.

    Parameters
    ----------
    ma: np.ndarray
        Local Mach number at each point.
        
    minf: float
        Freestream Mach number.
        
    gamma: float, optional
        Ratio of specific heats (default is 1.4 for air).
        
    Returns
    -------
    cp: np.ndarray
        Isentropic pressure coefficient at each point.
    '''
    denom = 2.0 + (gamma - 1.0) * ma ** 2
    numer = 2.0 + (gamma - 1.0) * minf ** 2
    return (2.0 / (gamma * minf ** 2)) * ((numer / denom) ** (gamma / (gamma - 1.0)) - 1.0)


class PyTSFoil(object):
    '''
    Python interface for TSFOIL Fortran module.
    
    Parameters
    ----------
    airfoil_coordinates: ndarray [n_points, 2] | None
        The coordinates of the airfoil.
        The data starts from the airfoil's trailing edge in the upper surface,
        and then goes counter-clockwise around the airfoil.
        
    airfoil_file: str | None
        The file containing the airfoil geometry.
        The data starts from the airfoil's trailing edge in the upper surface,
        and then goes counter-clockwise around the airfoil.
        
    work_dir: str | None
        The working directory.
        If None, the working directory is the parent directory of the script.
        
    output_dir: str | None
        The output directory.
        If None, the output directory is the working directory.
    
    '''
    def __init__(self,
            airfoil_coordinates: np.ndarray|None = None,
            airfoil_file: str|None = None,
            work_dir:str|None = None,
            output_dir:str|None = None):
        '''
        Initialize the TSFoil object.
        '''
        self.config = {}
        self.airfoil = {'file': airfoil_file, 'coordinates': airfoil_coordinates}
        self.mesh = {}
        self.data_summary = {}

        # The directory for Fortran output files (smry.out, tsfoil2.out)
        if work_dir is None:
            script_dir = Path(__file__).parent
            parent_dir = script_dir.parent
            os.chdir(parent_dir)
        else:
            os.chdir(work_dir)
                        
        self.work_dir = os.getcwd()
        
        # The directory for Python output files (cpxs.dat, field.dat)
        if output_dir is None:
            self.output_dir = self.work_dir
        else:
            self.output_dir = output_dir
        
        self._default_config()
    
    def set_config(self, **kwargs):
        '''
        Set the configuration parameters.
        '''
        for key, value in kwargs.items():
            if key in self.config:
                self.config[key] = value
            else:
                print(f"WARNING: Invalid configuration parameter: {key}")
    
    def run(self):
        '''
        Run the TSFoil_modern main program flow (matching main.f90 exactly).
        '''
        self.initialize_data()
        
        self.set_airfoil()
        
        self.set_mesh()
        
        self.compute_mesh_indices()

        self.run_fortran_solver()
        
        self.compute_data_summary()
        
        self.print_summary()
    
    def run_fortran_solver(self) -> None:
        '''
        Run the Fortran solver.
        '''
        # Scale variables to similarity form
        self.compute_scale()

        # Set far field boundary conditions
        self.compute_far_field_bc()

        self.compute_geometry_derivatives()

        # Singularity subtraction step D: regularise FXU/FXL before SETBC/SOLVE
        self._phi_sx_surface = None
        if self.config.get('apply_singularity_subtraction', False):
            self._apply_singularity_subtraction_bc()

        # Compute finite difference coefficients
        tsf.solver_base.difcoe()

        # Set boundary conditions
        tsf.solver_functions.setbc(0)

        # Solve transonic flow equations
        tsf.main_iteration.solve()

        # Restore original FXU/FXL so downstream computations (cdcole, etc.) are unaffected.
        if self.config.get('apply_singularity_subtraction', False) and hasattr(self, '_fxu_orig'):
            nfoil = self.mesh['nfoil']
            tsf.common_data.fxu[:nfoil] = self._fxu_orig
            tsf.common_data.fxl[:nfoil] = self._fxl_orig
    
    def _default_config(self):
        '''
        Set the default configuration parameters.
        '''
        # Default configuration (user-input parameters, namelist /INP/ in io_module.f90)
        # NU, NL, DELTA are set in set_airfoil()
        # IMAXI, JMAXI are set in set_mesh()
        self.config = {
            'ALPHA': 0.0,           # Angle of attack
            'CVERGE': 0.00001,      # Error criterion for convergence
            'DVERGE': 10.0,         # Error criterion for divergence
            'EMACH': 0.75,          # Mach number
            'EPS': 0.2,             # Artificial viscosity parameter (0.0-1.0)
            'IPRTER': 100,          # Print interval for convergence history
            'MAXIT': 1000,          # Maximum number of iterations
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
            'n_point_x': 81,        # Number of points in the x-direction (IMAXI)
            'n_point_y': 60,        # Number of points in the y-direction (JMAXI)
            'n_point_airfoil': 51,  # Number of points on the airfoil
            
            'flag_output': True,     # write solver process to tsfoil2.out
            'flag_output_summary': True,   # smry.out
            'flag_output_shock': True,     # cpxs.dat
            'flag_output_field': True,     # field.dat
            'flag_print_info': True, # print information to console

            # MAE leading-edge correction (post-processing, non-invasive)
            'apply_le_correction': False,

            # Singularity subtraction: regularise TSD outer solve at LE (steps A+C+E)
            # Populates 2D PHI_S for SYOR and stores phi_s,x for output_surface.
            # Should be combined with apply_le_correction for correct near-LE cp.
            'apply_singularity_subtraction': False,

            # Step D LE closure: modify FXU/FXL so phi_r = phi_1 - phi_s has
            # a bounded body slope at x=0 (removes the x^{-1/2} spike).
            # Requires apply_singularity_subtraction=True to take effect.
            'apply_step_d': False,
            # Extrapolation method for the LE closure ('linear'|'sqrt_fit'|'constant').
            # 'linear'   fits phi_ry = A + B*x and extrapolates intercept to x=0.
            #            Empirically best overall: significantly better than constant
            #            for mid-AoA cases and nearly tied with sqrt_fit for high-AoA.
            # 'sqrt_fit' fits phi_ry = A + B*sqrt(x), matching the theoretical form
            #            phi_ry ~ c1/delta + c2/delta*sqrt(x); slightly better for
            #            very high AoA but worse for mid-AoA.
            # 'constant' uses phi_ry[i_eff] directly (no fit); fastest but biased.
            'step_d_method': 'linear',

        }
        
        # Default parameters
        self.skiprows : int = 1
        self.x_scale : float = 5.0
        self.y_scale : float = 4.0

    def initialize_data(self) -> None:
        '''
        Initialize the data in Fortran module and Python module.
        '''
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
         
        # Constants
        self.n_mesh_points = tsf.common_data.n_mesh_points
        self.nmp_plus1 = tsf.common_data.nmp_plus1
        self.nmp_plus2 = tsf.common_data.nmp_plus2
        
        self._gamma = float(tsf.common_data.gam) # gamma = 1.4
        self._gam1 = float(tsf.common_data.gam1) # gamma + 1
        
        # Apply self.config to common data
        for key, value in self.config.items():
            setattr(tsf.common_data, key.lower(), value)

        # Print working directory and output directory
        if self.config['flag_print_info']:
            print(f"pyTSFoil working directory: {self.work_dir}")
            print(f"pyTSFoil output directory: {self.output_dir}")
            print()

    def set_airfoil(self) -> None:
        '''
        Read the airfoil geometry from a file, and set the airfoil geometry.
        
        Attributes
        ----------
        airfoil_file : str
            The file containing the airfoil geometry.
            The data starts from the airfoil's trailing edge in the upper surface,
            and then goes counter-clockwise around the airfoil.
            
        skiprows : int
            The number of rows to skip in the airfoil file.
        '''
        if self.airfoil['file'] is not None:
            x, y = np.loadtxt(self.airfoil['file'], skiprows=self.skiprows).T
        elif self.airfoil['coordinates'] is not None:
            x = self.airfoil['coordinates'][:, 0]
            y = self.airfoil['coordinates'][:, 1]
        else:
            raise ValueError("Either airfoil_file or airfoil_coordinates must be provided")
        
        le_pos = x.argmin()

        xu = x[:le_pos+1][::-1]
        yu = y[:le_pos+1][::-1]
        xl = x[le_pos:]
        yl = y[le_pos:]
        
        # Interpolate the airfoil and get the maximum thickness (DELTA)
        x_interp = np.linspace(np.min(x), np.max(x), num=501)
        yu_interp = np.interp(x_interp, xu, yu)
        yl_interp = np.interp(x_interp, xl, yl)
        t_max = np.max(yu_interp - yl_interp)
        
        self.airfoil['t_max'] = t_max
        self.airfoil['x'] = x
        self.airfoil['y'] = y
        self.airfoil['xu'] = xu
        self.airfoil['yu'] = yu
        self.airfoil['xl'] = xl
        self.airfoil['yl'] = yl
        
        tsf.common_data.delta = np.float32(t_max)

        # Parabolic nose fit for MAE leading-edge correction (step 1)
        self._fit_nose_geometry()

    def _fit_nose_geometry(self) -> None:
        """
        Fit parabolic nose parameters h (shape constant) and R_c (curvature radius).

        Model: half-thickness t(x) ~ h * sqrt(x) for x in (0, x_fit_max], c=1.
        Derives from ct(x/c) ~ 2h*(cx)^{1/2}  =>  t(x) = 2h*sqrt(c)*sqrt(x)/c = 2h*sqrt(x) for c=1.
        Therefore the polyfit slope h_coef = 2h  =>  h = h_coef / 2.
        Curvature radius: R_c = 2 * h^2 * delta^2 * c.
        """
        xu    = self.airfoil['xu']
        yu    = self.airfoil['yu']
        xl    = self.airfoil['xl']
        yl    = self.airfoil['yl']
        delta = self.airfoil['t_max']
        c     = 1.0  # normalized chord

        x_fit_max = 0.05
        mask = (xu > 1e-6) & (xu <= x_fit_max)
        if mask.sum() < 3:
            x_fit_max = 0.10
            mask = (xu > 1e-6) & (xu <= x_fit_max)

        x_fit  = xu[mask]
        yu_fit = yu[mask]
        yl_fit = np.interp(x_fit, xl, yl)

        half_thickness = (yu_fit - yl_fit) / 2.0   # physical half-thickness (c=1)

        # Fit: half_thickness = h_coef * sqrt(x)  (through origin)
        sqrt_x = np.sqrt(x_fit)
        h_coef = np.dot(half_thickness, sqrt_x) / np.dot(sqrt_x, sqrt_x)

        h   = h_coef / 2.0         # shape constant (half of sqrt(x) fit coefficient)
        R_c = 2.0 * h ** 2 * c    # parabolic nose radius: R_c = h_coef^2/2 for c=1

        self.airfoil['h_nose'] = h
        self.airfoil['R_c']    = R_c

        if self.config.get('flag_print_info', False):
            print(f"  Nose fit: h = {h:.5f},  R_c = {R_c:.6f}")

    def _apply_singularity_subtraction_bc(self) -> None:
        """
        Steps A+C+E (always) and optional D: populate 2D PHI_S for SYOR.

        Steps always applied:
          A - PHI_S 2D array filled so SYOR uses phi_tot,x = phi_r,x + phi_s,x
              for VC type-switching (restores correct subsonic/supersonic flag).
          C - SYOR Step C (already in Fortran) adds -L[phi_s] forcing so the
              combined equation is L[phi_1] = 0 (same physics as baseline, but
              solved via the bounded variable phi_r = phi_1 - phi_s).
          E - phi_s,x stored for post-processing velocity restoration.

        Step D (enabled by config 'apply_step_d': True):
          Modifies FXU/FXL so phi_r = phi_1 - phi_s has a bounded body slope.
          Uses the parabolic-nose formula phi_s,y = chi*h/(delta*sqrt(x)) with
          LE closure to remove the x=0 spike (where phi_s,y = 0 by construction).
          Original FXU/FXL are saved in _fxu_orig/_fxl_orig and restored after
          the solver finishes.

        Stores phi_sx_surface for step E in _phi_sx_surface.
        """
        if compute_surface_corrections is None:
            print("WARNING: leading_edge module unavailable; skipping singularity subtraction.")
            return

        h   = self.airfoil.get('h_nose', None)
        R_c = self.airfoil.get('R_c',    None)
        if h is None or R_c is None or R_c <= 0:
            print("WARNING: Nose geometry not fitted; skipping singularity subtraction.")
            return

        nfoil  = self.mesh['nfoil']
        x_foil = self.mesh['xx_airfoil'][:nfoil]
        delta  = float(tsf.common_data.delta)

        _phi_sy_upper, phi_sx_surface = compute_surface_corrections(
            x_foil, h, delta, R_c, self._cpfact, self._gamma,
        )

        # Step D: body-BC regularisation with LE closure.
        # Subtract phi_s,y from FXU/FXL so phi_r has a bounded surface slope.
        # At x=0 phi_s,y = 0 by construction (spike); the LE closure extrapolates
        # phi_ry from the first non-zero correction point back to x=0.
        if self.config.get('apply_step_d', False) and apply_step_d_le_closure is not None:
            fxu_cur = np.array(tsf.common_data.fxu[:nfoil], dtype=np.float64)
            fxl_cur = np.array(tsf.common_data.fxl[:nfoil], dtype=np.float64)
            self._fxu_orig = fxu_cur.astype(np.float32)
            self._fxl_orig = fxl_cur.astype(np.float32)
            fxu_mod, fxl_mod = apply_step_d_le_closure(
                fxu_cur, fxl_cur, _phi_sy_upper.astype(np.float64),
                x_foil=x_foil.astype(np.float64),
                method=self.config.get('step_d_method', 'sqrt_fit'),
            )
            tsf.common_data.fxu[:nfoil] = fxu_mod
            tsf.common_data.fxl[:nfoil] = fxl_mod

        # Store step-E correction for output_surface
        self._phi_sx_surface = phi_sx_surface

        # ── Steps A+C: fill 2D PHI_S on the full outer grid ───────────────────
        if compute_phi_s_2d is not None:
            imax = int(tsf.common_data.imax)
            jmax = int(tsf.common_data.jmax)
            X_1d = np.array(tsf.common_data.x[:imax], dtype=np.float64)
            Y_1d = np.array(tsf.common_data.y[:jmax], dtype=np.float64)
            phi_s_2d = compute_phi_s_2d(
                X_1d, Y_1d, R_c, self._cpfact, self._gamma
            )  # shape (jmax, imax), float32
            tsf.solver_data.phi_s[:jmax, :imax] = phi_s_2d
        else:
            print("WARNING: compute_phi_s_2d unavailable; PHI_S stays zero.")

    def set_mesh(self) -> None:
        '''
        Set the mesh coordinates.
        
        The user-defined mesh is provided to TSFOIL with 1-d arrays XIN, YIN.
        XIN and YIN are the x-coordinates and y-coordinates of the mesh points, respectively.
        The mesh is a 2-d nonuniform Cartesian grid.
        The airfoil has a unit chord length, the leading edge is at (0,0), and the trailing edge is at (1,0).
        The XIN distributes more points near x=0 and x=1, and the YIN distributes more points near y=0.
        
        Attributes
        ----------
        n_point_x: int
            The number of points in the x-direction.
            
        n_point_y: int
            The number of points in the y-direction.
            
        n_point_airfoil: int
            The number of points on the airfoil.
            
        x_scale: float
            The range of the x-coordinate of the mesh, x in [-x_scale, x_scale].
            
        y_scale: float
            The range of the y-coordinate of the mesh, y in [-y_scale, y_scale].
        '''

        #* Generate x-coordinates with clustering near x=0 and x=1 (interior points)
        # Split domain into three segments: [-x_scale, 0], [0, 1], [1, x_scale]
        # Distribute remaining points between left and right segments
        n_remaining = int((self.config['n_point_x']-self.config['n_point_airfoil'])*0.5) + 1
        
        # Segment 1: [x_min, 0] with clustering near x=0 (right end)
        x_left_norm = self.clustcos(n_remaining, a0=1.00, a1=0.999, beta=1.0)
        x_left = - x_left_norm[::-1] * self.x_scale
        
        # Segment 2: [0, 1] with clustering at both ends
        x_center = self.clustcos(self.config['n_point_airfoil'], a0=0.01, a1=0.96, beta=1.0)
        
        # Segment 3: [1, x_max] with clustering near x=1 (left end)
        x_right_norm = self.clustcos(n_remaining, a0=0.001, a1=0.1, beta=1.0)
        x_right = 1 + x_right_norm * (self.x_scale - 1)
        
        # Combine segments, removing duplicate boundary points
        xx = np.concatenate([
            x_left[:-1],    # Exclude right boundary (x=0)
            x_center,
            x_right[1:]     # Exclude left boundary (x=1)
        ])
        
        #* Generate symmetric distribution of y-coordinates with clustering near y=0
        half_points = self.config['n_point_y'] // 2 + 1  # Include y=0
        y_half = self.clustcos(half_points, a0=1.0, a1=0.999, beta=2.0)
        
        # Create symmetric distribution: negative half + positive half
        # y_half goes from 0 to 1, we want symmetric distribution about 0
        yy_normalized = np.concatenate([
            -y_half[1:][::-1],  # Negative side: -1 to 0 (excluding 0)
            y_half[1:]          # Positive side: 0 to 1 (excluding 0)
        ])
        
        # Scale to [-y_scale, y_scale]
        yy = yy_normalized * self.y_scale
        
        # Store mesh parameters
        self.config['n_point_x'] = xx.shape[0]
        self.config['n_point_y'] = yy.shape[0]

        self.mesh['x_min'] = -self.x_scale
        self.mesh['x_max'] = self.x_scale
        self.mesh['y_min'] = -self.y_scale
        self.mesh['y_max'] = self.y_scale
        self.mesh['xx'] = xx
        self.mesh['yy'] = yy
        self.mesh['xx_airfoil'] = x_center

        # imax, jmax equals to imaxi, jmaxi,
        # because the mesh point is already checked to be odd
        # for all sections (which was done in CKMESH in mesh_module.f90)
        tsf.common_data.imaxi = self.config['n_point_x']
        tsf.common_data.jmaxi = self.config['n_point_y']
        tsf.common_data.imax = self.config['n_point_x']
        tsf.common_data.jmax = self.config['n_point_y']
        
        tsf.common_data.x[:len(xx)] = xx.astype(np.float32)
        tsf.common_data.y[:len(yy)] = yy.astype(np.float32)
    
    def compute_mesh_indices(self):
        '''
        Compute mesh indices, including:
        1. ILE and ITE (leading and trailing edge)
        2. JLOW and JUP (lower and upper surface)
        '''
        # Find first point where X >= 0.0 (leading edge)
        ile = np.where(self.mesh['xx'] >= 0.0)[0][0] # 0-based index
        self.mesh['ile'] = ile # 0-based index
        tsf.common_data.ile = ile + 1 # 1-based index
        
        # Find first point where X > 1.0 (trailing edge)
        ite = np.where(self.mesh['xx'] > 1.0)[0][0] # 0-based index
        self.mesh['ite'] = ite - 1 # 0-based index
        tsf.common_data.ite = ite # 1-based index

        # Find first point where Y >= 0.0 (upper surface)
        j = np.where(self.mesh['yy'] >= 0.0)[0][0] # 0-based index

        self.mesh['jlow'] = j - 1 # 0-based index
        self.mesh['jup'] = j # 0-based index
        tsf.common_data.jlow = j
        tsf.common_data.jup = j + 1
        
        # Number of points on airfoil
        self.mesh['nfoil'] = self.mesh['ite'] - self.mesh['ile'] + 1
        tsf.common_data.nfoil = self.mesh['nfoil']

    @staticmethod
    def clustcos(n_points: int, a0=0.0079, a1=0.96, beta=1.0, index_point: int|None=None) -> np.ndarray:
        '''
        Point distribution on x-axis [0, 1]. (More points at both ends)

        Parameters
        ----------
        n_points: int
            total amount of points
            
        a0: float
            Parameter for distributing points near x=0.
            Smaller a0, more points near x=0.
            
        a1: float
            Parameter for distributing points near x=1.
            Larger a1, more points near x=1.
            
        beta: float
            Parameter for distribution points.
            
        index_point: int|None
            The index of the point to return.
            If None, return all points.
            
        Returns
        -------
        xx: np.ndarray|float
            The x-coordinates of the points.
            If index_point is not None, return the x-coordinate of the point at the given index.
        
        Examples
        ---------
        >>> xx = clustcos(n, a0, a1, beta)
        >>> xx = clustcos(n, a0, a1, beta, index_point=i)

        '''
        aa = np.power((1-np.cos(a0*np.pi))/2.0, beta)
        dd = np.power((1-np.cos(a1*np.pi))/2.0, beta) - aa
        
        if isinstance(index_point, int):
            yt = index_point/(n_points-1.0)
        else:
            yt = np.linspace(0.0, 1.0, num=n_points)
        
        a  = np.pi*(a0*(1-yt)+a1*yt)
        xx = (np.power((1-np.cos(a))/2.0,beta)-aa)/dd

        return xx

    def _cp_isentropic(self, ma: np.ndarray, minf: float) -> np.ndarray:
        '''
        Isentropic pressure coefficient from local Mach number and free-stream Mach number.

        More accurate than the linearized TSD formula for moderate-to-large perturbations.
        '''
        return calculate_cp_isentropic(ma, minf, gamma=self._gamma)

    def compute_geometry_derivatives(self):
        '''
        Compute airfoil geometry's derivatives (equivalent to BODY)
        
        This function translates the Fortran BODY subroutine to Python using scipy.
        It performs cubic spline interpolation on airfoil surfaces, computes volume,
        handles flap deflection, and computes camber and thickness distributions.
        '''
        # Get data from common_data (Fortran module variables)
        delta = self.airfoil['t_max']
        rigf = self.config['RIGF']
        
        # Airfoil geometry coordinates 
        xu = self.airfoil['xu']
        yu = self.airfoil['yu']
        xl = self.airfoil['xl']
        yl = self.airfoil['yl']
        
        # Mesh coordinates
        xfoil = self.mesh['xx_airfoil']
        nfoil = self.mesh['nfoil']
        
        # Flap parameters
        iflap = self.config['IFLAP']
        delflp = self.config['DELFLP']  
        flploc = self.config['FLPLOC']
        
        # Scaling factor
        delinv = 1.0 / delta

        # Upper surface cubic spline interpolation
        # Use not-a-knot BCs (scipy default) so the result is independent of
        # input point density.  Explicitly computed finite-difference slopes
        # diverge near the leading-edge singularity (dy/dx ~ 1/sqrt(x)) as the
        # input spacing shrinks with more points, producing n-dependent errors.
        cs_upper = CubicSpline(xu, yu)

        # Interpolate upper surface at mesh x-coordinates
        fu = cs_upper(xfoil) * delinv
        fxu = cs_upper(xfoil, 1) * delinv

        # Lower surface cubic spline interpolation
        cs_lower = CubicSpline(xl, yl)

        # Interpolate lower surface at mesh x-coordinates
        fl = cs_lower(xfoil) * delinv
        fxl = cs_lower(xfoil, 1) * delinv

        # At xfoil[0]=0 (leading edge) the spline analytic derivative diverges
        # for CST airfoils (dy/dx ~ 1/sqrt(x) as x→0).  The divergence rate
        # depends on the spacing of the first two input knots, making the result
        # strongly n-dependent.  The function values fu/fl at x=0 are always
        # correct (both surfaces meet at y=0), so use a forward finite-difference
        # from those converged function values instead.
        if xfoil[0] == 0.0:
            fxu[0] = (fu[1] - fu[0]) / (xfoil[1] - xfoil[0])
            fxl[0] = (fl[1] - fl[0]) / (xfoil[1] - xfoil[0])
        
        # Compute volume by Simpson's rule
        vol = integrate.simpson(y=fu-fl, x=xfoil)
        
        # Add flap deflection if any
        if iflap != 0:
            dflap = delflp / 57.29578  # Convert degrees to radians
            sdflap = np.sin(dflap)
            
            # Find flap hinge point
            ifp = 0
            for i in range(nfoil):
                if xfoil[i] >= flploc:
                    ifp = i
                    break
            
            # Apply flap deflection
            for i in range(ifp, nfoil):
                dely = (xfoil[i] - flploc) * sdflap * delinv
                fu[i] = fu[i] - dely
                fl[i] = fl[i] - dely
                fxu[i] = fxu[i] - dflap * delinv
                fxl[i] = fxl[i] - dflap * delinv
                
        # Apply rigidity factor correction to surface slopes
        fxu = fxu / np.sqrt(1.0 + rigf * (delta * fxu)**2)
        fxl = fxl / np.sqrt(1.0 + rigf * (delta * fxl)**2)

        # Store results in common_data arrays
        tsf.common_data.vol = vol
        
        tsf.common_data.fxu[:nfoil] = fxu.astype(np.float32)
        tsf.common_data.fxl[:nfoil] = fxl.astype(np.float32)
        
        # Print or log geometry (equivalent to PRBODY call)
        if self.config['flag_print_info']:
            print(f"Airfoil geometry computed successfully:")
            print(f"  Number of points: {nfoil}")
            print(f"  Volume: {vol:.6f}")
            print(f"  Max thickness: {delta:.6f}")
            if iflap != 0:
                print(f"  Flap deflection: {delflp:.2f} degrees at x={flploc:.3f}")
    
    def compute_scale(self) -> None:
        '''Scale physical variables to transonic similarity variables (CPFACT, CLFACT, CDFACT, CMFACT, YFACT, VFACT).'''
        emach = float(tsf.common_data.emach)
        delta = float(tsf.common_data.delta)
        simdef = self.config['SIMDEF']

        emach2 = emach * emach
        beta = 1.0 - emach2
        delrt1 = delta ** (1.0 / 3.0)
        delrt2 = delta ** (2.0 / 3.0)

        if simdef == 1:  # Cole scaling
            ak = beta / delrt2
            yfact = 1.0 / delrt1
            cpfact = delrt2
            clfact = delrt2
            cdfact = delrt2 * delta
            cmfact = delrt2
            vfact = delta * 57.295779
        elif simdef == 2:  # Spreiter scaling
            emroot = emach ** (2.0 / 3.0)
            ak = beta / (delrt2 * emroot * emroot)
            yfact = 1.0 / (delrt1 * emroot)
            cpfact = delrt2 / emroot
            clfact = cpfact
            cmfact = cpfact
            cdfact = cpfact * delta
            vfact = delta * 57.295779
        elif simdef == 3:  # Krupp scaling
            ak = beta / (delrt2 * emach)
            yfact = 1.0 / (delrt1 * emach ** 0.5)
            cpfact = delrt2 / (emach ** 0.75)
            clfact = cpfact
            cmfact = cpfact
            cdfact = cpfact * delta
            vfact = delta * 57.295779
        else:
            raise ValueError(f'SCALE: Invalid SIMDEF value: {simdef}')

        tsf.common_data.ak = np.float32(ak)

        tsf.common_data.alpha = np.float32(float(tsf.common_data.alpha) / vfact)

        # CLFACT and CMFACT stay in Fortran (used by main_iteration.SOLVE)
        tsf.solver_data.clfact = np.float32(clfact)
        tsf.solver_data.cmfact = np.float32(cmfact)

        # Store scaling factors as Python instance variables
        self._cpfact = float(cpfact)
        self._cdfact = float(cdfact)
        self._yfact = float(yfact)
        self._vfact = float(vfact)

        if abs(self._gam1) <= 0.0001:
            tsf.solver_data.sonvel = np.float32(1.0)
            self._cpstar = 0.0
        else:
            sonvel = ak / self._gam1
            tsf.solver_data.sonvel = np.float32(sonvel)
            self._cpstar = float(self._cp_isentropic(1.0, emach))

    def compute_far_field_bc(self) -> None:
        '''
        Compute far-field boundary conditions for outer boundaries (equivalent to FARFLD).

        Subsonic asymptotic forms for doublet and vortex located at X=0.5, Y=0.
        Boundary values are later multiplied by vortex/doublet strengths in RECIRC/REDUB.
        For supersonic freestream (ak <= 0), no far-field BC needed — returns immediately.
        '''
        ak = float(tsf.common_data.ak)
        if ak <= 0.0:
            return

        imin = int(tsf.common_data.imin)
        imax = int(tsf.common_data.imax)
        jmin = int(tsf.common_data.jmin)
        jmax = int(tsf.common_data.jmax)
        x_coords = tsf.common_data.x
        y_coords = tsf.common_data.y
        xsing = 0.5   # XSING parameter in solver_data

        rtk = np.sqrt(ak)
        twopi = 2.0 * np.pi
        coef1 = 1.0 / twopi
        coef2 = 1.0 / (twopi * rtk)

        yt = float(y_coords[jmax - 1]) * rtk
        yb = float(y_coords[jmin - 1]) * rtk
        xu_bc = float(x_coords[imin - 1]) - xsing
        xd_bc = float(x_coords[imax - 1]) - xsing

        # Top and bottom boundaries (vectorised over i = imin..imax, 0-based slice [imin-1:imax])
        xp = x_coords[imin - 1:imax].astype(np.float64) - xsing
        tsf.solver_data.dtop[imin - 1:imax] = (xp / (xp**2 + yt**2) * coef2).astype(np.float32)
        tsf.solver_data.dbot[imin - 1:imax] = (xp / (xp**2 + yb**2) * coef2).astype(np.float32)
        tsf.solver_data.vtop[imin - 1:imax] = (-np.arctan2(yt, xp) * coef1).astype(np.float32)
        tsf.solver_data.vbot[imin - 1:imax] = (-(np.arctan2(yb, xp) + twopi) * coef1).astype(np.float32)

        # Upstream and downstream boundaries (vectorised over j = jmin..jmax, 0-based slice [jmin-1:jmax])
        yj = y_coords[jmin - 1:jmax].astype(np.float64) * rtk
        q = np.pi - np.copysign(np.pi, yj)   # 0 if yj>=0, 2*pi if yj<0
        tsf.solver_data.dup[jmin - 1:jmax] = (xu_bc / (xu_bc**2 + yj**2) * coef2).astype(np.float32)
        tsf.solver_data.ddown[jmin - 1:jmax] = (xd_bc / (xd_bc**2 + yj**2) * coef2).astype(np.float32)
        tsf.solver_data.vup[jmin - 1:jmax] = (-(np.arctan2(yj, xu_bc) + q) * coef1).astype(np.float32)
        tsf.solver_data.vdown[jmin - 1:jmax] = (-(np.arctan2(yj, xd_bc) + q) * coef1).astype(np.float32)

        # Compute THETA(J,I) = angle at each mesh point (replaces Fortran ANGLE subroutine)
        yj_raw = y_coords[jmin - 1:jmax].astype(np.float64)
        yj_scaled = (yj_raw * rtk)[:, np.newaxis]    # (nj, 1)
        xp_2d = xp[np.newaxis, :]                     # (1, ni)
        r = np.sqrt(yj_raw[:, np.newaxis]**2 + xp_2d**2)
        atn = np.arctan2(yj_scaled, xp_2d)
        q_theta = np.pi - np.copysign(np.pi, yj_scaled)
        theta_2d = -(atn + q_theta) * coef1
        theta_2d = np.where(r <= 1.0, theta_2d * r, theta_2d)
        tsf.solver_data.theta[jmin - 1:jmax, imin - 1:imax] = theta_2d.astype(np.float32)

    def compute_data_summary(self):
        '''
        Compute the data summary.
        '''
        alpha = tsf.common_data.alpha
        clfact = tsf.solver_data.clfact
        cmfact = tsf.solver_data.cmfact

        # Compute lift and pitch coefficients
        self.data_summary['alpha'] = alpha * self._vfact
        self.data_summary['mach'] = tsf.common_data.emach
        self.data_summary['cl'] = tsf.solver_base.lift(clfact)
        self.data_summary['cm'] = tsf.solver_base.pitch(cmfact)
        self.data_summary['cpstar'] = self._cpstar
            
    def output_field(self) -> None:
        '''
        Output the field to a file in Tecplot format.
        Translates OUTPUT_FIELD from io_module.f90
        '''
        # Get mesh dimensions and coordinates
        imin = tsf.common_data.imin
        imax = tsf.common_data.imax
        jmin = tsf.common_data.jmin
        jmax = tsf.common_data.jmax
        iup = tsf.common_data.iup
        idown = tsf.common_data.idown
        
        x_coords = tsf.common_data.x
        y_coords = tsf.common_data.y
        
        # Get solver data
        P = tsf.solver_data.p  # Pressure array
        c1 = tsf.solver_data.c1
        cxl = tsf.solver_data.cxl
        cxc = tsf.solver_data.cxc
        cxr = tsf.solver_data.cxr
        cpfact = self._cpfact
        vfact = self._vfact
        
        # Get configuration parameters
        emach = tsf.common_data.emach
        alpha = tsf.common_data.alpha
        delta = tsf.common_data.delta
                        
        # Open output file
        if self.config['flag_output_field']:
            
            field_data = np.zeros((jmax - jmin + 1, imax - imin + 1, 6))
            
            with open(os.path.join(self.output_dir, "field.dat"), 'w') as f:
                # Write Tecplot header
                f.write('# Flow types: -1=Outside domain, 0=Elliptic, 1=Parabolic, 2=Hyperbolic, 3=Shock\n')
                f.write(f'# Mach = {emach:10.6f}\n')
                f.write(f'# Alpha = {alpha * vfact:10.6f}\n')
                f.write(f'# CPFACT = {cpfact:10.6f}\n')
                
                f.write('VARIABLES = "X", "Y", "Mach", "Cp", "P", "FlowType"\n')
                f.write(f'ZONE I= {imax - imin + 1:5d} J= {jmax - jmin + 1:5d} F= POINT\n')
                
            # Initialize VT array for flow type calculation  
            # Size it to accommodate all possible j indices
            vt = np.zeros((jmax + 1, 2))  # VT(J,1) and VT(J,2)
            for j in range(jmin, jmax + 1):
                vt[j, 0] = c1[1]  # VT(J,1) = C1(2) in Fortran (1-based) -> C1[1] in Python (0-based)
            
            # Write field data in point format
            for j in range(jmin, jmax + 1):
                for i in range(imin, imax + 1):
                    # Calculate flow variables
                    u = tsf.solver_base.px(i, j) # Computes U = DP/DX at point I,J
                    em = tsf.solver_functions.emach1(u, delta) # Computes Mach number from U
                    cp_val = self._cp_isentropic(em, emach) # Computes Cp from local Mach number using isentropic relation
                    
                    # Calculate flow type for points within the computational domain
                    if imin <= i <= imax and jmin <= j <= jmax:
                        # Flow type classification using PRTMC logic
                        vt[j, 1] = vt[j, 0]  # VT(J,2) = VT(J,1)
                        # Convert to 0-based indexing for accessing arrays
                        i_py = i - 1
                        j_py = j - 1
                        
                        if i >= iup and i <= idown:
                            # VT(J,1) = C1(I) - (CXL(I)*P(J,I-1) + CXC(I)*P(J,I) + CXR(I)*P(J,I+1))
                            vt[j, 0] = (c1[i_py] - (cxl[i_py] * P[j_py, i_py - 1] + 
                                                cxc[i_py] * P[j_py, i_py] + 
                                                cxr[i_py] * P[j_py, i_py + 1]))
                            
                            if vt[j, 0] > 0.0:
                                if vt[j, 1] < 0.0:
                                    # Shock point
                                    flow_type_num = 3.0
                                else:
                                    # Elliptic point (subsonic)
                                    flow_type_num = 0.0
                            else:
                                if vt[j, 1] < 0.0:
                                    # Hyperbolic point (supersonic)
                                    flow_type_num = 2.0
                                else:
                                    # Parabolic point (sonic)
                                    flow_type_num = 1.0
                        else:
                            # Outside computational domain
                            flow_type_num = -1.0
                    else:
                        # Outside computational domain
                        flow_type_num = -1.0
                    
                    # Convert to 0-based indexing for accessing coordinates
                    i_py = i - 1
                    j_py = j - 1
                    p_val = P[j_py, i_py]
                    
                    # Store data in field_data
                    field_data[j_py, i_py, 0] = x_coords[i_py]
                    field_data[j_py, i_py, 1] = y_coords[j_py]
                    field_data[j_py, i_py, 2] = em
                    field_data[j_py, i_py, 3] = cp_val
                    field_data[j_py, i_py, 4] = p_val
                    field_data[j_py, i_py, 5] = flow_type_num
                    
                    # Write data to file
                    with open(os.path.join(self.output_dir, "field.dat"), 'a') as f:
                        f.write(f' {x_coords[i_py]:15.12f}')
                        f.write(f' {y_coords[j_py]:15.12f}')
                        f.write(f' {em:15.12f}')
                        f.write(f' {cp_val:15.12f}')
                        f.write(f' {p_val:15.12f}')
                        f.write(f' {flow_type_num:15.12f}\n')
                        
            self.data_summary['field_data'] = field_data
            
            if self.config['flag_print_info']:
                print('Output to field.dat: Cp, Mach, Potential field data')
    
    def output_surface(self) -> None:
        '''
        Computes pressure coefficients and Mach numbers along the airfoil surface.
        Translated from the Fortran PRINT_SHOCK subroutine.
        
        Parameters
        ----------
        file_name : str, optional
            Output filename. Defaults to "cpxs.dat".
        '''        
        # Get required parameters from Fortran modules
        imin = tsf.common_data.imin
        imax = tsf.common_data.imax
        ile = tsf.common_data.ile
        ite = tsf.common_data.ite
        jlow = tsf.common_data.jlow
        jup = tsf.common_data.jup
        
        # Get required data from solver_data
        cpfact = self._cpfact
        cpstar = self._cpstar
        cjlow = tsf.solver_data.cjlow
        cjlow1 = tsf.solver_data.cjlow1
        cjup = tsf.solver_data.cjup
        cjup1 = tsf.solver_data.cjup1
        
        # Get coordinate arrays
        x_coords = tsf.common_data.x
        y_coords = tsf.common_data.y
        
        # Get configuration parameters
        delta = tsf.common_data.delta
        emach = tsf.common_data.emach

        # Initialize variables
        iem = 0
        cj01 = -y_coords[jlow-1] / (y_coords[jup-1] - y_coords[jlow-1])  # Convert to 0-based indexing
        cj02 = y_coords[jup-1] / (y_coords[jup-1] - y_coords[jlow-1])
        
        # Arrays to store results
        n_points = imax - imin + 1
        em1l = np.zeros(n_points)
        em1u = np.zeros(n_points)
        ul_arr = np.zeros(n_points)
        uu_arr = np.zeros(n_points)
        vl_arr = np.zeros(n_points)
        vu_arr = np.zeros(n_points)

        # Main computation loop
        for i_p1 in range(imin, imax + 1):  # Fortran 1-based indexing
            # Convert to 0-based indexing for Python arrays
            i_py = i_p1 - 1

            # Calculate UL_P1
            ul_p1 = cjlow * tsf.solver_base.px(i_p1, jlow) - cjlow1 * tsf.solver_base.px(i_p1, jlow - 1)
            if i_p1 > ite:
                ul_p1 = cj01 * tsf.solver_base.px(i_p1, jup) + cj02 * tsf.solver_base.px(i_p1, jlow)
            if i_p1 < ile:
                ul_p1 = cj01 * tsf.solver_base.px(i_p1, jup) + cj02 * tsf.solver_base.px(i_p1, jlow)

            # Compute Mach number
            em1l[i_py] = tsf.solver_functions.emach1(ul_p1, delta)
            if em1l[i_py] > 1.3:
                iem = 1

            # Calculate UU_P1
            uu_p1 = cjup * tsf.solver_base.px(i_p1, jup) - cjup1 * tsf.solver_base.px(i_p1, jup + 1)
            if i_p1 > ite:
                uu_p1 = ul_p1
            if i_p1 < ile:
                uu_p1 = ul_p1

            # Compute Mach number
            em1u[i_py] = tsf.solver_functions.emach1(uu_p1, delta)
            if em1u[i_py] > 1.3:
                iem = 1

            ul_arr[i_py] = ul_p1
            uu_arr[i_py] = uu_p1
            vl_arr[i_py] = tsf.solver_base.py(i_p1, jlow)
            vu_arr[i_py] = tsf.solver_base.py(i_p1, jup)

        # Singularity subtraction step E: restore phi_s,x so ul/uu = phi_tot,x
        if (self.config.get('apply_singularity_subtraction', False)
                and self._phi_sx_surface is not None):
            ile_0b = ile - 1   # 0-based
            ite_0b = ite - 1   # 0-based (inclusive)
            phi_sx = self._phi_sx_surface
            ul_arr[ile_0b:ite_0b + 1] += phi_sx
            uu_arr[ile_0b:ite_0b + 1] += phi_sx
            # Recompute Mach numbers for airfoil region with restored total velocity
            for k in range(ile_0b, ite_0b + 1):
                em1l[k] = tsf.solver_functions.emach1(float(ul_arr[k]), delta)
                em1u[k] = tsf.solver_functions.emach1(float(uu_arr[k]), delta)

        self.data_summary['cpu'] = self._cp_isentropic(em1u, emach)
        self.data_summary['cpl'] = self._cp_isentropic(em1l, emach)
        self.data_summary['mau'] = em1u
        self.data_summary['mal'] = em1l
        self.data_summary['uu'] = uu_arr
        self.data_summary['ul'] = ul_arr
        self.data_summary['vu'] = vu_arr
        self.data_summary['vl'] = vl_arr

        # MAE leading-edge composite correction (step 4)
        if self.config.get('apply_le_correction', False):
            self._apply_le_correction()

        # Output summary file
        if self.config['flag_output_summary']:
            with open(os.path.join(self.output_dir, "smry.out"), 'a') as f:

                # Check for detached shock
                cpl = self.data_summary['cpl']
                if cpl[imin-1] < cpstar and cpl[imin] > cpstar:
                    f.write('0 ***** CAUTION *****\n')
                    f.write(' DETACHED SHOCK WAVE UPSTREAM OF X-MESH,SOLUTION TERMINATED.\n')
                    return
                
                # Mach number warning
                if iem == 1:
                    f.write('0 ***** CAUTION *****\n')
                    f.write(' MAXIMUM MACH NUMBER EXCEEDS 1.3\n')
                    f.write(' SHOCK JUMPS IN ERROR IF UPSTREAM NORMAL MACH NUMBER GREATER THAN 1.3\n')
        
        # Output airfoil surface data to file
        if self.config['flag_output_shock']:
            
            with open(os.path.join(self.output_dir, "cpxs.dat"), 'w') as f:
                
                # Write coefficients
                f.write(f'# Output to cpxs.dat: Cp, Mach distribution on a x-line (Y=0) \n')
                f.write(f'# Mach = {emach:10.6f}\n')
                f.write(f'# Alpha = {self.data_summary["alpha"]:10.6f}\n')
                f.write(f'# CL = {self.data_summary["cl"]:10.6f}\n')
                f.write(f'# CM = {self.data_summary["cm"]:10.6f}\n')
                f.write(f'# Cp* = {self.data_summary["cpstar"]:10.6f}\n')
                
                # Write variable names
                f.write('VARIABLES = "X", "Cp-up", "M-up", "Cp-low", "M-low"\n')
                
                # Write data with Fortran-style formatting
                for i_p1 in range(imin, imax + 1):
                    i_py = i_p1 - 1
                    f.write(f"  {self.mesh['xx'][i_py]:10.5f}")
                    f.write(f"  {self.data_summary['cpu'][i_py]:10.5f}")
                    f.write(f"  {self.data_summary['mau'][i_py]:10.5f}")
                    f.write(f"  {self.data_summary['cpl'][i_py]:10.5f}")
                    f.write(f"  {self.data_summary['mal'][i_py]:10.5f}\n")
        
            if self.config['flag_print_info']:
                print('Output to cpxs.dat: Cp, Mach distribution on a x-line (Y=0)')
    
    def _adaptive_blend_range(self, active: bool=True) -> Dict[str, Any]:
        '''
        Adaptive blending range for leading-edge correction.
        The blending range is determined based on angle of attack and upper/lower surface.
        '''
        xpi = 0.00
        xb0 = 0.005
        xb1 = 0.10
        result = {
            'x_pure_inner_upper': xpi,
            'x_blend_range_upper': (xb0, xb1),
            'x_pure_inner_lower': xpi,
            'x_blend_range_lower': (xb0, xb1)
        }
        
        if active:
            alpha_rad = np.radians(self.data_summary['alpha'])
            xb0_u = max(xpi, xpi - alpha_rad * 1)
            xb1_u = max(xb0, xb0 - alpha_rad * 2)
            xb0_l = max(xpi, xpi + alpha_rad * 1)
            xb1_l = max(xb0, xb0 + alpha_rad * 4)
            
            result['x_blend_range_upper'] = (xb0_u, xb1_u)
            result['x_blend_range_lower'] = (xb0_l, xb1_l)

        
        return result
    
    def _apply_le_correction(self) -> None:
        """
        Apply MAE composite correction (post-processing, non-invasive).

        Replaces cpu/cpl/mau/mal in data_summary with composite values.
        Also stores corrected CL, CM in data_summary under 'cl_le', 'cm_le'.
        """
        if solve_inner_problem is None or apply_composite_correction is None:
            print("WARNING: leading_edge module not available; skipping LE correction.")
            return

        R_c   = self.airfoil.get('R_c', None)
        h     = self.airfoil.get('h_nose', None)
        if R_c is None or h is None or R_c <= 0:
            print("WARNING: Nose geometry not fitted; skipping LE correction.")
            return

        emach  = float(tsf.common_data.emach)

        # Resolve cache directory relative to the package location
        cache_dir = os.path.join(os.path.dirname(__file__),
                                 'leading_edge', 'inner_tables_cache')

        # Solve / load inner problem (cached per geometry)
        inner_tables = solve_inner_problem(
            h=h, c=1.0, gamma=self._gamma,
            cache_dir=cache_dir
        )

        # Mesh x-coordinates
        xx = self.mesh['xx']

        # Airfoil index range (1-based ile/ite, 0-based in Python)
        ile = int(tsf.common_data.ile) - 1   # 0-based
        ite = int(tsf.common_data.ite) - 1   # 0-based (inclusive)

        # Extract airfoil-region x-coordinates and raw perturbation velocities
        x_foil = xx[ile:ite + 1]
        cpu_foil = self.data_summary['cpu'][ile:ite + 1]
        cpl_foil = self.data_summary['cpl'][ile:ite + 1]

        # Apply composite correction to upper and lower surfaces
        result = self._adaptive_blend_range()
        cpu_new, mau_new, _ = apply_composite_correction(
            x_foil, cpu_foil, emach, R_c, inner_tables,
            x_blend_range=result['x_blend_range_upper'],
            gamma=self._gamma)
        cpl_new, mal_new, _ = apply_composite_correction(
            x_foil, cpl_foil, emach, R_c, inner_tables,
            x_blend_range=result['x_blend_range_lower'],
            gamma=self._gamma)

        # Write corrected values back into the full arrays
        cpu_full = self.data_summary['cpu'].copy()
        cpl_full = self.data_summary['cpl'].copy()
        mau_full = self.data_summary['mau'].copy()
        mal_full = self.data_summary['mal'].copy()

        cpu_full[ile:ite + 1] = cpu_new
        cpl_full[ile:ite + 1] = cpl_new
        mau_full[ile:ite + 1] = mau_new
        mal_full[ile:ite + 1] = mal_new

        self.data_summary['cpu'] = cpu_full
        self.data_summary['cpl'] = cpl_full
        self.data_summary['mau'] = mau_full
        self.data_summary['mal'] = mal_full

        # Step 5: corrected CL and CM by re-integration
        _trapz = np.trapezoid if hasattr(np, 'trapezoid') else np.trapz
        cl_le = float(_trapz(cpl_new - cpu_new, x_foil))
        cm_le = float(_trapz((cpl_new - cpu_new) * (0.25 - x_foil), x_foil))
        self.data_summary['cl_le'] = cl_le
        self.data_summary['cm_le'] = cm_le

        if self.config.get('flag_print_info', False):
            print(f"  LE correction applied: CL_le={cl_le:.5f}, CM_le={cm_le:.5f}")

    def cdcole_python(self, sonvel: float, yfact: float, delta: float) -> None:
        """
        Compute drag coefficient by momentum integral method.
        Integrates around a contour enclosing the body and along all shocks inside the contour.
        
        This is a Python translation of the CDCOLE subroutine from solver_base.f90.
        
        Parameters:
        -----------
        sonvel : float
            Speed of sound
        yfact : float  
            Scaling factor for Y-coordinate
        delta : float
            Maximum thickness of airfoil
        """
        # Get required variables from Fortran modules
        x_coords = tsf.common_data.x
        y_coords = tsf.common_data.y
        imin = tsf.common_data.imin
        imax = tsf.common_data.imax
        iup = tsf.common_data.iup
        ile = tsf.common_data.ile
        ite = tsf.common_data.ite
        n_mesh_points = tsf.common_data.n_mesh_points
        jmin = tsf.common_data.jmin
        jmax = tsf.common_data.jmax
        jup = tsf.common_data.jup
        jlow = tsf.common_data.jlow
        ak = tsf.common_data.ak
        fxl = tsf.common_data.fxl
        fxu = tsf.common_data.fxu
        
        # Get solver data
        cjup = tsf.solver_data.cjup
        cjup1 = tsf.solver_data.cjup1
        cjlow = tsf.solver_data.cjlow
        cjlow1 = tsf.solver_data.cjlow1
        cdfact = self._cdfact
        
        # Helper functions
        def trap_integration(xi_arr, arg_arr, n_points):
            """Trapezoidal integration (Python implementation of TRAP)"""
            sum_val = 0.0
            for i in range(n_points - 1):
                z = xi_arr[i + 1] - xi_arr[i]
                w = arg_arr[i + 1] + arg_arr[i]
                sum_val += z * w
            return 0.5 * sum_val
                
        def findsk(istart, iend, j_line, son_vel):
            """Find shock location on line J between ISTART and IEND"""
            isk = istart - 1
            u2 = tsf.solver_base.px(isk, j_line)
            
            while True:
                isk += 1
                u1 = u2
                u2 = tsf.solver_base.px(isk, j_line)
                if u1 > son_vel and u2 <= son_vel:
                    break
                if isk >= iend:
                    isk = -iend
                    break
            return isk
        
        def newisk(iskold, j_line, son_vel):
            """Find new location of shockwave given an initial guess"""
            i2 = iskold + 2
            isknew = iskold - 3
            u2 = tsf.solver_base.px(isknew, j_line)
            
            while True:
                isknew += 1
                u1 = u2
                u2 = tsf.solver_base.px(isknew, j_line)
                if u1 > son_vel and u2 <= son_vel:
                    break
                if isknew >= i2:
                    isknew = -isknew
                    break
            return isknew
        
        def drag_function(cdfact_in):
            """Compute drag coefficient by surface pressure integral (Python implementation of DRAG)"""
            xi = np.zeros(n_mesh_points)
            arg = np.zeros(n_mesh_points)
            
            k = 0
            arg[0] = 0.0
            xi[0] = x_coords[ile - 2]  # Convert to 0-based indexing
            
            for i in range(ile, ite + 1):
                k += 1
                pxup = cjup * tsf.solver_base.px(i, jup) - cjup1 * tsf.solver_base.px(i, jup + 1)
                pxlow = cjlow * tsf.solver_base.px(i, jlow) - cjlow1 * tsf.solver_base.px(i, jlow - 1)
                arg[k] = fxu[k - 1] * pxup - fxl[k - 1] * pxlow
                xi[k] = x_coords[i - 1]  # Convert to 0-based indexing
            
            k += 1
            arg[k] = 0.0
            xi[k] = x_coords[ite]  # Convert to 0-based indexing
            
            sum_val = trap_integration(xi, arg, k + 1)
            return -sum_val * cdfact_in * 2.0
        
        def prtsk(xi_arr, arg_arr, l_points, nshock, cdsk, lprt1):
            """Print shock wave information (Python implementation of PRTSK)"""
            if not self.config['flag_output_summary']:
                return
                
            cdycof = -cdfact * self._gam1 / (6.0 * yfact)
            poycof = delta**2 * self._gam1 * self._gamma / 12.0
                
            with open(os.path.join(self.output_dir, "smry.out"), 'a') as f:

                # Write header for first shock wave only
                if nshock == 1:
                    f.write('0\n')
                    f.write(' INVISCID WAKE PROFILES FOR INDIVIDUAL SHOCK WAVES WITHIN MOMENTUM CONTOUR\n')
                
                # Write shock information
                f.write('\n')  # blank line
                f.write(f'SHOCK{nshock:3d}\n')
                f.write(f' WAVE DRAG FOR THIS SHOCK={cdsk:12.6f}\n')
                f.write(f'      Y         CD(Y)        PO/POINF\n')
                
                # Write shock profile data
                for k in range(l_points):
                    yy = xi_arr[k] * yfact
                    cdy = cdycof * arg_arr[k]
                    poy = 1.0 + poycof * arg_arr[k]
                    f.write(f' {yy:12.8f}{cdy:12.8f}{poy:12.8f}\n')
                
                # Write footer if shock extends outside contour
                if lprt1 == 1:
                    f.write('\n')  # blank line
                    f.write(' SHOCK WAVE EXTENDS OUTSIDE CONTOUR\n')
                    f.write(' PRINTOUT OF SHOCK LOSSES ARE NOT AVAILABLE FOR REST OF SHOCK\n')
        
        # Main computation starts here
        gam123 = self._gam1 * 2.0 / 3.0
        iskold = 0
        
        # Set locations of contour boundaries
        
        # Upstream boundary
        if ak > 0.0:
            iu = (ile + imin) // 2
        else:
            iu = iup
        
        # Top and bottom boundaries
        # Subsonic freestream
        jt = jmax - 1
        jb = jmin + 1
        
        if ak <= 0.0:
            # Supersonic freestream
            # Set JB,JT to include only subsonic part of detached bow wave
            
            # Find bow shock wave
            istop = ile - 3
            ibow = findsk(iup, istop, jup, sonvel)
            
            if ibow < 0:
                
                # Shock is too close to body to do contour integral
                ule = tsf.solver_base.px(ile, jup)
                cd = drag_function(cdfact)
                
                if self.config['flag_output_summary']:
                    with open(os.path.join(self.output_dir, "smry.out"), 'a') as f:
                        if ule > sonvel:
                            f.write('1 SHOCK WAVE IS ATTACHED TO BODY\n')
                            f.write('  MOMENTUM INTEGRAL CANNOT BE DONE\n')
                            f.write('  DRAG OBTAINED FROM SURFACE PRESSURE INTEGRAL\n')
                        else:
                            f.write('1 DETACHED SHOCK WAVE IS TOO CLOSE TO BODY\n')
                            f.write('  MOMENTUM INTEGRAL CANNOT BE DONE\n')
                            f.write('  DRAG OBTAINED FROM SURFACE PRESSURE INTEGRAL\n')
                        f.write(f'0 CD={cd:12.6f}\n')
                        
                return
            
            # Search up shock to find tip of subsonic region
            isk = ibow
            jstart = jup + 1
            jt = jup - 1
            for j in range(jstart, jmax + 1):
                jt += 1
                iskold = isk
                isk = newisk(iskold, j, sonvel)
                if isk < 0:
                    break
            
            # Search down shock to find tip of subsonic region
            isk = ibow
            jb = jlow + 2
            for j in range(jmin, jlow + 1):
                jj = jlow - j + jmin
                jb -= 1
                iskold = isk
                isk = newisk(iskold, jj, sonvel)
                if isk < 0:
                    break
            
            # Save I location of bow shock wave on lower boundary
            ibow = iskold
        
        # Downstream boundary
        id_downstream = (ite + imax) // 2
        if tsf.solver_base.px(ite + 1, jup) >= sonvel:
            # Trailing edge is supersonic. Place downstream
            # boundary ahead of trailing edge to avoid tail shock
            i = ite
            while x_coords[i - 1] > 0.75:  # Convert to 0-based indexing
                i -= 1
            id_downstream = i
        
        # All boundaries are fixed
        # Compute integrals along boundaries
        
        # Integral on upstream boundary
        cdup = 0.0
        if ak >= 0.0:
            xi = np.zeros(n_mesh_points)
            arg = np.zeros(n_mesh_points)
            l = 0
            for j in range(jb, jt + 1):
                xi[l] = y_coords[j - 1]  # Convert to 0-based indexing
                u = tsf.solver_base.px(iu, j)
                v = tsf.solver_base.py(iu, j)
                arg[l] = ((ak - gam123 * u) * u * u - v * v) * 0.5
                l += 1
            sum_val = trap_integration(xi, arg, l)
            cdup = 2.0 * cdfact * sum_val
        
        # Integral on top boundary
        xi = np.zeros(n_mesh_points)
        arg = np.zeros(n_mesh_points)
        l = 0
        for i in range(iu, id_downstream + 1):
            xi[l] = x_coords[i - 1]  # Convert to 0-based indexing
            arg[l] = -tsf.solver_base.px(i, jt) * tsf.solver_base.py(i, jt)
            l += 1
        sum_val = trap_integration(xi, arg, l)
        cdtop = 2.0 * cdfact * sum_val
        
        # Integral on bottom boundary
        xi = np.zeros(n_mesh_points)
        arg = np.zeros(n_mesh_points)
        l = 0
        for i in range(iu, id_downstream + 1):
            arg[l] = tsf.solver_base.px(i, jb) * tsf.solver_base.py(i, jb)
            l += 1
        sum_val = trap_integration(xi, arg, l)
        cdbot = 2.0 * cdfact * sum_val
        
        # Integral on downstream boundary
        xi = np.zeros(n_mesh_points)
        arg = np.zeros(n_mesh_points)
        l = 0
        for j in range(jb, jt + 1):
            xi[l] = y_coords[j - 1]  # Convert to 0-based indexing
            u = tsf.solver_base.px(id_downstream, j)
            # If flow supersonic, use backward difference formula
            if u > sonvel:
                u = tsf.solver_base.px(id_downstream - 1, j)
            v = tsf.solver_base.py(id_downstream, j)
            arg[l] = ((gam123 * u - ak) * u * u + v * v) * 0.5
            l += 1
        
        sum_val = trap_integration(xi, arg, l)
        cddown = 2.0 * cdfact * sum_val
        
        # Integral on body boundary
        cdbody = 0.0
        if id_downstream <= ite:
            ilim = ite + 1
            xi = np.zeros(n_mesh_points)
            arg = np.zeros(n_mesh_points)
            l = 0
            for i in range(id_downstream, ilim + 1):
                ib = i - ile + 1
                xi[l] = x_coords[i - 1]  # Convert to 0-based indexing
                uu = cjup * tsf.solver_base.px(i, jup) - cjup1 * tsf.solver_base.px(i, jup + 1)
                ul = cjlow * tsf.solver_base.px(i, jlow) - cjlow1 * tsf.solver_base.px(i, jlow - 1)
                arg[l] = -uu * fxu[ib - 1] + ul * fxl[ib - 1]  # Convert to 0-based indexing
                l += 1
            sum_val = trap_integration(xi, arg, l)
            cdbody = 2.0 * cdfact * sum_val
        
        # Integration along shock waves
        cdwave = 0.0
        lprt1 = 0
        lprt2 = 0
        nshock = 0
        
        if ak <= 0.0:
            # Integrate along detached bow wave
            nshock += 1
            lprt1 = 1
            lprt2 = 1
            xi = np.zeros(n_mesh_points)
            arg = np.zeros(n_mesh_points)
            l = 0
            isk = ibow
            for j in range(jb, jt + 1):
                iskold = isk
                isk = newisk(iskold, j, sonvel)
                xi[l] = y_coords[j - 1]  # Convert to 0-based indexing
                arg[l] = (tsf.solver_base.px(isk + 1, j) - tsf.solver_base.px(isk - 2, j))**3
                l += 1
            sum_val = trap_integration(xi, arg, l)
            cdsk = -self._gam1 / 6.0 * cdfact * sum_val
            cdwave += cdsk
            prtsk(xi, arg, l, nshock, cdsk, lprt1)
        
        # Integrate along shocks above airfoil
        istart = ile
        
        # Loop to find and process all shocks above airfoil
        while True:
            isk = findsk(istart, ite, jup, sonvel)
            if isk < 0:
                break  # No more shocks found
            
            # Shock wave found
            istart = isk + 1
            nshock += 1
            lprt1 = 0
            xi = np.zeros(n_mesh_points)
            arg = np.zeros(n_mesh_points)
            l = 1
            xi[0] = 0.0
            arg[0] = (cjup * (tsf.solver_base.px(isk + 1, jup) - tsf.solver_base.px(isk - 2, jup)) -
                     cjup1 * (tsf.solver_base.px(isk + 1, jup + 1) - tsf.solver_base.px(isk - 2, jup + 1)))**3
            
            for j in range(jup, jt + 1):
                xi[l] = y_coords[j - 1]  # Convert to 0-based indexing
                arg[l] = (tsf.solver_base.px(isk + 1, j) - tsf.solver_base.px(isk - 2, j))**3
                iskold = isk
                jsk = j + 1
                isk = newisk(iskold, jsk, sonvel)
                if isk < 0:
                    break
                if isk > id_downstream:
                    lprt1 = 1
                    break
                l += 1
            
            if isk < 0:
                lprt1 = 1
            
            sum_val = trap_integration(xi, arg, l)
            cdsk = -self._gam1 / 6.0 * cdfact * sum_val
            cdwave += cdsk
            prtsk(xi, arg, l, nshock, cdsk, lprt1)
            if lprt1 == 1:
                lprt2 = 1
        
        # Integrate along shocks below airfoil
        istart = ile
        
        # Loop to find and process all shocks below airfoil
        while True:
            isk = findsk(istart, ite, jlow, sonvel)
            if isk < 0:
                break  # No more shocks found
            
            # Shock wave found
            istart = isk + 1
            nshock += 1
            lprt1 = 0
            xi = np.zeros(n_mesh_points)
            arg = np.zeros(n_mesh_points)
            l = 1
            xi[0] = 0.0
            arg[0] = (cjlow * (tsf.solver_base.px(isk + 1, jlow) - tsf.solver_base.px(isk - 2, jlow)) -
                     cjlow1 * (tsf.solver_base.px(isk + 1, jlow - 1) - tsf.solver_base.px(isk - 2, jlow - 1)))**3
            
            for jj in range(jb, jlow + 1):
                j = jlow + jb - jj
                xi[l] = y_coords[j - 1]  # Convert to 0-based indexing
                arg[l] = (tsf.solver_base.px(isk + 1, j) - tsf.solver_base.px(isk - 2, j))**3
                iskold = isk
                jsk = j - 1
                isk = newisk(iskold, jsk, sonvel)
                if isk < 0:
                    break
                if isk > id_downstream:
                    lprt1 = 1
                    break
                l += 1
            
            if isk < 0:
                lprt1 = 1
            
            sum_val = trap_integration(xi, arg, l)
            cdsk = -self._gam1 / 6.0 * (-sum_val)
            cdwave += cdsk
            prtsk(xi, arg, l, nshock, cdsk, lprt1)
            if lprt1 == 1:
                lprt2 = 1
        
        # Integration along shocks is complete
        # Printout CD information
        xu_loc = x_coords[iu - 1]  # Convert to 0-based indexing
        xd_loc = x_coords[id_downstream - 1]  # Convert to 0-based indexing
        yt_loc = y_coords[jt - 1] * yfact  # Convert to 0-based indexing
        yb_loc = y_coords[jb - 1] * yfact  # Convert to 0-based indexing
        cdc = cdup + cdtop + cdbot + cddown + cdbody
        cd = cdc + cdwave
        
        self.data_summary['cd'] = cd
        self.data_summary['cd_int'] = cdc
        self.data_summary['cd_wave'] = cdwave
        self.data_summary['cd_body'] = cdbody
        
        # Write drag coefficient breakdown
        if self.config['flag_output_summary']:
            with open(os.path.join(self.output_dir, "smry.out"), 'a') as f:
                
                f.write('1 CALCULATION OF DRAG COEFFICIENT BY MOMENTUM INTEGRAL METHOD\n')
                f.write('  BOUNDARIES OF CONTOUR USED CONTRIBUTION TO CD\n')
                f.write(f' UPSTREAM    X ={xu_loc:12.6f}  CDUP   ={cdup:12.6f}\n')
                f.write(f' DOWNSTREAM  X ={xd_loc:12.6f}  CDDOWN ={cddown:12.6f}\n')
                f.write(f' TOP         Y ={yt_loc:12.6f}  CDTOP  ={cdtop:12.6f}\n')
                f.write(f' BOTTOM      Y ={yb_loc:12.6f}  CDBOT  ={cdbot:12.6f}\n')
                f.write('\n')
                f.write(f'Number of shock inside contour, N =      {nshock:3d}\n')
                f.write(f'Body aft location,              X =      {xd_loc:15.9f}\n')
                f.write(f'Drag due to body,               CD_body ={cdbody:15.9f}\n')
                f.write(f'Drag due to shock,              CD_wave ={cdwave:15.9f}\n')
                f.write(f'Drag by momentum integral,      CD_int = {cdc:15.9f}\n')
                f.write(f'Total drag (CD_int + CD_wave),  CD =     {cd:15.9f}\n')
                f.write('\n')
                
                if nshock > 0 and lprt2 == 0:
                    f.write('NOTE - All shocks contained within contour, CD_wave equals total wave drag\n')
                
                if nshock > 0 and lprt2 == 1:
                    f.write('NOTE - One or more shocks extend outside of contour, CD_wave does not equal total wave drag\n')
    
    def print_summary(self) -> None:
        '''
        Main print driver: prints configuration parameters and calls specialized subroutines.
        Translates the PRINT subroutine from io_module.f90
        '''
        # Get required variables from Fortran modules
        simdef = tsf.common_data.simdef
        emach = tsf.common_data.emach
        delta = tsf.common_data.delta
        ak = tsf.common_data.ak
        
        # Get solver data variables
        dub = tsf.solver_data.dub
        cmfact = tsf.solver_data.cmfact
        clfact = tsf.solver_data.clfact
        sonvel = tsf.solver_data.sonvel
        abort1 = tsf.solver_data.abort1
        cpfact = self._cpfact
        cdfact = self._cdfact
        yfact = self._yfact
        vfact = self._vfact
        
        # Write summary file
        if self.config['flag_output_summary']:
            
            with open(os.path.join(self.output_dir, "smry.out"), 'w') as f:
                
                f.write(f'# Output to smry.out: Summary of the calculation\n')
                f.write(f'# Mach = {emach:10.6f}\n')
                f.write(f'# Alpha = {self.data_summary["alpha"]:10.6f}\n')
                f.write(f'# CL = {self.data_summary["cl"]:10.6f}\n')
                f.write(f'# CM = {self.data_summary["cm"]:10.6f}\n')
                f.write(f'# Cp* = {self.data_summary["cpstar"]:10.6f}\n')
                f.write(f'# DELTA = {delta:10.6f}\n')
                f.write(f'# K = {ak:10.6f}\n')
                f.write(f'# DOUBLET STRENGTH = {dub:10.6f}\n')
                f.write(f'# CPFACT = {cpfact:10.6f}\n')
                f.write(f'# CDFACT = {cdfact:10.6f}\n')
                f.write(f'# CMFACT = {cmfact:10.6f}\n')
                f.write(f'# CLFACT = {clfact:10.6f}\n')
                f.write(f'# YFACT = {yfact:10.6f}\n')
                f.write(f'# VFACT = {vfact:10.6f}\n')
                f.write(f'# SONVEL = {sonvel:10.6f}\n')
                f.write(f'# ABORT1 = {abort1:10.6f}\n')
                f.write(f'# SIMDEF = {simdef:10.6f}\n')

                f.write('0 PRINTOUT IN PHYSICAL VARIABLES. \n')
                
                # Print similarity parameter definition
                if simdef == 1:
                    f.write('0 DEFINITION OF SIMILARITY PARAMETERS BY COLE\n')
                elif simdef == 2:
                    f.write('0 DEFINITION OF SIMILARITY PARAMETERS BY SPREITER\n')
                elif simdef == 3:
                    f.write('0 DEFINITION OF SIMILARITY PARAMETERS BY KRUPP\n')
                
                f.write('0 BOUNDARY CONDITION FOR FREE AIR\n')
                f.write('0 DIFFERENCE EQUATIONS ARE FULLY CONSERVATIVE.\n')
                f.write('0 KUTTA CONDITION IS ENFORCED.\n')
        
        # Print data on Y=0 line
        self.output_surface()
        
        # Output field data
        self.output_field()

        # Momentum integral drag calculation
        self.cdcole_python(sonvel, yfact, delta)
        
    def plot_all_results(self, filename:str='tsfoil_results.png'):
        '''
        Plot all results, including:
        - Mesh distribution analysis (read from XIN, YIN in tsfoil2.out)
        - Mach number distribution on Y=0 line (read from cpxs.dat)
        - Mach number field (read from field.dat)

        Plot three sub-plots in one figure (1x3).
        '''
        
        # Create figure with 3 subplots arranged in 1 rows, 2 column
        fig, axes = plt.subplots(1, 2, figsize=(16, 5))
        fig.suptitle('TSFOIL Results Analysis', fontsize=16, fontweight='bold')
        
        # Plot 1: Mach Number Distribution on Y=0 line
        self._plot_mach_distribution_y0(axes[0])
        
        # Plot 2: Mach Number Field
        self._plot_mach_field(axes[1])
        
        plt.tight_layout()
        plt.savefig(os.path.join(self.output_dir, filename), dpi=300)
        plt.close()
        
    def _plot_mach_distribution_y0(self, ax):
        '''
        Plot Mach number distribution on Y=0 line from cpxs.dat
        '''
        # Plot Mach number distribution
        ax.plot(self.mesh['xx'], self.data_summary['mau'],  'b.-', linewidth=2, label='Upper Surface')
        ax.plot(self.mesh['xx'], self.data_summary['mal'], 'r.-', linewidth=2, label='Lower Surface')
        
        # Add reference line for sonic condition
        ax.axhline(y=1.0, color='k', linestyle='--', alpha=0.5, label='Sonic (M=1)')
        
        ax.set_xlabel('X/c')
        ax.set_ylabel('Mach Number')
        ax.set_title(f'(Wall) Mach Number Distribution on Y=0')
        ax.grid(True, alpha=0.3)
        ax.legend()
        ax.set_xlim([-0.2, 1.2])
        ax.set_ylim([-0.1, 1.5])
        
    def _plot_mach_field(self, ax):
        '''
        Plot Mach number field from field.dat
        '''

        x = self.data_summary['field_data'][:, :, 0]
        y = self.data_summary['field_data'][:, :, 1]
        mach = self.data_summary['field_data'][:, :, 2]
        
        # Create contour plot
        max_mach = max(1.5, np.max(mach))
        levels = np.linspace(0, max_mach, 16)
        contour = ax.contourf(x, y, mach, levels=levels, cmap='jet', extend='max')
        
        # Add colorbar
        cbar = plt.colorbar(contour, ax=ax, shrink=0.8)
        cbar.set_label('Mach Number')
        
        # Add airfoil outline
        x_airfoil = self.airfoil['x']
        y_airfoil = self.airfoil['y']
        ax.plot(x_airfoil, y_airfoil, 'k--', linewidth=0.5)
        
        ax.set_xlim([-0.5, 1.5])
        ax.set_ylim([-0.5, 0.5])
        ax.set_xlabel('X/c')
        ax.set_ylabel('Y/c')
        ax.set_title(f'Mach Number Field')
        ax.set_aspect('equal')


if __name__ == "__main__":
    
    pytsfoil = PyTSFoil(
        airfoil_file="rae2822.dat",
        work_dir=os.path.join('example', 'rae2822')
    )
    
    pytsfoil.set_config(
        ALPHA=0.5,
        EMACH=0.75,
        MAXIT=9999,
        NWDGE=0,
        n_point_x=200,
        n_point_y=80,
        n_point_airfoil=100,
        EPS=0.2,
        CVERGE=1e-6,
        flag_output=True,
        flag_output_summary=True,
        flag_output_shock=True,
        flag_output_field=True,
        flag_print_info=True,
    )
    
    pytsfoil.run()
    
    pytsfoil.plot_all_results()

