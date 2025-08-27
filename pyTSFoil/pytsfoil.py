'''
This is a python interface for TSFOIL.

The wind tunnel option is not implemented.
The 'CLSET' option is not implemented.

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

try:
    import tsfoil_fortran as tsf
except ImportError as e:
    print("ERROR: Could not import tsfoil_fortran module!")
    print(f"Import error: {e}")
    print()
    print("Make sure you have compiled the Fortran modules with f2py:")
    print("  python3 pyTSFoil/compile_f2py.py")
    sys.exit(1)

import os, socket, time, uuid, threading

def get_output_dir(base_dir: Path) -> Path:
    """
    在 base_dir 下为当前进程/线程创建唯一目录并返回。
    结构：base/run-YYYYmmdd-HHMMSS/host-<hostname>/pid-<pid>/thr-<tid>-<uuid4>
    """
    # 运行批次目录（同一次程序运行共享）
    ts = time.strftime("%Y%m%d-%H%M%S", time.localtime())
    host = socket.gethostname()
    pid = os.getpid()
    tid = threading.get_ident()  # 多进程下每个进程的线程ID空间独立

    run_dir = base_dir / f"run-{ts}" / f"host-{host}" / f"pid-{pid}"

    # 尝试创建一个带 UUID 的线程目录，避免极端碰撞
    for _ in range(5):
        leaf = f"thr-{tid}-{uuid.uuid4().hex[:8]}"
        outdir = run_dir / leaf
        try:
            outdir.mkdir(parents=True, exist_ok=False)
            return outdir
        except FileExistsError:
            continue
    # 极端情况下仍冲突，退而求其次用 mkdtemp
    from tempfile import mkdtemp
    return Path(mkdtemp(prefix="fallback-", dir=str(run_dir)))


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

        # Change to parent directory where input/output files are located
        if work_dir is None:
            script_dir = Path(__file__).parent
            parent_dir = script_dir.parent
            os.chdir(parent_dir)
        else:
            os.chdir(work_dir)
                        
        self.work_dir = os.getcwd()
        
        if output_dir is None:
            self.output_dir = get_output_dir(self.work_dir)
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
                raise ValueError(f"Invalid configuration parameter: {key}")
    
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
        tsf.solver_functions.scale()

        # Set far field boundary conditions
        tsf.solver_functions.farfld()
        
        self.compute_geometry_derivatives()
        
        # Compute finite difference coefficients
        tsf.solver_base.difcoe()
        
        # Set boundary conditions
        tsf.solver_functions.setbc(0)
        
        # Solve transonic flow equations
        tsf.main_iteration.solve()
    
    def _default_config(self):
        '''
        Set the default configuration parameters.
        '''
        # Default configuration (user-input parameters, namelist /INP/ in io_module.f90)
        # NU, NL, DELTA are set in set_airfoil()
        # IMAXI, JMAXI are set in set_mesh()
        self.config = {
            'AK': 0.0,              # Free stream similarity parameter
            'ALPHA': 0.0,           # Angle of attack
            'BCTYPE': 1,            # Boundary condition identifiers (1 = free air, 2 = tunnel)
            'CVERGE': 0.00001,      # Error criterion for convergence
            'DVERGE': 10.0,         # Error criterion for divergence
            'EMACH': 0.75,          # Mach number
            'EPS': 0.2,             # Convergence tolerance
            'FCR': 1,               # Whether difference equations are fully conservative (True)
            'IPRTER': 100,          # Print interval for convergence history
            'KUTTA': 1,             # Whether Kutta condition is enforced (True)
            'MAXIT': 1000,          # Maximum number of iterations
            'PHYS': 1,              # Physical (True) vs similarity (False)
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
            'n_point_x': 81,        # Number of points in the x-direction (IMAXI)
            'n_point_y': 60,        # Number of points in the y-direction (JMAXI)
            'n_point_airfoil': 51,  # Number of points on the airfoil
            
            'flag_output_solve': True,     # write solver process to tsfoil2.out
            'flag_output_summary': True,   # smry.out
            'flag_output_shock': True,     # cpxs.dat
            'flag_output_field': True,     # field.dat
            
            'flag_print_info': True,
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
        
        # Set AK=0 for physical coordinates
        if self.config['PHYS'] == 1:
            self.config['AK'] = 0.0
            
        # Constants
        self.n_mesh_points = tsf.common_data.n_mesh_points
        self.nmp_plus1 = tsf.common_data.nmp_plus1
        self.nmp_plus2 = tsf.common_data.nmp_plus2
        
        # Open output files
        tsf.io_module.open_output_file()
    
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
        
        tsf.common_data.nu = xu.shape[0]
        tsf.common_data.nl = xl.shape[0]
        tsf.common_data.delta = np.float32(t_max)
        
        tsf.common_data.xu[:len(xu)] = xu.astype(np.float32)
        tsf.common_data.yu[:len(yu)] = yu.astype(np.float32)
        tsf.common_data.xl[:len(xl)] = xl.astype(np.float32)
        tsf.common_data.yl[:len(yl)] = yl.astype(np.float32)

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
        
        # The final mesh array x, y is the same as xin, yin
        tsf.common_data.xin[:len(xx)] = xx.astype(np.float32)
        tsf.common_data.yin[:len(yy)] = yy.astype(np.float32)
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
        nu = xu.shape[0]
        nl = xl.shape[0]
        
        # Mesh coordinates
        xfoil = self.mesh['xx_airfoil']
        nfoil = self.mesh['nfoil']
        
        # Flap parameters
        iflap = self.config['IFLAP']
        delflp = self.config['DELFLP']  
        flploc = self.config['FLPLOC']
        
        # Scaling factor
        delinv = 1.0
        if self.config['PHYS'] == 1:
            delinv = 1.0 / delta

        # Upper surface cubic spline interpolation
        # Calculate derivatives at endpoints for boundary conditions
        dy1_u = (yu[1] - yu[0]) / (xu[1] - xu[0])
        dy2_u = (yu[nu-1] - yu[nu-2]) / (xu[nu-1] - xu[nu-2])
        
        # Create cubic spline with derivative boundary conditions
        cs_upper = CubicSpline(xu, yu, bc_type=((1, dy1_u), (1, dy2_u)))
        
        # Interpolate upper surface at mesh x-coordinates
        fu = cs_upper(xfoil) * delinv
        fxu = cs_upper(xfoil, 1) * delinv
        
        # Lower surface cubic spline interpolation  
        dy1_l = (yl[1] - yl[0]) / (xl[1] - xl[0])
        dy2_l = (yl[nl-1] - yl[nl-2]) / (xl[nl-1] - xl[nl-2])
        
        cs_lower = CubicSpline(xl, yl, bc_type=((1, dy1_l), (1, dy2_l)))
        
        # Interpolate lower surface at mesh x-coordinates
        fl = cs_lower(xfoil) * delinv
        fxl = cs_lower(xfoil, 1) * delinv
        
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
        
        # Compute camber and thickness
        camber = 0.5 * (fu + fl)
        thick = 0.5 * (fu - fl)
        
        # Apply rigidity factor correction to surface slopes
        fxu = fxu / np.sqrt(1.0 + rigf * (delta * fxu)**2)
        fxl = fxl / np.sqrt(1.0 + rigf * (delta * fxl)**2)
        
        # Store results in common_data arrays
        tsf.common_data.vol = vol
        
        # Pad arrays to expected size
        tsf.common_data.fu[:nfoil] = fu.astype(np.float32)
        tsf.common_data.fl[:nfoil] = fl.astype(np.float32)
        tsf.common_data.fxu[:nfoil] = fxu.astype(np.float32)
        tsf.common_data.fxl[:nfoil] = fxl.astype(np.float32)
        tsf.common_data.xfoil[:nfoil] = xfoil.astype(np.float32)
        tsf.common_data.camber[:nfoil] = camber.astype(np.float32)
        tsf.common_data.thick[:nfoil] = thick.astype(np.float32)
        
        # Print or log geometry (equivalent to PRBODY call)
        if self.config['flag_print_info']:
            print(f"Airfoil geometry computed successfully:")
            print(f"  Number of points: {nfoil}")
            print(f"  Volume: {vol:.6f}")
            print(f"  Max thickness: {delta:.6f}")
            if iflap != 0:
                print(f"  Flap deflection: {delflp:.2f} degrees at x={flploc:.3f}")
    
    def compute_data_summary(self):
        '''
        Compute the data summary.
        '''
        alpha = tsf.common_data.alpha
        vfact = tsf.solver_data.vfact
        clfact = tsf.solver_data.clfact
        cmfact = tsf.solver_data.cmfact
        
        # Compute lift and pitch coefficients
        self.data_summary['alpha'] = alpha * vfact
        self.data_summary['mach'] = tsf.common_data.emach
        self.data_summary['cl'] = tsf.solver_base.lift(clfact)
        self.data_summary['cm'] = tsf.solver_base.pitch(cmfact)
        self.data_summary['cpstar'] = tsf.solver_data.cpstar
            
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
        vfact = tsf.solver_data.vfact
        c1 = tsf.solver_data.c1
        cxl = tsf.solver_data.cxl
        cxc = tsf.solver_data.cxc
        cxr = tsf.solver_data.cxr
        cpfact = tsf.solver_data.cpfact
        
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
                    u = tsf.solver_base.px(i, j)  # Computes U = DP/DX at point I,J
                    em = tsf.solver_functions.emach1(u, delta)  # Computes Mach number from U
                    cp_val = -2.0 * u * cpfact  # CPFACT is a scaling factor for pressure coefficient
                    
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
    
    def output_shock(self) -> None:
        '''
        Output shock data, translating the Fortran PRINT_SHOCK subroutine.
        This function computes pressure coefficients and Mach numbers along the airfoil surface.
        
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
        cpfact = tsf.solver_data.cpfact

        cpstar = tsf.solver_data.cpstar
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
        phys = tsf.common_data.phys
        
        # Initialize variables
        iem = 0
        cj01 = -y_coords[jlow-1] / (y_coords[jup-1] - y_coords[jlow-1])  # Convert to 0-based indexing
        cj02 = y_coords[jup-1] / (y_coords[jup-1] - y_coords[jlow-1])
        
        # Arrays to store results
        n_points = imax - imin + 1
        em1l = np.zeros(n_points)
        em1u = np.zeros(n_points)
        cpu = np.zeros(n_points)
        cpl = np.zeros(n_points)
        
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
            
            # Store CPL value and compute Mach number
            cpl[i_py] = -2.0 * ul_p1 * cpfact
            em1l[i_py] = tsf.solver_functions.emach1(ul_p1, delta)
            if em1l[i_py] > 1.3:
                iem = 1
            
            # Calculate UU_P1
            uu_p1 = cjup * tsf.solver_base.px(i_p1, jup) - cjup1 * tsf.solver_base.px(i_p1, jup + 1)
            if i_p1 > ite:
                uu_p1 = ul_p1
            if i_p1 < ile:
                uu_p1 = ul_p1
            
            # Store CPU value and compute Mach number
            cpu[i_py] = -2.0 * uu_p1 * cpfact
            em1u[i_py] = tsf.solver_functions.emach1(uu_p1, delta)
            if em1u[i_py] > 1.3:
                iem = 1
        
        self.data_summary['cpu'] = cpu
        self.data_summary['cpl'] = cpl
        self.data_summary['mau'] = em1u
        self.data_summary['mal'] = em1l
        
        # Output summary file
        if self.config['flag_output_summary']:
            with open(os.path.join(self.output_dir, "smry.out"), 'a') as f:

                # Check for detached shock
                if cpl[imin-1] < cpstar and cpl[imin] > cpstar:
                    f.write('0 ***** CAUTION *****\n')
                    f.write(' DETACHED SHOCK WAVE UPSTREAM OF X-MESH,SOLUTION TERMINATED.\n')
                    return
                
                # Mach number warning
                if iem == 1 and phys:
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
                    f.write(f'  {self.mesh['xx'][i_py]:10.5f}')
                    f.write(f'  {self.data_summary['cpu'][i_py]:10.5f}')
                    f.write(f'  {self.data_summary['mau'][i_py]:10.5f}')
                    f.write(f'  {self.data_summary['cpl'][i_py]:10.5f}')
                    f.write(f'  {self.data_summary['mal'][i_py]:10.5f}\n')
        
            if self.config['flag_print_info']:
                print('Output to cpxs.dat: Cp, Mach distribution on a x-line (Y=0)')
    
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
        gam1 = tsf.common_data.gam1
        fxl = tsf.common_data.fxl
        fxu = tsf.common_data.fxu
        
        # Get solver data
        cjup = tsf.solver_data.cjup
        cjup1 = tsf.solver_data.cjup1
        cjlow = tsf.solver_data.cjlow
        cjlow1 = tsf.solver_data.cjlow1
        cdfact = tsf.solver_data.cdfact
        
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
                
            cdycof = -cdfact * gam1 / (6.0 * yfact)
            poycof = delta**2 * gam1 * (gam1 - 1.0) / 12.0
                
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
        gam123 = gam1 * 2.0 / 3.0
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
            cdsk = -gam1 / 6.0 * cdfact * sum_val
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
            cdsk = -gam1 / 6.0 * cdfact * sum_val
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
            cdsk = -gam1 / 6.0 * (-sum_val)
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
        phys = tsf.common_data.phys
        simdef = tsf.common_data.simdef
        bctype = tsf.common_data.bctype
        fcr = tsf.common_data.fcr
        kutta = tsf.common_data.kutta
        emach = tsf.common_data.emach
        delta = tsf.common_data.delta
        ak = tsf.common_data.ak
        
        # Get solver data variables
        dub = tsf.solver_data.dub
        cpfact = tsf.solver_data.cpfact
        cdfact = tsf.solver_data.cdfact
        cmfact = tsf.solver_data.cmfact
        clfact = tsf.solver_data.clfact
        yfact = tsf.solver_data.yfact
        vfact = tsf.solver_data.vfact
        sonvel = tsf.solver_data.sonvel
        abort1 = tsf.solver_data.abort1
        
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
                f.write(f'# BCTYPE = {bctype:10.6f}\n')
                f.write(f'# FCR = {fcr:10.6f}\n')
                f.write(f'# KUTTA = {kutta:10.6f}\n')
                f.write(f'# PHYS = {phys:10.6f}\n')
                f.write(f'# SIMDEF = {simdef:10.6f}\n')
                f.write(f'# SCALED POR = {tsf.common_data.por:10.6f}\n')

                # Print similarity/physical variables information
                if phys:
                    f.write('0 PRINTOUT IN PHYSICAL VARIABLES. \n')
                else:
                    f.write('0 PRINTOUT IN SIMILARITY VARIABLES.\n')
                
                # Print similarity parameter definition
                if simdef == 1:
                    f.write('0 DEFINITION OF SIMILARITY PARAMETERS BY COLE\n')
                elif simdef == 2:
                    f.write('0 DEFINITION OF SIMILARITY PARAMETERS BY SPREITER\n')
                elif simdef == 3:
                    f.write('0 DEFINITION OF SIMILARITY PARAMETERS BY KRUPP\n')
                
                # Print boundary condition information
                if bctype == 1:
                    f.write('0 BOUNDARY CONDITION FOR FREE AIR\n')
                elif bctype == 2:
                    f.write('0 BOUNDARY CONDITION FOR SOLID WALL\n')
                elif bctype == 3:
                    f.write('0 BOUNDARY CONDITION FOR FREE JET\n')
                
                # Print difference equation information
                if fcr:
                    f.write('0 DIFFERENCE EQUATIONS ARE FULLY CONSERVATIVE.\n')
                else:
                    f.write('0 DIFFERENCE EQUATIONS ARE NOT CONSERVATIVE AT SHOCK.\n')
                
                # Print Kutta condition information
                if kutta:
                    f.write('0 KUTTA CONDITION IS ENFORCED.\n')
                else:
                    f.write('0 LIFT COEFFICIENT SPECIFIED BY USER.\n')
        
        # Print shock and mach number on Y=0 line
        self.output_shock()
        
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
        CVERGE=1e-6
    )
    
    pytsfoil.run()
    
    pytsfoil.plot_all_results()

