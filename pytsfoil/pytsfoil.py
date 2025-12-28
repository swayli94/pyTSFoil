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

import numpy as np

# Import refactored modules
from .core import TSFoilCore
from .mesh import MeshHandler
from .geometry import GeometryProcessor
from .solver import SolverManager
from .output import OutputHandler
from .visualization import Visualizer

try:
    import tsfoil_fortran as tsf
except ImportError as e:
    print("ERROR: Could not import tsfoil_fortran module!")
    print(f"Import error: {e}")
    print()
    print("Make sure you have compiled the Fortran modules with f2py:")
    print("  python3 pyTSFoil/compile_f2py.py")
    import sys
    sys.exit(1)


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
            work_dir: str|None = None,
            output_dir: str|None = None):
        '''
        Initialize the TSFoil object.
        '''
        # Core data management
        self.core = TSFoilCore(airfoil_coordinates, airfoil_file, work_dir, output_dir)
        
        # Functional modules
        self.mesh_handler = MeshHandler(self.core)
        self.geometry = GeometryProcessor(self.core)
        self.solver = SolverManager(self.core)
        self.output = OutputHandler(self.core)
        self.viz = Visualizer(self.core)
        
        # For backward compatibility, expose some core attributes
        self.config = self.core.config
        self.airfoil = self.core.airfoil
        self.mesh = self.core.mesh
        self.data_summary = self.core.data_summary
        
        # Backward compatibility attributes
        self.skiprows = self.core.skiprows
        self.x_scale = self.core.x_scale
        self.y_scale = self.core.y_scale
        self.work_dir = self.core.work_dir
        self.output_dir = self.core.output_dir
    
    def set_config(self, **kwargs):
        '''
        Set the configuration parameters.
        '''
        self.core.set_config(**kwargs)
    
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
    
    # =============================================================
    # Delegate methods - for backward compatibility
    # =============================================================
    
    def initialize_data(self) -> None:
        '''Initialize the data in Fortran module and Python module.'''
        return self.core.initialize_data()
    
    def set_airfoil(self) -> None:
        '''Read the airfoil geometry from a file, and set the airfoil geometry.'''
        self.geometry.set_airfoil()
        # For backward compatibility, print information
        self.geometry.print_airfoil_info()
    
    def set_mesh(self) -> None:
        '''Set the mesh coordinates.'''
        self.mesh_handler.set_mesh()
        # For backward compatibility, print information
        self.mesh_handler.print_mesh_info()
    
    def compute_mesh_indices(self):
        '''Compute mesh indices, including ILE and ITE, JLOW and JUP.'''
        return self.mesh_handler.compute_mesh_indices()
    
    def run_fortran_solver(self) -> None:
        '''Run the Fortran solver.'''
        # First compute geometry derivatives
        self.geometry.compute_geometry_derivatives()
        # Then run solver
        return self.solver.run_fortran_solver()
        
    def compute_data_summary(self):
        '''Compute the data summary.'''
        return self.solver.compute_data_summary()
    
    def output_field(self) -> None:
        '''Output the field to a file in Tecplot format.'''
        return self.output.output_field()
    
    def output_shock(self) -> None:
        '''Output shock data.'''
        return self.output.output_shock()
    
    def print_summary(self) -> None:
        '''Main print driver: prints configuration parameters and calls specialized subroutines.'''
        self.output.print_summary()
        
        # Momentum integral drag calculation
        if 'cpstar' in self.core.data_summary:
            sonvel = tsf.solver_data.sonvel
            yfact = tsf.solver_data.yfact
            delta = tsf.common_data.delta
            self.solver.compute_drag_by_momentum_integral(sonvel, yfact, delta)
        
        # Print results summary
        self.output.print_results_summary()
    
    def plot_all_results(self, filename: str = 'tsfoil_results.png'):
        '''Plot all results.'''
        return self.viz.plot_all_results(filename)
    
    # =============================================================
    # New convenience methods
    # =============================================================
    
    def get_results_summary(self) -> dict:
        '''
        Get a comprehensive summary of computation results.
        
        Returns
        -------
        summary: dict
            Dictionary containing all important results
        '''
        # Merge various information
        summary = {}
        summary.update(self.output.get_summary_info())
        summary.update(self.geometry.get_airfoil_info())
        summary.update(self.mesh_handler.get_mesh_info())
        return summary
    
    def create_plots(self):
        '''Create all available plots.'''
        self.viz.plot_all_results()
        self.viz.plot_pressure_coefficient()
        self.viz.plot_airfoil_geometry()
        self.viz.create_comprehensive_report()
    
    def quick_analysis(self, **config_updates):
        '''
        Run a complete analysis with optional configuration updates.
        
        Parameters
        ----------
        **config_updates
            Configuration parameters to update before running
        '''
        if config_updates:
            self.set_config(**config_updates)
        
        print("Starting TSFOIL analysis...")
        self.run()
        print("Analysis completed.")
        
        if self.config['flag_print_info']:
            print("Creating visualization plots...")
            self.create_plots()
            print("All plots created.")
    
    # =============================================================
    # Backward compatible static methods
    # =============================================================
    
    @staticmethod 
    def clustcos(n_points: int, a0=0.0079, a1=0.96, beta=1.0, index_point: int|None=None) -> np.ndarray:
        '''Point distribution on x-axis [0, 1]. (More points at both ends)'''
        from .utils import clustcos
        return clustcos(n_points, a0, a1, beta, index_point)
    
    # =============================================================
    # Re-implementation of original methods (for complete backward compatibility)
    # =============================================================
    
    def compute_geometry_derivatives(self):
        '''Compute airfoil geometry's derivatives (equivalent to BODY)'''
        return self.geometry.compute_geometry_derivatives()
    
    def cdcole_python(self, sonvel: float, yfact: float, delta: float) -> None:
        """Compute drag coefficient by momentum integral method."""
        return self.solver.compute_drag_by_momentum_integral(sonvel, yfact, delta)
    
    def _default_config(self):
        '''Set the default configuration parameters.'''
        # This method is now implemented in core, here just for backward compatibility
        return self.core._default_config()
        
    def _plot_mach_distribution_y0(self, ax):
        '''Plot Mach number distribution on Y=0 line from cpxs.dat'''
        return self.viz._plot_mach_distribution_y0(ax)
        
    def _plot_mach_field(self, ax):
        '''Plot Mach number field from field.dat'''
        return self.viz._plot_mach_field(ax)


if __name__ == "__main__":
    
    import os
    
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

