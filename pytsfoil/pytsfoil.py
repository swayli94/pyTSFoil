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
import matplotlib.pyplot as plt

# Import refactored modules
from .core import TSFoilCore
from .mesh import MeshHandler
from .geometry import GeometryProcessor
from .viscous import ViscousCorrection
from .solver import SolverManager
from .output import OutputHandler
from .visualization import Visualizer
from .post_processing import PostProcessing
from .pre_processing import PreProcessing


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
        
        self.output = OutputHandler(self.core)
        self.viz = Visualizer(self.core)
        
        self.viscous_correction = ViscousCorrection(self.core)
        self.pre_processing = PreProcessing(self.core)
        self.post_processing = PostProcessing(self.core)
        
        self.solver = SolverManager(core=self.core, 
                            pre_processing=self.pre_processing,
                            post_processing=self.post_processing,
                            viscous_correction=self.viscous_correction)
        
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
        
        self.pre_process()
        
        self.run_solver()
        
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
        self.geometry.print_airfoil_info()
    
    def set_mesh(self) -> None:
        '''Set the mesh coordinates and compute mesh indices.'''
        self.mesh_handler.set_mesh()
        self.mesh_handler.print_mesh_info()
        self.mesh_handler.compute_mesh_indices()
        self.mesh_handler.compute_geometry_derivatives()
    
    def pre_process(self) -> None:
        '''Pre-process the data.'''
        self.pre_processing.apply_similarity_scaling()
        self.pre_processing.setup_farfield_boundary()
        self.pre_processing.compute_fd_coefficients()
        self.pre_processing.setup_body_boundary()
    
    def run_solver(self) -> None:
        '''Run the Fortran solver.'''
        self.solver.run_solver()
            
    def output_field(self) -> None:
        '''Output the field to a file in Tecplot format.'''
        self.output.output_field()
    
    def output_shock(self) -> None:
        '''Output shock data.'''
        self.output.output_shock()
    
    def print_summary(self) -> None:
        '''Main print driver: prints configuration parameters and calls specialized subroutines.'''
        self.post_processing.compute_data_summary()
        
        self.output.print_summary()
        
        # Momentum integral drag calculation
        if 'cpstar' in self.core.data_summary:
            self.post_processing.compute_drag_by_momentum_integral()
        
        # Print results summary
        self.output.print_results_summary()
    
    def plot_all_results(self, filename: str = 'tsfoil_results.png') -> None:
        '''Plot all results.'''
        self.viz.plot_all_results(filename)
    

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

