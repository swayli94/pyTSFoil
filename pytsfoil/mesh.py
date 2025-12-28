"""
TSFoil mesh processing module

Responsible for mesh generation, setup and index calculation, including:
- Mesh coordinate generation
- Mesh parameter setup
- Mesh index calculation (leading edge, trailing edge, upper and lower surfaces)
"""

import numpy as np
from .utils import clustcos

try:
    import tsfoil_fortran as tsf
except ImportError as e:
    raise ImportError("tsfoil_fortran module not available") from e


class MeshHandler:
    """Mesh handler class"""
    
    def __init__(self, core):
        """
        Initialize mesh handler
        
        Parameters
        ----------
        core: TSFoilCore
            Core data object
        """
        self.core = core
    
    def set_mesh(self) -> None:
        """
        Set mesh coordinates
        
        User-defined mesh is provided to TSFOIL through 1D arrays XIN, YIN.
        XIN and YIN are the x-coordinates and y-coordinates of mesh points, respectively.
        The mesh is a 2D non-uniform Cartesian grid.
        The airfoil has unit chord length, leading edge at (0,0), trailing edge at (1,0).
        XIN distributes more points near x=0 and x=1, YIN distributes more points near y=0.
        
        Attributes
        ----------
        n_point_x: int
            Number of points in x-direction
        n_point_y: int  
            Number of points in y-direction
        n_point_airfoil: int
            Number of points on airfoil
        x_scale: float
            Mesh x-coordinate range, x ∈ [-x_scale, x_scale]
        y_scale: float
            Mesh y-coordinate range, y ∈ [-y_scale, y_scale]
        """

        #* Generate x-coordinates with clustering near x=0 and x=1 (interior points)
        # Split domain into three segments: [-x_scale, 0], [0, 1], [1, x_scale]  
        # Distribute remaining points between left and right segments
        n_remaining = int((self.core.config['n_point_x'] - self.core.config['n_point_airfoil']) * 0.5) + 1
        
        # Segment 1: [x_min, 0], clustering near x=0 (right end)
        x_left_norm = clustcos(n_remaining, a0=1.00, a1=0.999, beta=1.0)
        x_left = - x_left_norm[::-1] * self.core.x_scale
        
        # Segment 2: [0, 1], clustering at both ends
        x_center = clustcos(self.core.config['n_point_airfoil'], a0=0.01, a1=0.96, beta=1.0)
        
        # Segment 3: [1, x_max], clustering near x=1 (left end)
        x_right_norm = clustcos(n_remaining, a0=0.001, a1=0.1, beta=1.0)
        x_right = 1 + x_right_norm * (self.core.x_scale - 1)
        
        # Combine segments, removing duplicate boundary points
        xx = np.concatenate([
            x_left[:-1],    # Exclude right boundary (x=0)
            x_center,
            x_right[1:]     # Exclude left boundary (x=1)
        ])
        
        #* Generate symmetric y-coordinate distribution about y=0 with clustering near y=0
        half_points = self.core.config['n_point_y'] // 2 + 1  # Include y=0
        y_half = clustcos(half_points, a0=1.0, a1=0.999, beta=2.0)
        
        # Create symmetric distribution: negative half + positive half
        # y_half goes from 0 to 1, we want symmetric distribution about 0
        yy_normalized = np.concatenate([
            -y_half[1:][::-1],  # Negative side: -1 to 0 (excluding 0)
            y_half[1:]          # Positive side: 0 to 1 (excluding 0)
        ])
        
        # Scale to [-y_scale, y_scale]
        yy = yy_normalized * self.core.y_scale
        
        # Store mesh parameters
        self.core.config['n_point_x'] = xx.shape[0]
        self.core.config['n_point_y'] = yy.shape[0]

        self.core.mesh['x_min'] = -self.core.x_scale
        self.core.mesh['x_max'] = self.core.x_scale
        self.core.mesh['y_min'] = -self.core.y_scale
        self.core.mesh['y_max'] = self.core.y_scale
        self.core.mesh['xx'] = xx
        self.core.mesh['yy'] = yy
        self.core.mesh['xx_airfoil'] = x_center

        # imax, jmax equals imaxi, jmaxi,
        # because mesh points are already checked to be odd for all sections (done in CKMESH in mesh_module.f90)
        tsf.common_data.imaxi = self.core.config['n_point_x']
        tsf.common_data.jmaxi = self.core.config['n_point_y']
        tsf.common_data.imax = self.core.config['n_point_x']
        tsf.common_data.jmax = self.core.config['n_point_y']
        
        # Final mesh arrays x, y are the same as xin, yin
        tsf.common_data.xin[:len(xx)] = xx.astype(np.float32)
        tsf.common_data.yin[:len(yy)] = yy.astype(np.float32)
        tsf.common_data.x[:len(xx)] = xx.astype(np.float32)
        tsf.common_data.y[:len(yy)] = yy.astype(np.float32)
    
    def compute_mesh_indices(self) -> None:
        """
        Compute mesh indices, including:
        1. ILE and ITE (leading and trailing edges)
        2. JLOW and JUP (lower and upper surfaces)
        """
        # Find first point where X >= 0.0 (leading edge)
        ile = np.where(self.core.mesh['xx'] >= 0.0)[0][0]  # 0-based indexing
        self.core.mesh['ile'] = ile  # 0-based indexing
        tsf.common_data.ile = ile + 1  # 1-based indexing
        
        # Find first point where X > 1.0 (trailing edge)
        ite = np.where(self.core.mesh['xx'] > 1.0)[0][0]  # 0-based indexing
        self.core.mesh['ite'] = ite - 1  # 0-based indexing
        tsf.common_data.ite = ite  # 1-based indexing

        # Find first point where Y >= 0.0 (upper surface)
        j = np.where(self.core.mesh['yy'] >= 0.0)[0][0]  # 0-based indexing

        self.core.mesh['jlow'] = j - 1  # 0-based indexing
        self.core.mesh['jup'] = j  # 0-based indexing
        tsf.common_data.jlow = j
        tsf.common_data.jup = j + 1
        
        # Number of points on airfoil
        self.core.mesh['nfoil'] = self.core.mesh['ite'] - self.core.mesh['ile'] + 1
        tsf.common_data.nfoil = self.core.mesh['nfoil']
    
    def get_mesh_info(self) -> dict:
        """
        Get mesh information summary
        
        Returns
        -------
        info: dict
            Dictionary containing key mesh information
        """
        return {
            'n_points_x': self.core.config['n_point_x'],
            'n_points_y': self.core.config['n_point_y'],
            'n_points_airfoil': self.core.config['n_point_airfoil'],
            'x_range': [self.core.mesh.get('x_min', 0), self.core.mesh.get('x_max', 0)],
            'y_range': [self.core.mesh.get('y_min', 0), self.core.mesh.get('y_max', 0)],
            'ile': self.core.mesh.get('ile', 0),
            'ite': self.core.mesh.get('ite', 0),
            'jlow': self.core.mesh.get('jlow', 0),
            'jup': self.core.mesh.get('jup', 0),
            'nfoil': self.core.mesh.get('nfoil', 0),
        }
    
    def print_mesh_info(self) -> None:
        """Print mesh information"""
        if not self.core.config['flag_print_info']:
            return
            
        info = self.get_mesh_info()
        print(f"Mesh Information:")
        print(f"  Grid points: {info['n_points_x']} x {info['n_points_y']}")
        print(f"  Airfoil points: {info['n_points_airfoil']}")
        print(f"  X range: [{info['x_range'][0]:.2f}, {info['x_range'][1]:.2f}]")
        print(f"  Y range: [{info['y_range'][0]:.2f}, {info['y_range'][1]:.2f}]")
        print(f"  Leading edge index: {info['ile']}")
        print(f"  Trailing edge index: {info['ite']}")
        print(f"  Lower surface j-index: {info['jlow']}")
        print(f"  Upper surface j-index: {info['jup']}")
        print(f"  Foil surface points: {info['nfoil']}")
        print()

