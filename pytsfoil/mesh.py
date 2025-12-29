"""
TSFoil mesh processing module

Responsible for mesh generation, setup and index calculation, including:
- Mesh coordinate generation
- Mesh parameter setup
- Mesh index calculation (leading edge, trailing edge, upper and lower surfaces)
"""

import numpy as np
from scipy.interpolate import CubicSpline
from scipy import integrate
from .utils import clustcos
from .core import TSFoilCore

from ._fortran import tsf


class MeshHandler:
    """Mesh handler class"""
    
    def __init__(self, core: TSFoilCore):
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
    
    def compute_geometry_derivatives(self) -> None:
        """
        Compute airfoil geometry derivatives (equivalent to Fortran BODY subroutine)
        
        This is a preprocessing step for the solver. It converts raw airfoil geometry
        into boundary condition format required by the solver (especially surface slopes
        fxu, fxl which are used directly for airfoil boundary conditions).
        
        Processing steps:
            1. Cubic spline interpolation on upper/lower surfaces to get coordinates
               (fu, fl) and slopes (fxu, fxl) at mesh x-coordinates
            2. Compute airfoil cross-sectional area (volume) using Simpson's rule
            3. Apply flap deflection correction if enabled (IFLAP != 0)
            4. Compute camber and thickness distributions
            5. Apply rigidity factor (RIGF) correction to surface slopes for 
               improved accuracy on thick airfoils in transonic flow
        
        Inputs:
            - Raw airfoil coordinates: xu, yu, xl, yl
            - Mesh x-coordinates: xfoil (from set_mesh)
            - Config parameters: PHYS, RIGF, IFLAP, DELFLP, FLPLOC
        
        Outputs (stored in tsf.common_data):
            - fu, fl: Upper/lower surface y-coordinates at mesh points
            - fxu, fxl: Upper/lower surface slopes (dy/dx) - key boundary condition input
            - xfoil: Airfoil mesh x-coordinates
            - camber, thick: Camber line and half-thickness distributions
            - vol: Airfoil cross-sectional area
        """
        # Get data from common_data (Fortran module variables)
        delta = self.core.airfoil['t_max']
        rigf = self.core.config['RIGF']
        
        # Airfoil geometry coordinates
        xu = self.core.airfoil['xu']
        yu = self.core.airfoil['yu'] 
        xl = self.core.airfoil['xl']
        yl = self.core.airfoil['yl']
        nu = xu.shape[0]
        nl = xl.shape[0]
        
        # Mesh coordinates
        xfoil = self.core.mesh['xx_airfoil']
        nfoil = self.core.mesh['nfoil']
        
        # Flap parameters
        iflap = self.core.config['IFLAP']
        delflp = self.core.config['DELFLP']  
        flploc = self.core.config['FLPLOC']
        
        # Scaling factor
        delinv = 1.0
        if self.core.config['PHYS'] == 1:
            delinv = 1.0 / delta

        # Upper surface cubic spline interpolation
        # Calculate endpoint derivatives as boundary conditions
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
        
        # Compute volume using Simpson's rule
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
        if self.core.config['flag_print_info']:
            print(f"Airfoil geometry computed successfully:")
            print(f"  Number of points: {nfoil}")
            print(f"  Volume: {vol:.6f}")
            print(f"  Max thickness: {delta:.6f}")
            if iflap != 0:
                print(f"  Flap deflection: {delflp:.2f} degrees at x={flploc:.3f}")
    
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

