"""
TSFoil geometry computation module

Responsible for airfoil geometry processing and computation, including:
- Airfoil data reading and setup
- Airfoil geometry derivative computation
- Spline interpolation and volume calculation
- Flap deflection processing
"""

import numpy as np
from .core import TSFoilCore

from ._fortran import tsf


class GeometryProcessor:
    """Geometry processor class"""
    
    def __init__(self, core: TSFoilCore):
        """
        Initialize geometry processor
        
        Parameters
        ----------
        core: TSFoilCore
            Core data object
        """
        self.core = core
    
    def set_airfoil(self) -> None:
        """
        Read airfoil geometry from file and set airfoil geometry data
        
        Data starts from trailing edge on upper surface, then goes counterclockwise around the airfoil.
        
        Attributes
        ----------
        airfoil_file : str
            File containing airfoil geometry
        airfoil_coordinates : np.ndarray  
            Airfoil coordinate array
        skiprows : int
            Number of rows to skip in airfoil file
        """
        if self.core.airfoil['file'] is not None:
            x, y = np.loadtxt(self.core.airfoil['file'], skiprows=self.core.skiprows).T
        elif self.core.airfoil['coordinates'] is not None:
            x : np.ndarray = self.core.airfoil['coordinates'][:, 0]
            y : np.ndarray = self.core.airfoil['coordinates'][:, 1]
        else:
            raise ValueError("Either airfoil_file or airfoil_coordinates must be provided")
        
        le_pos : int = x.argmin()

        xu : np.ndarray = x[:le_pos+1][::-1]
        yu : np.ndarray = y[:le_pos+1][::-1]
        xl : np.ndarray = x[le_pos:]
        yl : np.ndarray = y[le_pos:]
        
        # Interpolate airfoil and get maximum thickness (DELTA)
        x_interp = np.linspace(np.min(x), np.max(x), num=501)
        yu_interp = np.interp(x_interp, xu, yu)
        yl_interp = np.interp(x_interp, xl, yl)
        t_max = np.max(yu_interp - yl_interp)
        
        self.core.airfoil['t_max'] = t_max
        self.core.airfoil['x'] = x
        self.core.airfoil['y'] = y
        self.core.airfoil['xu'] = xu
        self.core.airfoil['yu'] = yu
        self.core.airfoil['xl'] = xl
        self.core.airfoil['yl'] = yl
        
        tsf.common_data.nu = xu.shape[0]
        tsf.common_data.nl = xl.shape[0]
        tsf.common_data.delta = np.float32(t_max)
        
        tsf.common_data.xu[:len(xu)] = xu.astype(np.float32)
        tsf.common_data.yu[:len(yu)] = yu.astype(np.float32)
        tsf.common_data.xl[:len(xl)] = xl.astype(np.float32)
        tsf.common_data.yl[:len(yl)] = yl.astype(np.float32)

    def get_airfoil_info(self) -> dict:
        """
        Get airfoil information summary
        
        Returns
        -------
        info: dict
            Dictionary containing key airfoil information
        """
        return {
            'max_thickness': self.core.airfoil.get('t_max', 0),
            'n_points_upper': len(self.core.airfoil.get('xu', [])),
            'n_points_lower': len(self.core.airfoil.get('xl', [])),
            'n_points_total': len(self.core.airfoil.get('x', [])),
            'x_range': [
                np.min(self.core.airfoil.get('x', [0])),
                np.max(self.core.airfoil.get('x', [0]))
            ],
            'y_range': [
                np.min(self.core.airfoil.get('y', [0])),
                np.max(self.core.airfoil.get('y', [0]))
            ],
            'has_flap': self.core.config['IFLAP'] != 0,
            'flap_deflection': self.core.config['DELFLP'],
            'flap_location': self.core.config['FLPLOC'],
        }
    
    def print_airfoil_info(self) -> None:
        """Print airfoil information"""
        if not self.core.config['flag_print_info']:
            return
            
        info = self.get_airfoil_info()
        print(f"Airfoil Information:")
        print(f"  Max thickness: {info['max_thickness']:.6f}")
        print(f"  Total points: {info['n_points_total']}")
        print(f"  Upper surface points: {info['n_points_upper']}")
        print(f"  Lower surface points: {info['n_points_lower']}")
        print(f"  X range: [{info['x_range'][0]:.6f}, {info['x_range'][1]:.6f}]")
        print(f"  Y range: [{info['y_range'][0]:.6f}, {info['y_range'][1]:.6f}]")
        if info['has_flap']:
            print(f"  Flap deflection: {info['flap_deflection']:.2f} degrees")
            print(f"  Flap location: {info['flap_location']:.3f}")
        print()
    
    def compute_camber_thickness_at(self, x_locations: np.ndarray) -> tuple:
        """
        Compute camber and thickness at specified x locations
        
        Parameters
        ----------
        x_locations: np.ndarray
            Array of x locations where to compute
            
        Returns  
        -------
        camber: np.ndarray
            Camber values
        thickness: np.ndarray
            Thickness values
        """
        # Need to run geometry calculation first
        if 'xu' not in self.core.airfoil or 'xl' not in self.core.airfoil:
            raise ValueError("Airfoil geometry not set. Call set_airfoil() first.")
        
        xu = self.core.airfoil['xu']
        yu = self.core.airfoil['yu'] 
        xl = self.core.airfoil['xl']
        yl = self.core.airfoil['yl']
        
        # Interpolate upper and lower surfaces
        yu_interp = np.interp(x_locations, xu, yu)
        yl_interp = np.interp(x_locations, xl, yl)
        
        # Compute camber and thickness
        camber = 0.5 * (yu_interp + yl_interp)
        thickness = 0.5 * (yu_interp - yl_interp)
        
        return camber, thickness

