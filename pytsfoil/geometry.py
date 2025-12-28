"""
TSFoil geometry computation module

Responsible for airfoil geometry processing and computation, including:
- Airfoil data reading and setup
- Airfoil geometry derivative computation
- Spline interpolation and volume calculation
- Flap deflection processing
"""

import numpy as np
from scipy.interpolate import CubicSpline
from scipy import integrate

try:
    import tsfoil_fortran as tsf
except ImportError as e:
    raise ImportError("tsfoil_fortran module not available") from e


class GeometryProcessor:
    """Geometry processor class"""
    
    def __init__(self, core):
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
            x = self.core.airfoil['coordinates'][:, 0]
            y = self.core.airfoil['coordinates'][:, 1]
        else:
            raise ValueError("Either airfoil_file or airfoil_coordinates must be provided")
        
        le_pos = x.argmin()

        xu = x[:le_pos+1][::-1]
        yu = y[:le_pos+1][::-1]
        xl = x[le_pos:]
        yl = y[le_pos:]
        
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

    def compute_geometry_derivatives(self) -> None:
        """
        Compute airfoil geometry derivatives (equivalent to BODY)
        
        This function translates the Fortran BODY subroutine to Python using scipy.
        It performs cubic spline interpolation on airfoil surfaces, computes volume,
        handles flap deflection, and computes camber and thickness distributions.
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

