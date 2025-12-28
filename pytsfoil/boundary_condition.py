'''
Boundary condition module for TSFoil solver
'''

import math
import numpy as np

try:
    import tsfoil_fortran as tsf
except ImportError as e:
    raise ImportError("tsfoil_fortran module not available") from e

class BoundaryCondition:
    """Boundary condition class"""

    def __init__(self, core):
        """Initialize boundary condition class"""
        self.core = core

    def angle(self) -> None:
        """
        Compute the angle THETA at each mesh point (vectorized version).
        
        This function calculates the angle array used for subsonic freestream
        flow with circulation effects. The angle is computed based on the 
        position relative to the singularity location (XSING, 0).
        
        Note: This is only called when AK > 0 (subsonic freestream).
        """
        # Get mesh bounds (1-based in Fortran, convert to 0-based for slicing)
        imin = tsf.common_data.imin
        imax = tsf.common_data.imax
        jmin = tsf.common_data.jmin
        jmax = tsf.common_data.jmax
        
        # Get constants
        pi = tsf.common_data.pi
        twopi = tsf.common_data.twopi
        ak = tsf.common_data.ak
        
        # Get mesh coordinates (as numpy arrays)
        x_coords = np.asarray(tsf.common_data.x)
        y_coords = np.asarray(tsf.common_data.y)
        
        # Get singularity location
        xsing = tsf.solver_data.xsing
        
        # Compute constants
        r2pi = 1.0 / twopi
        rtk = math.sqrt(abs(ak))
        
        # Extract relevant slices (convert to 0-based indexing)
        xx = x_coords[imin-1:imax] - xsing  # shape: (ni,)
        yj = y_coords[jmin-1:jmax]          # shape: (nj,)
        yy = yj * rtk                       # shape: (nj,)
        
        # Create 2D grids using broadcasting: xx[i], yy[j] -> (nj, ni)
        # xx_2d[j, i] = xx[i], yy_2d[j, i] = yy[j]
        xx_2d = xx[np.newaxis, :]  # shape: (1, ni)
        yy_2d = yy[:, np.newaxis]  # shape: (nj, 1)
        yj_2d = yj[:, np.newaxis]  # shape: (nj, 1)
        
        # Compute R = sqrt(Y(J)**2 + XX**2)
        r = np.sqrt(yj_2d**2 + xx_2d**2)  # shape: (nj, ni)
        
        # Compute ATN = atan2(YY, XX)
        atn = np.arctan2(yy_2d, xx_2d)  # shape: (nj, ni)
        
        # Compute Q = PI - sign(PI, YY)
        q = pi - np.copysign(pi, yy_2d)  # shape: (nj, 1), broadcasts to (nj, ni)
        
        # Compute THETA = -(ATN + Q) * R2PI
        theta = -(atn + q) * r2pi  # shape: (nj, ni)
        
        # Apply condition: if R <= 1.0, THETA = THETA * R
        mask = r <= 1.0
        theta = np.where(mask, theta * r, theta)
        
        # Assign to Fortran array (convert indices to 0-based)
        tsf.solver_data.theta[jmin-1:jmax, imin-1:imax] = theta.astype(np.float32)

