"""
Viscous correction module for shock/boundary layer interaction

Handles computation of Murman or Yoshihara viscous wedge corrections
to account for jump in displacement thickness due to shock/boundary 
layer interaction.
"""

import numpy as np
import math

try:
    import tsfoil_fortran as tsf
except ImportError as e:
    raise ImportError("tsfoil_fortran module not available") from e


class ViscousCorrection:
    """
    Viscous correction class for shock/boundary layer interaction
    
    Handles computation of Murman or Yoshihara viscous wedge corrections
    to account for jump in displacement thickness due to shock/boundary 
    layer interaction.
    """
    
    def __init__(self, core):
        """
        Initialize viscous correction handler
        
        Parameters
        ----------
        core : TSFoilCore
            Core data object containing flow parameters
        """
        self.core = core
    
    def findsk(self, istart: int, iend: int, j: int, sonvel: float) -> int:
        """
        Find shock location along line J
        
        Parameters
        ----------
        istart : int
            Starting I index (1-based)
        iend : int
            Ending I index (1-based)
        j : int
            Y index (1-based)
        sonvel : float
            Sonic velocity
            
        Returns
        -------
        int
            Shock location index (negative if no shock found)
        """
        isk = istart - 1
        u2 = self.core.px(isk, j)
        
        while True:
            isk += 1
            u1 = u2
            u2 = self.core.px(isk, j)
            if u1 > sonvel and u2 <= sonvel:
                return isk
            if isk >= iend:
                return -iend

    def wangle(self, am2: float, nw: int, g: float) -> float:
        """
        Compute wedge angle for viscous correction
        
        Parameters
        ----------
        am2 : float
            Square of Mach number upstream of shock
        nw : int
            Wedge type (1=Murman, 2=Yoshihara)
        g : float
            GAM1 = gamma + 1
            
        Returns
        -------
        float
            Wedge angle in radians
        """
        if nw == 1:
            # Murman wedge
            return 4.0 * ((am2 - 1.0) / 3.0) ** 1.5 / g
        else:
            # Yoshihara wedge
            am3 = 3.0 * am2
            am4 = 4.0 * am2
            am7 = 7.0 * am2
            rm = math.sqrt(3.0 * (am3 * am2 + am4 + 20.0))
            rs = math.sqrt(3.0 * (am3 * am2 - am4 + 13.0))
            s2tm = (am3 - 5.0 + rm) / am7
            s2ts = (am3 - 2.0 + rs) / am7
            tm = math.asin(math.sqrt(s2tm))
            ts = math.asin(math.sqrt(s2ts))
            ttm = math.tan(tm)
            tts = math.tan(ts)
            tdm = 5.0 * (am2 * s2tm - 1.0) / (ttm * (5.0 + am2 * (6.0 - 5.0 * s2tm)))
            tds = 5.0 * (am2 * s2ts - 1.0) / (tts * (5.0 + am2 * (6.0 - 5.0 * s2ts)))
            return 0.5 * (math.atan(tdm) + math.atan(tds))

    def compute_vwedge(self):
        """
        Compute Murman or Yoshihara viscous wedge
        
        Computes viscous wedge and modifies slope conditions to account for 
        jump in displacement thickness due to shock/boundary layer interaction.
        
        Returns
        -------
        tuple
            (am1, xshk, thamax, zeta, nvwprt, nishk):
            - am1: Mach numbers upstream of shocks (2x3 array)
            - xshk: Shock x-locations (2x3 array)
            - thamax: Maximum wedge angles (2x3 array)
            - zeta: Wedge length scales (2x3 array)
            - nvwprt: Number of shocks on upper and lower surfaces (2-element array)
            - nishk: Total number of shocks
        """
        # Get required variables from Fortran modules
        x_coords = tsf.common_data.x
        ile = tsf.common_data.ile
        ite = tsf.common_data.ite
        jup = tsf.common_data.jup
        jlow = tsf.common_data.jlow
        gam1 = tsf.common_data.gam1
        xdiff = tsf.common_data.xdiff
        delta = tsf.common_data.delta
        nwdge = tsf.common_data.nwdge
        reynld = tsf.common_data.reynld
        wconst = tsf.common_data.wconst
        sonvel = tsf.solver_data.sonvel
        wslp = tsf.solver_data.wslp
        
        # Initialize output arrays
        am1 = np.zeros((2, 3))
        xshk = np.zeros((2, 3))
        thamax = np.zeros((2, 3))
        zeta = np.zeros((2, 3))
        nvwprt = np.zeros(2, dtype=np.int32)
        nishk = 0
        
        # Zero out previous wedge slopes
        for j in range(2):  # 0, 1 -> surface index
            for i in range(ile, ite + 1):
                wslp[i - 1, j] = 0.0  # Convert to 0-based indexing
        
        sign = 1.0
        n = 0  # 0-based index for shock count on current surface
        istart = ile
        jmp = 0
        
        # Process upper (m=0) then lower (m=1) surface
        m = 0
        
        while m <= 1:
            # Find shock location
            j_surface = jup if m == 0 else jlow
            isk = self.findsk(istart, ite, j_surface, sonvel)
            
            if isk < 0:
                if m == 0:
                    # Move to lower surface
                    n = 0
                    istart = ile
                    sign = -sign
                    m = 1
                    continue
                else:
                    break  # No more shocks
            
            nishk += 1
            nvwprt[m] += 1
            
            # Compute X position of shock by interpolation
            v1 = self.core.px(isk - 1, j_surface)
            xshk[m, n] = x_coords[isk - 2] + (sonvel - v1) / ((self.core.px(isk, j_surface) - v1) * xdiff[isk - 1])
            
            # Compute flow properties 3 points upstream
            isk3 = isk - 3
            u = self.core.px(isk3, j_surface)
            am1[m, n] = self.core.emach1(u, delta)
            am1sq = am1[m, n] ** 2
            
            if am1sq <= 1.0:
                jmp = 1
            else:
                thamax[m, n] = self.wangle(am1sq, nwdge, gam1) * sign
                
                if nwdge == 1:
                    # Murman wedge
                    reyx = reynld * xshk[m, n]
                    cf = 0.02666 / (reyx ** 0.139)
                    dstar1 = 0.01738 * reyx ** 0.861 / reynld
                    
                    if n > 0 and jmp == 0:
                        dxs = xshk[m, n] - xshk[m, n - 1]
                        if dxs < zeta[m, n - 1]:
                            aeta = dxs / zeta[m, n - 1]
                            dstar1 = dxs * thamax[m, n - 1] * (1.0 + aeta * (aeta / 3.0 - 1.0))
                        else:
                            dstar1 = zeta[m, n - 1] * thamax[m, n - 1] / 3.0
                    
                    jmp = 0
                    zeta[m, n] = wconst * math.sqrt((am1sq - 1.0) / cf) * dstar1
                    
                    # Compute wedge slopes
                    xend = xshk[m, n] + zeta[m, n]
                    for i in range(isk, ite + 1):
                        if x_coords[i - 1] >= xend:
                            break
                        aeta = (x_coords[i - 1] - xshk[m, n]) / zeta[m, n]
                        wslp[i - 1, m] = thamax[m, n] * (1.0 - aeta) ** 2 / delta
                
                elif nwdge == 2:
                    # Yoshihara wedge
                    isk1 = isk - 1
                    for i in range(isk1, isk + 1):
                        wslp[i - 1, m] = thamax[m, n] / delta
            
            # Check for additional shock on surface
            n += 1
            if n >= 3:
                if m == 0:
                    # Move to lower surface
                    n = 0
                    istart = ile
                    sign = -sign
                    m = 1
                else:
                    break
            else:
                istart = isk + 2
        
        return am1, xshk, thamax, zeta, nvwprt, nishk

