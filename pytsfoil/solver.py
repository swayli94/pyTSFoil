"""
TSFoil solver management module

Responsible for numerical solving and post-processing computation, including:
- Fortran solver invocation
- Data summary computation  
- Drag coefficient calculation (momentum integral method)
- Flow field analysis
"""

import numpy as np
import math
import os
from .utils import trap_integration, find_shock_location, find_new_shock_location, print_shock_information

try:
    import tsfoil_fortran as tsf
except ImportError as e:
    raise ImportError("tsfoil_fortran module not available") from e


class SolverManager:
    """Solver manager class"""
    
    def __init__(self, core):
        """
        Initialize solver manager
        
        Parameters
        ----------
        core: TSFoilCore
            Core data object
        """
        self.core = core
    
    def scale(self) -> None:
        """
        Scale physical variables to transonic similarity variables
        
        Define scaling factors for physical variables:
            CPFACT, CLFACT, CMFACT, CDFACT, VFACT, YFACT
            
        If PHYS = True, all input/output quantities are in physical units normalized 
        by freestream values and airfoil chord. This method then scales the quantities 
        to transonic variables by the following convention:
            SIMDEF = 1: COLE SCALING
            SIMDEF = 2: SPREITER SCALING
            SIMDEF = 3: KRUPP SCALING
            
        If PHYS = False, input is already in scaled variables and no further scaling is done.
        """
        # Get required variables from Fortran modules
        phys = tsf.common_data.phys
        emach = tsf.common_data.emach
        simdef = tsf.common_data.simdef
        delta = tsf.common_data.delta
        gam1 = tsf.common_data.gam1
        jmin = tsf.common_data.jmin
        jmax = tsf.common_data.jmax
        
        if not phys:
            # PHYS = False, no scaling
            tsf.solver_data.cpfact = 1.0
            tsf.solver_data.cdfact = 1.0
            tsf.solver_data.clfact = 1.0
            tsf.solver_data.cmfact = 1.0
            tsf.solver_data.yfact = 1.0
            tsf.solver_data.vfact = 1.0
        else:
            # PHYS = True, compute constants
            emach2 = emach * emach
            beta = 1.0 - emach2
            delrt1 = delta ** (1.0 / 3.0)
            delrt2 = delta ** (2.0 / 3.0)
            
            if simdef == 1:
                # COLE SCALING
                ak = beta / delrt2
                yfact = 1.0 / delrt1
                cpfact = delrt2
                clfact = delrt2
                cdfact = delrt2 * delta
                cmfact = delrt2
                vfact = delta * 57.295779  # Convert radians to degrees
                
            elif simdef == 2:
                # SPREITER SCALING
                emroot = emach ** (2.0 / 3.0)
                ak = beta / (delrt2 * emroot * emroot)
                yfact = 1.0 / (delrt1 * emroot)
                cpfact = delrt2 / emroot
                clfact = cpfact
                cmfact = cpfact
                cdfact = cpfact * delta
                vfact = delta * 57.295779
                
            elif simdef == 3:
                # KRUPP SCALING
                ak = beta / (delrt2 * emach)
                yfact = 1.0 / (delrt1 * emach ** 0.5)
                cpfact = delrt2 / (emach ** 0.75)
                clfact = cpfact
                cmfact = cpfact
                cdfact = cpfact * delta
                vfact = delta * 57.295779
                
            else:
                raise ValueError(f"Invalid SIMDEF value: {simdef}. Must be 1, 2, or 3.")
            
            # Store computed values back to Fortran modules
            tsf.common_data.ak = ak
            tsf.solver_data.cpfact = cpfact
            tsf.solver_data.clfact = clfact
            tsf.solver_data.cmfact = cmfact
            tsf.solver_data.cdfact = cdfact
            tsf.solver_data.yfact = yfact
            tsf.solver_data.vfact = vfact
            
            # Scale Y mesh
            yfaciv = 1.0 / yfact
            for j in range(jmin, jmax + 1):
                tsf.common_data.yin[j - 1] = tsf.common_data.yin[j - 1] * yfaciv
            
            # Scale tunnel parameters (wall height)
            tsf.common_data.h = tsf.common_data.h / yfact
            tsf.common_data.por = tsf.common_data.por * yfact
            
            # Scale angle of attack
            tsf.common_data.alpha = tsf.common_data.alpha / vfact
        
        # Check value of AK for default
        if tsf.common_data.ak == 0.0:
            raise ValueError("AK value is zero. Invalid input parameters.")
        
        # Compute sonic velocity
        if abs(gam1) <= 0.0001:
            tsf.solver_data.sonvel = 1.0
            tsf.solver_data.cpstar = 0.0
        else:
            sonvel = tsf.common_data.ak / gam1
            cpstar = -2.0 * sonvel * tsf.solver_data.cpfact
            tsf.solver_data.sonvel = sonvel
            tsf.solver_data.cpstar = cpstar

    def farfld(self) -> None:
        """
        Compute far-field boundary conditions for outer boundaries
        
        The functional form of the potential on outer boundaries is prescribed.
        Equations represent asymptotic form for doublet and vortex in free air
        and wind tunnel environment. Doublet and vortex are located at X=XSING, Y=0.
        
        Boundary condition types (BCTYPE):
            1: Free air boundary condition
            2: Solid wall tunnel
            3: Free jet
            4: Ideal slotted wall
            5: Ideal perforated/porous wall
            6: General homogeneous wall boundary condition (not useable)
        """
        # Get required variables from Fortran modules
        ak = tsf.common_data.ak
        x_coords = tsf.common_data.x
        y_coords = tsf.common_data.y
        imin = tsf.common_data.imin
        imax = tsf.common_data.imax
        jmin = tsf.common_data.jmin
        jmax = tsf.common_data.jmax
        bctype = tsf.common_data.bctype
        pi = tsf.common_data.pi
        twopi = tsf.common_data.twopi
        halfpi = tsf.common_data.halfpi
        f = tsf.common_data.f
        h = tsf.common_data.h
        por = tsf.common_data.por
        xsing = tsf.solver_data.xsing
        
        # Test for supersonic or subsonic freestream
        if ak <= 0.0:
            # Supersonic freestream
            if f != 0.0 and h != 0.0:
                tsf.solver_data.fhinv = 1.0 / (f * h)
            else:
                tsf.solver_data.fhinv = 1.0
            # For supersonic case, upstream boundary conditions correspond to uniform
            # undisturbed flow. Downstream boundary required to be supersonic.
            # Top and bottom boundaries use simple wave solution.
            return
        
        rtk = math.sqrt(abs(ak))
        
        # Subsonic freestream
        # Set default values for tunnel wall parameters
        b_coef = 0.0
        omega0 = 1.0
        omega1 = 1.0
        omega2 = 1.0
        jet = 0.0
        psi0 = 1.0
        psi1 = 1.0
        psi2 = 1.0
        alpha0 = 0.0
        alpha1 = 0.0
        alpha2 = 0.0
        beta0 = 0.0
        beta1 = 0.0
        beta2 = 0.0
        rtkpor = 0.0
        
        # Branch to appropriate formulas depending on BCTYPE
        if bctype == 1:
            # BCTYPE = 1: FREE AIR BOUNDARY CONDITION
            # Set boundary ordinates
            yt = y_coords[jmax - 1] * rtk  # Convert to 0-based indexing
            yb = y_coords[jmin - 1] * rtk
            xu_bc = x_coords[imin - 1] - xsing
            xd_bc = x_coords[imax - 1] - xsing
            yt2 = yt * yt
            yb2 = yb * yb
            xu2 = xu_bc * xu_bc
            xd2 = xd_bc * xd_bc
            coef1 = 1.0 / twopi
            coef2 = 1.0 / (twopi * rtk)
            
            # Compute doublet and vortex terms on top and bottom boundaries
            for i in range(imin, imax + 1):
                xp = x_coords[i - 1] - xsing  # Convert to 0-based indexing
                xp2 = xp * xp
                tsf.solver_data.dtop[i - 1] = xp / (xp2 + yt2) * coef2
                tsf.solver_data.dbot[i - 1] = xp / (xp2 + yb2) * coef2
                tsf.solver_data.vtop[i - 1] = -math.atan2(yt, xp) * coef1
                tsf.solver_data.vbot[i - 1] = -(math.atan2(yb, xp) + twopi) * coef1
            
            # Compute doublet and vortex terms on upstream and downstream boundaries
            for j in range(jmin, jmax + 1):
                yj = y_coords[j - 1] * rtk  # Convert to 0-based indexing
                yj2 = yj * yj
                tsf.solver_data.dup[j - 1] = xu_bc / (xu2 + yj2) * coef2
                tsf.solver_data.ddown[j - 1] = xd_bc / (xd2 + yj2) * coef2
                q = pi - math.copysign(pi, yj)
                tsf.solver_data.vup[j - 1] = -(math.atan2(yj, xu_bc) + q) * coef1
                tsf.solver_data.vdown[j - 1] = -(math.atan2(yj, xd_bc) + q) * coef1
            
            if ak > 0.0:
                tsf.solver_base.angle()
            return
            
        elif bctype == 2:
            # BCTYPE = 2: SOLID WALL TUNNEL
            tsf.common_data.por = 0.0
            # Set constants for doublet solution
            b_coef = 0.5
            alpha0 = pi
            alpha1 = pi
            alpha2 = pi
            # Set constants for vortex solution
            beta0 = halfpi
            beta1 = halfpi
            beta2 = halfpi
            
        elif bctype == 3:
            # BCTYPE = 3: FREE JET
            tsf.common_data.f = 0.0
            f = 0.0
            rtkpor = 0.0
            # Set constants for doublet solution
            alpha0 = halfpi
            alpha1 = halfpi
            alpha2 = halfpi
            # Set constants for vortex solution
            jet = 0.5
            beta0 = 0.0
            beta1 = 0.0
            beta2 = 0.0
            
        elif bctype == 4:
            # BCTYPE = 4: IDEAL SLOTTED WALL
            rtkpor = 0.0
            tsf.solver_data.fhinv = 1.0 / (f * h)
            tsf.solver_data.rtkpor = rtkpor
            # Set constants for doublet solution
            alpha0, alpha1, alpha2, omega0, omega1, omega2 = self._droots(f, rtkpor, halfpi, pi, twopi)
            # Set constants for vortex solution
            jet = 0.5
            beta0, beta1, beta2, psi0, psi1, psi2 = self._vroots(f, rtkpor, pi)
            
        elif bctype == 5:
            # BCTYPE = 5: IDEAL PERFORATED/POROUS WALL
            tsf.common_data.f = 0.0
            f = 0.0
            rtkpor = rtk / por
            tsf.solver_data.rtkpor = rtkpor
            # Set constants for doublet solution
            alpha0 = halfpi - math.atan(-rtkpor)
            alpha1 = alpha0
            alpha2 = alpha0
            # Set constants for vortex solution
            beta0 = math.atan(rtkpor)
            beta1 = beta0
            beta2 = beta1
            
        elif bctype == 6:
            # BCTYPE = 6: GENERAL HOMOGENEOUS WALL BOUNDARY CONDITION
            # Boundary condition is not operable yet in finite difference subroutines
            rtkpor = rtk / por
            tsf.solver_data.rtkpor = rtkpor
            alpha0, alpha1, alpha2, omega0, omega1, omega2 = self._droots(f, rtkpor, halfpi, pi, twopi)
            beta0, beta1, beta2, psi0, psi1, psi2 = self._vroots(f, rtkpor, pi)
            raise RuntimeError("BCTYPE=6 is not useable")
            
        else:
            raise ValueError(f"FARFLD: Invalid BCTYPE = {bctype}")
        
        # Store computed values back to Fortran modules
        tsf.solver_data.b_coef = b_coef
        tsf.solver_data.omega0 = omega0
        tsf.solver_data.omega1 = omega1
        tsf.solver_data.omega2 = omega2
        tsf.solver_data.jet = jet
        tsf.solver_data.psi0 = psi0
        tsf.solver_data.psi1 = psi1
        tsf.solver_data.psi2 = psi2
        tsf.solver_data.alpha0 = alpha0
        tsf.solver_data.alpha1 = alpha1
        tsf.solver_data.alpha2 = alpha2
        tsf.solver_data.beta0 = beta0
        tsf.solver_data.beta1 = beta1
        tsf.solver_data.beta2 = beta2
        
        # Compute functional forms for upstream and downstream boundary conditions
        # for doublet and vortex (for tunnel wall cases only - BCTYPE 2,3,4,5,6)
        xu_bc = (x_coords[imin - 1] - xsing) / (rtk * h)
        xd_bc = (x_coords[imax - 1] - xsing) / (rtk * h)
        
        # Doublet terms
        coef1 = 0.5 / ak / h
        arg0 = alpha0
        arg1 = pi - alpha1
        arg2 = twopi - alpha2
        exarg0 = math.exp(-arg0 * xd_bc)
        exarg1 = math.exp(arg1 * xu_bc)
        exarg2 = math.exp(arg2 * xu_bc)
        
        for j in range(jmin, jmax + 1):
            yj = y_coords[j - 1] / h  # Convert to 0-based indexing
            tsf.solver_data.ddown[j - 1] = coef1 * (b_coef + omega0 * math.cos(yj * arg0) * exarg0)
            tsf.solver_data.dup[j - 1] = -coef1 * ((1.0 - b_coef) * omega1 * math.cos(yj * arg1) * exarg1 +
                                                    omega2 * math.cos(yj * arg2) * exarg2)
        
        # Vortex terms
        arg0 = beta0
        arg1 = pi + beta1
        arg2 = pi - beta2
        exarg0 = math.exp(-arg0 * xd_bc)
        exarg1 = math.exp(-arg1 * xd_bc)
        exarg2 = math.exp(arg2 * xu_bc)
        
        for j in range(jmin, jmax + 1):
            yj = y_coords[j - 1] / h  # Convert to 0-based indexing
            term = yj
            if jet == 0.0:
                term = math.sin(yj * arg0) / arg0 if arg0 != 0.0 else yj
            tsf.solver_data.vdown[j - 1] = -0.5 * (1.0 - math.copysign(1.0, yj) + 
                                                    (1.0 - jet) * psi0 * term * exarg0 +
                                                    psi1 * math.sin(yj * arg1) * exarg1 / arg1)
            term = 0.0
            if jet != 0.0:
                term = jet * yj / (1.0 + f)
            tsf.solver_data.vup[j - 1] = -0.5 * (1.0 - term - psi2 * math.sin(yj * arg2) * exarg2 / arg2)

    def _droots(self, f: float, rtkpor: float, halfpi: float, pi: float, twopi: float) -> tuple:
        """
        Compute constants ALPHA0, ALPHA1, ALPHA2, OMEGA0, OMEGA1, OMEGA2
        Used in formula for doublet in slotted wind tunnel with subsonic freestream
        
        Parameters
        ----------
        f : float
            Slot parameter
        rtkpor : float
            RTK/POR ratio
        halfpi : float
            Pi/2
        pi : float
            Pi
        twopi : float
            2*Pi
            
        Returns
        -------
        tuple
            (alpha0, alpha1, alpha2, omega0, omega1, omega2)
        """
        error_local = 0.00001
        max_iterations = 100
        
        # Compute ALPHA0
        alpha0 = 0.0
        for _ in range(max_iterations):
            temp = alpha0
            q = f * temp - rtkpor
            alpha0 = halfpi - math.atan(q)
            dalpha = abs(alpha0 - temp)
            if dalpha < error_local:
                break
        else:
            raise RuntimeError("DROOTS: Non-convergence of iteration for ALPHA0")
        
        # Compute ALPHA1
        alpha1 = 0.0
        for _ in range(max_iterations):
            temp = alpha1
            q = f * (temp - pi) - rtkpor
            alpha1 = halfpi - math.atan(q)
            dalpha = abs(alpha1 - temp)
            if dalpha < error_local:
                break
        else:
            raise RuntimeError("DROOTS: Non-convergence of iteration for ALPHA1")
        
        # Compute ALPHA2
        alpha2 = 0.0
        for _ in range(max_iterations):
            temp = alpha2
            q = f * (temp - twopi) - rtkpor
            alpha2 = halfpi - math.atan(q)
            dalpha = abs(alpha2 - temp)
            if dalpha < error_local:
                break
        else:
            raise RuntimeError("DROOTS: Non-convergence of iteration for ALPHA2")
        
        # Compute OMEGA0, OMEGA1, OMEGA2
        temp = 1.0 / math.tan(alpha0)
        omega0 = 1.0 / (1.0 + f / (1.0 + temp * temp))
        temp = 1.0 / math.tan(alpha1)
        omega1 = 1.0 / (1.0 + f / (1.0 + temp * temp))
        temp = 1.0 / math.tan(alpha2)
        omega2 = 1.0 / (1.0 + f / (1.0 + temp * temp))
        
        return alpha0, alpha1, alpha2, omega0, omega1, omega2

    def _vroots(self, f: float, rtkpor: float, pi: float) -> tuple:
        """
        Compute constants BETA0, BETA1, BETA2, PSI0, PSI1, PSI2
        Used in formula for vortex in slotted wind tunnel with subsonic freestream
        
        Parameters
        ----------
        f : float
            Slot parameter
        rtkpor : float
            RTK/POR ratio
        pi : float
            Pi
            
        Returns
        -------
        tuple
            (beta0, beta1, beta2, psi0, psi1, psi2)
        """
        error_local = 0.00001
        max_iterations = 100
        
        # Calculate BETA0
        beta0 = 0.0
        for _ in range(max_iterations):
            temp = beta0
            q = -f * temp + rtkpor
            beta0 = math.atan(q)
            dbeta = abs(temp - beta0)
            if dbeta < error_local:
                break
        else:
            raise RuntimeError("VROOTS: Non-convergence of iteration for BETA0")
        
        # Calculate BETA1
        beta1 = 0.0
        for _ in range(max_iterations):
            temp = beta1
            q = -f * (temp + pi) + rtkpor
            beta1 = math.atan(q)
            dbeta = abs(beta1 - temp)
            if dbeta < error_local:
                break
        else:
            raise RuntimeError("VROOTS: Non-convergence of iteration for BETA1")
        
        # Calculate BETA2
        beta2 = 0.0
        for _ in range(max_iterations):
            temp = beta2
            q = -f * (temp - pi) + rtkpor
            beta2 = math.atan(q)
            dbeta = abs(beta2 - temp)
            if dbeta < error_local:
                break
        else:
            raise RuntimeError("VROOTS: Non-convergence of iteration for BETA2")
        
        # Compute PSI0, PSI1, PSI2
        temp = math.tan(beta0)
        psi0 = 1.0 / (1.0 + f / (1.0 + temp * temp))
        temp = math.tan(beta1)
        psi1 = 1.0 / (1.0 + f / (1.0 + temp * temp))
        temp = math.tan(beta2)
        psi2 = 1.0 / (1.0 + f / (1.0 + temp * temp))
        
        return beta0, beta1, beta2, psi0, psi1, psi2

    def setbc(self, ijump: int) -> None:
        """
        Set solution limits and apply body slope boundary conditions
        
        SETBC sets the limits on range of I and J for solution of the difference equations.
        The body slope boundary condition at the current X mesh points on the body are 
        multiplied by mesh spacing constants and entered into arrays FXUBC and FXLBC 
        for use in subroutine SYOR.
        
        Parameters
        ----------
        ijump : int
            If <= 0, set full range of I and J limits
            If > 0, only update body boundary conditions
        """
        # Get required variables from Fortran modules
        imin = tsf.common_data.imin
        imax = tsf.common_data.imax
        ile = tsf.common_data.ile
        ite = tsf.common_data.ite
        jmin = tsf.common_data.jmin
        jmax = tsf.common_data.jmax
        ak = tsf.common_data.ak
        alpha = tsf.common_data.alpha
        bctype = tsf.common_data.bctype
        por = tsf.common_data.por
        fxl = tsf.common_data.fxl
        fxu = tsf.common_data.fxu
        
        cyyblu = tsf.solver_data.cyyblu
        cyybud = tsf.solver_data.cyybud
        wslp = tsf.solver_data.wslp
        
        KSTEP = 1  # Step size for circulation-jump boundary update
        
        # Set limits on I and J indices
        if ijump <= 0:
            # IJUMP <= 0, use full range of I and J
            int_val = 0
            if ak < 0.0:
                int_val = 1
            iup = imin + 1 + int_val
            idown = imax - 1 + int_val
            
            jint = 0
            if bctype == 1 and ak > 0.0:
                jint = 1
            if bctype == 3:
                jint = 1
            if bctype == 5 and por > 1.5:
                jint = 1
            jbot = jmin + jint
            jtop = jmax - jint
            
            # Store back to Fortran modules
            tsf.common_data.iup = iup
            tsf.common_data.idown = idown
            tsf.common_data.jbot = jbot
            tsf.common_data.jtop = jtop
        
        # Airfoil body boundary condition
        # Zero elements in arrays for upper and lower body boundary conditions
        for i in range(imin, imax + 1):
            tsf.solver_data.fxlbc[i - 1] = 0.0  # Convert to 0-based indexing
            tsf.solver_data.fxubc[i - 1] = 0.0
        
        # Enter body slopes at mesh points on airfoil
        # into arrays for body boundary conditions
        nfoil = ite - ile + 1
        if1 = nfoil + KSTEP
        i = ite + 1
        
        for n in range(1, nfoil + 1):
            i = i - 1
            if1 = if1 - KSTEP
            # Fortran WSLP(I,2) -> wslp[i-1, 1], WSLP(I,1) -> wslp[i-1, 0]
            tsf.solver_data.fxlbc[i - 1] = cyyblu * (fxl[if1 - 1] - alpha + wslp[i - 1, 1])
            tsf.solver_data.fxubc[i - 1] = cyybud * (fxu[if1 - 1] - alpha + wslp[i - 1, 0])

    def difcoe(self) -> None:
        """
        Compute finite-difference coefficients in x and y directions
        
        This is a Python translation of the DIFCOE subroutine from solver_base.f90.
        
        Computes:
        - Coefficients for (P)X and (P)XX in x-direction
        - Coefficients for (P)YY in y-direction
        - Coefficients for velocity formulas (XDIFF, YDIFF)
        - Coefficients for extrapolation formulas for airfoil surface properties
        - Special difference coefficients for PYY for airfoil boundary condition
        """
        # Get required variables from Fortran modules
        imin = tsf.common_data.imin
        imax = tsf.common_data.imax
        jmin = tsf.common_data.jmin
        jmax = tsf.common_data.jmax
        jlow = tsf.common_data.jlow
        jup = tsf.common_data.jup
        x_coords = tsf.common_data.x
        y_coords = tsf.common_data.y
        gam1 = tsf.common_data.gam1
        ak = tsf.common_data.ak
        
        c2 = gam1 * 0.5
        
        # Coefficients for (P)X and (P)XX at IMIN
        tsf.solver_data.cxxl[imin - 1] = 0.0
        tsf.solver_data.cxxr[imin - 1] = 0.0
        tsf.solver_data.cxxc[imin - 1] = 0.0
        tsf.solver_data.cxl[imin - 1] = 0.0
        tsf.solver_data.cxr[imin - 1] = 0.0
        tsf.solver_data.cxc[imin - 1] = 0.0
        
        # Coefficients for (P)X and (P)XX from I=IMIN+1 to I=IMAX-1
        for i in range(imin + 1, imax):
            dxl = x_coords[i - 1] - x_coords[i - 2]  # X(I) - X(I-1)
            dxr = x_coords[i] - x_coords[i - 1]      # X(I+1) - X(I)
            dxc = 0.5 * (x_coords[i] - x_coords[i - 2])  # 0.5 * (X(I+1) - X(I-1))
            
            # For VC
            tsf.solver_data.c1[i - 1] = ak / dxc
            
            # For (P)X
            tsf.solver_data.cxl[i - 1] = -c2 / (dxl * dxc)
            tsf.solver_data.cxr[i - 1] = c2 / (dxr * dxc)
            tsf.solver_data.cxc[i - 1] = -tsf.solver_data.cxl[i - 1] - tsf.solver_data.cxr[i - 1]
            
            # For (P)XX
            tsf.solver_data.cxxl[i - 1] = 1.0 / dxl
            tsf.solver_data.cxxr[i - 1] = 1.0 / dxr
            tsf.solver_data.cxxc[i - 1] = tsf.solver_data.cxxl[i - 1] + tsf.solver_data.cxxr[i - 1]
        
        # Coefficients for (P)X and (P)XX at IMAX
        dx = x_coords[imax - 1] - x_coords[imax - 2]  # X(IMAX) - X(IMAX-1)
        q = 1.0 / (dx * dx)
        tsf.solver_data.c1[imax - 1] = ak / dx
        tsf.solver_data.cxl[imax - 1] = -c2 * q
        tsf.solver_data.cxr[imax - 1] = c2 * q
        tsf.solver_data.cxc[imax - 1] = 0.0
        tsf.solver_data.cxxl[imax - 1] = 1.0 / dx
        tsf.solver_data.cxxr[imax - 1] = 1.0 / dx
        tsf.solver_data.cxxc[imax - 1] = tsf.solver_data.cxxl[imax - 1] + tsf.solver_data.cxxr[imax - 1]
        
        # Coefficients for (P)YY at JMIN
        dyu_min = y_coords[jmin] - y_coords[jmin - 1]  # Y(JMIN+1) - Y(JMIN)
        tsf.solver_data.cyyd[jmin - 1] = 2.0 / dyu_min
        tsf.solver_data.cyyu[jmin - 1] = 2.0 / (dyu_min * dyu_min)
        tsf.solver_data.cyyc[jmin - 1] = tsf.solver_data.cyyu[jmin - 1]
        
        # Coefficients for (P)YY from J=JMIN+1 to J=JMAX-1
        for j in range(jmin + 1, jmax):
            dyd = y_coords[j - 1] - y_coords[j - 2]  # Y(J) - Y(J-1)
            dyu = y_coords[j] - y_coords[j - 1]      # Y(J+1) - Y(J)
            dyc = y_coords[j] - y_coords[j - 2]      # Y(J+1) - Y(J-1)
            tsf.solver_data.cyyd[j - 1] = 2.0 / (dyd * dyc)
            tsf.solver_data.cyyu[j - 1] = 2.0 / (dyu * dyc)
            tsf.solver_data.cyyc[j - 1] = tsf.solver_data.cyyd[j - 1] + tsf.solver_data.cyyu[j - 1]
        
        # Coefficients for (P)YY at JMAX
        dyd = y_coords[jmax - 1] - y_coords[jmax - 2]  # Y(JMAX) - Y(JMAX-1)
        tsf.solver_data.cyyd[jmax - 1] = 2.0 / (dyd * dyd)
        tsf.solver_data.cyyu[jmax - 1] = 2.0 / dyd
        tsf.solver_data.cyyc[jmax - 1] = tsf.solver_data.cyyd[jmax - 1]
        
        # Coefficients for velocity formulas
        for i in range(imin + 1, imax + 1):
            tsf.common_data.xdiff[i - 1] = 1.0 / (x_coords[i - 1] - x_coords[i - 2])  # 1/(X(I) - X(I-1))
        
        for j in range(jmin + 1, jmax + 1):
            tsf.common_data.ydiff[j - 1] = 1.0 / (y_coords[j - 1] - y_coords[j - 2])  # 1/(Y(J) - Y(J-1))
        
        # Coefficients for extrapolation formulas for airfoil surface properties
        dy_low = y_coords[jlow - 1] - y_coords[jlow - 2]
        dy_up = y_coords[jup] - y_coords[jup - 1]
        tsf.solver_data.cjlow = -y_coords[jlow - 2] / dy_low  # -Y(JLOW-1) / (Y(JLOW) - Y(JLOW-1))
        tsf.solver_data.cjlow1 = -y_coords[jlow - 1] / dy_low  # -Y(JLOW) / (Y(JLOW) - Y(JLOW-1))
        tsf.solver_data.cjup = y_coords[jup] / dy_up  # Y(JUP+1) / (Y(JUP+1) - Y(JUP))
        tsf.solver_data.cjup1 = y_coords[jup - 1] / dy_up  # Y(JUP) / (Y(JUP+1) - Y(JUP))
        
        # Special difference coefficients for PYY for airfoil boundary condition
        # Upper surface
        tsf.solver_data.cyybud = -2.0 / (y_coords[jup] + y_coords[jup - 1])  # -2/(Y(JUP+1) + Y(JUP))
        tsf.solver_data.cyybuc = -tsf.solver_data.cyybud / dy_up  # -CYYBUD / (Y(JUP+1) - Y(JUP))
        tsf.solver_data.cyybuu = tsf.solver_data.cyybuc
        
        # Lower surface
        tsf.solver_data.cyyblu = -2.0 / (y_coords[jlow - 1] + y_coords[jlow - 2])  # -2/(Y(JLOW) + Y(JLOW-1))
        tsf.solver_data.cyyblc = tsf.solver_data.cyyblu / dy_low  # CYYBLU / (Y(JLOW) - Y(JLOW-1))
        tsf.solver_data.cyybld = tsf.solver_data.cyyblc

    def run_fortran_solver(self) -> None:
        """Run Fortran solver"""
        # Scale variables to similarity form
        self.scale()

        # Set far field boundary conditions
        self.farfld()
        
        # Compute finite difference coefficients
        self.difcoe()
        
        # Set boundary conditions
        self.setbc(0)
        
        # Solve transonic flow equations
        # tsf.main_iteration.solve()
        self.solve()
    
    def solve(self) -> None:
        """
        Main iteration loop: solver, convergence, and flow updates
        
        This is a Python translation of the SOLVE subroutine from main_iteration.f90.
        Internal Fortran subroutines (RECIRC, SYOR, REDUB, RESET, VWEDGE, SETBC) 
        are still called via the tsf library.
        
        Optimized using NumPy vectorized operations for better performance.
        """
        # Get required variables from Fortran modules (cache locally to reduce access overhead)
        y_coords = tsf.common_data.y
        ak = tsf.common_data.ak
        bctype = tsf.common_data.bctype
        nwdge = tsf.common_data.nwdge
        iprter = tsf.common_data.iprter
        maxit = tsf.common_data.maxit
        imin = tsf.common_data.imin
        jmin = tsf.common_data.jmin
        jmax = tsf.common_data.jmax
        iup = tsf.common_data.iup
        idown = tsf.common_data.idown
        jtop = tsf.common_data.jtop
        jbot = tsf.common_data.jbot
        we = tsf.common_data.we
        cverge = tsf.common_data.cverge
        dverge = tsf.common_data.dverge
        flag_output = tsf.common_data.flag_output
        
        clfact = tsf.solver_data.clfact
        cmfact = tsf.solver_data.cmfact
        kstep = tsf.solver_data.kstep
        
        # Get direct references to Fortran arrays (avoid repeated module lookups)
        p_arr = tsf.solver_data.p
        pold_arr = tsf.solver_data.pold
        emu_arr = tsf.solver_data.emu
        theta_arr = tsf.solver_data.theta
        c1_arr = tsf.solver_data.c1
        
        # Constants
        NDUB = 25  # Number of iterations between updating doublet strength
        
        # Initialize using NumPy slice assignment (much faster than loops)
        tsf.solver_data.abort1 = False
        pold_arr[:, :] = 0.0
        emu_arr[:, :] = 0.0
        
        # Calculate maximum iterations based on refinement level
        maxitm = maxit
        
        # Set relaxation parameter based on refinement level
        kk = 2  # 0-based index for WE(3) in Fortran
        wep = we[kk]
        tsf.solver_data.wi = 1.0 / wep
        
        if flag_output == 1:
            print(f"\n   WE = {wep:7.4f}     EPS = {tsf.common_data.eps:8.4f}     MAXIT FOR THIS MESH = {maxitm:4d}")
            print(f"\n  ITER     CL        CM    IERR JERR    ERROR")
            print(f"   IRL  JRL    BIGRL        ERCIRC")
        
        # Precompute index arrays for circulation update (only if needed)
        # This avoids recomputing indices in every iteration
        if ak >= 0.0 and bctype == 1:
            # Build index mapping once
            j_indices = []
            i_indices = []
            jk_indices = []
            ik_indices = []
            
            ik = iup - imin
            for i in range(iup, idown + 1):
                ik = ik + kstep
                jk = jbot - jmin
                for j in range(jbot, jtop + 1):
                    jinc = kstep
                    if y_coords[j - 1] < 0.0 and y_coords[j] > 0.0:
                        jinc = 2 * kstep - 1
                    jk = jk + jinc
                    j_indices.append(j - 1)
                    i_indices.append(i - 1)
                    jk_indices.append(jk - 1)
                    ik_indices.append(ik - 1)
            
            # Convert to numpy arrays for vectorized access
            j_idx = np.array(j_indices, dtype=np.intp)
            i_idx = np.array(i_indices, dtype=np.intp)
            jk_idx = np.array(jk_indices, dtype=np.intp)
            ik_idx = np.array(ik_indices, dtype=np.intp)
            do_circ_update = True
        else:
            do_circ_update = False
        
        # Precompute slice indices
        j_slice = slice(jmin - 1, jmax)  # Python 0-based slice for J range
        i2 = 1  # Fixed index for I2 (0-based)
        
        # Main iteration loop
        converged = False
        abort1 = False
        
        for iter_num in range(1, maxitm + 1):
            
            # Initialize EMU and POLD arrays using vectorized operations
            # Fortran: POLD(JMIN:JMAX, I2) = P(JMIN:JMAX, IUP-1), EMU(JMIN:JMAX, I2) = 0.0
            pold_arr[j_slice, i2] = p_arr[j_slice, iup - 2]
            emu_arr[j_slice, i2] = 0.0
            
            # Set EMU for subsonic flow (vectorized)
            if ak <= 0.0:
                emu_arr[j_slice, i2] = c1_arr[1]  # C1(2) -> c1[1]
            
            # Set output flag for this iteration
            outerr = (iter_num % iprter == 0) or (iter_num == 1)
            
            # Update circulation-jump boundary
            dcirc = tsf.main_iteration.recirc()
            
            # Perform SOR sweep
            i1_fortran = 1
            i2_fortran = 2
            result = tsf.main_iteration.syor(i1_fortran, i2_fortran, outerr)
            bigrl, irl, jrl, ierror, jerror, error = result[0], result[1], result[2], result[3], result[4], result[5]
            
            # Update circulation for subsonic freestream flow (vectorized)
            if do_circ_update:
                # Vectorized update: P[j,i] += DCIRC * THETA[jk,ik]
                p_arr[j_idx, i_idx] += dcirc * theta_arr[jk_idx, ik_idx]
            
            # Update doublet strength every NDUB iterations
            if iter_num % NDUB == 0:
                tsf.main_iteration.redub()
            
            # Reset boundary conditions
            tsf.main_iteration.reset()
            
            # Compute viscous wedge if enabled
            am1 = xshk = thamax = zeta = nvwprt = nishk = None
            if nwdge > 0:
                am1, xshk, thamax, zeta, nvwprt, nishk = self._vwedge()
                self.setbc(1)
            
            # Print iteration results if needed
            if outerr and flag_output == 1:
                cl_local = tsf.solver_base.lift(clfact)
                cm_local = tsf.solver_base.pitch(cmfact)
                ercirc = abs(dcirc)
                
                print(f" {iter_num:4d}{cl_local:10.5f}{cm_local:10.5f}{ierror:5d}{jerror:5d}{error:13.4e}"
                      f"{irl:4d}{jrl:4d}{bigrl:13.4e}{ercirc:13.4e}")
                
                # Output viscous wedge quantities if enabled
                if nwdge > 0 and nvwprt is not None:
                    print("          COMPUTED VISCOUS WEDGE QUANTITIES")
                    
                    # Upper surface shocks
                    if nvwprt[0] > 0:
                        print(" UPPER SHOCK        X/C          MACH NO         THETA          ZETA")
                        for n in range(nvwprt[0]):
                            if am1[0, n] > 1.0:
                                tha = thamax[0, n] * 57.29578
                                print(f"{n + 1:9d}{xshk[0, n]:15.5f}{am1[0, n]:15.5f}{tha:15.5f}{zeta[0, n]:15.5f}")
                            else:
                                print(f"{n + 1:9d}     WEAK SHOCK, NO WEDGE INCLUDED")
                    
                    # Lower surface shocks
                    if nvwprt[0] > 0:
                        print(" LOWER SHOCK        X/C          MACH NO         THETA          ZETA")
                        for n in range(nvwprt[0]):
                            if am1[1, n] > 1.0:
                                tha = thamax[1, n] * 57.29578
                                print(f"{n + 1:9d}{xshk[1, n]:15.5f}{am1[1, n]:15.5f}{tha:15.5f}{zeta[1, n]:15.5f}")
                            else:
                                print(f"{n + 1:9d}     WEAK SHOCK, NO WEDGE INCLUDED")
                    
                    if nishk == 0:
                        print("     NO VISCOUS WEDGE, SINCE NO SHOCKS EXIST ")
                    
                    print(f"\n  ITER     CL        CM    IERR JERR    ERROR    IRL  JRL    BIGRL        ERCIRC")
            
            # Check convergence
            if error <= cverge:
                converged = True
                if flag_output == 1:
                    print(f"\n\n                    ........SOLUTION CONVERGED........")
                    print(f"Solution converged after {iter_num} iterations.")
                break
            
            # Check divergence
            if error >= dverge:
                abort1 = True
                tsf.solver_data.abort1 = True
                if flag_output == 1:
                    print(f"\n\n                    ******  SOLUTION DIVERGED  ******")
                    print(f"Solution diverged after {iter_num} iterations.")
                break
        
        # Handle case where iteration limit is reached
        if not converged and not abort1 and flag_output == 1:
            print(f"\n\n                    ******  ITERATION LIMIT REACHED  ******")
            print(f"Iteration limit reached after {maxitm} iterations.")
        
    def compute_data_summary(self) -> None:
        """Compute data summary"""
        alpha = tsf.common_data.alpha
        vfact = tsf.solver_data.vfact
        clfact = tsf.solver_data.clfact
        cmfact = tsf.solver_data.cmfact
        
        # Compute lift and pitch coefficients
        self.core.data_summary['alpha'] = alpha * vfact
        self.core.data_summary['mach'] = tsf.common_data.emach
        self.core.data_summary['cl'] = tsf.solver_base.lift(clfact)
        self.core.data_summary['cm'] = tsf.solver_base.pitch(cmfact)
        self.core.data_summary['cpstar'] = tsf.solver_data.cpstar
    
    def compute_drag_by_momentum_integral(self, sonvel: float, yfact: float, delta: float) -> None:
        """
        Compute drag coefficient by momentum integral method
        Integrate around a contour enclosing the body and along all shocks inside the contour
        
        This is a Python translation of the CDCOLE subroutine from solver_base.f90.
        
        Parameters
        ----------
        sonvel : float
            Speed of sound
        yfact : float  
            Y-coordinate scaling factor
        delta : float
            Airfoil maximum thickness
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
        
        # Main computation starts here
        gam123 = gam1 * 2.0 / 3.0
        iskold = 0
        
        # Set contour boundary locations
        
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
            ibow = find_shock_location(iup, istop, jup, sonvel)
            
            if ibow < 0:
                # Shock is too close to body to do contour integral
                ule = tsf.solver_base.px(ile, jup)
                cd = self._compute_drag_by_surface_pressure(cdfact)
                
                if self.core.config['flag_output_summary']:
                    with open(os.path.join(self.core.output_dir, "smry.out"), 'a') as f:
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
                isk = find_new_shock_location(iskold, j, sonvel)
                if isk < 0:
                    break
            
            # Search down shock to find tip of subsonic region
            isk = ibow
            jb = jlow + 2
            for j in range(jmin, jlow + 1):
                jj = jlow - j + jmin
                jb -= 1
                iskold = isk
                isk = find_new_shock_location(iskold, jj, sonvel)
                if isk < 0:
                    break
            
            # Save I location of bow shock wave on lower boundary
            ibow = iskold
        
        # Downstream boundary
        id_downstream = (ite + imax) // 2
        if tsf.solver_base.px(ite + 1, jup) >= sonvel:
            # Trailing edge is supersonic. Place downstream boundary ahead of trailing edge to avoid tail shock
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
            # If flow is supersonic, use backward difference formula
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
                isk = find_new_shock_location(iskold, j, sonvel)
                xi[l] = y_coords[j - 1]  # Convert to 0-based indexing
                arg[l] = (tsf.solver_base.px(isk + 1, j) - tsf.solver_base.px(isk - 2, j))**3
                l += 1
            sum_val = trap_integration(xi, arg, l)
            cdsk = -gam1 / 6.0 * cdfact * sum_val
            cdwave += cdsk
            print_shock_information(xi, arg, l, nshock, cdsk, lprt1, 
                                  self.core.output_dir, self.core.config['flag_output_summary'])
        
        # Integrate along shocks above airfoil
        istart = ile
        
        # Loop to find and process all shocks above airfoil
        while True:
            isk = find_shock_location(istart, ite, jup, sonvel)
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
                isk = find_new_shock_location(iskold, jsk, sonvel)
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
            print_shock_information(xi, arg, l, nshock, cdsk, lprt1,
                                  self.core.output_dir, self.core.config['flag_output_summary'])
            if lprt1 == 1:
                lprt2 = 1
        
        # Integrate along shocks below airfoil
        istart = ile
        
        # Loop to find and process all shocks below airfoil
        while True:
            isk = find_shock_location(istart, ite, jlow, sonvel)
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
                isk = find_new_shock_location(iskold, jsk, sonvel)
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
            print_shock_information(xi, arg, l, nshock, cdsk, lprt1,
                                  self.core.output_dir, self.core.config['flag_output_summary'])
            if lprt1 == 1:
                lprt2 = 1
        
        # Integration along shock waves is complete
        # Print CD information
        xu_loc = x_coords[iu - 1]  # Convert to 0-based indexing
        xd_loc = x_coords[id_downstream - 1]  # Convert to 0-based indexing
        yt_loc = y_coords[jt - 1] * yfact  # Convert to 0-based indexing
        yb_loc = y_coords[jb - 1] * yfact  # Convert to 0-based indexing
        cdc = cdup + cdtop + cdbot + cddown + cdbody
        cd = cdc + cdwave
        
        self.core.data_summary['cd'] = cd
        self.core.data_summary['cd_int'] = cdc
        self.core.data_summary['cd_wave'] = cdwave
        self.core.data_summary['cd_body'] = cdbody
        
        # Write drag coefficient breakdown
        if self.core.config['flag_output_summary']:
            with open(os.path.join(self.core.output_dir, "smry.out"), 'a') as f:
                
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
    
    def _compute_drag_by_surface_pressure(self, cdfact_in: float) -> float:
        """Compute drag coefficient by surface pressure integration"""
        n_mesh_points = tsf.common_data.n_mesh_points
        ile = tsf.common_data.ile
        ite = tsf.common_data.ite
        jup = tsf.common_data.jup
        jlow = tsf.common_data.jlow
        x_coords = tsf.common_data.x
        fxu = tsf.common_data.fxu
        fxl = tsf.common_data.fxl
        
        cjup = tsf.solver_data.cjup
        cjup1 = tsf.solver_data.cjup1
        cjlow = tsf.solver_data.cjlow
        cjlow1 = tsf.solver_data.cjlow1
        
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

    def _px(self, i: int, j: int) -> float:
        """
        Compute U = DP/DX at point I,J (Python version of solver_base.PX)
        
        Parameters
        ----------
        i : int
            X index (1-based Fortran indexing)
        j : int
            Y index (1-based Fortran indexing)
        
        Returns
        -------
        float
            Velocity component dP/dX at point (i,j)
        """
        imin = tsf.common_data.imin
        imax = tsf.common_data.imax
        xdiff = tsf.common_data.xdiff
        p_arr = tsf.solver_data.p
        
        # Convert to 0-based indexing for array access
        i0 = i - 1
        j0 = j - 1
        
        if i == imin:
            # Upstream boundary
            return (1.5 * xdiff[i] * (p_arr[j0, i0 + 1] - p_arr[j0, i0]) -
                    0.5 * xdiff[i + 1] * (p_arr[j0, i0 + 2] - p_arr[j0, i0 + 1]))
        elif i == imax:
            # Downstream boundary
            return (1.5 * xdiff[i0] * (p_arr[j0, i0] - p_arr[j0, i0 - 1]) -
                    0.5 * xdiff[i0 - 1] * (p_arr[j0, i0 - 1] - p_arr[j0, i0 - 2]))
        else:
            # Interior mesh point
            pji = p_arr[j0, i0]
            return 0.5 * (xdiff[i] * (p_arr[j0, i0 + 1] - pji) + 
                         xdiff[i0] * (pji - p_arr[j0, i0 - 1]))

    def _findsk(self, istart: int, iend: int, j: int, sonvel: float) -> int:
        """
        Find shock location along line J (Python version of solver_base.FINDSK)
        
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
        u2 = self._px(isk, j)
        
        while True:
            isk += 1
            u1 = u2
            u2 = self._px(isk, j)
            if u1 > sonvel and u2 <= sonvel:
                return isk
            if isk >= iend:
                return -iend

    def _wangle(self, am2: float, nw: int, g: float) -> float:
        """
        Compute wedge angle for viscous correction (Python version of WANGLE)
        
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

    def _vwedge(self):
        """
        Compute Murman or Yoshihara viscous wedge (Python version of VWEDGE)
        
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
            isk = self._findsk(istart, ite, j_surface, sonvel)
            
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
            v1 = self._px(isk - 1, j_surface)
            xshk[m, n] = x_coords[isk - 2] + (sonvel - v1) / ((self._px(isk, j_surface) - v1) * xdiff[isk - 1])
            
            # Compute flow properties 3 points upstream
            isk3 = isk - 3
            u = self._px(isk3, j_surface)
            am1[m, n] = self.core.emach1(u, delta)
            am1sq = am1[m, n] ** 2
            
            if am1sq <= 1.0:
                jmp = 1
            else:
                thamax[m, n] = self._wangle(am1sq, nwdge, gam1) * sign
                
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

