'''
Boundary condition module for TSFoil solver
'''

import math
import numpy as np
from .core import TSFoilCore
from ._fortran import tsf

class PreProcessing:
    """Pre-processing class"""

    def __init__(self, core: TSFoilCore):
        """Initialize boundary condition class"""
        self.core = core

    def compute_circulation_angle(self) -> None:
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
        pi = np.pi
        twopi = 2.0 * np.pi
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

    def apply_similarity_scaling(self) -> None:
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

    def setup_farfield_boundary(self) -> None:
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
        pi = np.pi
        twopi = 2.0 * np.pi
        halfpi = 0.5 * np.pi
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
                self.compute_circulation_angle()
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
            alpha0, alpha1, alpha2, omega0, omega1, omega2 = self._compute_doublet_parameters(f, rtkpor, halfpi, pi, twopi)
            # Set constants for vortex solution
            jet = 0.5
            beta0, beta1, beta2, psi0, psi1, psi2 = self._compute_vortex_parameters(f, rtkpor, pi)
            
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
            alpha0, alpha1, alpha2, omega0, omega1, omega2 = self._compute_doublet_parameters(f, rtkpor, halfpi, pi, twopi)
            beta0, beta1, beta2, psi0, psi1, psi2 = self._compute_vortex_parameters(f, rtkpor, pi)
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

    def _compute_doublet_parameters(self, f: float, rtkpor: float, halfpi: float, pi: float, twopi: float) -> tuple:
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

    def _compute_vortex_parameters(self, f: float, rtkpor: float, pi: float) -> tuple:
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

    def compute_fd_coefficients(self) -> None:
        """
        Compute finite-difference coefficients in x and y directions.
        
        This function is called ONCE during the preprocessing/initialization phase
        (before the main iteration solver). All coefficients are computed based on
        grid coordinates (x_coords, y_coords) and remain constant during iterations.  
        Computed Coefficients:
        ----------------------
        +-------------------------+------------------------+----------------------------+
        | Coefficient Type        | Variables              | Purpose                    |
        +-------------------------+------------------------+----------------------------+
        | X-direction 1st deriv.  | CXL, CXR, CXC          | Compute dP/dx              |
        | X-direction 2nd deriv.  | CXXL, CXXR, CXXC       | Compute d²P/dx²            |
        | Y-direction 2nd deriv.  | CYYD, CYYU, CYYC       | Compute d²P/dy²            |
        | Velocity formula coeff. | XDIFF, YDIFF           | Compute velocity field     |
        | Airfoil extrapolation   | CJLOW, CJLOW1,         | Extrapolate airfoil        |
        |                         | CJUP, CJUP1            | surface properties         |
        | Boundary condition      | CYYBUD, CYYBUC, CYYBUU | Special PYY coefficients   |
        | (upper/lower surface)   | CYYBLU, CYYBLC, CYYBLD | for airfoil BC             |
        +-------------------------+------------------------+----------------------------+
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

    def setup_body_boundary(self, update_only: bool = False) -> None:
        """
        Set solution limits and apply body slope boundary conditions
        
        Sets the limits on range of I and J for solution of the difference equations.
        The body slope boundary condition at the current X mesh points on the body are 
        multiplied by mesh spacing constants and entered into arrays FXUBC and FXLBC 
        for use in subroutine SYOR.
        
        Parameters
        ----------
        update_only : bool, optional
            If False (default), set full range of I/J limits and update boundary conditions.
            If True, only update body boundary conditions (used after viscous wedge correction).
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
        
        # Set limits on I and J indices (only during initialization)
        if not update_only:
            int_val = 1 if ak < 0.0 else 0
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
        
        # Airfoil body boundary condition (vectorized)
        # Zero the boundary condition arrays
        tsf.solver_data.fxlbc[imin - 1:imax] = 0.0
        tsf.solver_data.fxubc[imin - 1:imax] = 0.0
        
        # Compute body slopes at mesh points on airfoil using NumPy
        nfoil = ite - ile + 1
        
        # Index slices (0-based)
        # airfoil_slice: mesh indices [ile-1, ile, ..., ite-1]
        # foil_data_slice: airfoil data indices [0, 1, ..., nfoil-1]
        airfoil_slice = slice(ile - 1, ite)
        foil_data_slice = slice(0, nfoil)
        
        # Get numpy array views
        fxl_arr = np.asarray(fxl)
        fxu_arr = np.asarray(fxu)
        wslp_arr = np.asarray(wslp)
        
        # Vectorized boundary condition computation
        fxlbc_vals = cyyblu * (fxl_arr[foil_data_slice] - alpha + wslp_arr[airfoil_slice, 1])
        fxubc_vals = cyybud * (fxu_arr[foil_data_slice] - alpha + wslp_arr[airfoil_slice, 0])
        
        # Assign to Fortran arrays
        tsf.solver_data.fxlbc[airfoil_slice] = fxlbc_vals.astype(np.float32)
        tsf.solver_data.fxubc[airfoil_slice] = fxubc_vals.astype(np.float32)

