"""
TSFoil solver management module

Responsible for numerical solving and post-processing computation, including:
- Fortran solver invocation
- Data summary computation  
- Drag coefficient calculation (momentum integral method)
- Flow field analysis
"""

import numpy as np
from typing import Final
from .core import TSFoilCore
from .viscous import ViscousCorrection
from .pre_processing import PreProcessing
from .post_processing import PostProcessing

from ._fortran import tsf


class SolverManager:
    """Solver manager class"""
    
    def __init__(self, core: TSFoilCore, 
                 pre_processing: PreProcessing, 
                 post_processing: PostProcessing,
                 viscous_correction: ViscousCorrection):

        self.core = core
        self.viscous_correction = viscous_correction
        self.pre_processing = pre_processing
        self.post_processing = post_processing
        
        # Solver attributes
        self.kstep : Final[int] = 1 # Step size for circulation-jump boundary update
        self.ndub : Final[int] = 25 # Number of iterations between updating doublet strength

    def recirc(self) -> float:
        """
        Update circulation-jump boundary after Kutta or M divergence.
        
        RECIRC computes:
        1.) Jump in P at trailing edge = CIRCTE
        2.) Circulation for farfield boundary = CIRCFF
        3.) Jump in P along slit Y=0, X > 1 by linear interpolation between CIRCTE and CIRCFF
        
        Returns
        -------
        dcirc : float
            Circulation change
        """
        # Get variables from Fortran modules
        x = tsf.common_data.x
        imax = tsf.common_data.imax
        ite = tsf.common_data.ite
        jup = tsf.common_data.jup
        jlow = tsf.common_data.jlow
        clset = tsf.common_data.clset
        kutta = tsf.common_data.kutta
        wcirc = tsf.common_data.wcirc
        
        p = tsf.solver_data.p
        pjump = tsf.solver_data.pjump
        clfact = tsf.solver_data.clfact
        circff = tsf.solver_data.circff
        circte = tsf.solver_data.circte
        cjup = tsf.solver_data.cjup
        cjup1 = tsf.solver_data.cjup1
        cjlow = tsf.solver_data.cjlow
        cjlow1 = tsf.solver_data.cjlow1
        
        # Compute jump in potential at trailing edge (0-based indexing)
        cteold = circte
        # Fortran: P(JUP, ITE) -> p[jup-1, ite-1]
        pup = cjup * p[jup - 1, ite - 1] - cjup1 * p[jup, ite - 1]
        plow = cjlow * p[jlow - 1, ite - 1] - cjlow1 * p[jlow - 2, ite - 1]
        circte = pup - plow
        
        # Compute far field circulation
        circo = circff
        if kutta:
            circff = (1.0 - wcirc) * circo + circte * wcirc
        else:
            circff = 0.5 * clset / clfact
        
        # Fix jump in P at airfoil trailing edge if KUTTA=.FALSE.
        # and lift of airfoil exceeds CLSET
        if not kutta:
            circte = circff
        dcirc = circte - cteold
        
        # Set jump in P along Y = 0, X > 1 by linear interpolation (vectorized)
        factor = (circff - circte) / (x[imax - 1] - 1.0)
        idx_slice = slice(ite - 1, imax)  # ITE to IMAX (0-based)
        pjump[idx_slice] = circte + (x[idx_slice] - 1.0) * factor
        
        # Update Fortran module variables
        tsf.solver_data.circte = circte
        tsf.solver_data.circff = circff
        
        return dcirc

    def redub(self) -> None:
        """
        Update doublet strength DUB for nonlinear correction.
        
        For lifting free air flows, doublet strength is set equal to model volume.
        For other flows, the nonlinear contribution is added.
        
        Optimized using NumPy vectorized operations.
        """
        # Get variables from Fortran modules
        y = tsf.common_data.y
        imin = tsf.common_data.imin
        imax = tsf.common_data.imax
        jmin = tsf.common_data.jmin
        jmax = tsf.common_data.jmax
        gam1 = tsf.common_data.gam1
        xdiff = tsf.common_data.xdiff
        bctype = tsf.common_data.bctype
        vol = tsf.common_data.vol
        
        p = tsf.solver_data.p
        circff = tsf.solver_data.circff
        
        # For lifting free air flows with circulation, set doublet strength equal to model volume
        if bctype == 1 and abs(circff) >= 0.0001:
            tsf.solver_data.dub = vol
            return
        
        # Compute double integral of U*U over mesh domain for doublet strength
        # U = (∂P/∂x) is centered midway between X mesh points.
        # Vectorized implementation using NumPy
        
        # Index ranges (0-based)
        j_slice = slice(jmin - 1, jmax)  # JMIN to JMAX
        i_start = imin - 1
        i_end = imax - 1  # IEND = IMAX - 1
        
        # Compute ∂P/∂x for all i in range [imin-1, iend) and all j in [jmin-1, jmax)
        # Shape: (jmax - jmin + 1, iend - i_start)
        dp_dx = p[j_slice, i_start + 1:i_end + 1] - p[j_slice, i_start:i_end]
        dp_dx_sq = dp_dx * dp_dx  # (∂P/∂x)²
        
        # Y-coordinates for integration
        y_coords = y[j_slice]  # Shape: (jmax - jmin + 1,)
        
        # Compute Y-direction differences for trapezoidal rule
        dy = np.diff(y_coords)  # Shape: (jmax - jmin,)
        
        # Trapezoidal integration in Y-direction for each i column
        # sum = 0.5 * sum((y[j+1] - y[j]) * (arg[j+1] + arg[j]))
        # Vectorized: sum over j for each i
        y_integrals = 0.5 * np.sum(dy[:, np.newaxis] * (dp_dx_sq[1:, :] + dp_dx_sq[:-1, :]), axis=0)
        
        # X-direction weighting with xdiff
        xdiff_weights = xdiff[i_start + 1:i_end + 1]  # xdiff[i+1] for i in range
        dblsum = np.sum(y_integrals * xdiff_weights)
        
        # Apply scaling factor and update doublet strength
        dblsum = gam1 * 0.25 * dblsum
        tsf.solver_data.dub = vol + dblsum

    def reset(self) -> None:
        """
        Reset far-field boundary values after mesh change or Kutta.
        
        Updates far field boundary conditions for subsonic freestream flows.
        
        Optimized using NumPy vectorized operations.
        """
        # Get variables from Fortran modules
        imin = tsf.common_data.imin
        imax = tsf.common_data.imax
        jmin = tsf.common_data.jmin
        jmax = tsf.common_data.jmax
        jup = tsf.common_data.jup
        bctype = tsf.common_data.bctype
        
        p = tsf.solver_data.p
        circff = tsf.solver_data.circff
        dub = tsf.solver_data.dub
        
        dup = tsf.solver_data.dup
        ddown = tsf.solver_data.ddown
        dtop = tsf.solver_data.dtop
        dbot = tsf.solver_data.dbot
        vup = tsf.solver_data.vup
        vdown = tsf.solver_data.vdown
        vtop = tsf.solver_data.vtop
        vbot = tsf.solver_data.vbot
        
        # Set boundary conditions at upstream and downstream ends
        # Build k indices with special jump at jup (vectorized)
        n_j = jmax - jmin + 1
        j_arr = np.arange(jmin, jmax + 1)  # 1-based j values
        
        # k starts at jmin, increments by kstep each step, with extra jump at jup
        # k[0] = jmin - kstep + kstep = jmin
        # k[i] = jmin + i*kstep + (kstep-1 if any j <= jup in [jmin, jmin+i])
        k_arr = jmin + np.arange(n_j) * self.kstep
        # Add extra jump for indices where j >= jup
        k_arr[j_arr >= jup] += self.kstep - 1
        
        # Convert to 0-based indices
        j_idx = j_arr - 1  # 0-based j indices
        k_idx = k_arr - 1  # 0-based k indices
        
        # Vectorized assignment for upstream boundary (imin)
        p[j_idx, imin - 1] = circff * vup[k_idx] + dub * dup[k_idx]
        # Vectorized assignment for downstream boundary (imax)
        p[j_idx, imax - 1] = circff * vdown[k_idx] + dub * ddown[k_idx]
        
        # Update boundary conditions on top and bottom
        if bctype == 1:
            n_i = imax - imin + 1
            # k indices: k = imin + (i - imin) * kstep = i * kstep (for kstep=1)
            k_arr_i = imin + np.arange(n_i) * self.kstep
            k_idx_i = k_arr_i - 1  # 0-based k indices
            i_idx = np.arange(imin - 1, imax)  # 0-based i indices
            
            # Vectorized assignment for bottom boundary (jmin)
            p[jmin - 1, i_idx] = circff * vbot[k_idx_i] + dub * dbot[k_idx_i]
            # Vectorized assignment for top boundary (jmax)
            p[jmax - 1, i_idx] = circff * vtop[k_idx_i] + dub * dtop[k_idx_i]

    def bcend(self, i: int, diag: np.ndarray, rhs: np.ndarray) -> None:
        """
        Insert wall boundary conditions.
        
        Parameters
        ----------
        i : int
            Current x-index (1-based Fortran indexing)
        diag : np.ndarray
            Diagonal elements of tridiagonal matrix
        rhs : np.ndarray
            Right-hand side vector
        """
        # Get variables from Fortran modules
        x = tsf.common_data.x
        y = tsf.common_data.y
        iup = tsf.common_data.iup
        idown = tsf.common_data.idown
        jmin = tsf.common_data.jmin
        jmax = tsf.common_data.jmax
        jtop = tsf.common_data.jtop
        jbot = tsf.common_data.jbot
        ak = tsf.common_data.ak
        xdiff = tsf.common_data.xdiff
        bctype = tsf.common_data.bctype
        por = tsf.common_data.por
        
        p = tsf.solver_data.p
        cyyd = tsf.solver_data.cyyd
        cyyu = tsf.solver_data.cyyu
        circff = tsf.solver_data.circff
        fhinv = tsf.solver_data.fhinv
        
        # 0-based indices
        i0 = i - 1
        jbot0 = jbot - 1
        jtop0 = jtop - 1
        jmin0 = jmin - 1
        jmax0 = jmax - 1
        
        if bctype == 1:
            # BCTYPE = 1, FREE AIR
            # Dirichlet boundary condition for subsonic freestream
            if ak > 0.0:
                return
            # Neumann boundary condition for supersonic freestream
            rtk = np.sqrt(abs(ak))
            dfacl = -cyyd[jbot0] * rtk * xdiff[i0]
            dfacu = -cyyu[jtop0] * rtk * xdiff[i0]
            rfacl = dfacl * (p[jmin0, i0] - p[jmin0, i0 - 1])
            rfacu = dfacu * (p[jmax0, i0] - p[jmax0, i0 - 1])
            # Apply Neumann
            diag[jbot0] += dfacl
            diag[jtop0] += dfacu
            rhs[jbot0] -= rfacl - cyyd[jbot0] * p[jbot0 - 1, i0]
            rhs[jtop0] -= rfacu - cyyu[jtop0] * p[jtop0 + 1, i0]
            
        elif bctype == 2:
            # BCTYPE = 2, SOLID WALL
            # Neumann boundary condition = 0, no modification needed
            return
            
        elif bctype == 3:
            # BCTYPE = 3, FREE JET
            # Dirichlet boundary condition
            if ak < 0.0:
                pjmin = 0.0
                pjmax = 0.0
            else:
                pjmin = -0.75 * circff
                pjmax = -0.25 * circff
            # Apply Dirichlet
            rhs[jbot0] -= cyyd[jbot0] * (pjmin - p[jbot0 - 1, i0])
            rhs[jtop0] -= cyyu[jtop0] * (pjmax - p[jtop0 + 1, i0])
            
        elif bctype == 4:
            # BCTYPE = 4, IDEAL SLOTTED WALL
            dfacl = -fhinv * cyyd[jbot0]
            dfacu = -fhinv * cyyu[jtop0]
            if ak < 0.0:
                rfacl = dfacl * p[jbot0, i0]
                rfacu = dfacu * p[jtop0, i0]
            else:
                rfacl = dfacl * (0.75 * circff + p[jbot0, i0])
                rfacu = dfacu * (0.25 * circff + p[jtop0, i0])
            # Apply Neumann
            diag[jbot0] += dfacl
            diag[jtop0] += dfacu
            rhs[jbot0] -= rfacl - cyyd[jbot0] * p[jbot0 - 1, i0]
            rhs[jtop0] -= rfacu - cyyu[jtop0] * p[jtop0 + 1, i0]
            
        elif bctype == 5:
            # BCTYPE = 5, POROUS/PERFORATED WALL
            if por > 1.5:
                # Dirichlet boundary condition for POR > 1.5
                if i != iup:
                    return
                # Integrate PX using old values of potential
                pjmin = p[jmin0, iup - 1]
                term = -0.5 / (por * (y[jmin0] - y[jmin0 + 1]))
                for ii in range(iup - 1, idown):  # IUP to IDOWN (0-based)
                    p[jmin0, ii] = p[jmin0, ii - 1] - term * (x[ii] - x[ii - 1]) * \
                                   (p[jmin0, ii] + p[jmin0, ii - 1] - p[jmin0 + 1, ii] - p[jmin0 + 1, ii - 1])
                pjmax = p[jmax0, iup - 1]
                term = 0.5 / (por * (y[jmax0] - y[jmax0 - 1]))
                for ii in range(iup - 1, idown):
                    p[jmax0, ii] = p[jmax0, ii - 1] - term * (x[ii] - x[ii - 1]) * \
                                   (p[jmax0, ii] + p[jmax0, ii - 1] - p[jmax0 - 1, ii] - p[jmax0 - 1, ii - 1])
                rhs[jbot0] -= cyyd[jbot0] * (p[jbot0 - 1, i0] - pjmin)
                rhs[jtop0] -= cyyu[jtop0] * (p[jtop0 + 1, i0] - pjmax)
            else:
                # Neumann boundary condition for POR < 1.5
                dfacl = -cyyd[jbot0] * por * xdiff[i0]
                dfacu = -cyyu[jtop0] * por * xdiff[i0]
                rfacl = dfacl * (p[jmin0, i0] - p[jmin0, i0 - 1])
                rfacu = dfacu * (p[jmax0, i0] - p[jmax0, i0 - 1])
                # Apply Neumann
                diag[jbot0] += dfacl
                diag[jtop0] += dfacu
                rhs[jbot0] -= rfacl - cyyd[jbot0] * p[jbot0 - 1, i0]
                rhs[jtop0] -= rfacu - cyyu[jtop0] * p[jtop0 + 1, i0]
                
        elif bctype == 6:
            raise NotImplementedError("BCTYPE=6 is not implemented")

    def syor(self, i1: int, i2: int, outerr: bool) -> tuple:
        """
        Perform one SOR sweep computing residuals and updating P.
        
        Parameters
        ----------
        i1 : int
            First index for potential values (1-based)
        i2 : int
            Second index for potential values (1-based)
        outerr : bool
            Whether to compute and output error
            
        Returns
        -------
        tuple
            (bigrl, irl, jrl, ierror, jerror, error)
            - bigrl: Maximum residual value
            - irl, jrl: Location of maximum residual
            - ierror, jerror: Location of maximum error
            - error: Maximum error
        """
        # Get variables from Fortran modules
        x = tsf.common_data.x
        iup = tsf.common_data.iup
        idown = tsf.common_data.idown
        ile = tsf.common_data.ile
        ite = tsf.common_data.ite
        jmin = tsf.common_data.jmin
        jmax = tsf.common_data.jmax
        jup = tsf.common_data.jup
        jlow = tsf.common_data.jlow
        jtop = tsf.common_data.jtop
        jbot = tsf.common_data.jbot
        ak = tsf.common_data.ak
        fcr = tsf.common_data.fcr
        eps = tsf.common_data.eps
        
        p = tsf.solver_data.p
        pjump = tsf.solver_data.pjump
        emu = tsf.solver_data.emu
        pold = tsf.solver_data.pold
        wi = tsf.solver_data.wi
        fxubc = tsf.solver_data.fxubc
        fxlbc = tsf.solver_data.fxlbc
        
        cxl = tsf.solver_data.cxl
        cxc = tsf.solver_data.cxc
        cxr = tsf.solver_data.cxr
        cxxl = tsf.solver_data.cxxl
        cxxc = tsf.solver_data.cxxc
        cxxr = tsf.solver_data.cxxr
        c1 = tsf.solver_data.c1
        cyyc = tsf.solver_data.cyyc
        cyyd = tsf.solver_data.cyyd
        cyyu = tsf.solver_data.cyyu
        cyybuc = tsf.solver_data.cyybuc
        cyybuu = tsf.solver_data.cyybuu
        cyyblc = tsf.solver_data.cyyblc
        cyybld = tsf.solver_data.cyybld
        
        # Allocate working arrays
        diag = np.zeros(jmax + 1)
        rhs = np.zeros(jmax + 1)
        vc = np.zeros(jmax + 1)
        sub = np.zeros(jmax + 1)
        sup = np.zeros(jmax + 1)
        save_var = np.zeros(jmax + 1)
        
        # Initialize outputs
        bigrl = 0.0
        irl = 0
        jrl = 0
        ierror = 0
        jerror = 0
        error = 0.0
        
        # 0-based indices
        jbot0 = jbot - 1
        jtop0 = jtop - 1
        jup0 = jup - 1
        jlow0 = jlow - 1
        jmin0 = jmin - 1
        jmax0 = jmax - 1
        
        im2 = iup - 1
        if ak < 0.0:
            im2 = iup - 2
        
        # Main loop over x-direction
        for i in range(iup, idown + 1):
            i0 = i - 1  # 0-based index
            epsx = eps / ((x[i0] - x[i0 - 1]) ** 2)
            
            # Compute VC = 1 - M**2 (vectorized for j in [jbot, jtop])
            j_range = slice(jbot0, jtop0 + 1)
            vc[j_range] = c1[i0] - (cxl[i0] * pold[j_range, i2 - 1] + 
                                     cxc[i0] * p[j_range, i0] + 
                                     cxr[i0] * p[j_range, i0 + 1])
            emu[j_range, i1 - 1] = 0.0
            pold[j_range, i1 - 1] = p[j_range, i0]
            
            # Set EMU where VC < 0
            emu[j_range, i1 - 1] = np.where(vc[j_range] < 0.0, vc[j_range], 0.0)
            
            if not fcr:
                emu[j_range, i2 - 1] = emu[j_range, i1 - 1]
            
            # Compute elements of matrix (vectorized)
            diag[j_range] = ((emu[j_range, i1 - 1] - vc[j_range]) * cxxc[i0] * wi + 
                             emu[j_range, i2 - 1] * cxxr[i0 - 1] - cyyc[j_range])
            sup[j_range] = cyyd[j_range]
            sub[j_range] = cyyu[j_range]
            
            # Compute residual (vectorized)
            rhs[j_range] = -((vc[j_range] - emu[j_range, i1 - 1]) * 
                             (cxxl[i0] * p[j_range, i0 - 1] - 
                              cxxc[i0] * p[j_range, i0] + 
                              cxxr[i0] * p[j_range, i0 + 1]))
            
            rhs[j_range] -= (emu[j_range, i2 - 1] * 
                             (cxxl[i0 - 1] * p[j_range, im2 - 1] - 
                              cxxc[i0 - 1] * p[j_range, i0 - 1] + 
                              cxxr[i0 - 1] * p[j_range, i0]))
            
            # Interior points for y-direction
            ja = jbot + 1
            jb = jtop - 1
            if ja <= jb:
                j_interior = slice(ja - 1, jb)
                rhs[j_interior] -= (cyyd[j_interior] * p[ja - 2:jb - 1, i0] - 
                                    cyyc[j_interior] * p[j_interior, i0] + 
                                    cyyu[j_interior] * p[ja:jb + 1, i0])
            
            # Boundary terms for JBOT
            rhs[jbot0] -= (-cyyc[jbot0] * p[jbot0, i0] + cyyu[jbot0] * p[jbot0 + 1, i0])
            if jbot != jmin:
                rhs[jbot0] -= cyyd[jbot0] * p[jbot0 - 1, i0]
            
            # Boundary terms for JTOP
            rhs[jtop0] -= (cyyd[jtop0] * p[jtop0 - 1, i0] - cyyc[jtop0] * p[jtop0, i0])
            if jtop != jmax:
                rhs[jtop0] -= cyyu[jtop0] * p[jtop0 + 1, i0]
            
            # Check for airfoil B.C. and Kutta slice
            if i >= ile and i <= ite:
                # Airfoil B.C. - Upper surface
                j0 = jup0
                diag[j0] += cyyc[j0] - cyybuc
                sup[j0] = 0.0
                sub[j0] = cyybuu
                rhs[j0] += (cyyd[j0] * p[j0 - 1, i0] - cyyc[j0] * p[j0, i0] + 
                           cyyu[j0] * p[j0 + 1, i0] -
                           (-cyybuc * p[j0, i0] + cyybuu * p[j0 + 1, i0] + fxubc[i0]))
                
                # Airfoil B.C. - Lower surface
                j0 = jlow0
                diag[j0] += cyyc[j0] - cyyblc
                sup[j0] = cyybld
                sub[j0] = 0.0
                rhs[j0] += (cyyd[j0] * p[j0 - 1, i0] - cyyc[j0] * p[j0, i0] + 
                           cyyu[j0] * p[j0 + 1, i0] -
                           (-cyyblc * p[j0, i0] + cyybld * p[j0 - 1, i0] + fxlbc[i0]))
                           
            elif i > ite:
                # Kutta slice change
                rhs[jlow0] += cyyu[jlow0] * pjump[i0]
                rhs[jup0] -= cyyd[jup0] * pjump[i0]
            
            # Insert wall B.C.
            self.bcend(i, diag, rhs)
            
            # Compute max residual
            if outerr:
                arhs = np.abs(rhs[j_range])
                max_idx = np.argmax(arhs)
                if arhs[max_idx] > bigrl:
                    bigrl = arhs[max_idx]
                    irl = i
                    jrl = jbot + max_idx
            
            # Add PXT (artificial dissipation)
            diag[j_range] -= epsx
            rhs[j_range] -= epsx * (p[j_range, i0 - 1] - pold[j_range, i2 - 1])
            
            # Solve tridiagonal matrix equation
            # Forward elimination
            dnom = 1.0 / diag[jbot0]
            save_var[jbot0] = sub[jbot0] * dnom
            rhs[jbot0] = rhs[jbot0] * dnom
            
            for j in range(jbot + 1, jtop + 1):
                j0 = j - 1
                denominator = diag[j0] - sup[j0] * save_var[j0 - 1]
                dnom = 1.0 / denominator
                save_var[j0] = sub[j0] * dnom
                rhs[j0] = (rhs[j0] - sup[j0] * rhs[j0 - 1]) * dnom
                if abs(rhs[j0]) < 1.0e-30:
                    rhs[j0] = 0.0
            
            # Back-substitution
            for k in range(1, jtop - jbot + 1):
                j = jtop - k
                j0 = j - 1
                rhs[j0] = rhs[j0] - save_var[j0] * rhs[j0 + 1]
                if abs(rhs[j0]) < 1.0e-30:
                    rhs[j0] = 0.0
            
            # Compute new P
            p[j_range, i0] += rhs[j_range]
            
            # Compute max error
            arhs = np.abs(rhs[j_range])
            max_idx = np.argmax(arhs)
            if arhs[max_idx] > error:
                error = arhs[max_idx]
                ierror = i
                jerror = jbot + max_idx
            
            # Supersonic freestream flow condition
            if ak <= 0.0 and i == idown - 1:
                p[jmin0:jmax0 + 1, idown] = p[jmin0:jmax0 + 1, idown - 2]
            
            # Swap I1 and I2 indices
            i1, i2 = i2, i1
            im2 = i - 1
        
        return (bigrl, irl, jrl, ierror, jerror, error)

    def run_solver(self) -> None:
        """
        Main iteration loop: solver, convergence, and flow updates
        
        This is a Python translation of the SOLVE subroutine from main_iteration.f90.
        Core solver functions (RECIRC, SYOR, REDUB, RESET, BCEND) are now implemented 
        in Python with NumPy vectorized operations for better performance.
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
        
        # Get direct references to Fortran arrays (avoid repeated module lookups)
        p_arr = tsf.solver_data.p
        pold_arr = tsf.solver_data.pold
        emu_arr = tsf.solver_data.emu
        theta_arr = tsf.solver_data.theta
        c1_arr = tsf.solver_data.c1
                
        # Initialize using NumPy slice assignment (much faster than loops)
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
                ik = ik + self.kstep
                jk = jbot - jmin
                for j in range(jbot, jtop + 1):
                    jinc = self.kstep
                    if y_coords[j - 1] < 0.0 and y_coords[j] > 0.0:
                        jinc = 2 * self.kstep - 1
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
            j_idx = i_idx = jk_idx = ik_idx = None  # Avoid UnboundLocalError
        
        # Precompute slice indices
        j_slice = slice(jmin - 1, jmax)  # Python 0-based slice for J range
        i2 = 1  # Fixed index for I2 (0-based)
        
        # Cache Fortran function references to avoid repeated attribute lookups
        _syor = tsf.main_iteration.syor
        _recirc = tsf.main_iteration.recirc
        _redub = tsf.main_iteration.redub
        _reset = tsf.main_iteration.reset
        
        '''
        # [SLOW] Python method references for recirc, redub, reset
        # _syor = self.syor
        # _recirc = self.recirc
        # _redub = self.redub
        # _reset = self.reset
        '''
        
        # Cache method references
        _compute_vwedge = self.viscous_correction.compute_vwedge if nwdge > 0 else None
        _setup_body_boundary = self.pre_processing.setup_body_boundary if nwdge > 0 else None
        _lift = self.post_processing.lift
        _pitch = self.post_processing.pitch
        
        # Precompute conditions
        do_vwedge = nwdge > 0
        do_output = flag_output == 1
        ak_subsonic = ak <= 0.0
        c1_val = c1_arr[1] if ak_subsonic else None  # C1(2) -> c1[1]
        iup_m2 = iup - 2  # Precompute iup-2 index
        
        # Main iteration loop
        converged = False
        abort1 = False
        
        for iter_num in range(1, maxitm + 1):
            
            # Initialize EMU and POLD arrays using vectorized operations
            # Fortran: POLD(JMIN:JMAX, I2) = P(JMIN:JMAX, IUP-1), EMU(JMIN:JMAX, I2) = 0.0
            pold_arr[j_slice, i2] = p_arr[j_slice, iup_m2]
            emu_arr[j_slice, i2] = 0.0
            
            # Set EMU for subsonic flow (vectorized)
            if ak_subsonic:
                emu_arr[j_slice, i2] = c1_val
            
            # Set output flag for this iteration
            outerr = (iter_num % iprter == 0) or (iter_num == 1)
            
            # Update circulation-jump boundary
            dcirc = _recirc()
            
            # Perform SOR sweep
            result = _syor(1, 2, outerr)
            bigrl, irl, jrl, ierror, jerror, error = result[0], result[1], result[2], result[3], result[4], result[5]
            
            # Update circulation for subsonic freestream flow (vectorized)
            if do_circ_update:
                # Vectorized update: P[j,i] += DCIRC * THETA[jk,ik]
                p_arr[j_idx, i_idx] += dcirc * theta_arr[jk_idx, ik_idx]
            
            # Update doublet strength every ndub iterations
            if iter_num % self.ndub == 0:
                _redub()
            
            # Reset boundary conditions
            _reset()
            
            # Compute viscous wedge if enabled
            if do_vwedge:
                am1, xshk, thamax, zeta, nvwprt, nishk = _compute_vwedge()
                _setup_body_boundary(update_only=True)
            else:
                am1 = xshk = thamax = zeta = nvwprt = nishk = None
            
            # Print iteration results if needed
            if outerr and do_output:
                cl_local = _lift(clfact)
                cm_local = _pitch(cmfact)
                ercirc = abs(dcirc)
                
                print(f" {iter_num:4d}{cl_local:10.5f}{cm_local:10.5f}{ierror:5d}{jerror:5d}{error:13.4e}"
                      f"{irl:4d}{jrl:4d}{bigrl:13.4e}{ercirc:13.4e}")
                
                # Output viscous wedge quantities if enabled
                if do_vwedge and nvwprt is not None:
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
                if do_output:
                    print(f"\n\n                    ........SOLUTION CONVERGED........")
                    print(f"Solution converged after {iter_num} iterations.")
                break
            
            # Check divergence
            if error >= dverge:
                abort1 = True
                if do_output:
                    print(f"\n\n                    ******  SOLUTION DIVERGED  ******")
                    print(f"Solution diverged after {iter_num} iterations.")
                break
        
        # Handle case where iteration limit is reached
        if not converged and not abort1 and do_output:
            print(f"\n\n                    ******  ITERATION LIMIT REACHED  ******")
            print(f"Iteration limit reached after {maxitm} iterations.")
