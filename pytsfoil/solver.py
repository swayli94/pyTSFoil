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

    def run_solver(self) -> None:
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
        
        # Get direct references to Fortran arrays (avoid repeated module lookups)
        p_arr = tsf.solver_data.p
        pold_arr = tsf.solver_data.pold
        emu_arr = tsf.solver_data.emu
        theta_arr = tsf.solver_data.theta
        c1_arr = tsf.solver_data.c1
        
        # Constants
        NDUB = 25  # Number of iterations between updating doublet strength
        
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
                am1, xshk, thamax, zeta, nvwprt, nishk = self.viscous_correction.compute_vwedge()
                self.pre_processing.setup_body_boundary(update_only=True)
            
            # Print iteration results if needed
            if outerr and flag_output == 1:
                cl_local = self.post_processing.lift(clfact)
                cm_local = self.post_processing.pitch(cmfact)
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
                if flag_output == 1:
                    print(f"\n\n                    ******  SOLUTION DIVERGED  ******")
                    print(f"Solution diverged after {iter_num} iterations.")
                break
        
        # Handle case where iteration limit is reached
        if not converged and not abort1 and flag_output == 1:
            print(f"\n\n                    ******  ITERATION LIMIT REACHED  ******")
            print(f"Iteration limit reached after {maxitm} iterations.")
