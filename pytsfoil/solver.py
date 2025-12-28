"""
TSFoil solver management module

Responsible for numerical solving and post-processing computation, including:
- Fortran solver invocation
- Data summary computation  
- Drag coefficient calculation (momentum integral method)
- Flow field analysis
"""

import numpy as np
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
    
    def run_fortran_solver(self) -> None:
        """Run Fortran solver"""
        # Scale variables to similarity form
        tsf.solver_functions.scale()

        # Set far field boundary conditions
        tsf.solver_functions.farfld()
        
        self._compute_geometry_derivatives()
        
        # Compute finite difference coefficients
        tsf.solver_base.difcoe()
        
        # Set boundary conditions
        tsf.solver_functions.setbc(0)
        
        # Solve transonic flow equations
        tsf.main_iteration.solve()
    
    def _compute_geometry_derivatives(self) -> None:
        """Compute geometry derivatives (internal call to geometry processor methods)"""
        # Call geometry processor methods here
        # Note: In the refactored version, geometry calculation should be done in the geometry processor
        # This is just for compatibility with the original code
        pass
    
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

