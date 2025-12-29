"""
TSFoil output processing module

Responsible for various data output and printing, including:
- Flow field data output (Tecplot format)
- Shock data output
- Summary information printing
- Result file generation
"""

import os
import numpy as np
from .core import TSFoilCore

from ._fortran import tsf


class OutputHandler:
    """Output handler class"""
    
    def __init__(self, core: TSFoilCore):
        """
        Initialize output handler
        
        Parameters
        ----------
        core: TSFoilCore
            Core data object
        """
        self.core = core
    
    def output_field(self) -> None:
        """
        Output flow field to Tecplot format file
        Translated from OUTPUT_FIELD in io_module.f90
        """
        # Get mesh dimensions and coordinates
        imin = tsf.common_data.imin
        imax = tsf.common_data.imax
        jmin = tsf.common_data.jmin
        jmax = tsf.common_data.jmax
        iup = tsf.common_data.iup
        idown = tsf.common_data.idown
        
        x_coords = tsf.common_data.x
        y_coords = tsf.common_data.y
        
        # Get solver data
        P = tsf.solver_data.p  # Pressure array
        vfact = tsf.solver_data.vfact
        c1 = tsf.solver_data.c1
        cxl = tsf.solver_data.cxl
        cxc = tsf.solver_data.cxc
        cxr = tsf.solver_data.cxr
        cpfact = tsf.solver_data.cpfact
        
        # Get configuration parameters
        emach = tsf.common_data.emach
        alpha = tsf.common_data.alpha
        delta = tsf.common_data.delta
                        
        # Open output file
        if self.core.config['flag_output_field']:
            
            field_data = np.zeros((jmax - jmin + 1, imax - imin + 1, 6))
            
            with open(os.path.join(self.core.output_dir, "field.dat"), 'w') as f:
                # Write Tecplot header
                f.write('# Flow types: -1=Outside domain, 0=Elliptic, 1=Parabolic, 2=Hyperbolic, 3=Shock\n')
                f.write(f'# Mach = {emach:10.6f}\n')
                f.write(f'# Alpha = {alpha * vfact:10.6f}\n')
                f.write(f'# CPFACT = {cpfact:10.6f}\n')
                
                f.write('VARIABLES = "X", "Y", "Mach", "Cp", "P", "FlowType"\n')
                f.write(f'ZONE I= {imax - imin + 1:5d} J= {jmax - jmin + 1:5d} F= POINT\n')
                
            # Initialize VT array for flow type calculation  
            # Size it to accommodate all possible j indices
            vt = np.zeros((jmax + 1, 2))  # VT(J,1) and VT(J,2)
            for j in range(jmin, jmax + 1):
                vt[j, 0] = c1[1]  # VT(J,1) = C1(2) in Fortran (1-based) -> C1[1] in Python (0-based)
            
            # Write field data in point format
            for j in range(jmin, jmax + 1):
                for i in range(imin, imax + 1):
                    # Calculate flow variables
                    u = self.core.px(i, j)  # Computes U = DP/DX at point I,J
                    em = self.core.emach1(u, delta)  # Computes Mach number from U
                    cp_val = -2.0 * u * cpfact  # CPFACT is a scaling factor for pressure coefficient
                    
                    # Calculate flow type for points within the computational domain
                    if imin <= i <= imax and jmin <= j <= jmax:
                        # Flow type classification using PRTMC logic
                        vt[j, 1] = vt[j, 0]  # VT(J,2) = VT(J,1)
                        # Convert to 0-based indexing for accessing arrays
                        i_py = i - 1
                        j_py = j - 1
                        
                        if i >= iup and i <= idown:
                            # VT(J,1) = C1(I) - (CXL(I)*P(J,I-1) + CXC(I)*P(J,I) + CXR(I)*P(J,I+1))
                            vt[j, 0] = (c1[i_py] - (cxl[i_py] * P[j_py, i_py - 1] + 
                                                cxc[i_py] * P[j_py, i_py] + 
                                                cxr[i_py] * P[j_py, i_py + 1]))
                            
                            if vt[j, 0] > 0.0:
                                if vt[j, 1] < 0.0:
                                    # Shock point
                                    flow_type_num = 3.0
                                else:
                                    # Elliptic point (subsonic)
                                    flow_type_num = 0.0
                            else:
                                if vt[j, 1] < 0.0:
                                    # Hyperbolic point (supersonic)
                                    flow_type_num = 2.0
                                else:
                                    # Parabolic point (sonic)
                                    flow_type_num = 1.0
                        else:
                            # Outside computational domain
                            flow_type_num = -1.0
                    else:
                        # Outside computational domain
                        flow_type_num = -1.0
                    
                    # Convert to 0-based indexing for accessing coordinates
                    i_py = i - 1
                    j_py = j - 1
                    p_val = P[j_py, i_py]
                    
                    # Store data in field_data
                    field_data[j_py, i_py, 0] = x_coords[i_py]
                    field_data[j_py, i_py, 1] = y_coords[j_py]
                    field_data[j_py, i_py, 2] = em
                    field_data[j_py, i_py, 3] = cp_val
                    field_data[j_py, i_py, 4] = p_val
                    field_data[j_py, i_py, 5] = flow_type_num
                    
                    # Write data to file
                    with open(os.path.join(self.core.output_dir, "field.dat"), 'a') as f:
                        f.write(f' {x_coords[i_py]:15.12f}')
                        f.write(f' {y_coords[j_py]:15.12f}')
                        f.write(f' {em:15.12f}')
                        f.write(f' {cp_val:15.12f}')
                        f.write(f' {p_val:15.12f}')
                        f.write(f' {flow_type_num:15.12f}\n')
                        
            self.core.data_summary['field_data'] = field_data
            
            if self.core.config['flag_print_info']:
                print('Output to field.dat: Cp, Mach, Potential field data')
    
    def output_shock(self) -> None:
        """
        Output shock data, translating Fortran PRINT_SHOCK subroutine
        This function computes pressure coefficients and Mach numbers on airfoil surface
        
        Parameters
        ----------
        file_name : str, optional
            Output filename. Defaults to "cpxs.dat"
        """        
        # Get required parameters from Fortran modules
        imin = tsf.common_data.imin
        imax = tsf.common_data.imax
        ile = tsf.common_data.ile
        ite = tsf.common_data.ite
        jlow = tsf.common_data.jlow
        jup = tsf.common_data.jup
        
        # Get required data from solver_data
        cpfact = tsf.solver_data.cpfact

        cpstar = tsf.solver_data.cpstar
        cjlow = tsf.solver_data.cjlow
        cjlow1 = tsf.solver_data.cjlow1
        cjup = tsf.solver_data.cjup
        cjup1 = tsf.solver_data.cjup1
        
        # Get coordinate arrays
        x_coords = tsf.common_data.x
        y_coords = tsf.common_data.y
        
        # Get configuration parameters
        delta = tsf.common_data.delta
        emach = tsf.common_data.emach
        phys = tsf.common_data.phys
        
        # Initialize variables
        iem = 0
        cj01 = -y_coords[jlow-1] / (y_coords[jup-1] - y_coords[jlow-1])  # Convert to 0-based indexing
        cj02 = y_coords[jup-1] / (y_coords[jup-1] - y_coords[jlow-1])
        
        # Arrays to store results
        n_points = imax - imin + 1
        em1l = np.zeros(n_points)
        em1u = np.zeros(n_points)
        cpu = np.zeros(n_points)
        cpl = np.zeros(n_points)
        
        # Main computation loop
        for i_p1 in range(imin, imax + 1):  # Fortran 1-based indexing
            # Convert to 0-based indexing for Python arrays
            i_py = i_p1 - 1
            
            # Calculate UL_P1
            ul_p1 = cjlow * self.core.px(i_p1, jlow) - cjlow1 * self.core.px(i_p1, jlow - 1)
            if i_p1 > ite:
                ul_p1 = cj01 * self.core.px(i_p1, jup) + cj02 * self.core.px(i_p1, jlow)
            if i_p1 < ile:
                ul_p1 = cj01 * self.core.px(i_p1, jup) + cj02 * self.core.px(i_p1, jlow)
            
            # Store CPL value and compute Mach number
            cpl[i_py] = -2.0 * ul_p1 * cpfact
            em1l[i_py] = self.core.emach1(ul_p1, delta)
            if em1l[i_py] > 1.3:
                iem = 1
            
            # Calculate UU_P1
            uu_p1 = cjup * self.core.px(i_p1, jup) - cjup1 * self.core.px(i_p1, jup + 1)
            if i_p1 > ite:
                uu_p1 = ul_p1
            if i_p1 < ile:
                uu_p1 = ul_p1
            
            # Store CPU value and compute Mach number
            cpu[i_py] = -2.0 * uu_p1 * cpfact
            em1u[i_py] = self.core.emach1(uu_p1, delta)
            if em1u[i_py] > 1.3:
                iem = 1
        
        self.core.data_summary['cpu'] = cpu
        self.core.data_summary['cpl'] = cpl
        self.core.data_summary['mau'] = em1u
        self.core.data_summary['mal'] = em1l
        
        # Output summary file
        if self.core.config['flag_output_summary']:
            with open(os.path.join(self.core.output_dir, "smry.out"), 'a') as f:

                # Check for detached shock
                if cpl[imin-1] < cpstar and cpl[imin] > cpstar:
                    f.write('0 ***** CAUTION *****\n')
                    f.write(' DETACHED SHOCK WAVE UPSTREAM OF X-MESH,SOLUTION TERMINATED.\n')
                    return
                
                # Mach number warning
                if iem == 1 and phys:
                    f.write('0 ***** CAUTION *****\n')
                    f.write(' MAXIMUM MACH NUMBER EXCEEDS 1.3\n')
                    f.write(' SHOCK JUMPS IN ERROR IF UPSTREAM NORMAL MACH NUMBER GREATER THAN 1.3\n')
        
        # Output airfoil surface data to file
        if self.core.config['flag_output_shock']:
            
            with open(os.path.join(self.core.output_dir, "cpxs.dat"), 'w') as f:
                
                # Write coefficients
                f.write(f'# Output to cpxs.dat: Cp, Mach distribution on a x-line (Y=0) \n')
                f.write(f'# Mach = {emach:10.6f}\n')
                f.write(f'# Alpha = {self.core.data_summary["alpha"]:10.6f}\n')
                f.write(f'# CL = {self.core.data_summary["cl"]:10.6f}\n')
                f.write(f'# CM = {self.core.data_summary["cm"]:10.6f}\n')
                f.write(f'# Cp* = {self.core.data_summary["cpstar"]:10.6f}\n')
                
                # Write variable names
                f.write('VARIABLES = "X", "Cp-up", "M-up", "Cp-low", "M-low"\n')
                
                # Write data with Fortran-style formatting
                for i_p1 in range(imin, imax + 1):
                    i_py = i_p1 - 1
                    f.write(f"  {self.core.mesh['xx'][i_py]:10.5f}")
                    f.write(f"  {self.core.data_summary['cpu'][i_py]:10.5f}")
                    f.write(f"  {self.core.data_summary['mau'][i_py]:10.5f}")
                    f.write(f"  {self.core.data_summary['cpl'][i_py]:10.5f}")
                    f.write(f"  {self.core.data_summary['mal'][i_py]:10.5f}\n")
        
            if self.core.config['flag_print_info']:
                print('Output to cpxs.dat: Cp, Mach distribution on a x-line (Y=0)')
    
    def print_summary(self) -> None:
        """
        Main print driver: prints configuration parameters and calls specialized subroutines
        Translated from PRINT subroutine in io_module.f90
        """
        # Get required variables from Fortran modules
        phys = tsf.common_data.phys
        simdef = tsf.common_data.simdef
        bctype = tsf.common_data.bctype
        fcr = tsf.common_data.fcr
        kutta = tsf.common_data.kutta
        emach = tsf.common_data.emach
        delta = tsf.common_data.delta
        ak = tsf.common_data.ak
        
        # Get solver data variables
        dub = tsf.solver_data.dub
        cpfact = tsf.solver_data.cpfact
        cdfact = tsf.solver_data.cdfact
        cmfact = tsf.solver_data.cmfact
        clfact = tsf.solver_data.clfact
        yfact = tsf.solver_data.yfact
        vfact = tsf.solver_data.vfact
        sonvel = tsf.solver_data.sonvel
        
        # Write summary file
        if self.core.config['flag_output_summary']:
            
            with open(os.path.join(self.core.output_dir, "smry.out"), 'w') as f:
                
                f.write(f'# Output to smry.out: Summary of the calculation\n')
                f.write(f'# Mach = {emach:10.6f}\n')
                f.write(f'# Alpha = {self.core.data_summary["alpha"]:10.6f}\n')
                f.write(f'# CL = {self.core.data_summary["cl"]:10.6f}\n')
                f.write(f'# CM = {self.core.data_summary["cm"]:10.6f}\n')
                f.write(f'# Cp* = {self.core.data_summary["cpstar"]:10.6f}\n')
                f.write(f'# DELTA = {delta:10.6f}\n')
                f.write(f'# K = {ak:10.6f}\n')
                f.write(f'# DOUBLET STRENGTH = {dub:10.6f}\n')
                f.write(f'# CPFACT = {cpfact:10.6f}\n')
                f.write(f'# CDFACT = {cdfact:10.6f}\n')
                f.write(f'# CMFACT = {cmfact:10.6f}\n')
                f.write(f'# CLFACT = {clfact:10.6f}\n')
                f.write(f'# YFACT = {yfact:10.6f}\n')
                f.write(f'# VFACT = {vfact:10.6f}\n')
                f.write(f'# SONVEL = {sonvel:10.6f}\n')
                f.write(f'# BCTYPE = {bctype:10.6f}\n')
                f.write(f'# FCR = {fcr:10.6f}\n')
                f.write(f'# KUTTA = {kutta:10.6f}\n')
                f.write(f'# PHYS = {phys:10.6f}\n')
                f.write(f'# SIMDEF = {simdef:10.6f}\n')
                f.write(f'# SCALED POR = {tsf.common_data.por:10.6f}\n')

                # Print similarity/physical variables information
                if phys:
                    f.write('0 PRINTOUT IN PHYSICAL VARIABLES. \n')
                else:
                    f.write('0 PRINTOUT IN SIMILARITY VARIABLES.\n')
                
                # Print similarity parameter definition
                if simdef == 1:
                    f.write('0 DEFINITION OF SIMILARITY PARAMETERS BY COLE\n')
                elif simdef == 2:
                    f.write('0 DEFINITION OF SIMILARITY PARAMETERS BY SPREITER\n')
                elif simdef == 3:
                    f.write('0 DEFINITION OF SIMILARITY PARAMETERS BY KRUPP\n')
                
                # Print boundary condition information
                if bctype == 1:
                    f.write('0 BOUNDARY CONDITION FOR FREE AIR\n')
                elif bctype == 2:
                    f.write('0 BOUNDARY CONDITION FOR SOLID WALL\n')
                elif bctype == 3:
                    f.write('0 BOUNDARY CONDITION FOR FREE JET\n')
                
                # Print difference equation information
                if fcr:
                    f.write('0 DIFFERENCE EQUATIONS ARE FULLY CONSERVATIVE.\n')
                else:
                    f.write('0 DIFFERENCE EQUATIONS ARE NOT CONSERVATIVE AT SHOCK.\n')
                
                # Print Kutta condition information
                if kutta:
                    f.write('0 KUTTA CONDITION IS ENFORCED.\n')
                else:
                    f.write('0 LIFT COEFFICIENT SPECIFIED BY USER.\n')
        
        # Print shock and Mach number on Y=0 line
        self.output_shock()
        
        # Output field data
        self.output_field()
    
    def get_summary_info(self) -> dict:
        """
        Get computation results summary
        
        Returns
        -------
        summary: dict
            Dictionary containing main computation results
        """
        return {
            'alpha': self.core.data_summary.get('alpha', 0),
            'mach': self.core.data_summary.get('mach', 0),
            'cl': self.core.data_summary.get('cl', 0),
            'cm': self.core.data_summary.get('cm', 0),
            'cd': self.core.data_summary.get('cd', 0),
            'cpstar': self.core.data_summary.get('cpstar', 0),
            'cd_wave': self.core.data_summary.get('cd_wave', 0),
            'cd_body': self.core.data_summary.get('cd_body', 0),
            'cd_int': self.core.data_summary.get('cd_int', 0),
        }
    
    def print_results_summary(self) -> None:
        """Print main computation results"""
        if not self.core.config['flag_print_info']:
            return
            
        summary = self.get_summary_info()
        print("="*50)
        print("TSFOIL CALCULATION RESULTS SUMMARY")
        print("="*50)
        print(f"Mach number:      {summary['mach']:10.6f}")
        print(f"Angle of attack:  {summary['alpha']:10.6f} deg")
        print(f"Lift coefficient: {summary['cl']:10.6f}")
        print(f"Drag coefficient: {summary['cd']:10.6f}")
        print(f"Moment coefficient: {summary['cm']:10.6f}")
        print(f"Critical Cp:      {summary['cpstar']:10.6f}")
        if summary['cd_wave'] != 0 or summary['cd_body'] != 0:
            print(f"Wave drag:        {summary['cd_wave']:10.6f}")
            print(f"Body drag:        {summary['cd_body']:10.6f}")
            print(f"Integral drag:    {summary['cd_int']:10.6f}")
        print("="*50)
        print()

