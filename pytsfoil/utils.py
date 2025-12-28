"""
TSFoil utilities module

Contains various static utility functions and helper methods, such as:
- Grid point distribution functions
- Mathematical helper functions
- Data processing tools
"""

import os
import numpy as np

try:
    import tsfoil_fortran as tsf
except ImportError:
    raise ImportError("tsfoil_fortran module not available")
    

def clustcos(n_points: int, a0=0.0079, a1=0.96, beta=1.0, index_point: int|None=None) -> np.ndarray:
    """
    Point distribution function on x-axis [0, 1] with denser points at both ends

    Parameters
    ----------
    n_points: int
        Total number of points
        
    a0: float
        Parameter controlling point distribution near x=0
        Smaller a0 gives denser points near x=0
        
    a1: float
        Parameter controlling point distribution near x=1
        Larger a1 gives denser points near x=1
        
    beta: float
        Distribution control parameter
        
    index_point: int|None
        Index of the point to return
        If None, return all points
        
    Returns
    -------
    xx: np.ndarray|float
        x-coordinates of the points
        If index_point is specified, return x-coordinate of the point at that index
    
    Examples
    --------
    >>> xx = clustcos(n, a0, a1, beta)
    >>> xx = clustcos(n, a0, a1, beta, index_point=i)
    """
    aa = np.power((1-np.cos(a0*np.pi))/2.0, beta)
    dd = np.power((1-np.cos(a1*np.pi))/2.0, beta) - aa
    
    if isinstance(index_point, int):
        yt = index_point/(n_points-1.0)
    else:
        yt = np.linspace(0.0, 1.0, num=n_points)
    
    a  = np.pi*(a0*(1-yt)+a1*yt)
    xx = (np.power((1-np.cos(a))/2.0,beta)-aa)/dd

    return xx


def trap_integration(xi_arr: np.ndarray, arg_arr: np.ndarray, n_points: int) -> float:
    """
    Trapezoidal integration (Python version of TRAP function)
    
    Parameters
    ----------
    xi_arr: np.ndarray
        x-coordinate array
    arg_arr: np.ndarray  
        Integrand function value array
    n_points: int
        Number of integration points
        
    Returns
    -------
    sum_val: float
        Integration result
    """
    sum_val = 0.0
    for i in range(n_points - 1):
        z = xi_arr[i + 1] - xi_arr[i]
        w = arg_arr[i + 1] + arg_arr[i]
        sum_val += z * w
    return 0.5 * sum_val


def find_shock_location(istart: int, iend: int, j_line: int, son_vel: float) -> int:
    """
    Find shock location on line J between ISTART and IEND
    
    Parameters
    ----------  
    istart: int
        Starting i index
    iend: int
        Ending i index
    j_line: int
        j line index
    son_vel: float
        Speed of sound
        
    Returns
    -------
    isk: int
        Shock location index, returns negative value if not found
    """
    isk = istart - 1
    u2 = tsf.solver_base.px(isk, j_line)
    
    while True:
        isk += 1
        u1 = u2
        u2 = tsf.solver_base.px(isk, j_line)
        if u1 > son_vel and u2 <= son_vel:
            break
        if isk >= iend:
            isk = -iend
            break
    return isk


def find_new_shock_location(iskold: int, j_line: int, son_vel: float) -> int:
    """
    Find new shock location based on initial guess
    
    Parameters
    ----------
    iskold: int  
        Old shock location
    j_line: int
        j line index
    son_vel: float
        Speed of sound
        
    Returns  
    -------
    isknew: int
        New shock location, returns negative value if not found
    """
    i2 = iskold + 2
    isknew = iskold - 3
    u2 = tsf.solver_base.px(isknew, j_line)
    
    while True:
        isknew += 1
        u1 = u2
        u2 = tsf.solver_base.px(isknew, j_line)
        if u1 > son_vel and u2 <= son_vel:
            break
        if isknew >= i2:
            isknew = -isknew
            break
    return isknew


def compute_drag_by_pressure_integral(cdfact_in: float) -> float:
    """
    Compute drag coefficient by surface pressure integration (Python version of DRAG function)
    
    Parameters
    ----------
    cdfact_in: float
        Drag factor
        
    Returns
    -------
    drag: float
        Drag coefficient
    """
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


def validate_parameters(config: dict) -> None:
    """
    Validate the validity of configuration parameters
    
    Parameters
    ----------
    config: dict
        Configuration parameter dictionary
        
    Raises
    ------
    ValueError
        If parameters are not within valid range
    """
    if config['EMACH'] < 0.5 or config['EMACH'] > 2.0:
        raise ValueError("EMACH must be between 0.5 and 2.0")
    if config['ALPHA'] < -9.0 or config['ALPHA'] > 9.0:
        raise ValueError("ALPHA must be between -9.0 and 9.0")
    if config['NWDGE'] > 0 and config['EMACH'] > 1.0:
        raise ValueError("NWDGE must be 0 if EMACH <= 1.0")


def print_shock_information(xi_arr: np.ndarray, arg_arr: np.ndarray, l_points: int, 
                          nshock: int, cdsk: float, lprt1: int, 
                          output_dir: str, flag_output_summary: bool) -> None:
    """
    Print shock information (Python version of PRTSK function)
    
    Parameters
    ----------
    xi_arr: np.ndarray
        xi coordinate array
    arg_arr: np.ndarray
        Parameter array
    l_points: int
        Number of points
    nshock: int
        Shock number
    cdsk: float
        Wave drag for this shock
    lprt1: int
        Print flag
    output_dir: str
        Output directory
    flag_output_summary: bool
        Whether to output summary
    """
    if not flag_output_summary:
        return
    
    cdfact = tsf.solver_data.cdfact
    gam1 = tsf.common_data.gam1
    yfact = tsf.solver_data.yfact
    delta = tsf.common_data.delta
    
    cdycof = -cdfact * gam1 / (6.0 * yfact)
    poycof = delta**2 * gam1 * (gam1 - 1.0) / 12.0
        
    with open(os.path.join(output_dir, "smry.out"), 'a') as f:
        # Write header only for the first shock
        if nshock == 1:
            f.write('0\n')
            f.write(' INVISCID WAKE PROFILES FOR INDIVIDUAL SHOCK WAVES WITHIN MOMENTUM CONTOUR\n')
        
        # Write shock information
        f.write('\n')  # blank line
        f.write(f'SHOCK{nshock:3d}\n')
        f.write(f' WAVE DRAG FOR THIS SHOCK={cdsk:12.6f}\n')
        f.write(f'      Y         CD(Y)        PO/POINF\n')
        
        # Write shock profile data
        for k in range(l_points):
            yy = xi_arr[k] * yfact
            cdy = cdycof * arg_arr[k]
            poy = 1.0 + poycof * arg_arr[k]
            f.write(f' {yy:12.8f}{cdy:12.8f}{poy:12.8f}\n')
        
        # Write footer if shock extends outside contour
        if lprt1 == 1:
            f.write('\n')  # blank line
            f.write(' SHOCK WAVE EXTENDS OUTSIDE CONTOUR\n')
            f.write(' PRINTOUT OF SHOCK LOSSES ARE NOT AVAILABLE FOR REST OF SHOCK\n')

