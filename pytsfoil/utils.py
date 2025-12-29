"""
TSFoil utilities module

Contains various static utility functions and helper methods, such as:
- Grid point distribution functions
- Mathematical helper functions
- Data processing tools
"""

import numpy as np
    

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
