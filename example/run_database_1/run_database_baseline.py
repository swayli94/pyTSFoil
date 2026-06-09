'''
Compare pyTSFoil baseline results with RANS.

Baseline: SIMDEF = 3
'''
import os
import sys
path = os.path.abspath(os.path.dirname(__file__))
path_root = os.path.abspath(os.path.join(os.path.dirname(__file__), '..', '..'))
sys.path.append(path)
sys.path.append(path_root)

import time
import copy
import numpy as np
from typing import Dict, Any, List
import multiprocessing as mp
import matplotlib.pyplot as plt
from scipy.interpolate import interp1d

from cst_modeling.section import cst_foil
from pytsfoil import PyTSFoil
from airfoil_database.utils import (load_airfoil_database_from_json, save_results_to_json)

baseline_config = {
    'AK': 0.0,              # Free stream similarity parameter
    'ALPHA': 0.5,           #* Angle of attack
    'CVERGE': 1e-5,         # Error criterion for convergence
    'DVERGE': 10.0,         # Error criterion for divergence
    'EMACH': 0.75,          #* Mach number
    'EPS': 0.2,             # Convergence tolerance
    'IPRTER': 100,          # Print interval for convergence history
    'MAXIT': 9999,          # Maximum number of iterations
    'RIGF': 0.0,            # Rigidity factor for transonic effects
    'SIMDEF': 3,            # Similarity scaling (1 = Cole, 2 = Spreiter, 3 = Krupp)
    'WCIRC': 1.0,           # Weight for circulation jump at trailing edge (0.0-1.0)
    'WE': [1.8, 1.9, 1.95], # SOR relaxation factors
    'NWDGE': 0,             # Viscous wedge parameters (0 = no wedge, 1 = Murman wedge, 2 = Yoshihara wedge)
    'REYNLD': 4.0E6,        # Reynolds number
    'WCONST': 4.0,          # Wall constant for Murman wedge
    'IFLAP': 0,             # Flap flag
    'DELFLP': 0.0,          # Flap deflection angle
    'FLPLOC': 0.77,         # Flap location
    'n_point_x': 200,       #* Number of points in the x-direction (IMAXI)
    'n_point_y': 80,        #* Number of points in the y-direction (JMAXI)
    'n_point_airfoil': 100, #* Number of points on the airfoil
    'flag_output': False,     # write solver process to tsfoil2.out
    'flag_output_summary': False,   # smry.out
    'flag_output_shock': False,     # cpxs.dat
    'flag_output_field': False,     # field.dat
    'flag_print_info': False,
}

fname_database = os.path.join(path_root, "airfoil_database", "airfoil_database.json")
fname_results = os.path.join(path, "results_database.json")
path_temp_results = os.path.join(path, "temp_results")
os.makedirs(path_temp_results, exist_ok=True)
path_figure_cp = os.path.join(path, "figures")
os.makedirs(path_figure_cp, exist_ok=True)

N_PROCESS = 15
N_MAX_RUN = None
PLOT_CP = True
DPI = 100


def run_pytsfoil_analysis(airfoil: Dict[str, Any]) -> Dict[str, Any]:
    """
    Worker function to run a single PyTSFoil analysis in a separate process.
    
    Parameters:
    -----------
    airfoil: Dict[str, Any]
        Dictionary containing:
        - 'entry_index': index of the sample (for tracking)
        - 'airfoil_id': airfoil ID
        - 'tmax': airfoil maximum relative thickness
        - 'cst_u': upper CST coefficients
        - 'cst_l': lower CST coefficients
        - 'Ma': Mach number
        - 'AoA': angle of attack
        - 'Re': Reynolds number
        - 'CL_RANS': RANS CL value for reference
        - 'Cd_RANS': RANS Cd value for reference
        - 'Cm_RANS': RANS Cm value for reference
        - 'plot_cp': whether to plot Cp distribution
        - 'xu', 'yu', 'cpu', 'mwu', 'xl', 'yl', 'cpl', 'mwl': raw airfoil coordinates and Cp/Mach distributions (for reference)

    Returns:
    --------
    result: Dict[str, Any]
        - 'success': whether the analysis completed successfully
        - 'params': input parameters for reference
        - 'xx': x-coordinates of the mesh points
        - 'mau': Mach distribution on upper surface
        - 'mal': Mach distribution on lower surface
        - 'cpu': pressure coefficient distribution on upper surface
        - 'cpl': pressure coefficient distribution on lower surface
        - 'cl': computed lift coefficient
        - 'cd': computed drag coefficient
        - 'cm': computed moment coefficient
        - 'elapsed_time': time taken for the analysis
        - 'error': error message if failed
    """
    try:
        start_time = time.time()
        
        cst_u = airfoil['cst_u']
        cst_l = airfoil['cst_l']
        tmax = airfoil['tmax']
        x, yu, yl, _, _ = cst_foil(201, cst_u, cst_l, x=None, t=tmax, tail=0.0)
        
        xx = np.concatenate((x[::-1], x[1:]))
        yy = np.concatenate((yu[::-1], yl[1:]))
        airfoil_coordinates = np.column_stack((xx, yy))
        
        # Create PyTSFoil instance (each process gets its own isolated Fortran data)
        pytsfoil = PyTSFoil(
            airfoil_coordinates=airfoil_coordinates,
            work_dir=path
        )
        
        # Set configuration
        case_config = copy.deepcopy(baseline_config)
        case_config.update({
            'ALPHA': airfoil['AoA'],
            'EMACH': airfoil['Ma'],
            'REYNLD': airfoil['Re']
        })
        pytsfoil.set_config(**case_config)
        
        #* Run analysis
        pytsfoil.run()

        #* Compare with reference data from database
        cl = pytsfoil.data_summary['cl']
        cd = pytsfoil.data_summary['cd']
        cm = pytsfoil.data_summary['cm']
        
        xx = pytsfoil.mesh['xx']
        cpu = pytsfoil.data_summary['cpu']
        cpl = pytsfoil.data_summary['cpl']
        mau = pytsfoil.data_summary['mau']
        mal = pytsfoil.data_summary['mal']
        
        fcpu = interp1d(xx, cpu, kind='linear', fill_value='extrapolate')
        fcpl = interp1d(xx, cpl, kind='linear', fill_value='extrapolate')
        fmau = interp1d(xx, mau, kind='linear', fill_value='extrapolate')
        fmal = interp1d(xx, mal, kind='linear', fill_value='extrapolate')
                
        _cpu = fcpu(airfoil['xu']); _err_u = np.mean((_cpu - airfoil['cpu'])**2)
        _cpl = fcpl(airfoil['xl']); _err_l = np.mean((_cpl - airfoil['cpl'])**2)
        rmse_cp = np.sqrt(0.5 * (_err_u + _err_l))
        
        _mau = fmau(airfoil['xu']); _err_u = np.mean((_mau - airfoil['mwu'])**2)
        _mal = fmal(airfoil['xl']); _err_l = np.mean((_mal - airfoil['mwl'])**2)
        rmse_mw = np.sqrt(0.5 * (_err_u + _err_l))
        
        errors = {
            'cl': cl - airfoil['CL_RANS'],
            'cd': cd - airfoil['Cd_RANS'],
            'cm': cm - airfoil['Cm_RANS'],
            'cp': rmse_cp,
            'mw': rmse_mw
        }
        
        if airfoil['plot_cp']:
            plot_comparison(airfoil, errors, xx, cpu, cpl, mau, mal)
        
        elapsed_time = time.time() - start_time
        
        #* Save results
        result = {
            'success': True,
            'entry_index': airfoil['entry_index'],
            'airfoil_id': airfoil['airfoil_id'],
            'Ma': airfoil['Ma'],
            'AoA': airfoil['AoA'],
            'Re': airfoil['Re'],
            'xx': xx,
            'mau': mau,
            'mal': mal,
            'cpu': cpu,
            'cpl': cpl,
            'cl': cl,
            'cd': cd,
            'cm': cm,
            'errors': errors,
            'elapsed_time': elapsed_time,
        }
        
        # Save each result immediately to avoid data loss on crash
        fname = os.path.join(path_temp_results, f"result_{airfoil['entry_index']}.json")
        save_results_to_json([result], fname)
        
        return result
        
    except Exception as e:
        return {
            'success': False,
            'entry_index': airfoil['entry_index'],
            'airfoil_id': airfoil['airfoil_id'],
            'Ma': airfoil['Ma'],
            'AoA': airfoil['AoA'],
            'Re': airfoil['Re'],
            'elapsed_time': 0,
            'error': str(e)
        }

def run_parallel_analysis(airfoils: List[Dict[str, Any]],
                n_processes: int = 4) -> List[Dict[str, Any]]:
    '''
    Run PyTSFoil analyses in parallel for a list of airfoils.
    
    Parameters:
    -----------
    airfoils: List[Dict[str, Any]]
        List of dictionaries containing airfoil data and parameters.
    n_processes: int
        Number of parallel processes to use.
    
    Returns:
    --------
    results: List[Dict[str, Any]]
        List of results dictionaries from each analysis.
    '''
    with mp.Pool(processes=n_processes) as pool:
        results = pool.map(run_pytsfoil_analysis, airfoils)
    return results

def plot_comparison(airfoil: Dict[str, Any], errors: Dict[str, float],
            xx: np.ndarray, cpu: np.ndarray, cpl: np.ndarray,
            mau: np.ndarray, mal: np.ndarray) -> None:
    '''
    Plot the computed Cp & Mw distributions against the RANS results for a single case.
    
    Parameters:
    -----------
    airfoil: Dict[str, Any]
        Airfoil parameters and RANS results.
    errors: Dict[str, float]
        Dictionary containing errors in CL, Cd, Cm, Cp and Mw.
    xx: np.ndarray
        x-coordinates of the mesh points.
    cpu: np.ndarray
        Computed pressure coefficient distribution on upper surface.
    cpl: np.ndarray
        Computed pressure coefficient distribution on lower surface.
    mau: np.ndarray
        Computed Mach number distribution on upper surface.
    mal: np.ndarray
        Computed Mach number distribution on lower surface.
    '''
    entry_index = airfoil['entry_index']
    airfoil_id = airfoil['airfoil_id']
    Ma = airfoil['Ma']
    AoA = airfoil['AoA']

    fig, axes = plt.subplots(1, 3, figsize=(18, 5))

    title = f'Airfoil {airfoil_id} | Ma={Ma:.2f}, AoA={AoA:.2f}°'
    text  = f'CL error: {errors["cl"]:7.4f} | Cd error: {errors["cd"]:7.4f}   '
    text += f'Cm error: {errors["cm"]:7.4f} | Cp RMSE: {errors["cp"]:7.4f} | Mw RMSE: {errors["mw"]:7.4f}'
    fig.suptitle(title + '\n' + text, fontsize=11, fontfamily='monospace', y=1.02)

    # --- subplot 1: airfoil geometry ---
    ax = axes[0]
    ax.plot(airfoil['xu'], airfoil['yu'], 'g-', label='Airfoil')
    ax.plot(airfoil['xl'], airfoil['yl'], 'g-', label=None)
    ax.set_xlabel('x/c')
    ax.set_ylabel('y/c')
    ax.set_title('Geometry')
    ax.set_xlim(-0.05, 1.05)
    # ax.set_aspect('equal')
    ax.legend()
    ax.grid()

    # --- subplot 2: Cp distribution ---
    ax = axes[1]
    ax.plot(xx, cpu, 'b-', label='TSD')
    ax.plot(xx, cpl, 'b-', label=None)
    ax.plot(airfoil['xu'], airfoil['cpu'], 'g-', label='RANS', marker='x', markevery=5)
    ax.plot(airfoil['xl'], airfoil['cpl'], 'g-', label=None, marker='x', markevery=5)
    ax.invert_yaxis()
    ax.set_xlabel('x/c')
    ax.set_ylabel('Cp')
    ax.set_title('Cp Distribution')
    ax.set_xlim(-0.05, 1.05)
    ax.legend()
    ax.grid()

    # --- subplot 3: Mach number distribution ---
    ax = axes[2]
    ax.plot(xx, mau, 'b-', label='TSD')
    ax.plot(xx, mal, 'b-', label=None)
    ax.plot(airfoil['xu'], airfoil['mwu'], 'g-', label='RANS', marker='x', markevery=5)
    ax.plot(airfoil['xl'], airfoil['mwl'], 'g-', label=None, marker='x', markevery=5)
    ax.set_xlabel('x/c')
    ax.set_ylabel('Mw')
    ax.set_title('Mach Distribution')
    ax.set_xlim(-0.05, 1.05)
    ax.legend()
    ax.grid()

    fig.tight_layout()
    fig.savefig(os.path.join(path_figure_cp, f"entry_{entry_index}.png"),
                dpi=DPI, bbox_inches='tight')
    plt.close(fig)


if __name__ == '__main__':
    
    dict_airfoils = load_airfoil_database_from_json(fname_database)
    airfoils = list(dict_airfoils.values())
    
    print(f"Read {len(airfoils)} airfoils from database.")
    if N_MAX_RUN is not None:
        airfoils = airfoils[:N_MAX_RUN]
        for airfoil in airfoils:
            airfoil['plot_cp'] = True
        print(f"Limiting to first {N_MAX_RUN} cases for testing.")
    
    results_database = run_parallel_analysis(airfoils, n_processes=N_PROCESS)
    print("Completed parallel analysis for all airfoils.")

    save_results_to_json(results_database, fname_results)
