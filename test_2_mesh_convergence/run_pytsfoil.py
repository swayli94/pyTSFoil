
import os
path = os.path.dirname(os.path.abspath(__file__))

import time
import json
import numpy as np
from typing import Dict, Any
from cst_modeling.foil import cst_foil
from pytsfoil import PyTSFoil
import matplotlib.pyplot as plt

EPS = 0.5
MINF = 0.7
AOA = 3.5

N_AIRFOIL_POINTS = 1001
DPI=150

baseline_config = {
    'ALPHA': AOA,           # Angle of attack
    'REYNLD': 6.5E6,        # Reynolds number
    'CVERGE': 1e-5,         # Error criterion for convergence
    'DVERGE': 10.0,         # Error criterion for divergence
    'EMACH': MINF,          # Mach number
    'EPS': EPS,             # Artificial viscosity parameter (0.0-1.0)
    'IPRTER': 100,          # Print interval for convergence history
    'MAXIT': 9999,          # Maximum number of iterations
    'RIGF': 0.0,            # Rigidity factor for transonic effects
    'SIMDEF': 3,            # Similarity scaling (1 = Cole, 2 = Spreiter, 3 = Krupp)
    'WCIRC': 1.0,           # Weight for circulation jump at trailing edge (0.0-1.0)
    'WE': [1.8, 1.9, 1.95], # SOR relaxation factors
    'NWDGE': 0,             # Viscous wedge parameters (0 = no wedge, 1 = Murman wedge, 2 = Yoshihara wedge)
    'WCONST': 4.0,          # Wall constant for Murman wedge
    'IFLAP': 0,             # Flap flag
    'DELFLP': 0.0,          # Flap deflection angle
    'FLPLOC': 0.77,         # Flap location
    'n_point_x': 200,       # Number of points in the x-direction (IMAXI)
    'n_point_y': 80,        # Number of points in the y-direction (JMAXI)
    'n_point_airfoil': 100, # Number of points on the airfoil
    'flag_output': False,   # write solver process to tsfoil2.out
    'flag_output_summary': False,   # smry.out
    'flag_output_shock': False,     # cpxs.dat
    'flag_output_field': False,     # field.dat
    'flag_print_info': False,
}


def convert(obj):
    if isinstance(obj, np.ndarray):
        return obj.tolist()
    if isinstance(obj, dict):
        return {k: convert(v) for k, v in obj.items()}
    if isinstance(obj, list):
        return [convert(v) for v in obj]
    return obj


def run_pytsfoil_analysis(params: Dict[str, Any]) -> Dict[str, Any]:
    """
    Worker function to run a single PyTSFoil analysis in a separate process.
    
    Parameters:
    -----------
    params: Dict[str, Any]
        Dictionary containing:
        - 'label': Name identifier for this case
        - 'config': Configuration parameters for PyTSFoil
        - 'n_airfoil_points': Number of points to define the airfoil geometry
        - 'cst_u': CST coefficients for upper surface
        - 'cst_l': CST coefficients for lower surface
        
    Returns:
    --------
    Dict[str, Any]: Results dictionary with case info and outcomes
    """
    try:

        start_time = time.time()
        
        x, yu, yl, _, _ = cst_foil(nn=params['n_airfoil_points'],
                            cst_u=params['cst_u'], cst_l=params['cst_l'])
        
        xx = np.concatenate((x[::-1], x[1:]))
        yy = np.concatenate((yu[::-1], yl[1:]))
        airfoil_coordinates = np.column_stack((xx, yy))
            
        pytsfoil = PyTSFoil(
            airfoil_coordinates=airfoil_coordinates,
            work_dir=path
        )

        pytsfoil.set_config(**params['config'])
               
        pytsfoil.run()

        result = {
            'success': True,
            'label': params['label'],
            'elapsed_time': time.time() - start_time,
            'cl': pytsfoil.data_summary['cl'],
            'cd': pytsfoil.data_summary['cd'],
            'cm': pytsfoil.data_summary['cm'],
            'xx': pytsfoil.mesh['xx'].copy(),
            'mau': pytsfoil.data_summary['mau'].copy(),
            'mal': pytsfoil.data_summary['mal'].copy(),
        }
        return result
        
    except Exception as e:
        print(f"Error in case {params['label']}: {str(e)}")
        return {
            'success': False,
            'label': params['label'],
            'elapsed_time': 0,
            'error': str(e),
        }


def plot_all_mach_distributions(results,
                    fname='combined_mach_distributions.png'):
    """
    Plot Mach number distributions from all successful cases in a single figure.
    """
    successful_results = [r for r in results if r['success']]
    
    if not successful_results:
        print("No successful cases to plot.")
        return
    
    # Create single figure for both upper and lower surfaces
    fig, ax = plt.subplots(1, 1, figsize=(12, 8))
    
    # Define colors for different cases
    colors = plt.cm.tab10(np.linspace(0, 1, len(successful_results)))
    
    for i, result in enumerate(successful_results):
        xx = result['xx']
        mau = result['mau']
        mal = result['mal']

        color = colors[i]
        
        # Plot upper surface (solid line with circles)
        ax.plot(xx, mau, 
                linewidth=2, color=color, linestyle='-', markersize=4,
                label=result['label'])
        
        # Plot lower surface (dashed line with squares)
        ax.plot(xx, mal, 
                linewidth=2, color=color, linestyle='-', markersize=4,
                label=None)
    
    # Add reference line for sonic condition
    ax.axhline(y=1.0, color='k', linestyle=':', linewidth=2, alpha=0.7, label='Sonic (M=1)')
    
    # Configure plot
    ax.set_xlabel('X/c', fontsize=12)
    ax.set_ylabel('Mach Number', fontsize=12)
    ax.grid(True, alpha=0.3)
    ax.legend(bbox_to_anchor=(1.05, 1), loc='upper left', fontsize=10)
    ax.set_xlim([-0.1, 1.1])
    ax.set_ylim([-0.1, 1.5])
    
    plt.tight_layout()
    plt.savefig(fname, dpi=DPI, bbox_inches='tight')
    plt.close()


if __name__ == "__main__":
    
    print('path: ', path)
    
    cst_u = [ 0.12829643, 0.12670863, 0.16065898, 0.14942386, 0.15102884, 0.22416928, 0.16078175, 0.20998555, 0.18608795, 0.21052324]
    cst_l = [-0.12927128,-0.13176061,-0.17044964,-0.07045476,-0.33888064, 0.00991923,-0.20070721,-0.03536713,-0.04397496, 0.06436195]
    cst_u = np.array(cst_u)
    cst_l = np.array(cst_l)
    
    # n_point_x, n_point_y, n_point_airfoil
    testing_parameters = [
        (100, 40, 50), (150, 60, 80), (200, 80, 100),
        (300, 120, 160), (400, 160, 200)
    ]
    
    results = []
    for i, vals in enumerate(testing_parameters):
        
        config = baseline_config.copy()
        config['n_point_x'] = vals[0]
        config['n_point_y'] = vals[1]
        config['n_point_airfoil'] = vals[2]
        
        params = {
            'label': f'Airfoil {vals[0]} points',
            'config': config,
            'n_airfoil_points': N_AIRFOIL_POINTS,
            'cst_u': cst_u,
            'cst_l': cst_l,
        }
        
        result = run_pytsfoil_analysis(params)
        result['entry_index'] = i
        result['n_point_x'] = vals[0]
        result['n_point_y'] = vals[1]
        result['n_point_airfoil'] = vals[2]
        results.append(result)
        
        text = f"Case {i+1} ({vals[0]} points): "
        text+= f'cl: {result["cl"]:.6f}, cd: {result["cd"]:.6f}, cm: {result["cm"]:.6f}'
        print(text)
        
        
    fname = os.path.join(path, f'comparison_M{MINF:.2f}_AOA{AOA:.1f}_EPS{EPS:.1f}.png')
    plot_all_mach_distributions(results, fname)
    
    fname = os.path.join(path, f'summary_M{MINF:.2f}_AOA{AOA:.1f}_EPS{EPS:.1f}.txt')
    results_dict = {r['entry_index']: convert(r) for r in results}
    results_dict = dict(sorted(results_dict.items()))
    with open(fname, 'w') as f:
        json.dump(results_dict, f, indent=4)



