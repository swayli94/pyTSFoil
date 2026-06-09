
import os
path = os.path.dirname(os.path.abspath(__file__))

'''
NWDGE = 0 | EPS = 0.2
# 1 cl: 0.62971729, cd: 0.00421030, cm: -0.13245095
# 2 cl: 0.62816113, cd: 0.00360215, cm: -0.13857070
# 3 cl: 0.63184190, cd: 0.00374173, cm: -0.14258274
# 4 cl: 0.63271499, cd: 0.00338739, cm: -0.14376149

NWDGE = 0 | EPS = 0.5
# 1 cl: 0.62215549, cd: 0.00349368, cm: -0.12985286
# 2 cl: 0.62590075, cd: 0.00358676, cm: -0.13785198
# 3 cl: 0.62419486, cd: 0.00341201, cm: -0.14086580
# 4 cl: 0.63247246, cd: 0.00339274, cm: -0.14376561

NWDGE = 0 | EPS = 1.0
# 1 cl: 0.62211412, cd: 0.00348963, cm: -0.12984428
# 2 cl: 0.62592578, cd: 0.00358766, cm: -0.13786510
# 3 cl: 0.62415069, cd: 0.00340669, cm: -0.14085943
# 4 cl: 0.62580568, cd: 0.00308161, cm: -0.14246471
'''

import numpy as np
from cst_modeling.foil import cst_foil
from pytsfoil import PyTSFoil
import matplotlib.pyplot as plt

N_AIRFOIL_GEO = 201
NWDGE = 0
EPS = 1.0


def run_pytsfoil(pytsfoil: PyTSFoil, airfoil_coordinates: np.ndarray, 
                n_point_x=200, n_point_y=80, n_point_airfoil=100,
                output=True):
    
    pytsfoil.set_config(
        ALPHA=0.5,
        EMACH=0.75,
        MAXIT=9999,
        NWDGE=NWDGE,
        n_point_x=n_point_x,
        n_point_y=n_point_y,
        n_point_airfoil=n_point_airfoil,
        EPS=EPS,
        CVERGE=1e-5,
        flag_output=False,
        flag_output_summary=output,
        flag_output_shock=output,
        flag_output_field=output,
        flag_print_info=False,
    )
    
    pytsfoil.airfoil['coordinates'] = airfoil_coordinates.copy()
    
    pytsfoil.run()
    
    result = {
        'success': True,
        'label': None,
        'config': pytsfoil.config.copy(),
        'cl': pytsfoil.data_summary['cl'],
        'cd': pytsfoil.data_summary['cd'],
        'cm': pytsfoil.data_summary['cm'],
        'xx': pytsfoil.mesh['xx'].copy(),
        'mau': pytsfoil.data_summary['mau'].copy(),
        'mal': pytsfoil.data_summary['mal'].copy(),
    }
    
    return result

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
    ax.set_title('Mach Number Distribution - Upper and Lower Surfaces', fontsize=14, fontweight='bold')
    ax.grid(True, alpha=0.3)
    ax.legend(bbox_to_anchor=(1.05, 1), loc='upper left', fontsize=10)
    ax.set_xlim([-0.1, 1.1])
    ax.set_ylim([0, 1.5])
    
    plt.tight_layout()
    plt.savefig(fname, dpi=300, bbox_inches='tight')
    plt.close()


if __name__ == "__main__":
    
    print('path: ', path)
    
    cst_u = [ 0.12829643, 0.12670863, 0.16065898, 0.14942386, 0.15102884, 0.22416928, 0.16078175, 0.20998555, 0.18608795, 0.21052324]
    cst_l = [-0.12927128,-0.13176061,-0.17044964,-0.07045476,-0.33888064, 0.00991923,-0.20070721,-0.03536713,-0.04397496, 0.06436195]
    cst_u = np.array(cst_u)
    cst_l = np.array(cst_l)
    
    x, yu, yl, _, _ = cst_foil(N_AIRFOIL_GEO, cst_u, cst_l)
    
    xx = np.concatenate((x[::-1], x[1:]))
    yy = np.concatenate((yu[::-1], yl[1:]))
    airfoil_coordinates = np.column_stack((xx, yy))
    
    pytsfoil = PyTSFoil(
        airfoil_coordinates=airfoil_coordinates,
        work_dir=path
    )
    
    mesh_sizes = [(100, 40, 50), (150, 60, 80), (200, 80, 100), (300, 120, 160)]
    
    results = []
    for i in range(len(mesh_sizes)):
        
        result = run_pytsfoil(pytsfoil, airfoil_coordinates, 
                            n_point_x=mesh_sizes[i][0],
                            n_point_y=mesh_sizes[i][1],
                            n_point_airfoil=mesh_sizes[i][2])
        
        print(f'# {i+1} cl: {result["cl"]:.8f}, cd: {result["cd"]:.8f}, cm: {result["cm"]:.8f}')
        
        result['label'] = f'Mesh {mesh_sizes[i][0]}x{mesh_sizes[i][1]}x{mesh_sizes[i][2]}'
        results.append(result)
        
    fname = os.path.join(path, f'mesh_convergence_{NWDGE}_{EPS}.png')
    plot_all_mach_distributions(results, fname)



