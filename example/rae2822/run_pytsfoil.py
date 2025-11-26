
import os
path = os.path.dirname(os.path.abspath(__file__))

'''
# Optional: When doing multi-branch development,
# do not pip install the package, but add the project root to Python path.

import sys
project_root = os.path.abspath(os.path.join(path, '..', '..'))
if project_root not in sys.path:
    sys.path.insert(0, project_root)
'''

import numpy as np
from pytsfoil import PyTSFoil


def run_pytsfoil(pytsfoil: PyTSFoil, airfoil_coordinates: np.ndarray, output=True):
    
    pytsfoil.set_config(
        ALPHA=0.5,
        EMACH=0.75,
        MAXIT=9999,
        NWDGE=0,
        n_point_x=200,
        n_point_y=80,
        n_point_airfoil=100,
        EPS=0.2,
        CVERGE=1e-6,
        flag_output=output,
        flag_output_summary=output,
        flag_output_shock=output,
        flag_output_field=output,
        flag_print_info=output,
    )
    
    pytsfoil.airfoil['coordinates'] = airfoil_coordinates.copy()
    
    pytsfoil.run()
    
    cl = pytsfoil.data_summary['cl']
    cd = pytsfoil.data_summary['cd']
    cm = pytsfoil.data_summary['cm']
    
    return cl, cd, cm


if __name__ == "__main__":
    
    print('path: ', path)
    
    n_trials = 3
    
    x, y = np.loadtxt(os.path.join(path, 'rae2822.dat'), skiprows=1).T
    airfoil_coordinates = np.column_stack((x, y))
    
    pytsfoil = PyTSFoil(
        airfoil_coordinates=airfoil_coordinates,
        work_dir=path
    )
    
    for i in range(n_trials):
        
        cl, cd, cm = run_pytsfoil(pytsfoil, airfoil_coordinates, output=i==n_trials-1)
        
        print(f'# {i+1} cl: {cl:.8f}, cd: {cd:.8f}, cm: {cm:.8f}')
    
    pytsfoil.plot_all_results()



