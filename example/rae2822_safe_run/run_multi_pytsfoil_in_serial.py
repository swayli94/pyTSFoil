'''
This script is used to run multiple pytsfoil instances in serial.

Verify the results are consistent, i.e., memory is not contaminated.    
'''

import os
import sys

path = os.path.dirname(os.path.abspath(__file__))
# Add project root to Python path for multi-branch development
project_root = os.path.abspath(os.path.join(path, '..', '..'))
if project_root not in sys.path:
    sys.path.insert(0, project_root)

import numpy as np

from pyTSFoil.pytsfoil import PyTSFoil

def run_pytsfoil(pytsfoil: PyTSFoil, airfoil_coordinates: np.ndarray):
    
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
        flag_output=False,
        flag_output_summary=False,
        flag_output_shock=False,
        flag_output_field=False,
        flag_print_info=False,
    )
    
    pytsfoil.airfoil['coordinates'] = airfoil_coordinates.copy()
    
    pytsfoil.run()
    
    cl = pytsfoil.data_summary['cl']
    cd = pytsfoil.data_summary['cd']
    cm = pytsfoil.data_summary['cm']
    
    return cl, cd, cm

if __name__ == "__main__":
    
    print('path: ', path)
    
    x, y = np.loadtxt(os.path.join(path, 'rae2822.dat'), skiprows=1).T
    airfoil_coordinates = np.column_stack((x, y))
    
    pytsfoil = PyTSFoil(
        airfoil_coordinates=airfoil_coordinates,
        work_dir=path
    )
    
    for i in range(3):
        
        cl, cd, cm = run_pytsfoil(pytsfoil, airfoil_coordinates)
        
        print(f'cl: {cl:.8f}, cd: {cd:.8f}, cm: {cm:.8f}')
    
    