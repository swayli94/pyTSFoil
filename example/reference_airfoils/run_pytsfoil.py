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
from typing import List
from pyTSFoil.pytsfoil import PyTSFoil
from model.database import AirfoilDatabase
import matplotlib.pyplot as plt


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

def plot_all_airfoils(airfoils: List[dict], path: str, fname: str = 'all_airfoils_pytsfoil.png'):
    '''
    Plot all airfoils in the database with their ID and name
    
    airfoils: List[dict]
        - name: str
        - xx: np.ndarray
        - mau: np.ndarray
        - mal: np.ndarray
    '''
    n_airfoils = len(airfoils)
    
    # Calculate subplot grid dimensions
    n_cols = int(np.ceil(np.sqrt(n_airfoils)))
    n_rows = int(np.ceil(n_airfoils / n_cols))
    
    # Create figure with subplots
    fig, axes = plt.subplots(n_rows, n_cols, figsize=(4*n_cols, 3*n_rows))
    
    # Handle case of single subplot
    if n_airfoils == 1:
        axes = [axes]
    elif n_rows == 1 or n_cols == 1:
        axes = axes.flatten()
    else:
        axes = axes.flatten()
    
    # Plot each airfoil
    for i in range(n_airfoils):
        
        axes[i].plot(airfoils[i]['xx'], airfoils[i]['mau'], 'k-', linewidth=1.5)
        axes[i].plot(airfoils[i]['xx'], airfoils[i]['mal'], 'k-', linewidth=1.5)
        axes[i].set_aspect('equal')
        axes[i].grid(True, alpha=0.3)
        axes[i].set_xlim(-0.05, 1.05)
        axes[i].set_ylim(-0.10, 1.60)
        axes[i].set_title(f'{airfoils[i]["name"]} (ID: {i})', fontsize=10)
        axes[i].set_xlabel('x/c')
        axes[i].set_ylabel('y/c')
    
    # Hide unused subplots
    for i in range(n_airfoils, len(axes)):
        axes[i].set_visible(False)
    
    plt.tight_layout()
    plt.savefig(os.path.join(path, fname))
    plt.close()

def run_all_airfoils(path: str):
    
    database = AirfoilDatabase(fname_database=os.path.join(path, 'reference-airfoils-cst.dat'))
    database.plot_all_airfoils(path, fname='all_airfoils.png')
    
    airfoils : List[dict] = []
    
    for i in range(database.n_airfoils):
        
        airfoil_coordinates, x, yu, yl = database.get_airfoil_coordinates(i)
        
        tmax = np.max(yu - yl)
        
        pytsfoil = PyTSFoil(
            airfoil_coordinates=airfoil_coordinates,
            work_dir=path
        )
        cl, cd, cm = run_pytsfoil(pytsfoil, airfoil_coordinates)
        
        print(f'{i} {database.airfoils[i]["name"]}: cl= {cl:.8f}, cd= {cd:.8f}, cm= {cm:.8f}, tmax= {tmax:.8f}')
        
        airfoils.append({
            'name': database.airfoils[i]['name'],
            'xx': pytsfoil.mesh['xx'],
            'mau': pytsfoil.data_summary['mau'],
            'mal': pytsfoil.data_summary['mal']
        })
    
    plot_all_airfoils(airfoils, path, fname='all_airfoils_pytsfoil.png')
    
def run_selected_airfoils(path: str):
    
    database = AirfoilDatabase(fname_database=os.path.join(path, 'selected-airfoils-cst.dat'))
    database.plot_all_airfoils(path, fname='selected_airfoils.png')
    
    airfoils : List[dict] = []
    
    for i in range(database.n_airfoils):
        
        airfoil_coordinates, x, yu, yl = database.get_airfoil_coordinates(i)
        
        tmax = np.max(yu - yl)
        
        pytsfoil = PyTSFoil(
            airfoil_coordinates=airfoil_coordinates,
            work_dir=path
        )
        cl, cd, cm = run_pytsfoil(pytsfoil, airfoil_coordinates)
        
        print(f'{i} {database.airfoils[i]["name"]}: cl= {cl:.8f}, cd= {cd:.8f}, cm= {cm:.8f}, tmax= {tmax:.8f}')
        
        airfoils.append({
            'name': database.airfoils[i]['name'],
            'xx': pytsfoil.mesh['xx'],
            'mau': pytsfoil.data_summary['mau'],
            'mal': pytsfoil.data_summary['mal']
        })
    
    plot_all_airfoils(airfoils, path, fname='selected_airfoils_pytsfoil.png')
    

if __name__ == "__main__":
    
    print('path: ', path)
    
    # run_all_airfoils(path)
    
    run_selected_airfoils(path)
    