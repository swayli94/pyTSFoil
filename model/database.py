

import numpy as np
from typing import List, Tuple, Callable
import matplotlib.pyplot as plt
import os
from cst_modeling.foil import cst_foil, check_validity



class AirfoilDatabase():
    '''
    Airfoil database for XFoil environment
    '''
    def __init__(self, fname_database='reference-airfoils-cst.dat', n_cst=10, n_point_geo=201):
        
        self.fname_database = fname_database
        self.n_cst = n_cst
        self.n_point_geo = n_point_geo
        
        self.airfoils : List[dict] = []
        
        self.read_database()
        
    def read_database(self) -> None:
        
        with open(self.fname_database, 'r') as f:
            lines = f.readlines()

        for line in lines:
            
            if line[0] == '#' or len(line) <= 0:
                continue
            
            data = line.split()
            
            airfoil = {
                'name' : data[0],
                'tmax' : float(data[1]),
                'tail' : float(data[2]),
                'angle-TE' : float(data[3]),
                'cst_u' : np.array([float(x) for x in data[4:4+self.n_cst]]),
                'cst_l' : np.array([float(x) for x in data[4+self.n_cst:4+2*self.n_cst]]),
            }
            
            self.airfoils.append(airfoil)
            
    @property
    def n_airfoils(self) -> int:
        return len(self.airfoils)

    def get_airfoil_coordinates(self, ID: int) -> Tuple[np.ndarray, np.ndarray, np.ndarray, np.ndarray]:
        
        if ID >= len(self.airfoils):
            ID = ID % len(self.airfoils)
        
        cst_u = self.airfoils[ID]['cst_u']
        cst_l = self.airfoils[ID]['cst_l']
        
        x, yu, yl, _, _ = cst_foil(self.n_point_geo, cst_u, cst_l)
        
        xx = np.concatenate((x[::-1], x[1:]))
        yy = np.concatenate((yu[::-1], yl[1:]))
        
        return np.column_stack((xx, yy)), x, yu, yl

    def get_random_airfoil_coordinates(self, n_max_try: int = 10, 
                    check_validity_function: Callable|None = None,
                    ) -> Tuple[np.ndarray, np.ndarray, np.ndarray, np.ndarray]:
        '''
        Get random airfoil coordinates from the database.

        Parameters
        ----------
        n_max_try : int, optional
            Maximum number of tries to generate a valid airfoil.
            The default is 10.
        check_validity_function : Callable, optional
            Function to check if the airfoil is valid.
            The default is None, which means no check is performed.

        Returns
        -------
        airfoil_coordinates : np.ndarray
            The airfoil coordinates.
        x : np.ndarray
            The x coordinates of the airfoil.
        yu : np.ndarray
            The upper surface coordinates of the airfoil.
        yl : np.ndarray
            The lower surface coordinates of the airfoil.
        '''
        
        is_valid = False
        n_try = 0
        
        while not is_valid:

            id0 = np.random.randint(0, len(self.airfoils))
            id1 = np.random.randint(0, len(self.airfoils))
            alpha = np.random.rand()
            
            if n_try >= n_max_try:
                alpha = 1.0
            
            airfoil_0, x, yu0, yl0 = self.get_airfoil_coordinates(id0)
            airfoil_1, _, yu1, yl1 = self.get_airfoil_coordinates(id1)
            
            airfoil_coordinates = alpha*airfoil_0 + (1-alpha)*airfoil_1
            
            yu_new = alpha*yu0 + (1-alpha)*yu1
            yl_new = alpha*yl0 + (1-alpha)*yl1
            
            is_action_valid = check_validity(x, yu_new, yl_new)
            
            if is_action_valid and check_validity_function is not None:
                is_check_valid = check_validity_function(airfoil_coordinates)
            else:
                is_check_valid = True
            
            is_valid = is_action_valid and is_check_valid
            
            if n_try > n_max_try and not is_valid:
                raise ValueError('Failed to generate valid airfoil coordinates')
            
            n_try += 1
        
        return airfoil_coordinates, x, yu_new, yl_new

    def plot_all_airfoils(self, path: str, fname: str = 'all_airfoils.png'):
        '''
        Plot all airfoils in the database with their ID and name
        '''
        n_airfoils = len(self.airfoils)
        
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
        for i, airfoil in enumerate(self.airfoils):
            # Get actual airfoil coordinates
            airfoil_coords, x, yu, yl = self.get_airfoil_coordinates(i)
            
            is_action_valid = check_validity(x, yu, yl)
            if not is_action_valid:
                print(f'Airfoil {i} {airfoil["name"]} is invalid')
            
            # Plot the airfoil geometry
            axes[i].plot(airfoil_coords[:, 0], airfoil_coords[:, 1], 'k-', linewidth=1.5)
            axes[i].set_aspect('equal')
            axes[i].grid(True, alpha=0.3)
            axes[i].set_xlim(-0.05, 1.05)
            axes[i].set_ylim(-0.15, 0.15)
            axes[i].set_title(f'{airfoil["name"]} (ID: {i})', fontsize=10)
            axes[i].set_xlabel('x/c')
            axes[i].set_ylabel('y/c')
        
        # Hide unused subplots
        for i in range(n_airfoils, len(axes)):
            axes[i].set_visible(False)
        
        plt.tight_layout()
        plt.savefig(os.path.join(path, fname))
        plt.close()






