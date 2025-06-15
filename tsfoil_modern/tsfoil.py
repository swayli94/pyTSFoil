'''
A python interface for TSFOIL.

Functions:

1. Define airfoil geometry.
2. Define mesh.
3. Define other parameters.
4. Run TSFOIL.
5. Plot results.


'''

import os
import sys

path = os.path.dirname(os.path.abspath(__file__))
if path not in sys.path:
    sys.path.append(path)

import shutil
import numpy as np
from scipy.optimize import fsolve
import matplotlib.pyplot as plt
from matplotlib.colors import LinearSegmentedColormap
import matplotlib.patches as patches


class TSFoil(object):
    '''
    A python interface for TSFOIL.

    Functions
    ---------

    1. Define airfoil geometry.
    2. Define mesh.
    3. Define other parameters.
    4. Run TSFOIL.
    5. Plot results.
    '''

    def __init__(self, exe_path:str|None = None):
        '''
        Initialize the TSFoil object.
        '''
        self.airfoil = {}
        self.mesh = {}
        self.data_summary = {}
        self.data_cpxs = {}
        self.data_field = {}

        self.PROGRAM_NAME = "tsfoil_modern"
        self._set_default_config()
        self._set_executable_path(exe_path)
        self._ensure_executable_permissions()

    def set_airfoil(self, airfoil_file: str, skiprows: int = 1) -> None:
        '''
        Read the airfoil geometry from a file, and set the airfoil geometry.
        
        Parameters
        ----------
        airfoil_file : str
            The file containing the airfoil geometry.
            The data starts from the airfoil's trailing edge in the upper surface,
            and then goes counter-clockwise around the airfoil.
            
        skiprows : int
            The number of rows to skip in the airfoil file.
        '''
        x, y = np.loadtxt(airfoil_file, skiprows=skiprows).T
        le_pos = x.argmin()

        xu = x[:le_pos+1][::-1]
        yu = y[:le_pos+1][::-1]
        xl = x[le_pos:]
        yl = y[le_pos:]

        fmt={'float_kind':'{:9.7f}'.format}

        self.airfoil['x'] = x
        self.airfoil['y'] = y
        self.airfoil['XU'] = np.array2string(xu, formatter=fmt, separator=", ")[1:-1]
        self.airfoil['YU'] = np.array2string(yu, formatter=fmt, separator=", ")[1:-1]
        self.airfoil['XL'] = np.array2string(xl, formatter=fmt, separator=", ")[1:-1]
        self.airfoil['YL'] = np.array2string(yl, formatter=fmt, separator=", ")[1:-1]
        self.airfoil['NU'] = xu.shape[0]
        self.airfoil['NL'] = xl.shape[0]

        self.airfoil['name'] = airfoil_file.replace(".dat", "")

    def set_mesh(self,
            n_point_x: int = 81, n_point_y: int = 60, n_point_airfoil: int = 51,
            x_scale: float = 5.0, y_scale: float = 4.0
            ) -> None:
        '''
        Set the mesh coordinates.
        
        The user-defined mesh is provided to TSFOIL with 1-d arrays XIN, YIN.
        XIN and YIN are the x-coordinates and y-coordinates of the mesh points, respectively.
        The mesh is a 2-d nonuniform Cartesian grid.
        The airfoil has a unit chord length, the leading edge is at (0,0), and the trailing edge is at (1,0).
        The XIN distributes more points near x=0 and x=1, and the YIN distributes more points near y=0.
        
        Parameters
        ----------
        n_point_x: int
            The number of points in the x-direction.
            
        n_point_y: int
            The number of points in the y-direction.
            
        n_point_airfoil: int
            The number of points on the airfoil.
            
        x_scale: float
            The range of the x-coordinate of the mesh, x in [-x_scale, x_scale].
            
        y_scale: float
            The range of the y-coordinate of the mesh, y in [-y_scale, y_scale].
        '''

        #* Generate x-coordinates with clustering near x=0 and x=1 (interior points)
        # Split domain into three segments: [-x_scale, 0], [0, 1], [1, x_scale]
        # Distribute remaining points between left and right segments
        n_remaining = int((n_point_x-n_point_airfoil)*0.5) + 1
        
        # Segment 1: [x_min, 0] with clustering near x=0 (right end)
        x_left_norm = self.clustcos(n_remaining, a0=1.00, a1=0.999, beta=1.0)
        x_left = - x_left_norm[::-1] * x_scale
        
        # Segment 2: [0, 1] with clustering at both ends
        x_center = self.clustcos(n_point_airfoil, a0=0.01, a1=0.96, beta=1.0)
        
        # Segment 3: [1, x_max] with clustering near x=1 (left end)
        x_right_norm = self.clustcos(n_remaining, a0=0.001, a1=0.1, beta=1.0)
        x_right = 1 + x_right_norm * (x_scale - 1)
        
        # Combine segments, removing duplicate boundary points
        xx = np.concatenate([
            x_left[:-1],    # Exclude right boundary (x=0)
            x_center[1:],   # Exclude x=0 (for simulation stability)
            x_right[1:]     # Exclude left boundary (x=1)
        ])
        
        #* Generate symmetric distribution of y-coordinates with clustering near y=0
        half_points = n_point_y // 2 + 1  # Include y=0
        y_half = self.clustcos(half_points, a0=1.0, a1=0.999, beta=2.0)
        
        # Create symmetric distribution: negative half + positive half
        # y_half goes from 0 to 1, we want symmetric distribution about 0
        yy_normalized = np.concatenate([
            -y_half[1:][::-1],  # Negative side: -1 to 0 (excluding 0)
            y_half[1:]          # Positive side: 0 to 1 (excluding 0)
        ])
        
        # Scale to [-y_scale, y_scale]
        yy = yy_normalized * y_scale
        
        # Store mesh parameters
        self.mesh['n_point_x'] = xx.shape[0]
        self.mesh['n_point_y'] = yy.shape[0]
        self.mesh['n_point_airfoil'] = n_point_airfoil
        self.mesh['x_min'] = -x_scale
        self.mesh['x_max'] = x_scale
        self.mesh['y_min'] = -y_scale
        self.mesh['y_max'] = y_scale
        self.mesh['xx'] = xx
        self.mesh['yy'] = yy
        self.mesh['xx_airfoil'] = x_center
        
        # Format arrays for TSFOIL input
        fmt = {'float_kind': '{:9.7f}'.format}
        self.mesh['XIN'] = np.array2string(xx, formatter=fmt, separator=", ")[1:-1]
        self.mesh['YIN'] = np.array2string(yy, formatter=fmt, separator=", ")[1:-1]

    def set_config(self, config_dict: dict|None = None):
        '''
        Set the configuration parameters.
        '''
        self.config['NU'] = self.airfoil['NU']
        self.config['NL'] = self.airfoil['NL']
        self.config['IMAXI'] = self.mesh['n_point_x']
        self.config['JMAXI'] = self.mesh['n_point_y']
        self.config['XU'] = self.airfoil['XU']
        self.config['YU'] = self.airfoil['YU']
        self.config['XL'] = self.airfoil['XL']
        self.config['YL'] = self.airfoil['YL']
        self.config['XIN'] = self.mesh['XIN']
        self.config['YIN'] = self.mesh['YIN']

        if config_dict is not None:
            self.config.update(config_dict)

    def write_inp(self, filename:str|None=None):
        '''
        Write the input file.
        
        Parameters
        ----------
        filename: str
            The name of the input file.
        '''
        title = 'Input File for TSFOIL (%s)' % self.airfoil['name']
        if filename is None:
            filename = self.airfoil['name'] + '.inp'
        
        settings = "\n".join(["{} = {}".format(k,v) for k,v in self.config.items()])
        
        with open(filename, 'w') as f:
            f.write("{}\n$INP\n{}\n$END\n".format(title,settings))

    def load_smry(self, filename:str='smry.out') -> dict:
        '''
        Load the summary file.
        
        Parameters
        ----------
        filename: str
            The name of the summary file.
        '''
        if not os.path.exists(filename):
            raise FileNotFoundError(f"Summary file {filename} not found")
        
        if os.path.getsize(filename) == 0:
            raise ValueError(f"Summary file {filename} is empty - {self.PROGRAM_NAME} likely failed to execute properly")
            
        self.data_summary = {}
        
        with open(filename, 'r') as f:
            lines = f.readlines()
            for line in lines:
                if line.startswith('0'):
                    continue
                line = line.split()
                self.data_summary[line[-3]] = float(line[-1])
                
        return self.data_summary

    def load_cpxs(self, filename:str='cpxs.dat') -> dict:
        '''
        Load the cpxs.dat file.
        
        Parameters
        ----------
        filename: str
            The name of the cpxs.dat file.
            
        Returns
        -------
        data: dict
            Dictionary containing:
            - 'x': np.ndarray of x-coordinates
            - 'cp_up': np.ndarray of upper surface Cp values
            - 'm_up': np.ndarray of upper surface Mach numbers
            - 'cp_low': np.ndarray of lower surface Cp values  
            - 'm_low': np.ndarray of lower surface Mach numbers
        '''
        if not os.path.exists(filename):
            raise FileNotFoundError(f"cpxs.dat not found at: {filename}")
        
        # Load data, skipping first 6 lines (header + VARIABLES line)
        data_array = np.loadtxt(filename, skiprows=6)
        
        self.data_cpxs = {
            'x': data_array[:, 0],
            'cp_up': data_array[:, 1], 
            'm_up': data_array[:, 2],
            'cp_low': data_array[:, 3],
            'm_low': data_array[:, 4]
        }
        
        return self.data_cpxs

    def load_field(self, filename:str='field.dat') -> dict:
        '''
        Load the field.dat file.
        
        Parameters
        ----------
        filename: str
            The name of the field.dat file.
            
        Returns
        -------
        data: dict
            Dictionary containing:
            - 'ni': int, number of points in i-direction
            - 'nj': int, number of points in j-direction
            - 'x': np.ndarray of x-coordinates (2D grid)
            - 'y': np.ndarray of y-coordinates (2D grid)
            - 'mach': np.ndarray of Mach number values (2D grid)
            - 'cp': np.ndarray of pressure coefficient values (2D grid)
            - 'potential': np.ndarray of potential values (2D grid)
            - 'flow_type': np.ndarray of flow type values (2D grid)
 
        '''
        if not os.path.exists(filename):
            raise FileNotFoundError(f"field.dat not found at: {filename}")
        
        # Read zone info and data
        data_lines = []
        
        with open(filename, 'r') as f:
            lines = f.readlines()
            for line in lines:
                line = line.strip()
                
                if line.startswith('ZONE'):
                    # Parse zone info: ZONE I= 201 J= 152 F= POINT
                    ni = int(line.split('I=')[1].split()[0])
                    nj = int(line.split('J=')[1].split()[0])
                elif line.startswith('VARIABLES'):
                    continue
                elif line.startswith('#'):
                    continue
                else:
                    data_lines.append(line)
        
        # Parse data
        data = []
        for line in data_lines:
            values = [float(x) for x in line.split()]
            if len(values) >= 6:  # X, Y, Mach, Cp, P, FlowType (6 values per line)
                data.append(values[:6])
        
        if not data:
            raise ValueError(f"No valid data found in {filename}")
        
        data = np.array(data)
        
        # Reshape data to grid (nj rows, ni columns)
        self.data_field = {
            'ni': ni,
            'nj': nj,
            'x': data[:, 0].reshape(nj, ni),
            'y': data[:, 1].reshape(nj, ni),
            'mach': data[:, 2].reshape(nj, ni),
            'cp': data[:, 3].reshape(nj, ni),
            'potential': data[:, 4].reshape(nj, ni),
            'flow_type': data[:, 5].reshape(nj, ni)
        }
        
        return self.data_field

    @staticmethod
    def clustcos(n_points: int, a0=0.0079, a1=0.96, beta=1.0, index_point: int|None=None) -> np.ndarray:
        '''
        Point distribution on x-axis [0, 1]. (More points at both ends)

        Parameters
        ----------
        n_points: int
            total amount of points
            
        a0: float
            Parameter for distributing points near x=0.
            Smaller a0, more points near x=0.
            
        a1: float
            Parameter for distributing points near x=1.
            Larger a1, more points near x=1.
            
        beta: float
            Parameter for distribution points.
            
        index_point: int|None
            The index of the point to return.
            If None, return all points.
            
        Returns
        -------
        xx: np.ndarray|float
            The x-coordinates of the points.
            If index_point is not None, return the x-coordinate of the point at the given index.
        
        Examples
        ---------
        >>> xx = clustcos(n, a0, a1, beta)
        >>> xx = clustcos(n, a0, a1, beta, index_point=i)

        '''
        aa = np.power((1-np.cos(a0*np.pi))/2.0, beta)
        dd = np.power((1-np.cos(a1*np.pi))/2.0, beta) - aa
        
        if isinstance(index_point, int):
            yt = index_point/(n_points-1.0)
        else:
            yt = np.linspace(0.0, 1.0, num=n_points)
        
        a  = np.pi*(a0*(1-yt)+a1*yt)
        xx = (np.power((1-np.cos(a))/2.0,beta)-aa)/dd

        return xx

    def _set_default_config(self):
        '''
        Set the default configuration.
        '''
        self.config = {
            "ALPHA": 0.5,
            "EMACH": 0.75,
            "MAXIT": 9999
        }

    def _set_executable_path(self, exe_path:str|None = None):
        '''
        Set the path to the executable.
        '''
        # Set the path to the executable
        if exe_path is None:
            # Default to executable in the same directory as this script
            script_dir = os.path.dirname(os.path.abspath(__file__))
            # Detect Windows and use .exe extension
            if os.name == 'nt' or sys.platform.startswith('win'):
                exe_name = self.PROGRAM_NAME + ".exe"
            else:
                exe_name = self.PROGRAM_NAME
            self.exe_path = os.path.join(script_dir, exe_name)
        else:
            # Use user-defined path
            if os.path.isfile(exe_path):
                self.exe_path = exe_path
            elif os.path.isdir(exe_path):
                # If directory provided, append executable name
                if os.name == 'nt' or sys.platform.startswith('win'):
                    exe_name = self.PROGRAM_NAME + ".exe"
                else:
                    exe_name = self.PROGRAM_NAME
                self.exe_path = os.path.join(exe_path, exe_name)
            else:
                # Assume it's a full path even if file doesn't exist yet
                self.exe_path = exe_path

    def _ensure_executable_permissions(self):
        """Ensure the executable file has proper permissions on Linux/Unix systems"""
        import stat
        if os.path.exists(self.exe_path):
            try:
                # Get current permissions
                current_permissions = os.stat(self.exe_path).st_mode
                # Add execute permission for owner, group, and others
                new_permissions = current_permissions | stat.S_IXUSR | stat.S_IXGRP | stat.S_IXOTH
                os.chmod(self.exe_path, new_permissions)
                print(f"Set executable permissions for: {self.exe_path}")
            except PermissionError:
                print(f"Warning: Could not set executable permissions for {self.exe_path}")
        else:
            print(f"Note: Executable file {self.exe_path} not found yet")
    
    def exec(self) -> None:
        '''
        Run the executable.
        '''
        self.write_inp()
        
        # Check if executable exists
        if not os.path.isfile(self.exe_path):
            raise FileNotFoundError(f"{self.PROGRAM_NAME} not found at: {self.exe_path}")
        
        # Use relative path for input file to avoid path length issues
        inp_file_relative = os.path.basename(self.airfoil['name'] + '.inp')
        
        # Print debug information
        print(f"Executing: {self.exe_path}")
        print(f"Input file (relative): {inp_file_relative}")
        print(f"Working directory: {os.getcwd()}")
        
        # Use subprocess instead of os.system for better error handling
        import subprocess
        try:
            # Use relative path for input file to avoid path length limitations
            cmd = [self.exe_path, inp_file_relative]
            
            result = subprocess.run(cmd, 
                        capture_output=True, text=True, cwd=os.getcwd(), shell=False)
            
            print(f"Exit code: {result.returncode}")
            if result.stdout:
                print(f"stdout: {result.stdout}")
            if result.stderr:
                print(f"stderr: {result.stderr}")
                
            if result.returncode != 0:
                print(f"Warning: {self.PROGRAM_NAME} returned exit code {result.returncode}")
                
        except FileNotFoundError as e:
            print(f"Failed to execute {self.PROGRAM_NAME}: {e}")
            raise

    def fixed_cl(self, cl:float, alpha_0:float=1.0) -> float|None:
        '''
        Find the angle of attack for a given lift coefficient.
        
        Parameters
        ----------
        cl: float
            The lift coefficient.
            
        alpha_0: float
            The initial angle of attack.
            
        Returns
        -------
        alpha: float|None
            The angle of attack.
            If None, the angle of attack is not found.
            
        Examples
        --------
        >>> alpha = fixed_cl(cl)
        >>> alpha = fixed_cl(cl, alpha_0)
        '''
        def exec_wrapper(alpha):
            self.config["ALPHA"] = alpha.item()
            self.exec()
            data = self.load_smry()
            return data["cl"].item() - cl
        
        res = fsolve(exec_wrapper, x0=alpha_0, xtol=0.005)
        
        if res:
            print(f"Fixed CL: {cl} at alpha = {res[0]:.3f}")
            return res[0]
        else:
            print("Failed to find fixed CL")
            return None

    def move_result_files(self, output_dir:str|None=None, filename_prefix:str|None=None):
        '''
        Move the result files to the output directory, including:
        - smry.out
        - cpxs.dat
        - field.dat
        - tsfoil2.out
        '''
        if output_dir is None:
            output_dir = os.getcwd()
        if not os.path.exists(output_dir):
            os.makedirs(output_dir)
            
        if filename_prefix is None:
            filename_prefix = ''
            
        for filename in ['smry.out', 'cpxs.dat', 'field.dat', 'tsfoil2.out']:
            if os.path.exists(filename):
                shutil.move(filename, os.path.join(output_dir, filename_prefix + filename))
            else:
                print(f"Warning: {filename} not found")
                
    def plot_all_results(self, filename:str='tsfoil_results.png'):
        '''
        Plot all results, including:
        - Mesh distribution analysis (read from XIN, YIN in tsfoil2.out)
        - Mach number distribution on Y=0 line (read from cpxs.dat)
        - Mach number field (read from field.dat)

        Plot three sub-plots in one figure (1x3).
        '''
        
        # Create figure with 3 subplots arranged in 1 rows, 3 column
        fig, axes = plt.subplots(1, 3, figsize=(20, 5))
        fig.suptitle('TSFOIL Results Analysis', fontsize=16, fontweight='bold')
        
        # Plot 1: Mesh Distribution Analysis
        self._plot_mesh_distribution(axes[0])
        
        # Plot 2: Mach Number Distribution on Y=0 line
        self._plot_mach_distribution_y0(axes[1])
        
        # Plot 3: Mach Number Field
        self._plot_mach_field(axes[2])
        
        plt.tight_layout()
        plt.savefig(filename, dpi=300)
    
    def _plot_mesh_distribution(self, ax):
        '''
        Plot mesh distribution analysis
        '''
        xx = self.mesh['xx']
        yy = self.mesh['yy']
        xx_airfoil = self.mesh['xx_airfoil']
        
        #* X/Y-direction mesh density analysis.
        dx = np.diff(xx)
        x_mid = (xx[1:] + xx[:-1]) / 2
        dy = np.diff(yy)
        y_mid = (yy[1:] + yy[:-1]) / 2
        ax.plot(x_mid, dx, 'bo-', markersize=2, linewidth=1, label='X-direction')
        ax.plot(y_mid, dy, 'g*-', markersize=2, linewidth=1, label='Y-direction')
        ax.set_xlabel('x (y)')
        ax.set_ylabel('Δ (mesh spacing)')
        ax.set_title('Mesh Spacing')
        ax.grid(True, alpha=0.3)
        ax.legend()
        ax.set_yscale('log')   

    def _plot_mach_distribution_y0(self, ax):
        '''
        Plot Mach number distribution on Y=0 line from cpxs.dat
        '''
        if not self.data_cpxs:
            self.load_cpxs()
        
        x = self.data_cpxs['x']
        m_up = self.data_cpxs['m_up']
        m_low = self.data_cpxs['m_low']
        
        # Plot Mach number distribution
        ax.plot(x, m_up,  'b.-', linewidth=2, label='Upper Surface')
        ax.plot(x, m_low, 'r.-', linewidth=2, label='Lower Surface')
        
        # Add reference line for sonic condition
        ax.axhline(y=1.0, color='k', linestyle='--', alpha=0.5, label='Sonic (M=1)')
        
        ax.set_xlabel('X/c')
        ax.set_ylabel('Mach Number')
        ax.set_title(f'(Wall) Mach Number Distribution on Y=0')
        ax.grid(True, alpha=0.3)
        ax.legend()
        ax.set_xlim([-0.2, 1.2])
        
    def _plot_mach_field(self, ax):
        '''
        Plot Mach number field from field.dat
        '''
        if not self.data_field:
            self.load_field()

        x = self.data_field['x']
        y = self.data_field['y']
        mach = self.data_field['mach']
        
        # Create contour plot
        max_mach = max(1.5, np.max(mach))
        levels = np.linspace(0, max_mach, 16)
        contour = ax.contourf(x, y, mach, levels=levels, cmap='jet', extend='max')
        
        # Add colorbar
        cbar = plt.colorbar(contour, ax=ax, shrink=0.8)
        cbar.set_label('Mach Number')
        
        # Add airfoil outline
        x_airfoil = self.airfoil['x']
        y_airfoil = self.airfoil['y']
        ax.plot(x_airfoil, y_airfoil, 'k--', linewidth=0.5)
        
        ax.set_xlim([-0.5, 1.5])
        ax.set_ylim([-0.5, 0.5])
        ax.set_xlabel('X/c')
        ax.set_ylabel('Y/c')
        ax.set_title(f'Mach Number Field')
        ax.set_aspect('equal')

if __name__ == "__main__":
    
    # TSFoil Example Usage with Plotting
    print("=" * 30)
    print("TSFoil Python Interface - Example Usage")
    print("=" * 30)
    
    # Initialize TSFoil (you can specify tsfoil.exe path if needed)
    tsfoil = TSFoil()
    
    # Load airfoil data
    print("\n1. Loading airfoil data...")
    tsfoil.set_airfoil(os.path.join(path, 'rae2822.dat'))
    
    tsfoil.set_mesh(
        n_point_x=200,
        n_point_y=80,
        n_point_airfoil=100
    )
    
    tsfoil.set_config({
            "ALPHA": 0.5,
            "EMACH": 0.75,
            "MAXIT": 9999,
            'NWDGE': 0
        })
    
    # Single case execution
    print("\n2. Running single case (Mach 0.75)...")
    tsfoil.exec()
    tsfoil.load_smry()
    tsfoil.plot_all_results()   
    
    print('CL = ', tsfoil.data_summary['CL'])
    print('CD = ', tsfoil.data_summary['CD'])

    