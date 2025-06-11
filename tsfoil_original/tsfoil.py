import os
import sys

path = os.path.dirname(os.path.abspath(__file__))
if path not in sys.path:
    sys.path.append(path)

import numpy as np
import pandas as pd
import os
from io import StringIO
from scipy.optimize import fsolve

import matplotlib.pyplot as plt
from matplotlib.colors import LinearSegmentedColormap
import imageio


NUM_LEVELS = 100
CBAR_TIKCS = 0.1


class Plotter(object):

    def gen_cmap(self):
        c = ["blue","cyan","lightgreen", "yellow","red"]
        v = np.linspace(0, 1, len(c))
        cmap=LinearSegmentedColormap.from_list('bgr',list(zip(v,c)), N=128)
        return cmap

    def gen_png_filename(self, row):
        values = row[["airfoil", "mach", "alpha"]].to_list()
        return "_".join(map(str, values)) +".png"

    def gen_vertical_label(self, row):
        values = row[["mach", "alpha", "cl", "cm", "total cdwave"]].to_list()
        names  = [x.ljust(8) for x in ["Mach", "Alpha", "CL", "CM", "CDwave"]]
        airfoil= row["airfoil"].upper().center(17)
        details= "\n".join(["{}={:8.5f}".format(k,v) for k,v in zip(names, values)])
        return "{}\n{}".format(airfoil, details)

    def gen_horizontal_header(self):
        return "".join([x.center(8) for x in ["Mach", "Alpha", "CL", "CM", "CDwave"]])

    def gen_horizontal_label(self, row):
        values = row[["mach", "alpha", "cl", "cm", "total cdwave"]].to_list()
        return "".join(["{:8.5f}".format(v) for v in values])

    def gen_cpx_mach_ax(self, mach=False):
        fig, ax = plt.subplots(figsize=(8, 6))
        plt.rcParams['font.family'] = 'monospace'

        ax.set_title("Mach-x" if mach else "Cp-x")
        ax.set_ylabel("Mach" if mach else "Cp")
        ax.set_xlabel("x/c")

        ax.set_xlim(-0.1, 1.1)
        if not mach:
            ax.invert_yaxis()
        return ax
        
    def legend_magic(self, ax):
        leg = ax.legend(frameon=False, loc="upper right")
        for item in leg.legend_handles:
            item.set_visible(False)
            
    def plot_cpx(self, buffer_row=-1, save_to_folder=None):
        row = self.buffer.iloc[buffer_row]
        ax = self.gen_cpx_mach_ax(mach=False)
        ax.axhline(y=row["cp*"], color='r', linestyle='--', lw=1)
        ax.plot(np.concatenate([row['x_c'], row['x_c'][::-1]]), 
                np.concatenate([row['cp_up'], row['cp_low'][::-1]]), 
                lw=0.8, label=self.gen_vertical_label(row))

        self.legend_magic(ax)
        
        if save_to_folder:
            filename = os.path.join(save_to_folder, f"cp_x_mach_{row['mach']:.3f}.png")
            plt.savefig(filename, dpi=300, bbox_inches='tight')
            print(f"      Saved: {filename}")
        
        plt.show(block=False)
        
    def plot_machx(self, buffer_row=-1, save_to_folder=None):
        row = self.buffer.iloc[buffer_row]
        ax = self.gen_cpx_mach_ax(mach=True)
        ax.axhline(y=1, color='r', linestyle='--', lw=1)
        ax.plot(np.concatenate([row['x_c'], row['x_c'][::-1]]), 
                np.concatenate([row['m_up'], row['m_low'][::-1]]), 
                lw=0.8, label=self.gen_vertical_label(row))

        self.legend_magic(ax)
        
        if save_to_folder:
            filename = os.path.join(save_to_folder, f"mach_x_mach_{row['mach']:.3f}.png")
            plt.savefig(filename, dpi=300, bbox_inches='tight')
            print(f"      Saved: {filename}")
        
        plt.show(block=False)
        
    def pseq_machx(self, df=None, save_to_folder=None):
        df = df if isinstance(df, pd.core.frame.DataFrame) else self.buffer
        ax = self.gen_cpx_mach_ax(mach=True)
        ax.axhline(y=1, color='r', linestyle='--', lw=1, label=self.gen_horizontal_header())

        for index, row in df.iterrows():
            ax.plot(np.concatenate([row['x_c'], row['x_c'][::-1]]), 
                    np.concatenate([row['m_up'], row['m_low'][::-1]]), 
                    lw=0.8, label=self.gen_horizontal_label(row))

        self.legend_magic(ax)
        
        if save_to_folder:
            filename = os.path.join(save_to_folder, "mach_sequence.png")
            plt.savefig(filename, dpi=300, bbox_inches='tight')
            print(f"      Saved: {filename}")
        
        plt.show(block=False)

    def gen_contour_fig(self, mach=False):
        fig, ax = plt.subplots(figsize=(8, 6))
        plt.rcParams['font.family'] = 'monospace'

        ax.set_title('Contours of Mach' if mach else 'Contours of Cp')
        ax.set_ylabel("y/c")
        ax.set_xlabel("x/c")
        ax.set_xlim(-0.5, 1.5)
        ax.set_ylim(-0.75, 0.75)

        return fig, ax
        
    def plot_contour(self, row, mach=False, savepng=False, custom_filename=None):
        fig, ax  = self.gen_contour_fig(mach)

        X, Y = np.meshgrid(row["mesh_x"], row["mesh_y"])
        Z = row["mach_map"] if mach else row["cp_map"]

        increment = (Z.max() - Z.min())/NUM_LEVELS
        ax.text(-0.4, -0.7, "Increment :{:1.3f}".format(increment), fontsize=8, color='black')

        foil=[(0, 1), (0, 0), 'black']
        ax.plot(*foil, label=self.gen_vertical_label(row))
        
        cs = ax.contour(X, Y, Z, 
                levels=np.linspace(Z.min(), Z.max(), NUM_LEVELS),
                linewidths=0.5,
                cmap=self.gen_cmap())

        sm = plt.cm.ScalarMappable(cmap = cs.cmap)
        sm.set_array(Z)  # Set the array for the ScalarMappable
        fig.colorbar(sm, ax=ax, ticks=np.arange(Z.min(), Z.max(), CBAR_TIKCS))

        self.legend_magic(ax)

        if savepng:
            if custom_filename:
                filename = custom_filename
            else:
                filename = self.gen_png_filename(row)
            plt.savefig(filename, dpi=300, bbox_inches='tight')
            plt.close()
            return filename
        plt.show(block=False)

    def plot_isomach(self, buffer_row=-1, savepng=False, save_to_folder=None):
        if save_to_folder and not savepng:
            # If save_to_folder is specified, automatically save
            row = self.buffer.iloc[buffer_row]
            filename = os.path.join(save_to_folder, f"isomach_mach_{row['mach']:.3f}.png")
            self.plot_contour(row, True, savepng=True, custom_filename=filename)
            print(f"      Saved: {filename}")
        else:
            self.plot_contour(self.buffer.iloc[buffer_row], True, savepng)

    def plot_isocp(self, buffer_row=-1, savepng=False, save_to_folder=None):
        if save_to_folder and not savepng:
            # If save_to_folder is specified, automatically save
            row = self.buffer.iloc[buffer_row]
            filename = os.path.join(save_to_folder, f"isocp_mach_{row['mach']:.3f}.png")
            self.plot_contour(row, False, savepng=True, custom_filename=filename)
            print(f"      Saved: {filename}")
        else:
            self.plot_contour(self.buffer.iloc[buffer_row], False, savepng)

    def plot_grid(self, save_to_folder=None):
        row = self.buffer.iloc[-1]
        fig, ax = plt.subplots(figsize=(8, 6))

        ax.text(-0.4, -0.7, "{} x {}".format(len(row["mesh_x"]), len(row["mesh_y"])), fontsize=8)

        X, Y = np.meshgrid(row["mesh_x"], row["mesh_y"])
        ax.vlines(X[0], *Y[[0,-1],0], lw=0.5)
        ax.hlines(Y[:,0], *X[0, [0,-1]], lw=0.5)

        foil=[(0, 1), (0, 0), 'black']
        ax.plot(*foil)

        ax.set_title('Grid Specification')
        ax.set_ylabel("y/c")
        ax.set_xlabel("x/c")
        ax.set_xlim(-0.5, 1.5)
        ax.set_ylim(-0.75, 0.75)

        if save_to_folder:
            filename = os.path.join(save_to_folder, "grid_layout.png")
            plt.savefig(filename, dpi=300, bbox_inches='tight')
            print(f"      Saved: {filename}")

        plt.show(block=False)

    def gplot(self, x, y, row=None, color='red', save_to_folder=None):
        fig, ax = plt.subplots(figsize=(8, 6))
        ax.set_title("{} vs {}".format(x.upper(), y.upper()))
        ax.set_xlabel("{}".format(x.upper()))
        ax.set_ylabel("{}".format(y.upper()))
        if row:
            ax.plot(self.buffer.iloc[row][x], self.buffer.iloc[row][y], lw=0.8, color=color)
        else:
            ax.plot(self.buffer[x], self.buffer[y], lw=0.8, color=color)
            
        if save_to_folder:
            safe_x = x.replace(" ", "_").replace("/", "_")
            safe_y = y.replace(" ", "_").replace("/", "_")
            filename = os.path.join(save_to_folder, f"{safe_x}_vs_{safe_y}.png")
            plt.savefig(filename, dpi=300, bbox_inches='tight')
            print(f"      Saved: {filename}")
            
        plt.show(block=False)

    def animate(self, mach=True):
        images = []
        for index, row in self.buffer.iterrows():
            images.append(imageio.imread(self.plot_contour(row, mach=mach, savepng=True)))
        imageio.mimsave("Animation.gif", images, fps=5)


class TSFoil(Plotter):
    
    def __init__(self, tsfoil_exe_path=None):
        self.airfoil      = {}
        self.config       = {"ICUT" :0, "MAXIT":2500}
        self.current_foil = ""
        self.buffer       = pd.DataFrame()
        self.LEN_HEADER   = 26
        self.basename_foil= ""
        
        # Set the path to tsfoil.exe
        if tsfoil_exe_path is None:
            # Default to tsfoil.exe in the same directory as this script
            script_dir = os.path.dirname(os.path.abspath(__file__))
            self.tsfoil_exe_path = os.path.join(script_dir, "tsfoil.exe")
        else:
            # Use user-defined path
            if os.path.isfile(tsfoil_exe_path):
                self.tsfoil_exe_path = tsfoil_exe_path
            elif os.path.isdir(tsfoil_exe_path):
                # If directory provided, append tsfoil.exe
                self.tsfoil_exe_path = os.path.join(tsfoil_exe_path, "tsfoil.exe")
            else:
                # Assume it's a full path even if file doesn't exist yet
                self.tsfoil_exe_path = tsfoil_exe_path

    def load(self, datfile):
        x, y = np.loadtxt(datfile, skiprows=1).T
        le_pos = x.argmin()

        xu = x[:le_pos+1][::-1]
        yu = y[:le_pos+1][::-1]
        xl = x[le_pos:]
        yl = y[le_pos:]

        fmt={'float_kind':'{:9.7f}'.format}

        self.airfoil["XU"] = np.array2string(xu, formatter=fmt, separator=", ")[1:-1]
        self.airfoil["YU"] = np.array2string(yu, formatter=fmt, separator=", ")[1:-1]
        self.airfoil["XL"] = np.array2string(xl, formatter=fmt, separator=", ")[1:-1]
        self.airfoil["YL"] = np.array2string(yl, formatter=fmt, separator=", ")[1:-1]

        self.current_foil  = datfile
        self.basename_foil = datfile.replace(".dat", "")
        self.inp_file      = "{}.inp".format(self.basename_foil)

    def set(self, config_dict):
        self.config.update(config_dict)

    def gen_inp(self, filename, title="Input File for TSFOIL"):
        settings    = "\n".join(["{} = {}".format(k,v) for k,v in self.config.items()])
        cordinates  = "\n".join(["{} = {}".format(k,v) for k,v in self.airfoil.items()])
        with open(filename, 'w') as f:
            f.write("{}\n$INP\n{}\n{}\n$END\n".format(title,settings,cordinates))    
    
    def extract_smry(self, filename="smry.out"):
        # Check if file exists and is not empty
        if not os.path.exists(filename):
            raise FileNotFoundError(f"Summary file {filename} not found")
        
        if os.path.getsize(filename) == 0:
            raise ValueError(f"Summary file {filename} is empty - tsfoil.exe likely failed to execute properly")
            
        df = pd.read_csv(filename, skiprows=9, sep="=", header=None).set_index(0).T
        df.columns = [c.strip().lower() for c in df.columns.str.replace('0',"")]
        return df

    def extract_mesh(self, filename="mesh.out"):
        x, y = open(filename).read().split("0\n\n")[1:]
        x = np.genfromtxt(StringIO(x[self.LEN_HEADER:].replace("\n", "")))[:-1]
        y = np.genfromtxt(StringIO(y[self.LEN_HEADER:].replace("\n", "")))[::-1][:-1]
        return x,y

    def extract_cpxs(self, filename="cpxs.out"):
        return np.genfromtxt(filename, skip_header=5).T

    def extract_mmap(self, filename="mmap.out"):
        return np.loadtxt(filename)

    def extract_cpmp(self, filename="cpmp.out"):
        return np.loadtxt(filename)

    def extract_cnvg(self, filename="smry.out"):
        return "SOLUTION CONVERGED" in open(filename).read()

    def extract_iter(self, filename="cnvg.out"):
        iteration, error = np.loadtxt("cnvg.out", skiprows=2, usecols=(0,5)).T
        return iteration, error

    def extract_res(self):
        df = self.extract_smry()
        df["converged"] = self.extract_cnvg()
        df.insert(0,'airfoil', self.current_foil)
        x, y = self.extract_mesh()
        df["mesh_x"] = [x]
        df["mesh_y"] = [y]

        i, x_c, cp_up, m_up, cp_low, m_low = self.extract_cpxs()
        df["x_c"]   = [x_c]
        df["cp_up"] = [cp_up]
        df["m_up"]  = [m_up]
        df["cp_low"]= [cp_low]
        df["m_low"] = [m_low]

        iteration, error = self.extract_iter()
        df["iter"]  = [iteration]
        df["error"] = [error]
        df["mach_map"] = [self.extract_mmap()]
        df["cp_map"]   = [self.extract_cpmp()]

        self.buffer = pd.concat([self.buffer, df], ignore_index=True)    
    
    def exec(self, key=None, value=None, absorb=True):
        if key:
            self.config[key.upper()] = value

        self.gen_inp(self.inp_file)
        
        # Check if tsfoil.exe exists
        if not os.path.isfile(self.tsfoil_exe_path):
            raise FileNotFoundError(f"tsfoil.exe not found at: {self.tsfoil_exe_path}")
        
        # Use relative path for input file to avoid path length issues
        inp_file_relative = os.path.basename(self.inp_file)
        
        # Print debug information
        print(f"Executing: {self.tsfoil_exe_path}")
        print(f"Input file (relative): {inp_file_relative}")
        print(f"Working directory: {os.getcwd()}")
        
        # Use subprocess instead of os.system for better error handling
        import subprocess
        try:
            # Use relative path for input file to avoid tsfoil.exe path length limitations
            cmd = [self.tsfoil_exe_path, inp_file_relative]
            print(f"Command: {cmd}")
            
            result = subprocess.run(cmd, 
                                  capture_output=True, text=True, cwd=os.getcwd(),
                                  shell=False)
            
            print(f"Exit code: {result.returncode}")
            if result.stdout:
                print(f"stdout: {result.stdout}")
            if result.stderr:
                print(f"stderr: {result.stderr}")
                
            if result.returncode != 0:
                print(f"Warning: tsfoil.exe returned exit code {result.returncode}")
                
        except FileNotFoundError as e:
            print(f"Failed to execute tsfoil.exe: {e}")
            raise

        if absorb:
            self.extract_res()

    def fixed_cl(self, cl, alpha_0=1):
        def exec_wrapper(alpha):
            self.exec("ALPHA", value=alpha.item(), absorb=False)
            return self.extract_smry()["cl"].item() - cl
        res = fsolve(exec_wrapper, x0=alpha_0, xtol=0.005)
        if res:
            self.extract_res()


if __name__ == "__main__":
    
    # TSFoil Example Usage with Plotting
    print("=" * 60)
    print("TSFoil Python Interface - Example Usage")
    print("=" * 60)
    
    # Create plots directory
    plots_dir = "plots"
    if not os.path.exists(plots_dir):
        os.makedirs(plots_dir)
        print(f"Created plots directory: {plots_dir}")
    else:
        print(f"Using existing plots directory: {plots_dir}")
    
    # Initialize TSFoil (you can specify tsfoil.exe path if needed)
    tsfoil = TSFoil()
    
    # Load airfoil data
    print("\n1. Loading airfoil data...")
    tsfoil.load(os.path.join(path, "rae2822.dat"))
    tsfoil.set({"alpha": 0.0})  # Set angle of attack to 0.0 degrees
    
    # Single case execution
    print("\n2. Running single case (Mach 0.75)...")
    tsfoil.exec("EMACH", 0.75)
    
    # Display buffer information
    print(f"\nBuffer contains {len(tsfoil.buffer)} case(s)")
    print("Available data columns:", list(tsfoil.buffer.columns))
    
    # Plot single case results
    print("\n3. Plotting single case results...")
    
    print("   - Grid layout")
    tsfoil.plot_grid(save_to_folder=plots_dir)
    
    print("   - Cp-x distribution")
    tsfoil.plot_cpx(save_to_folder=plots_dir)
    
    print("   - Mach-x distribution") 
    tsfoil.plot_machx(save_to_folder=plots_dir)
    
    print("   - Mach contours")
    tsfoil.plot_isomach(save_to_folder=plots_dir)
    
    print("   - Cp contours")
    tsfoil.plot_isocp(save_to_folder=plots_dir)
    
    # Clear buffer for sweep study
    print("\n4. Performing Mach number sweep...")
    tsfoil.buffer = pd.DataFrame()  # Clear previous results
    
    # Mach number sweep from 0.5 to 0.95
    mach_range = np.arange(0.5, 0.9, 0.1)
    print(f"   Running {len(mach_range)} cases from Mach {mach_range[0]} to {mach_range[-1]}")
    
    for i, m in enumerate(mach_range):
        print(f"   Case {i+1}/{len(mach_range)}: Mach {m:.3f}")
        try:
            tsfoil.exec("EMACH", m)
        except Exception as e:
            print(f"   Warning: Case Mach {m:.3f} failed: {e}")
            continue
    
    print(f"\nSweep completed. Buffer now contains {len(tsfoil.buffer)} cases")
    
    # Display first few results
    if len(tsfoil.buffer) > 0:
        print("\nFirst 5 cases summary:")
        summary_cols = ["mach", "alpha", "cl", "cm", "total cdwave", "converged"]
        available_cols = [col for col in summary_cols if col in tsfoil.buffer.columns]
        print(tsfoil.buffer[available_cols].head())
        
        # Check convergence
        if "converged" in tsfoil.buffer.columns:
            converged_cases = tsfoil.buffer["converged"].sum()
            print(f"\nConverged cases: {converged_cases}/{len(tsfoil.buffer)}")
        
        # Plot sweep results
        print("\n5. Plotting sweep results...")
        
        # Plot sequence of Mach distributions (every 4th case)
        print("   - Mach distribution sequence (every 4th case)")
        if len(tsfoil.buffer) > 4:
            tsfoil.pseq_machx(tsfoil.buffer.iloc[::4], save_to_folder=plots_dir)
        
        # Plot drag divergence curve
        if "total cdwave" in tsfoil.buffer.columns:
            print("   - Drag divergence curve")
            tsfoil.gplot("mach", "total cdwave", save_to_folder=plots_dir)
        
        # Plot convergence history for last case
        if "iter" in tsfoil.buffer.columns and "error" in tsfoil.buffer.columns:
            print("   - Convergence history (last case)")
            tsfoil.gplot("iter", "error", row=-1, color='blue', save_to_folder=plots_dir)
        
        # Optional: Generate animation (commented out as it takes time)
        print("\n6. Animation generation (optional)...")
        print("   To generate Cp contour animation, uncomment the following line:")
        print("   # tsfoil.animate(mach=False)")
        print("   To generate Mach contour animation, uncomment the following line:")
        print("   # tsfoil.animate(mach=True)")
        
        # Uncomment one of these to generate animation:
        # tsfoil.animate(mach=False)  # Cp contour animation
        tsfoil.animate(mach=True)   # Mach contour animation
        
    print("\n" + "=" * 60)
    print("Example completed! Close plot windows to exit.")
    print("=" * 60)
