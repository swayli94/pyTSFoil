# pyTSFoil

A Python interface for TSFOIL2, an inviscid transonic small-disturbance (TSD) solver for flow past lifting airfoils.

## Overview

TSFOIL2 is a CFD solver known for its rapid solution time, ease of use, and open-source architecture.
It solves the transonically-scaled perturbation potential and similarity variables to compute the following quantities:

- Pressure coefficient distribution (Cp) along airfoil surfaces
- Lift and drag coefficients through surface integration
- Transonic flow field analysis

**Reference**: Murman, E.M., Bailey, F.R., and Johnson, M.L., "TSFOIL - A Computer Code for Two-Dimensional Transonic Calculations, Including Wind-Tunnel Wall Effects and Wave Drag Evaluation," NASA SP-347, March 1975.

**Original TSFOIL2**: <http://www.dept.aoe.vt.edu/~mason/Mason_f/MRsoft.html#TSFOIL2>

## Features

- **Fast CFD Analysis**: Direct Python interface to modernized Fortran TSFOIL2 solver
- **Flexible Input**: Support for airfoil coordinate files or numpy arrays
- **Comprehensive Output**: Pressure distributions, flow fields, lift/drag coefficients
- **Visualization**: Built-in plotting capabilities for results analysis
- **Example Cases**: Includes RAE2822 airfoil test cases and demonstrations

## Installation

### Prerequisites

- Python 3.8 or higher
- NumPy, SciPy, Matplotlib
- Fortran compiler for f2py, meson compilation (gfortran is recommended)
- Linux is recommended (for easier usage of meson)
- cst-modeling3d is recommended (for airfoil geometric modelling)

### Install Package

```bash
sudo apt update
sudo apt install gfortran

# Install from source
git clone https://github.com/swayli94/pyTSFoil.git
cd pyTSFoil
pip install -e .

# Or install from PyPI
pip install pytsfoil>=0.2.1

# Test installation
python -c "import pytsfoil; print('✓ pytsfoil installed successfully')" || echo "pytsfoil failed to import"

# Optional: Install cst-modeling3d
pip install cst-modeling3d
```

## Quick Start

```python
from pytsfoil import PyTSFoil

# Load airfoil coordinates (or provide as numpy array)
pytsfoil = PyTSFoil(
    airfoil_coordinates=airfoil_coordinates,  # coordinates of the airfoil (x, y) [n_points, 2]
    airfoil_file='path/to/airfoil.dat',       # file containing the airfoil geometry (provide either airfoil_coordinates or airfoil_file)
    work_dir='output_directory',              # output directory for Fortran output files (smry.out, tsfoil2.out)
    output_dir='output_directory',            # output directory for Python output files (cpxs.dat, field.dat)
)

# Configure flow conditions
pytsfoil.set_config(
    ALPHA=0.5,      # Angle of attack (degrees)
    EMACH=0.75,     # Mach number
    MAXIT=9999,     # Maximum iterations
    n_point_x=200,  # Grid points in x-direction
    n_point_y=80,   # Grid points in y-direction
    EPS=0.2,        # Grid stretching parameter
    CVERGE=1e-6,    # Convergence criteria
    flag_output=True,           # write solver process to tsfoil2.out
    flag_output_summary=True,   # write summary to smry.out
    flag_output_shock=True,     # write shock data to cpxs.dat
    flag_output_field=True,     # write field data to field.dat
    flag_print_info=True,       # print information to console
)

# Run analysis
pytsfoil.run()

# Plot results
pytsfoil.plot_all_results()

# Access results
cp_upper = pytsfoil.data_summary['cp_upper']
cp_lower = pytsfoil.data_summary['cp_lower']
cl = pytsfoil.data_summary['cl']
cd = pytsfoil.data_summary['cd']
```

## Package Structure

```text
pyTSFoil/
├── pytsfoil.py           # Main PyTSFoil class and CFD interface
├── tsfoil_fortran.*      # Compiled Fortran module
├── compile_f2py.py       # Fortran compilation script
├── __init__.py           # Package initialization
└── example/              # Example cases
    ├── rae2822/          # Basic PyTSFoil usage example
    └── rae2822_mp/       # Multi-process PyTSFoil usage example
```

## Important Notes

**Data Security Warning**: All PyTSFoil instances in the same Python process share underlying Fortran module data. For thread safety:

- Use only one PyTSFoil instance per Python process
- For parallel analyses, use `multiprocessing.Pool`
- Each subprocess gets isolated Fortran data

**Safe parallel usage**:

```python
import multiprocessing as mp

def run_analysis(params):
    pytsfoil = PyTSFoil()  # Each process gets its own data
    # ... run analysis
    return results

with mp.Pool() as pool:
    results = pool.map(run_analysis, case_list)
```
