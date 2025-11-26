# pyTSFoil

A Python interface for TSFOIL2, an inviscid transonic small-disturbance (TSD) solver for flow past lifting airfoils. This package provides both a direct Python API for computational fluid dynamics analysis and OpenAI Gym-compatible environments for reinforcement learning applications.

## Overview

TSFOIL2 is a CFD solver unknown for its rapid solution time, ease of use, and open-source architecture. It solves the transonically-scaled perturbation potential and similarity variables to compute:

- Pressure coefficient distribution (Cp) along airfoil surfaces
- Lift and drag coefficients through surface integration
- Transonic flow field analysis

**Reference**: Murman, E.M., Bailey, F.R., and Johnson, M.L., "TSFOIL - A Computer Code for Two-Dimensional Transonic Calculations, Including Wind-Tunnel Wall Effects and Wave Drag Evaluation," NASA SP-347, March 1975.

**Original TSFOIL2**: <http://www.dept.aoe.vt.edu/~mason/Mason_f/MRsoft.html#TSFOIL2>

## Features

- **Fast CFD Analysis**: Direct Python interface to modernized Fortran TSFOIL2 solver
- **Reinforcement Learning Ready**: Multiple gym environments for RL applications
- **Flexible Input**: Support for airfoil coordinate files or numpy arrays
- **Comprehensive Output**: Pressure distributions, flow fields, lift/drag coefficients
- **Visualization**: Built-in plotting capabilities for results analysis
- **Example Cases**: Includes RAE2822 airfoil test cases and demonstrations

## Installation

### Prerequisites

- Python 3.8 or higher
- NumPy, SciPy, Matplotlib
- Fortran compiler (for f2py compilation)
- OpenAI Gym (for RL environments)

### Install Dependencies

```bash
pip install numpy scipy matplotlib gymnasium
```

### Compile Fortran Module

The package requires compilation of the Fortran module using f2py:

```bash
cd pyTSFoil
python compile_f2py.py
```

### Install Package

```bash
pip install -e .
```

## Quick Start

### Basic CFD Analysis

```python
import os
import numpy as np
from pyTSFoil.pytsfoil import PyTSFoil

# Load airfoil coordinates (or provide as numpy array)
pytsfoil = PyTSFoil(
    airfoil_file='path/to/airfoil.dat',
    work_dir='output_directory'
)

# Configure flow conditions
pytsfoil.set_config(
    ALPHA=0.5,      # Angle of attack (degrees)
    EMACH=0.75,     # Mach number
    MAXIT=9999,     # Maximum iterations
    n_point_x=200,  # Grid points in x-direction
    n_point_y=80,   # Grid points in y-direction
    EPS=0.2,        # Grid stretching parameter
    CVERGE=1e-6     # Convergence criteria
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

### Gym Environment for Reinforcement Learning

```python
import numpy as np
from pyTSFoil.environment.env_template import TSFoilEnv_Template

# Load airfoil coordinates
x, y = np.loadtxt('rae2822.dat', skiprows=1).T
airfoil_coordinates = np.column_stack((x, y))

# Create environment
env = TSFoilEnv_Template(
    airfoil_coordinates=airfoil_coordinates,
    output_dir='./output',
    render_mode='save'
)

# Run environment
env.reset()
env.render()

# Take actions (e.g., modify Mach number)
for i in range(5):
    action = np.array([1.02 + 0.02*i])  # Mach number values
    obs, reward, terminated, truncated, info = env.step(action)
    env.render()

# Save trajectory
env.save_trajectory('trajectory.json')
env.close()
```

## Package Structure

```text
pyTSFoil/
├── pytsfoil.py           # Main PyTSFoil class and CFD interface
├── tsfoil_fortran.*      # Compiled Fortran module
├── compile_f2py.py       # Fortran compilation script
└── environment/          # Gym environments
    ├── basic.py          # Basic class for environment
    ├── feature.py        # Feature extraction functions
    ├── env_template.py   # Template environment
    └── utils.py          # Additional environment variants

example/
├── rae2822/              # Basic PyTSFoil usage example
├── env_template/         # Template environment example
├── env_FigState_GlobalAction/  # Global action RL environment
└── env_FigState_BumpAction/    # Bump action RL environment
```

## Available Environments

1. **TSFoilEnv_Template**: Basic template for custom RL environments
2. **TSFoilEnv_FigState_GlobalAction**: Environment with figure-based observations and global actions
3. **TSFoilEnv_FigState_BumpAction**: Environment with figure-based observations and bump-based airfoil modifications

## Important Notes

⚠️ **Data Security Warning**: All PyTSFoil instances in the same Python process share underlying Fortran module data. For thread safety:

- Use only one PyTSFoil instance per Python process
- For parallel analyses, use `multiprocessing.Pool`
- Each subprocess gets isolated Fortran data

**Safe parallel usage**:

```python
import multiprocessing as mp

def run_analysis(params):
    pytsfoil = PyTSFoil()  # ✅ Each process gets its own data
    # ... run analysis
    return results

with mp.Pool() as pool:
    results = pool.map(run_analysis, case_list)
```

## Examples

The `example/` directory contains several demonstration cases:

- **rae2822/**: Direct PyTSFoil usage with RAE2822 airfoil
- **env_template/**: Basic gym environment usage
- **env_FigState_GlobalAction/**: Advanced RL environment with global actions
- **env_FigState_BumpAction/**: RL environment with localized airfoil modifications
