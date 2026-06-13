# pyTSFoil

A Python interface for TSFOIL2, a transonic small-disturbance (TSD) solver for flow past lifting airfoils, with viscous-inviscid coupling via an Integral Boundary Layer (IBL) method.

## Overview

TSFOIL2 is a CFD solver known for its rapid solution time, ease of use, and open-source architecture.
It solves the transonically-scaled perturbation potential and similarity variables to compute the following quantities:

- Pressure coefficient distribution (Cp) along airfoil surfaces
- Lift and drag coefficients through surface integration
- Transonic flow field analysis

The IBL module adds viscous effects via an effective-body (wall-slope) coupling approach, enabling:

- Laminar and turbulent boundary layer development (Thwaites → Michel → Head)
- Displacement thickness correction to the TSD wall boundary condition
- Skin friction drag estimation

**Reference**: Murman, E.M., Bailey, F.R., and Johnson, M.L., "TSFOIL - A Computer Code for Two-Dimensional Transonic Calculations, Including Wind-Tunnel Wall Effects and Wave Drag Evaluation," NASA SP-347, March 1975.

**Original TSFOIL2**: <http://www.dept.aoe.vt.edu/~mason/Mason_f/MRsoft.html#TSFOIL2>

## Features

- **Fast CFD Analysis**: Direct Python interface to modernized Fortran TSFOIL2 solver
- **Viscous-Inviscid Coupling**: IBL displacement-thickness wall-slope correction
  via `run_ibl_coupled()`
- **Boundary Layer Physics**: Thwaites (laminar), Michel's criterion (transition),
  Head's entrainment method (turbulent),
  with compressible von Kármán correction for transonic flow
- **TE Correction**: Optional trailing-edge slope correction within the IBL framework
  to represent (boundary layer trailing-edge separation) wake effects
- **Flexible Input**: Support for airfoil coordinate files or numpy arrays
- **Comprehensive Output**: Pressure distributions, flow fields, lift/drag coefficients,
  boundary layer quantities
- **Visualization**: Built-in plotting capabilities for results analysis
- **Example Cases**: Inviscid, viscous (IBL-coupled), and multi-process RAE2822 examples

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
# 0.2.4: for TSD only; 0.3.0+: for TSD + IBL coupling
pip install pytsfoil>=0.3.0

# Test installation
python -c "import pytsfoil; print('pytsfoil', pytsfoil.__version__, 'installed successfully')"

# Optional: Install cst-modeling3d
pip install cst-modeling3d
```

## Quick Start

### Inviscid TSD analysis

```python
from pytsfoil import PyTSFoil

pytsfoil = PyTSFoil(
    airfoil_coordinates=airfoil_coordinates,  # ndarray [n_points, 2], TE→upper→LE→lower→TE
    # airfoil_file='path/to/airfoil.dat',     # alternative: load from file
    work_dir='output_directory',              # directory for Fortran output files (smry.out, tsfoil2.out)
    output_dir='output_directory',            # directory for Python output files (cpxs.dat, field.dat)
)

pytsfoil.set_config(
    ALPHA=0.5,      # Angle of attack (degrees)
    EMACH=0.75,     # Mach number
    REYNLD=6.5e6,   # Reynolds number (used by IBL/Viscous wedge; harmless for inviscid run)
    MAXIT=9999,     # Maximum iterations
    n_point_x=200,  # Grid points in x-direction
    n_point_y=80,   # Grid points in y-direction
    EPS=0.2,        # Artificial viscosity parameter
    CVERGE=1e-6,    # Convergence criterion
    flag_output=True,
    flag_output_summary=True,
    flag_output_shock=True,
    flag_output_field=True,
    flag_print_info=True,
)

pytsfoil.run()
pytsfoil.plot_all_results()

# Access results
cp_upper = pytsfoil.data_summary['cpu']   # Cp on upper surface (full mesh x-line)
cp_lower = pytsfoil.data_summary['cpl']   # Cp on lower surface
ma_upper = pytsfoil.data_summary['mau']   # Wall Mach number, upper
ma_lower = pytsfoil.data_summary['mal']   # Wall Mach number, lower
cl       = pytsfoil.data_summary['cl']
cd       = pytsfoil.data_summary['cd']    # Wave drag (momentum integral method)
```

### Viscous IBL-coupled analysis

```python
from pytsfoil import PyTSFoil, IBL

pytsfoil = PyTSFoil(airfoil_coordinates=airfoil_coordinates, work_dir='output_dir')
pytsfoil.set_config(EMACH=0.75, ALPHA=0.5, REYNLD=6.5e6, MAXIT=9999,
                    n_point_x=200, n_point_y=80, NWDGE=0, flag_print_info=True)

ibl = IBL(Re=6.5e6, M_inf=0.75)

pytsfoil.run()  # Run initial inviscid TSD (warm start for IBL coupling)
# You may save the baseline TSD results here if desired (e.g., cp distributions, cl/cd)

history = pytsfoil.run_ibl_coupled(
    ibl=ibl,
    n_outer=10,              # number of viscous-inviscid coupling cycles
    x_tr_upper=0.0,          # forced transition x/c (None → Michel's criterion)
    x_tr_lower=0.0,
    ibl_relax=0.5,           # under-relaxation for wall-slope update
    maxit_inner=200,         # TSD iterations per warm-start
    i_outer_repair=3,        # iteration index to start trailing-edge repair
    use_te_correction=True,  # apply TE δ* blending correction
    d_angle_TE=0.0,          # target TE slope deviation from free-stream (degrees)
    x_blend_start=0.9,       # x/c where the TE correction ramp begins
)

# Access coupled results
cl      = pytsfoil.data_summary['cl']
cd_wave = pytsfoil.data_summary['cd']
cd_f    = pytsfoil.data_summary['ibl_cd_f']    # friction drag
cd_tot  = cd_wave + cd_f
upper   = pytsfoil.data_summary['ibl_upper']   # IBL result dict (upper surface)
lower   = pytsfoil.data_summary['ibl_lower']   # IBL result dict (lower surface)

# IBL result dict keys: 's', 'ue', 'theta', 'delta_star', 'H', 'cf',
#                       'x_tr', 'i_tr', 'laminar_mask', 'delta_star_raw'
delta_star_upper = upper['delta_star']
x_transition     = upper['x_tr']
cf_upper         = upper['cf']
```

## Package Structure

```text
pyTSFoil/
├── pytsfoil/
│   ├── __init__.py           # Package init (auto-compiles Fortran if needed)
│   ├── pytsfoil.py           # PyTSFoil class: TSD solver interface + IBL coupling
│   ├── ibl.py                # IBL class: laminar/transition/turbulent BL solver
│   ├── tsfoil_fortran.*      # Compiled Fortran module (generated by compile_f2py.py)
│   ├── compile_f2py.py       # Fortran compilation script
│   └── src/                  # Fortran source files for TSFOIL2 solver
└── example/
    ├── rae2822/              # Basic inviscid PyTSFoil usage
    ├── rae2822_ibl/          # IBL-coupled TSD: viscous analysis with TE correction
    └── rae2822_mp/           # Multi-process parallel PyTSFoil usage
```

## API Reference

### `PyTSFoil`

| Method | Description |
|---|---|
| `__init__(airfoil_coordinates, airfoil_file, work_dir, output_dir)` | Initialize solver |
| `set_config(**kwargs)` | Set flow and solver parameters |
| `run()` | Run inviscid TSD analysis |
| `run_ibl_coupled(ibl, n_outer, ...)` | Run viscous-inviscid coupled analysis |
| `plot_all_results(filename)` | Plot Mach distribution and Mach field |

Key `set_config` parameters:

| Parameter | Default | Description |
|---|---|---|
| `EMACH` | 0.75 | Freestream Mach number |
| `ALPHA` | 0.0 | Angle of attack (degrees) |
| `REYNLD` | 4.0e6 | Reynolds number (used by IBL) |
| `MAXIT` | 1000 | Maximum solver iterations |
| `CVERGE` | 1e-5 | Convergence criterion |
| `EPS` | 0.2 | Artificial viscosity parameter (0–1) |
| `SIMDEF` | 3 | Similarity scaling: 1=Cole, 2=Spreiter, 3=Krupp |
| `NWDGE` | 0 | Viscous wedge: 0=none, 1=Murman, 2=Yoshihara |
| `n_point_x` | 81 | Grid points in x-direction |
| `n_point_y` | 60 | Grid points in y-direction |
| `n_point_airfoil` | 51 | Grid points over the airfoil chord |

### `IBL`

Integral Boundary Layer solver for 2D airfoil flows.

```python
ibl = IBL(Re=6.5e6, M_inf=0.75)

result = ibl.run(
    xx=pytsfoil.mesh['xx'][ile:ite+1],   # x/c coordinates
    mach=pytsfoil.data_summary['mau'][ile:ite+1],  # edge Mach
    yy=yu_foil,                          # surface y/c (optional, improves arc-length)
    x_tr_forced=None,                    # forced transition x/c (None → Michel)
)
cd_f = ibl.friction_drag(upper, lower)
```

Physics implemented:

- **Laminar**: Thwaites' method (1949) with White's polynomial correlations
- **Transition**: Michel's criterion (1951)
- **Turbulent**: Head's entrainment ODE (1958), Ludwieg-Tillmann skin friction (1950),
  with compressible von Kármán correction (−Me² term)

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

**IBL coupling requirements**:

- Set `NWDGE=0` when using `run_ibl_coupled()` — the viscous wedge option is incompatible with the displacement-thickness wall-slope correction
- `run_ibl_coupled()` performs the initial inviscid solve automatically; do not call `run()` beforehand

## Version History

- v0.1.*: Initial release with basic TSD solver interface (not fully functional)
- v0.2.*: Basic TSD solver interface (fully functional after v0.2.4; suggested to use v0.2.8)
- v0.3.*: Enhanced IBL coupling framework (in development, fully functional after v0.3.0)
