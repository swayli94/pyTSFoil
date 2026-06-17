# pyTSFoil

A Python interface for TSFOIL2, a transonic small-disturbance (TSD) solver for flow past lifting airfoils,
with viscous-inviscid coupling via an Integral Boundary Layer (IBL) method.

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
# >=0.2.4: for TSD only
# >=0.3.3: for TSD + IBL coupling + TE & CSF correction
pip install pytsfoil>=0.3.3

# Test installation
python -c "import pytsfoil; print('pytsfoil', pytsfoil.__version__, 'installed successfully')"

# Optional: Install cst-modeling3d
pip install cst-modeling3d
```

## Quick Start

### Easy mode — `run_airfoil_analysis`

The simplest way to use pyTSFoil.  All solver parameters are pre-tuned;
you only supply the geometry and flight conditions.

```python
import numpy as np
from pytsfoil import run_airfoil_analysis

# Airfoil coordinates: TE (upper) → LE → TE (lower), counter-clockwise
coords = np.loadtxt('rae2822.dat', skiprows=1)   # shape (N, 2)

# One-line viscous analysis (TSD + IBL coupling, recommended defaults)
r = run_airfoil_analysis(coords, Mach=0.75, AoA_degrees=0.5, Re=6.5e6)

# Scalar aerodynamic coefficients
print(f"CL={r['cl']:.5f}  CD_wave={r['cd_wave']:.5f}  "
      f"CD_f={r['cd_friction']:.5f}  CD_total={r['cd_total']:.5f}")

# Pure-inviscid baseline is always included for comparison
b = r['baseline']
print(f"Baseline: CL={b['cl']:.5f}  CD_wave={b['cd_wave']:.5f}")

# Surface distributions (indexed over the full mesh x-line)
ile, ite = r['ile'], r['ite']
cp_upper_foil = r['cpu'][ile:ite+1]   # Cp on the airfoil chord
ma_upper_foil = r['mau'][ile:ite+1]   # Mach on the airfoil chord

# IBL boundary layer results
upper = r['ibl_upper']
print(f"Transition: x_tr_upper={upper['x_tr']:.3f}")
delta_star = upper['delta_star']   # displacement thickness δ*(x)
cf         = upper['cf']           # skin friction coefficient cf(x)
```

Override any parameter without changing the rest:

```python
r = run_airfoil_analysis(
    coords, Mach=0.75, AoA_degrees=0.5, Re=6.5e6,
    configs={
        # TSD solver keys
        'n_point_x':       300,    # denser mesh
        'n_point_airfoil': 150,
        'flag_print_info': False,  # suppress console output
        # IBL coupling keys
        'n_outer':    15,          # more coupling iterations
        'x_tr_upper': 0.05,        # forced transition at 5 % chord
        'x_tr_lower': 0.10,
        # Directory keys
        'work_dir':   '/tmp/my_run',
        'flag_output_shock': True, # write cpxs.dat to work_dir
    },
)
```

Inviscid (TSD only) mode:

```python
r = run_airfoil_analysis(
    coords, Mach=0.75, AoA_degrees=0.5, Re=6.5e6,
    flag_IBL=False,
)
print(f"CL={r['cl']:.5f}  CD_wave={r['cd_wave']:.5f}")
```

**Return value keys**

| Key | Description |
|---|---|
| `cl`, `cm` | Lift and pitching-moment coefficients |
| `cd_wave` | Wave drag (momentum integral method) |
| `cd_friction` | Friction drag (IBL); `0.0` when `flag_IBL=False` |
| `cd_total` | `cd_wave + cd_friction` |
| `xx`, `xx_foil` | x-coordinates of the full mesh and airfoil chord |
| `ile`, `ite` | Leading/trailing edge indices in `xx` |
| `cpu`, `cpl` | Upper/lower surface Cp (full mesh x-line) |
| `mau`, `mal` | Upper/lower surface Mach (full mesh x-line) |
| `cpstar` | Critical pressure coefficient Cp* (sonic condition) |
| `baseline` | Dict with inviscid-only `cl`, `cm`, `cd_wave`, `cpu`, `cpl`, `mau`, `mal` |
| `ibl_upper`, `ibl_lower` | IBL result dicts (`delta_star`, `cf`, `x_tr`, …); `None` when `flag_IBL=False` |
| `history` | Per-iteration dicts from IBL outer loop |
| `solver` | Underlying `PyTSFoil` instance for advanced access |

See `example/rae2822_wrapper/run_wrapper.py` for a complete working example.

## Advanced mode

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
cd       = pytsfoil.data_summary['cd']    # Total drag
```

### Viscous IBL-coupled analysis

```python
from pytsfoil import PyTSFoil, IBL

pytsfoil = PyTSFoil(airfoil_coordinates=airfoil_coordinates, work_dir='output_dir')
pytsfoil.set_config(EMACH=0.75, ALPHA=0.5, REYNLD=6.5e6, MAXIT=9999, RIGF=0.2,
                    n_point_x=200, n_point_y=80, NWDGE=2, flag_print_info=True)

ibl = IBL(Re=6.5e6, M_inf=0.75)

pytsfoil.run()  # Run initial inviscid TSD (warm start for IBL coupling)
# You may save the baseline TSD results here if desired (e.g., cp distributions, cl/cd)

history = pytsfoil.run_ibl_coupled(
    ibl=ibl,
    n_outer=10,             # number of viscous-inviscid coupling cycles
    x_tr_upper=0.0,         # forced transition x/c (None → Michel's criterion)
    x_tr_lower=0.0,
    coupling_relax_final=0.1,   # final relaxation factor for coupling (0–1)
    mach_smooth_sigma = 2.0,    # Gaussian smoothing sigma to the Mach distribution before IBL
    slope_smooth_sigma = 3.0,   # Gaussian smoothing sigma to d(δ*)/dx after IBL
    delta_star_max = 0.05,      # Upper bound on δ*/c clipped
    slope_correction_max = 0.1, # Maximum absolute value of d(δ*)/dx applied to BC
    maxit_inner=200,        # TSD iterations per warm-start
    i_outer_repair=3,       # iteration index to start trailing-edge repair
    use_te_correction=True, # apply TE δ* blending correction
    te_relax=0.5,           # relaxation factor for TE correction (0–1)
    x_blend_start=0.9,      # x/c where the TE correction ramp begins
    use_divergence_check = False,   # Divergence check correction (DCC)
)

# Access coupled results
cl      = pytsfoil.data_summary['cl']
cd_total = pytsfoil.data_summary['cd']
cd_wave = pytsfoil.data_summary['cd_wave']
cd_friction = pytsfoil.data_summary['cd_friction']
upper   = pytsfoil.data_summary['ibl_upper']   # IBL result dict (upper surface)
lower   = pytsfoil.data_summary['ibl_lower']   # IBL result dict (lower surface)

# IBL result dict keys: 's', 'ue', 'theta', 'delta_star', 'H', 'cf',
#                       'x_tr', 'i_tr', 'laminar_mask', 'delta_star_raw'
delta_star_upper = upper['delta_star']
x_transition     = upper['x_tr']
cf_upper         = upper['cf']
```

### Large Mach and AoA correction

When the free-stream Mach number and angle of attack are sufficiently large, the TSD assumptions break down.
Sometimes, the shock can be pushed past the trailing edge (TE), this will trigger a diverged solution or
a non-physical supersonic flow over the entire surface, which is not physically realistic.
To mitigate this, we need to use the TSD-IBL coupling with a Divergence Check Correction (DCC) method.
The DCC method implements a check for numerical divergence in the beginning of coupling iterations.
If divergence is detected, the TSD-IBL coupling restarts with AoA=0 (instead of the target AoA)
to allow the flow to stabilize before ramping up to the full AoA.

In the meantime, PyTSFoil implements a simple correction method that adaptively adds artificial dissipation (`EPS`)
and correction terms (sonic penalty, which drives TE local Mach number towards one)
based on the local flow conditions near TE.
This correction is denoted as "Correction of Full-Supersonic (CFS)",
which is only a simple heuristic approach to recover a more physical solution,
as opposed to a diverged solution or a non-physical supersonic flow over the entire surface.
This is activated by setting `flag_CFS=True` in the configuration.
However, the CFS has no benefit anymore once the DC is activated, therefore, it will be deprecated in the future.

```python
# Correction of Full-Supersonic (CFS) parameters
pytsfoil.set_config(
        flag_CFS=True,      # Flag to enable CFS correction
        BETA_SONIC=100.0,   # Sonic penalty strength multiplier (EPS * BETA_SONIC)
        EPS_AMPL=500.0,     # EPS amplification factor at trailing-edge columns in CFS
        ITER_START_CFS=100  # Minimum iteration count before CFS can trigger
        )

# Divergence Check Correction (DCC)
pytsfoil.run_ibl_coupled(
        ***
        use_divergence_check = True,
        )
```

## Package Structure

```text
pyTSFoil/
├── pytsfoil/
│   ├── __init__.py           # Package init (auto-compiles Fortran if needed)
│   ├── wrapper.py            # run_airfoil_analysis: easy-to-use one-call interface
│   ├── pytsfoil.py           # PyTSFoil class: TSD solver interface + IBL coupling
│   ├── ibl.py                # IBL class: laminar/transition/turbulent BL solver
│   ├── tsfoil_fortran.*      # Compiled Fortran module (generated by compile_f2py.py)
│   ├── compile_f2py.py       # Fortran compilation script
│   └── src/                  # Fortran source files for TSFOIL2 solver
└── example/
    ├── rae2822_wrapper/      # Recommended starting point: run_airfoil_analysis usage
    ├── rae2822/              # Basic inviscid PyTSFoil usage
    ├── rae2822_ibl/          # IBL-coupled TSD: viscous analysis with TE correction
    ├── rae2822_correction/   # IBL-coupled TSD: comparison of different corrections
    └── rae2822_mp/           # Multi-process parallel PyTSFoil usage
```

## API Reference

### `run_airfoil_analysis` (recommended for most users)

```python
from pytsfoil import run_airfoil_analysis

results = run_airfoil_analysis(
    airfoil_coordinates,   # ndarray (N, 2): TE→upper→LE→lower→TE
    Mach,                  # float: free-stream Mach number
    AoA_degrees,           # float: angle of attack in degrees
    Re,                    # float: chord-based Reynolds number
    flag_IBL  = True,      # bool: enable TSD–IBL viscous coupling
    flag_TEC  = True,      # bool: enable trailing-edge δ* correction
    flag_CFS  = True,      # bool: enable full-supersonic correction
    flag_DCC  = True,      # bool: enable divergence check correction (DCC)
    configs   = {},        # dict: override any solver or IBL parameter
)
```

`configs` keys are split automatically into three categories:

| Category | Examples |
|---|---|
| TSD solver (→ `set_config`) | `MAXIT`, `CVERGE`, `EPS`, `RIGF`, `NWDGE`, `n_point_x`, `n_point_y`, `n_point_airfoil`, `flag_output_shock`, `flag_print_info`, … |
| IBL coupling (→ `run_ibl_coupled`) | `n_outer`, `x_tr_upper`, `x_tr_lower`, `maxit_inner`, `coupling_relax_final`, `i_outer_repair`, `te_relax`, `x_blend_start`, … |
| Directories | `work_dir`, `output_dir` |

**Safe parallel usage** with `multiprocessing`:

```python
from multiprocessing import Pool
from pytsfoil import run_airfoil_analysis

def worker(args):
    coords, mach, aoa, re = args
    return run_airfoil_analysis(coords, mach, aoa, re,
                                configs={'flag_print_info': False})

cases = [(coords, 0.73, 0.3, 6.5e6),
         (coords, 0.75, 0.5, 6.5e6),
         (coords, 0.77, 0.8, 6.5e6)]

with Pool(3) as p:
    results = p.map(worker, cases)
```

> **Note**: use `multiprocessing` (separate processes), not `threading`.
> All `PyTSFoil` instances in the same process share Fortran module state.

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
| `MAXIT` | 9999 | Maximum solver iterations |
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
cd_friction = ibl.friction_drag(upper, lower)
```

Physics implemented:

- **Laminar**: Thwaites' method (1949) with White's polynomial correlations
- **Transition**: Michel's criterion (1951)
- **Turbulent**: Head's entrainment ODE (1958), Ludwieg-Tillmann skin friction (1950),
  with compressible von Kármán correction (−Me² term)

## Important Notes

**Fortran compilation**: The Fortran module is automatically compiled on first import.
If you modify the Fortran source files, delete the existing `tsfoil_fortran.*` files to trigger recompilation.
But you should be careful when using multiple python environments with different python versions.
You are suggested to manually compile the Fortran module by calling the `compile_f2py.py`
with the absolute path of the python executable you want to use. For example:

```bash
cd pytsfoil
absolute/path/to/python compile_f2py.py
```

**Data Security Warning**: All `PyTSFoil` instances in the same Python process share underlying Fortran module data.
For parallel analyses, use `multiprocessing.Pool` (each subprocess gets its own isolated Fortran state).
Do **not** use `threading`. See `example/rae2822_mp/` or the `run_airfoil_analysis` parallel example in the API Reference above.

## Version History

- v0.1.*: Initial release with basic TSD solver interface (not fully functional)
- v0.2.*: Basic TSD solver interface (fully functional after v0.2.4; suggested to use v0.2.8)
- v0.3.*: Enhanced IBL coupling framework (in development, suggested to use >=v0.3.3)
