Examples
========

All example scripts live in the ``example/`` directory of the repository.

.. contents:: Contents
   :local:
   :depth: 1

RAE 2822 — easy-mode wrapper
------------------------------

**Path:** ``example/rae2822_wrapper/run_wrapper.py``

Demonstrates :func:`~pytsfoil.run_airfoil_analysis` — the recommended starting point
for new users.

* Loads the RAE 2822 airfoil from ``rae2822.dat``.
* Runs a fully coupled TSD + IBL analysis.
* Prints aerodynamic coefficients and plots Cp, Mach, and boundary-layer quantities.

**Typical output:**

.. code-block:: text

   [pyTSFoil] Inviscid baseline  Ma=0.750  AoA=0.50°  Re=6.50e+06
     CL=0.82043  CM=-0.11304  CD_wave=0.01521

   [IBL  1/10] CL=0.79551  Cd_f=0.00401  x_tr_u=0.064  x_tr_l=0.651  |dδ*/dx|_max_u=0.0821
   ...
   [IBL 10/10] CL=0.79923  Cd_f=0.00430  x_tr_u=0.064  x_tr_l=0.651  |dδ*/dx|_max_u=0.0384

   [pyTSFoil] IBL-coupled result  Ma=0.750  AoA=0.50°  Re=6.50e+06
     CL=0.79923  CM=-0.10997  CD_wave=0.01285  CD_friction=0.00430  CD_total=0.01715


RAE 2822 — inviscid TSD only
------------------------------

**Path:** ``example/rae2822/run_pytsfoil.py``

Uses the :class:`~pytsfoil.PyTSFoil` class directly for a pure inviscid TSD run,
with full control over mesh and solver settings.

Key settings used:

.. code-block:: python

   ts.set_config(
       EMACH  = 0.75,
       ALPHA  = 0.5,
       MAXIT  = 9999,
       EPS    = 0.2,
       n_point_x       = 200,
       n_point_y       = 80,
       n_point_airfoil = 100,
       flag_output_shock = True,
       flag_output_field = True,
   )


RAE 2822 — IBL-coupled TSD
----------------------------

**Path:** ``example/rae2822_ibl/run_pytsfoil.py``

Demonstrates full manual control of the IBL coupling loop:

* Creates a :class:`~pytsfoil.PyTSFoil` and an :class:`~pytsfoil.IBL` instance separately.
* Configures coupling parameters (relaxation, smoothing, TE correction).
* Plots the convergence history of CL and friction drag across outer iterations.


RAE 2822 — correction comparison
----------------------------------

**Path:** ``example/rae2822_correction/``

Compares four configurations at a challenging flight condition:

1. Pure inviscid TSD (no corrections)
2. CFS correction only
3. DCC (Divergence Check Correction)
4. CFS + DCC

Useful for understanding the effect of each stability correction on the solution
at high Mach / high AoA combinations.


RAE 2822 — multi-process parallel
-----------------------------------

**Path:** ``example/rae2822_mp/``

Shows how to sweep a Mach-AoA polar using ``multiprocessing.Pool`` with
:func:`~pytsfoil.run_airfoil_analysis`.  Each worker process runs an independent
TSD + IBL analysis in its own subprocess, avoiding shared Fortran state.

.. code-block:: python

   from multiprocessing import Pool
   from pytsfoil import run_airfoil_analysis
   import numpy as np

   coords = np.loadtxt('rae2822.dat', skiprows=1)

   def worker(case):
       ma, aoa = case
       return run_airfoil_analysis(
           coords, Mach=ma, AoA_degrees=aoa, Re=6.5e6,
           configs={'flag_print_info': False},
       )

   cases = [(0.73, 0.3), (0.75, 0.5), (0.77, 0.8), (0.79, 1.0)]

   with Pool(4) as pool:
       results = pool.map(worker, cases)

Running the examples
---------------------

.. code-block:: bash

   cd example/rae2822_wrapper
   python run_wrapper.py

   cd ../rae2822
   python run_pytsfoil.py

   cd ../rae2822_ibl
   python run_pytsfoil.py
