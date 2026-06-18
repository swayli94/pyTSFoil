pyTSFoil
========

**pyTSFoil** is a Python interface for TSFOIL2, a transonic small-disturbance (TSD) solver
for flow past lifting airfoils, extended with a viscous-inviscid Integral Boundary Layer (IBL)
coupling method.

Key capabilities
----------------

* **Fast transonic CFD** — direct Python interface to a modernised Fortran TSFOIL2 solver.
* **Viscous-inviscid coupling** — IBL displacement-thickness wall-slope correction
  via :func:`~pytsfoil.PyTSFoil.run_ibl_coupled`.
* **Boundary layer physics** — Thwaites (laminar), Michel's criterion (transition),
  Head's entrainment method (turbulent), with compressible von Kármán correction.
* **Trailing-edge correction** — optional TE δ\* blending to handle boundary-layer
  blow-up near the trailing edge.
* **Easy-to-use interface** — :func:`~pytsfoil.run_airfoil_analysis` wraps everything
  in a single function call with sensible defaults.
* **Safe parallel use** — designed for ``multiprocessing.Pool`` across many flight conditions.

.. code-block:: python

   import numpy as np
   from pytsfoil import run_airfoil_analysis

   coords = np.loadtxt('rae2822.dat', skiprows=1)   # TE → upper → LE → lower → TE
   r = run_airfoil_analysis(coords, Mach=0.75, AoA_degrees=0.5, Re=6.5e6)

   print(f"CL={r['cl']:.4f}  CD_wave={r['cd_wave']:.5f}  CD_f={r['cd_friction']:.5f}")

.. toctree::
   :maxdepth: 2
   :caption: User Guide

   installation
   quickstart
   userguide

.. toctree::
   :maxdepth: 2
   :caption: API Reference

   api/index

.. toctree::
   :maxdepth: 1
   :caption: Background & Examples

   theory
   background
   examples
   changelog

Indices and tables
------------------

* :ref:`genindex`
* :ref:`modindex`
* :ref:`search`
