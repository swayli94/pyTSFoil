Installation
============

Prerequisites
-------------

* Python ≥ 3.8
* ``gfortran`` (or another Fortran compiler) and ``f2py`` (ships with NumPy)
* Linux or macOS (Windows is not officially supported)

Optional
~~~~~~~~

* ``cst-modeling3d`` — for CST-parametrised airfoil geometry generation.

Install from PyPI
-----------------

The recommended way for most users:

.. code-block:: bash

   pip install pytsfoil>=0.3.3

Version notes:

* ``>=0.2.4``: TSD solver only (inviscid).
* ``>=0.3.3``: TSD + IBL coupling + trailing-edge and CFS corrections.
* ``>=0.3.5``: Recommended stable release.

Install from source
-------------------

.. code-block:: bash

   sudo apt update && sudo apt install gfortran   # Ubuntu/Debian
   # brew install gcc                              # macOS (provides gfortran)

   git clone https://github.com/swayli94/pyTSFoil.git
   cd pyTSFoil
   pip install -e .

The package compiles the Fortran extension automatically on first import (using
``f2py``).  If compilation fails at import time you can trigger it manually:

.. code-block:: bash

   cd pytsfoil
   python compile_f2py.py

Verify the installation
-----------------------

.. code-block:: bash

   python -c "import pytsfoil; print('pytsfoil', pytsfoil.__version__, 'installed')"

Optional: install cst-modeling3d
---------------------------------

.. code-block:: bash

   pip install cst-modeling3d

Dependencies
------------

Required (installed automatically via pip):

=================  ==========
Package            Minimum
=================  ==========
NumPy              1.18
SciPy              1.5
Matplotlib         3.0
=================  ==========
