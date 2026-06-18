User Guide
==========

This page covers the advanced API of pyTSFoil for users who need direct control over
each stage of the analysis.

.. contents:: Contents
   :local:
   :depth: 2
   :class: this-will-duplicate-information-and-it-is-still-useful-here


Inviscid TSD analysis
---------------------

:class:`~pytsfoil.PyTSFoil` exposes the full TSFOIL2 solver.

.. code-block:: python

   import numpy as np
   from pytsfoil import PyTSFoil

   coords = np.loadtxt('rae2822.dat', skiprows=1)

   ts = PyTSFoil(
       airfoil_coordinates=coords,
       work_dir='output/',       # Fortran output files (smry.out, tsfoil2.out)
       output_dir='output/',     # Python output files (cpxs.dat, field.dat)
   )

   ts.set_config(
       EMACH  = 0.75,    # Mach number
       ALPHA  = 0.5,     # angle of attack (degrees)
       REYNLD = 6.5e6,   # Reynolds number (used by IBL / viscous wedge)
       MAXIT  = 9999,    # maximum iterations
       EPS    = 0.2,     # artificial viscosity (0-1)
       CVERGE = 1e-5,    # convergence criterion
       n_point_x       = 200,
       n_point_y       = 80,
       n_point_airfoil = 100,
       flag_output_shock = True,   # write cpxs.dat
       flag_output_field = True,   # write field.dat
       flag_print_info   = True,
   )

   ts.run()

   cl  = ts.data_summary['cl']
   cd  = ts.data_summary['cd']      # total drag (momentum integral)
   cpu = ts.data_summary['cpu']     # Cp upper surface (full mesh x-line)
   cpl = ts.data_summary['cpl']     # Cp lower surface
   mau = ts.data_summary['mau']     # Wall Mach upper
   mal = ts.data_summary['mal']     # Wall Mach lower

   ts.plot_all_results()            # quick Mach / Cp plot


``set_config`` parameter reference
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

================================  =========  =============================================
Parameter                         Default    Description
================================  =========  =============================================
``EMACH``                         0.75       Freestream Mach number
``ALPHA``                         0.0        Angle of attack (degrees, −9 to 9)
``REYNLD``                        4.0e6      Reynolds number (used by IBL / wedge)
``MAXIT``                         1000       Maximum solver iterations
``CVERGE``                        1e-5       Convergence criterion
``DVERGE``                        10.0       Divergence criterion (residual upper bound)
``EPS``                           0.2        Artificial viscosity (0-1)
``RIGF``                          0.0        Rigidity factor for surface-slope limiting
``SIMDEF``                        3          Similarity scaling: 1=Cole, 2=Spreiter, 3=Krupp
``NWDGE``                         0          Viscous wedge: 0=none, 1=Murman, 2=Yoshihara
``WCIRC``                         1.0        Circulation weight at trailing edge
``WE``                            [1.8,…]    SOR relaxation factors (list of 3)
``n_point_x``                     81         Total grid points in x-direction
``n_point_y``                     60         Total grid points in y-direction
``n_point_airfoil``               51         Grid points over the airfoil chord
``flag_output``                   True       Write solver log to ``tsfoil2.out``
``flag_output_summary``           True       Write summary to ``smry.out``
``flag_output_shock``             True       Write Cp/Mach to ``cpxs.dat``
``flag_output_field``             True       Write flow field to ``field.dat``
``flag_print_info``               True       Print progress to console
``flag_CFS``                      True       Enable Correction of Full-Supersonic
``BETA_SONIC``                    500.0      Sonic-penalty multiplier (CFS)
``EPS_AMPL``                      200.0      EPS amplification factor at TE (CFS)
``ITER_START_CFS``                100        Minimum iteration count before CFS triggers
================================  =========  =============================================


Viscous IBL-coupled analysis
-----------------------------

After the initial inviscid run, call :meth:`~pytsfoil.PyTSFoil.run_ibl_coupled` to add
viscous effects through displacement-thickness wall-slope correction.

.. code-block:: python

   from pytsfoil import PyTSFoil, IBL

   ts = PyTSFoil(airfoil_coordinates=coords, work_dir='output/')
   ts.set_config(
       EMACH  = 0.75,
       ALPHA  = 0.5,
       REYNLD = 6.5e6,
       MAXIT  = 9999,
       RIGF   = 0.2,      # recommended for IBL coupling
       NWDGE  = 2,        # Yoshihara wedge near shocks
       n_point_x = 200, n_point_y = 80,
       flag_print_info = True,
   )

   ibl = IBL(Re=6.5e6, M_inf=0.75)

   ts.run()  # optional warm-start; run_ibl_coupled always starts fresh internally

   history = ts.run_ibl_coupled(
       ibl                  = ibl,
       n_outer              = 10,
       x_tr_upper           = None,   # None → Michel's criterion
       x_tr_lower           = None,
       coupling_relax_final = 0.1,
       mach_smooth_sigma    = 2.0,
       slope_smooth_sigma   = 3.0,
       delta_star_max       = 0.05,
       slope_correction_max = 0.1,
       maxit_inner          = 200,
       i_outer_repair       = 3,
       use_te_correction    = True,
       te_relax             = 0.5,
       x_blend_start        = 0.9,
       use_divergence_check = False,
   )

   cl          = ts.data_summary['cl']
   cd_total    = ts.data_summary['cd']
   cd_wave     = ts.data_summary['cd_wave']
   cd_friction = ts.data_summary['cd_friction']

   upper = ts.data_summary['ibl_upper']
   print(f"Transition (upper): x_tr={upper['x_tr']:.3f}")
   print(f"Max δ* (upper): {upper['delta_star'].max():.4f}")

``run_ibl_coupled`` parameter reference
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

============================  =========  =============================================
Parameter                     Default    Description
============================  =========  =============================================
``ibl``                       required   Pre-configured :class:`~pytsfoil.IBL` instance
``n_outer``                   10         Number of outer coupling cycles
``x_tr_upper``                None       Forced upper transition x/c (None → Michel)
``x_tr_lower``                None       Forced lower transition x/c (None → Michel)
``coupling_relax_final``      0.1        Final under-relaxation factor (0-1)
``mach_smooth_sigma``         2.0        Gaussian σ (mesh points) applied to Mach before IBL
``slope_smooth_sigma``        3.0        Gaussian σ applied to dδ\*/dx after IBL
``delta_star_max``            0.05       Upper clip on δ\*/c
``slope_correction_max``      0.1        Max dδ\*/dx applied to wall BC
``maxit_inner``               200        TSD iterations per warm-start
``i_outer_repair``            3          Outer iteration to start TE δ\* repair
``use_te_correction``         False      Apply TE δ\* blending correction
``te_relax``                  0.5        Relaxation factor for TE correction (0-1)
``x_blend_start``             0.8        x/c where TE correction ramp begins
``use_divergence_check``      False      Enable Divergence Check Correction (DCC)
============================  =========  =============================================


Corrections and stability
--------------------------

Correction of Full-Supersonic (CFS)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When the Mach number and angle of attack push the shock past the trailing edge, the
solver can converge to a non-physical fully-supersonic solution.  The CFS correction
adds a sonic penalty and locally amplifies artificial viscosity near the trailing edge
to guide the solver back to a physical solution.

Enable via ``set_config``:

.. code-block:: python

   ts.set_config(
       flag_CFS       = True,
       BETA_SONIC     = 100.0,   # sonic penalty = EPS × BETA_SONIC
       EPS_AMPL       = 500.0,   # EPS factor at TE columns
       ITER_START_CFS = 100,     # minimum iterations before CFS triggers
   )

Divergence Check Correction (DCC)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

For difficult cases (high AoA, large Mach, strong shocks), the TSD-IBL coupling
may diverge on the first few outer iterations.  DCC detects this and restarts the
coupling from AoA = 0°, then ramps the angle of attack up over the outer loop.

.. code-block:: python

   history = ts.run_ibl_coupled(
       ibl                  = ibl,
       use_divergence_check = True,
       ...
   )

.. note::

   CFS has no benefit once DCC is activated and will be deprecated in a future release.


Using the IBL solver directly
------------------------------

:class:`~pytsfoil.IBL` can be used standalone without a ``PyTSFoil`` instance if you
have an edge Mach distribution from another source.

.. code-block:: python

   from pytsfoil import IBL
   import numpy as np

   ibl = IBL(Re=6.5e6, M_inf=0.75)

   xx   = np.linspace(0, 1, 100)     # x/c stations
   mach = np.ones(100) * 0.8         # edge Mach distribution

   result = ibl.run(
       xx           = xx,
       mach         = mach,
       yy           = None,           # optional surface y/c
       x_tr_forced  = None,           # None → Michel's criterion
   )

   delta_star = result['delta_star']
   cf         = result['cf']
   x_tr       = result['x_tr']

   # Friction drag (both surfaces)
   cd_f = ibl.friction_drag(upper=result, lower=result)


Parallel analysis with multiprocessing
---------------------------------------

All ``PyTSFoil`` instances in the same process share the underlying Fortran module
state.  For parallel analyses, use ``multiprocessing.Pool`` so each worker process
gets its own isolated copy of the Fortran data.  **Do not use threading.**

.. code-block:: python

   from multiprocessing import Pool
   from pytsfoil import run_airfoil_analysis
   import numpy as np

   coords = np.loadtxt('rae2822.dat', skiprows=1)

   def worker(args):
       mach, aoa = args
       return run_airfoil_analysis(
           coords, Mach=mach, AoA_degrees=aoa, Re=6.5e6,
           configs={'flag_print_info': False},
       )

   cases = [(0.73, 0.3), (0.75, 0.5), (0.77, 0.8)]

   with Pool(3) as p:
       results = p.map(worker, cases)

   for r, (ma, aoa) in zip(results, cases):
       print(f"Ma={ma}  AoA={aoa}°  CL={r['cl']:.4f}  CDtot={r['cd_total']:.5f}")

.. warning::

   Using ``threading`` instead of ``multiprocessing`` will cause shared Fortran state
   corruption and produce incorrect or non-reproducible results.


Output files
-------------

When the corresponding ``flag_output_*`` options are enabled, the solver writes
several output files to ``output_dir``:

=================  =========================================================
File               Contents
=================  =========================================================
``cpxs.dat``       Cp and Mach distributions on the airfoil surface
``field.dat``      Full 2-D flow field in Tecplot point format
``smry.out``       Aerodynamic coefficients and shock-wave drag summary
``tsfoil2.out``    Iteration-by-iteration convergence log
=================  =========================================================

The ``field.dat`` format uses the Tecplot ASCII point format with variables
``X``, ``Y``, ``Mach``, ``Cp``, ``P``, ``FlowType``.  Flow types are:
``-1`` = outside domain, ``0`` = subsonic, ``1`` = sonic, ``2`` = supersonic,
``3`` = shock.
