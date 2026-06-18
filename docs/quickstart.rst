Quick Start
===========

This page shows the fastest paths to running a transonic airfoil analysis with pyTSFoil.

Airfoil coordinate format
--------------------------

All interfaces expect coordinates in **counter-clockwise** order starting from the
**trailing edge on the upper surface**:

.. code-block:: text

   TE (upper) → LE → TE (lower)

The chord is normalised to 1 (leading edge at ``x = 0``, trailing edge at ``x = 1``).

Easy mode — ``run_airfoil_analysis``
-------------------------------------

:func:`~pytsfoil.run_airfoil_analysis` handles everything in one call.
Default parameters are tuned for typical transonic cases (0.6 ≤ Ma ≤ 0.9,
AoA within ±4°).

Viscous analysis (TSD + IBL)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: python

   import numpy as np
   from pytsfoil import run_airfoil_analysis

   coords = np.loadtxt('rae2822.dat', skiprows=1)   # shape (N, 2)

   r = run_airfoil_analysis(coords, Mach=0.75, AoA_degrees=0.5, Re=6.5e6)

   print(f"CL={r['cl']:.5f}  CD_wave={r['cd_wave']:.5f}  "
         f"CD_f={r['cd_friction']:.5f}  CD_total={r['cd_total']:.5f}")

   # Surface distributions (indexed over full mesh x-line)
   ile, ite = r['ile'], r['ite']
   cp_upper_foil = r['cpu'][ile:ite+1]   # Cp on the airfoil chord

   # IBL results
   upper = r['ibl_upper']
   print(f"Transition: x_tr={upper['x_tr']:.3f}")
   delta_star = upper['delta_star']     # displacement thickness δ*(x)

Inviscid baseline only
~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: python

   r = run_airfoil_analysis(
       coords, Mach=0.75, AoA_degrees=0.5, Re=6.5e6,
       flag_IBL=False,
   )
   print(f"CL={r['cl']:.5f}  CD_wave={r['cd_wave']:.5f}")

Overriding individual parameters
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Pass a ``configs`` dict to override any solver or IBL parameter without
changing the rest:

.. code-block:: python

   r = run_airfoil_analysis(
       coords, Mach=0.75, AoA_degrees=0.5, Re=6.5e6,
       configs={
           'n_point_x':       300,    # denser x-mesh
           'n_point_airfoil': 150,
           'x_tr_upper':      0.05,   # forced transition at 5 % chord
           'n_outer':         15,     # more IBL coupling iterations
           'flag_print_info': False,  # suppress console output
           'work_dir':        '/tmp/my_run',
           'flag_output_shock': True, # write cpxs.dat to work_dir
       },
   )

Return value
~~~~~~~~~~~~~

The returned ``dict`` contains:

===============  ============================================================
Key              Description
===============  ============================================================
``cl``           Lift coefficient
``cm``           Pitching-moment coefficient (about quarter-chord)
``cd_wave``      Wave drag (momentum integral method)
``cd_friction``  Friction drag (IBL); ``0.0`` when ``flag_IBL=False``
``cd_shape``     Shape drag (TSD momentum integral)
``cd_total``     ``cd_wave + cd_friction + cd_shape``
``xx``           x-coordinates of the full computational mesh
``xx_foil``      x-coordinates over the airfoil chord [0, 1]
``ile``          Leading-edge index in ``xx``
``ite``          Trailing-edge index in ``xx`` (inclusive)
``cpu``          Upper-surface Cp (full mesh x-line)
``cpl``          Lower-surface Cp (full mesh x-line)
``mau``          Upper-surface Mach number (full mesh x-line)
``mal``          Lower-surface Mach number (full mesh x-line)
``cpstar``       Critical pressure coefficient Cp\* (sonic condition)
``baseline``     Dict with inviscid TSD baseline ``cl``, ``cd_wave``, etc.
``ibl_upper``    IBL result dict for upper surface (``None`` if inviscid)
``ibl_lower``    IBL result dict for lower surface (``None`` if inviscid)
``history``      List of per-iteration dicts from the IBL outer loop
``solver``       Underlying :class:`~pytsfoil.PyTSFoil` instance
===============  ============================================================

Each ``ibl_upper`` / ``ibl_lower`` dict has the keys:

====================  ========================================================
Key                   Description
====================  ========================================================
``s``                 Arc-length / c from leading edge
``ue``                Edge velocity ratio u\ :sub:`e` / u\ :sub:`∞`
``theta``             Momentum thickness / c
``delta_star``        Displacement thickness δ\* / c
``H``                 Shape factor H = δ\* / θ
``cf``                Skin friction coefficient c\ :sub:`f`
``x_tr``              Transition location x/c
``i_tr``              Transition index in the mesh
``laminar_mask``      Boolean array: ``True`` where flow is laminar
``delta_star_raw``    δ\* before trailing-edge correction
====================  ========================================================

Plotting results
~~~~~~~~~~~~~~~~~

.. code-block:: python

   import matplotlib.pyplot as plt

   xx_foil = r['xx_foil']
   ile, ite = r['ile'], r['ite']

   fig, axes = plt.subplots(1, 2, figsize=(12, 4))

   # Cp distribution
   ax = axes[0]
   ax.plot(xx_foil, r['cpu'][ile:ite+1], label='Upper')
   ax.plot(xx_foil, r['cpl'][ile:ite+1], label='Lower')
   ax.axhline(r['cpstar'], ls='--', color='k', label='Cp*')
   ax.invert_yaxis()
   ax.set_xlabel('x/c')
   ax.set_ylabel('Cp')
   ax.legend()
   ax.set_title(f"Cp  Ma={r['solver'].config['EMACH']}")

   # Displacement thickness
   ax = axes[1]
   ax.plot(xx_foil, r['ibl_upper']['delta_star'], label='Upper δ*')
   ax.plot(xx_foil, r['ibl_lower']['delta_star'], label='Lower δ*')
   ax.set_xlabel('x/c')
   ax.set_ylabel('δ* / c')
   ax.legend()
   ax.set_title('Displacement thickness')

   plt.tight_layout()
   plt.show()

Advanced usage
--------------

For direct control over each solver stage, see :doc:`userguide`.
