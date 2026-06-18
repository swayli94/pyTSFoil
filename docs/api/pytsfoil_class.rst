``PyTSFoil``
============

.. autoclass:: pytsfoil.PyTSFoil
   :members:
   :undoc-members: False
   :show-inheritance:
   :member-order: bysource
   :special-members: __init__

.. rubric:: Public methods summary

.. autosummary::
   :nosignatures:

   pytsfoil.PyTSFoil.__init__
   pytsfoil.PyTSFoil.set_config
   pytsfoil.PyTSFoil.run
   pytsfoil.PyTSFoil.run_ibl_coupled
   pytsfoil.PyTSFoil.plot_all_results
   pytsfoil.PyTSFoil.compute_wave_drag

.. rubric:: Data attributes

After :meth:`~pytsfoil.PyTSFoil.run` or
:meth:`~pytsfoil.PyTSFoil.run_ibl_coupled` completes, the following
instance attributes are populated:

``config`` : dict
    Active solver configuration (read/write via :meth:`~pytsfoil.PyTSFoil.set_config`).

``mesh`` : dict
    Mesh data:

    ==================  ======================================================
    Key                 Description
    ==================  ======================================================
    ``xx``              x-coordinates of all mesh points
    ``yy``              y-coordinates of all mesh points
    ``ile``             0-based leading-edge index in ``xx``
    ``ite``             0-based trailing-edge index in ``xx`` (inclusive)
    ``nfoil``           Number of mesh points over the airfoil chord
    ``fxu``             Upper-surface normalised slope dy/dx (mesh points)
    ``fxl``             Lower-surface normalised slope dy/dx (mesh points)
    ==================  ======================================================

``airfoil`` : dict
    Airfoil geometry:

    ==================  ======================================================
    Key                 Description
    ==================  ======================================================
    ``xu``, ``yu``      Upper surface coordinates (LE → TE)
    ``xl``, ``yl``      Lower surface coordinates (LE → TE)
    ``t_max``           Maximum thickness / chord (used as TSD length scale δ)
    ==================  ======================================================

``data_summary`` : dict
    Results (updated after each solve):

    ====================  ======================================================
    Key                   Description
    ====================  ======================================================
    ``cl``                Lift coefficient
    ``cm``                Pitching-moment coefficient
    ``cd``                Total drag coefficient
    ``cd_wave``           Wave drag coefficient
    ``cd_shape``          Shape drag (body-surface pressure integral)
    ``cd_friction``       Friction drag (from IBL; 0.0 for inviscid runs)
    ``cpu``               Upper-surface Cp (full mesh x-line)
    ``cpl``               Lower-surface Cp (full mesh x-line)
    ``mau``               Upper-surface Mach number (full mesh x-line)
    ``mal``               Lower-surface Mach number (full mesh x-line)
    ``cpstar``            Critical pressure coefficient Cp\*
    ``P_field``           2-D potential perturbation field (jmax × imax)
    ``ibl_upper``         Final IBL result dict, upper surface
    ``ibl_lower``         Final IBL result dict, lower surface
    ====================  ======================================================
