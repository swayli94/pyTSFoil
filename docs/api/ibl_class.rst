``IBL``
=======

.. autoclass:: pytsfoil.IBL
   :members:
   :undoc-members: False
   :show-inheritance:
   :member-order: bysource
   :special-members: __init__

.. rubric:: Public methods summary

.. autosummary::
   :nosignatures:

   pytsfoil.IBL.__init__
   pytsfoil.IBL.run
   pytsfoil.IBL.friction_drag
   pytsfoil.IBL.wall_slope_correction
   pytsfoil.IBL.correction_dstar
   pytsfoil.IBL.smooth_mach
   pytsfoil.IBL.clip_and_smooth_slope
   pytsfoil.IBL.repair_dstar

.. rubric:: Physics background

The IBL solver integrates boundary-layer equations along each surface from the
stagnation point (x/c = 0) to the trailing edge (x/c = 1):

**Laminar region** (Thwaites, 1949)
   Thwaites' quadrature formula for momentum thickness growth, with White's
   polynomial correlations for shape factor H and skin friction c\ :sub:`f`.

**Transition** (Michel, 1951)
   Michel's criterion: transition when
   Re\ :sub:`θ` > 2.9 × 10\ :sup:`-0.4` Re\ :sub:`x`\ :sup:`0.4`.
   Alternatively, the transition location can be forced via ``x_tr_forced``.

**Turbulent region** (Head, 1958)
   Head's entrainment ODE is integrated alongside the von Kármán integral
   momentum equation. Skin friction uses the Ludwieg-Tillmann (1950) formula
   with a compressible von Kármán correction (−M\ :sub:`e`\ :sup:`2` term).
