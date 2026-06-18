Physics Background
==================

.. note::

   This page gives a compact summary of the physical models.
   For the full mathematical derivation see :doc:`theory`.

.. contents:: Contents
   :local:
   :depth: 2


Transonic Small-Disturbance (TSD) theory
-----------------------------------------

The Transonic Small-Disturbance equation is a mixed-type nonlinear PDE for the
perturbation potential φ arising from thin-airfoil theory in transonic flow.
In the form used by TSFOIL2 (Murman *et al.*, 1975), the governing equation is:

.. math::

   \bigl[K - (\gamma+1) \varphi_x\bigr] \varphi_{xx} + \varphi_{yy} = 0

where :math:`K = (1 - M_\infty^2) / \delta^{2/3}` is the transonic similarity
parameter, :math:`\delta` is the maximum thickness-to-chord ratio, and
:math:`\gamma` is the ratio of specific heats.

* When :math:`K - (\gamma+1)\varphi_x > 0` the equation is **elliptic** (subsonic).
* When :math:`K - (\gamma+1)\varphi_x < 0` the equation is **hyperbolic** (supersonic).
* At the transition the flow is locally **sonic**.

TSFOIL2 uses a mixed finite-difference scheme (Murman-Cole type) with artificial
viscosity to handle the elliptic-hyperbolic switch and capture shocks.

Similarity scaling
~~~~~~~~~~~~~~~~~~~

Three scalings are supported (set via ``SIMDEF``):

===  ============  ================================================================
ID   Name          :math:`K`
===  ============  ================================================================
1    Cole          :math:`(1-M^2) / \delta^{2/3}`
2    Spreiter      :math:`(1-M^2) / (\delta^{2/3} M^{4/3})`
3    Krupp         :math:`(1-M^2) / (\delta^{2/3} M)` *(recommended)*
===  ============  ================================================================

Wall boundary condition
~~~~~~~~~~~~~~~~~~~~~~~

On the airfoil chord the wall BC enforces the surface slope:

.. math::

   \varphi_y(x, 0^\pm) = \left(\frac{dY}{dx} - \alpha\right) \varphi_x(x, 0^\pm)

where :math:`dY/dx` is the normalised airfoil slope and :math:`\alpha` is the
angle of attack scaled to similarity variables.  The IBL coupling modifies
:math:`dY/dx` by adding :math:`\pm d\delta^*/dx / \delta` on the upper/lower
surface.

Far-field boundary conditions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The outer boundaries use the subsonic asymptotic solution for a doublet-vortex
combination located at :math:`x = 0.5`, using potential theory.  For supersonic
free-stream (:math:`K \leq 0`) no far-field BC is applied.

Wave drag
~~~~~~~~~~

Wave drag is computed by the **momentum integral method** (Cole's formula),
integrating around a contour enclosing the body and along each shock wave inside
the contour.  For each shock at position :math:`x_s`:

.. math::

   c_{D,\text{wave}} = -\cdfact \frac{\gamma+1}{6 Y_{\text{fact}}}
                        \int_{y_b}^{y_t} \Delta u^3 \, dy

where :math:`\Delta u` is the velocity jump across the shock.


Integral Boundary Layer (IBL) method
--------------------------------------

Given the edge Mach distribution :math:`M_e(x)` from TSD, the IBL solver
integrates the compressible boundary-layer equations from the leading edge.

Edge velocity
~~~~~~~~~~~~~

The edge velocity ratio is obtained via the isentropic relation:

.. math::

   \frac{u_e}{u_\infty} = \frac{M_e}{M_\infty}
       \left(\frac{1 + \tfrac{\gamma-1}{2} M_\infty^2}
             {1 + \tfrac{\gamma-1}{2} M_e^2}\right)^{1/2}

Laminar region — Thwaites (1949)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Thwaites' quadrature formula:

.. math::

   \theta^2(x) = \frac{0.45 \nu}{u_e^6}
                 \int_0^x u_e^5 \, dx

The shape factor :math:`H` and skin friction :math:`c_f` are obtained from
White's polynomial correlations with the Thwaites parameter
:math:`\lambda = (\theta^2 / \nu) \, du_e/ds`.

Transition — Michel (1951)
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Transition is predicted when:

.. math::

   Re_\theta > 2.9 \times 10^{-4} \, Re_x^{0.4}

The user may override this with a fixed transition location ``x_tr_forced``.

Turbulent region — Head (1958)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Two coupled ODEs are integrated simultaneously:

**von Kármán integral:**

.. math::

   \frac{d\theta}{ds} = \frac{c_f}{2} - (H+2-M_e^2)\,\frac{\theta}{u_e}\frac{du_e}{ds}

**Head's entrainment equation:**

.. math::

   \frac{d}{ds}\bigl[\theta H_1\bigr]
       = \frac{u_e'}{u_e}\theta H_1 + F(H_1)

where :math:`H_1 = (\delta - \delta^*)/\theta` and :math:`F(H_1)` is Head's
entrainment function.  Skin friction uses the Ludwieg-Tillmann formula.


Viscous-inviscid coupling
--------------------------

The coupling strategy is the **displacement-body** (wall-slope) approach:

1. TSD is solved on the physical airfoil geometry.
2. IBL gives the displacement thickness :math:`\delta^*(x)` on each surface.
3. The effective wall slope is updated:

   .. math::

      \left.\frac{dY}{dx}\right._{\text{eff, upper}} =
          \left.\frac{dY}{dx}\right._{\text{airfoil}} + \frac{1}{\delta}\frac{d\delta^*_u}{dx}

   and analogously (minus sign) for the lower surface.

4. TSD is re-solved with the updated BCs (warm-started from the current
   potential field :math:`\varphi`).

The outer loop repeats :math:`N_{\text{outer}}` times with linear under-relaxation
to ensure convergence.

Trailing-edge correction
~~~~~~~~~~~~~~~~~~~~~~~~~

Near the trailing edge the IBL displacement thickness tends to diverge
(boundary-layer blow-up associated with trailing-edge separation).
The optional TE correction blends the IBL δ\* linearly to zero at the trailing edge
starting from :math:`x/c = x_{\text{blend}}`, which mitigates the singularity
while preserving the bulk displacement effect.


References
----------

* Murman, E.M., Bailey, F.R., and Johnson, M.L., "TSFOIL — A Computer Code for
  Two-Dimensional Transonic Calculations, Including Wind-Tunnel Wall Effects and
  Wave Drag Evaluation," NASA SP-347, 1975.
* Thwaites, B. "Approximate Calculation of the Laminar Boundary Layer."
  ARC R&M 1314, 1949.
* Michel, R. "Étude de la Transition sur les Profils d'Aile."
  ONERA Rep. 1/1578A, 1951.
* Head, M.R. "Entrainment in the Turbulent Boundary Layer."
  ARC R&M 3152, 1958.
* Ludwieg, H. & Tillmann, W. "Investigations of the Wall-Shearing Stress in
  Turbulent Boundary Layers." NACA TM 1285, 1950.
* White, F.M. *Viscous Fluid Flow*, 3rd ed. McGraw-Hill, 2006.
