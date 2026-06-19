Theory
======

.. contents:: Contents
   :local:
   :depth: 3
   :class: this-will-duplicate-information-and-it-is-still-useful-here

This chapter gives a self-contained derivation of the two physical models
implemented in pyTSFoil: the **Transonic Small-Disturbance (TSD)** equation
and the **Integral Boundary Layer (IBL)** method, followed by a description of
the viscous-inviscid coupling strategy.

----

Transonic Small-Disturbance (TSD) Theory
-----------------------------------------

Physical assumptions
~~~~~~~~~~~~~~~~~~~~~

TSD theory is a simplification of the full-potential equation valid for
**thin airfoils** at **transonic Mach numbers**.  The key assumptions are:

1. The flow is steady, two-dimensional, and isentropic everywhere except
   across shock waves.
2. The airfoil is **thin**: maximum thickness-to-chord ratio
   :math:`\delta = (t/c)_{\max} \ll 1`.
3. The angle of attack :math:`\alpha \ll 1` (expressed in radians or
   scaled similarity variables).
4. The free-stream Mach number :math:`M_\infty` is close to 1, so
   :math:`1 - M_\infty^2 = O(\delta^{2/3})`.

Under these assumptions the velocity potential :math:`\Phi` can be written as

.. math::

   \Phi(x, y) = U_\infty \bigl[x + \varphi(x, y)\bigr],

where :math:`U_\infty` is the free-stream speed and :math:`\varphi \ll x`
is the **perturbation potential**.

From the full-potential equation
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The full inviscid irrotational flow satisfies the exact potential equation

.. math::

   \left(1 - \frac{u^2}{a^2}\right)\Phi_{xx}
   - \frac{2 u v}{a^2}\Phi_{xy}
   + \left(1 - \frac{v^2}{a^2}\right)\Phi_{yy} = 0,

where :math:`u = \Phi_x`, :math:`v = \Phi_y`, and :math:`a` is the local
speed of sound.  For a thin airfoil the cross-derivative term is
:math:`O(\delta^{2/3})` smaller than the retained terms and can be dropped.
The perturbation velocities satisfy :math:`u = U_\infty(1 + \varphi_x)` and
:math:`v = U_\infty \varphi_y`, with :math:`\varphi_x \ll 1`.  Using the
isentropic relation to express :math:`a^2` in terms of :math:`\varphi_x`
and retaining only the dominant transonic terms yields the **Transonic
Small-Disturbance equation**:

.. math::
   :label: tsd

   \bigl[(1 - M_\infty^2) - (\gamma + 1) M_\infty^2 \varphi_x\bigr]\, \varphi_{xx}
   + \varphi_{yy} = 0.

This is a **mixed-type** nonlinear PDE: it is elliptic where the coefficient
of :math:`\varphi_{xx}` is positive (subsonic local flow) and hyperbolic where
it is negative (supersonic local flow).

Similarity transformation
~~~~~~~~~~~~~~~~~~~~~~~~~~

The solution of :eq:`tsd` depends on :math:`M_\infty` and :math:`\delta` only
through the **transonic similarity parameter** :math:`K`.  A similarity
transformation rescales the coordinates and the potential so that the
equation becomes independent of :math:`\delta`:

.. math::

   \tilde{x} = x, \qquad
   \tilde{y} = a\, y, \qquad
   \tilde{\varphi} = \frac{\varphi}{b},

where the stretching factor :math:`a` and potential scale :math:`b` depend on the
choice of similarity rule (see table below).  Substituting into :eq:`tsd` gives
the reduced equation

.. math::
   :label: tsd_sim

   \bigl[K - \mathcal{N}\,\tilde{\varphi}_{\tilde{x}}\bigr]
   \tilde{\varphi}_{\tilde{x}\tilde{x}}
   + \tilde{\varphi}_{\tilde{y}\tilde{y}} = 0,

where :math:`K` is the transonic similarity parameter and
:math:`\mathcal{N}` is the SIMDEF-dependent nonlinear coefficient:

.. list-table:: Similarity scalings (``SIMDEF``)
   :header-rows: 1
   :widths: 6 10 26 15 17 26

   * - ID
     - Name
     - :math:`K`
     - Stretching :math:`a`
     - Potential scale :math:`b`
     - Nonlinear coeff. :math:`\mathcal{N}`
   * - 1
     - Cole
     - :math:`\dfrac{1-M_\infty^2}{\delta^{2/3}}`
     - :math:`\delta^{1/3}`
     - :math:`\delta^{2/3}`
     - :math:`(\gamma+1)M_\infty^2`
   * - 2
     - Spreiter
     - :math:`\dfrac{1-M_\infty^2}{\delta^{2/3} M_\infty^{4/3}}`
     - :math:`\delta^{1/3} M_\infty^{2/3}`
     - :math:`\delta^{2/3} M_\infty^{-2/3}`
     - :math:`(\gamma+1)`
   * - 3 *(default)*
     - Krupp
     - :math:`\dfrac{1-M_\infty^2}{\delta^{2/3} M_\infty}`
     - :math:`\delta^{1/3} M_\infty^{1/2}`
     - :math:`\delta^{2/3} M_\infty^{-1/2}`
     - :math:`(\gamma+1)M_\infty^{1/2}`

The pressure coefficient in physical space is recovered by

.. math::
   :label: cp

   C_p = -2\,\varphi_x = -2\,b\,\tilde{\varphi}_{\tilde{x}}.

TSFOIL2 stores the similarity-space solution and converts back to
physical coefficients via the pre-computed scaling factors
``CPFACT``, ``CLFACT``, ``CDFACT``, ``CMFACT``.

Boundary conditions
~~~~~~~~~~~~~~~~~~~~

**Wall (airfoil surface).** The kinematic condition on the upper (+) and
lower (-) surfaces of the thin airfoil, translated to the slit
:math:`y = 0^{\pm}`, is

.. math::
   :label: wall_bc

   \varphi_y(x, 0^\pm) =
   \frac{1}{\delta}
   \left(\frac{dY^\pm}{dx} - \tilde{\alpha}\right)
   \varphi_x(x, 0^\pm), \qquad 0 \le x \le 1,

where :math:`Y^\pm(x)/\delta` is the normalised upper/lower surface ordinate
and :math:`\tilde{\alpha} = \alpha_{\rm rad}/\delta` is the angle of attack
scaled to similarity variables (``ALPHA`` in degrees divided by
:math:`\delta \times 180/\pi`).  In code, the slopes :math:`dY^\pm/dx` are
stored in the arrays ``FXU`` and ``FXL``.

**Trailing edge (Kutta condition).** The circulation is determined by
requiring a finite velocity at the trailing edge.  TSFOIL2 implements this
through the parameter ``WCIRC`` (weight for the circulation jump).

**Far field.** On the outer boundaries, the perturbation potential is
approximated by the subsonic asymptotic form of a doublet-vortex pair
located at :math:`x = 0.5`, :math:`y = 0`:

.. math::

   \varphi \sim C_D\,\frac{x - 0.5}{r^2}
           + C_L\,\frac{-y}{2\pi r^2},
   \qquad r^2 = (x-0.5)^2 + \beta^2 y^2,

where :math:`\beta = \sqrt{K}` and the doublet strength :math:`C_D` and
vortex strength :math:`C_L` are updated iteratively.  For supersonic
free-stream (:math:`K \le 0`) no far-field correction is needed.

Local Mach number and pressure coefficient
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The local Mach number at any point in the flow field is

.. math::
   :label: local_mach

   M_{\rm loc}^2 = M_\infty^2\bigl[1 + (\gamma+1)\,b\,\tilde{\varphi}_{\tilde{x}}\bigr].

The pressure coefficient is computed from the isentropic relation rather than
the linearised TSD formula, giving better accuracy for moderate perturbations:

.. math::
   :label: cp_isen

   C_p = \frac{2}{\gamma M_\infty^2}
         \left[
           \left(\frac{1 + \tfrac{\gamma-1}{2}M_\infty^2}
                      {1 + \tfrac{\gamma-1}{2}M_{\rm loc}^2}\right)^{\!\gamma/(\gamma-1)}
           - 1
         \right].

The **critical pressure coefficient** :math:`C_p^*` corresponds to
:math:`M_{\rm loc} = 1` in :eq:`cp_isen` and is stored as ``cpstar``.

Shock relations
~~~~~~~~~~~~~~~~

In the TSD framework, shocks are treated as discontinuities in :math:`\varphi_x`
satisfying the Rankine-Hugoniot relations projected onto the thin-airfoil
plane.  The entropy rise across a normal shock of upstream Mach number
:math:`M_1` is

.. math::

   \frac{\Delta s}{c_v} \approx \frac{\gamma(\gamma+1)}{12}\,(M_1^2 - 1)^3
   + O\bigl((M_1^2-1)^4\bigr),

which confirms that the entropy rise (and hence wave drag) is third-order in
shock strength — the motivation for Cole's momentum-integral drag formula.

Numerical scheme
~~~~~~~~~~~~~~~~~

TSFOIL2 solves :eq:`tsd_sim` by a **Murman-Cole** type finite-difference scheme:

* The equation type is checked cell-by-cell: if :math:`K - (\gamma+1)\tilde\varphi_{\tilde x} > 0`
  at the current point, a **central difference** (consistent with elliptic
  type) is used; otherwise a **backward (upwind) difference** is used,
  consistent with the hyperbolic type and providing the correct shock-capturing
  entropy condition.
* The nonlinear system is iterated by **Line Successive Over-Relaxation (SOR)**
  with three user-supplied relaxation factors (``WE``).
* Artificial viscosity of strength ``EPS`` is added globally to damp
  oscillations and stabilise the iteration.

Wave drag
~~~~~~~~~~

The wave drag is evaluated by a **momentum contour integral** (Cole's formula)
rather than surface pressure integration, which gives better accuracy for
shocks that do not quite satisfy the Rankine-Hugoniot jump conditions on the
discrete mesh.  For each shock crossing the contour:

.. math::
   :label: wave_drag

   c_{D,\rm wave} = \frac{\gamma+1}{6}\,
                    \frac{b}{M_\infty}\,
                    \int_{y_b}^{y_t} (\Delta u)^3 \, dy,

where :math:`\Delta u = u_1 - u_2` is the perturbation velocity jump and
:math:`b / M_\infty` (``CDFACT`` in the code) converts
similarity drag to physical units.  The contour integrals along the upstream,
downstream, top, and bottom boundaries close the control volume.

----

Integral Boundary Layer (IBL) Method
--------------------------------------

The IBL method replaces the full Reynolds-averaged Navier-Stokes equations in
the thin viscous layer with **ordinary differential equations** for integrated
quantities, which can be marched from leading edge to trailing edge in
:math:`O(N)` operations.

Boundary-layer integral equations
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Starting from the compressible Navier-Stokes equations, applying the thin
boundary-layer approximation, and integrating across the boundary layer
thickness :math:`\delta` gives the following exact integral relations.

**Displacement thickness** :math:`\delta^*`:

.. math::

   \delta^* = \int_0^\delta \left(1 - \frac{\rho u}{\rho_e u_e}\right) dy.

**Momentum thickness** :math:`\theta`:

.. math::

   \theta = \int_0^\delta \frac{\rho u}{\rho_e u_e}
            \left(1 - \frac{u}{u_e}\right) dy.

**Shape factor**:

.. math::

   H = \frac{\delta^*}{\theta}.

**von Kármán momentum integral** (compressible, with Mach number correction):

.. math::
   :label: vk

   \frac{d\theta}{ds}
   + \left(H + 2 - M_e^2\right) \frac{\theta}{u_e} \frac{du_e}{ds}
   = \frac{c_f}{2},

where :math:`s` is the arc-length coordinate along the surface,
:math:`u_e` is the boundary-layer edge velocity,
:math:`M_e` is the edge Mach number, and
:math:`c_f = \tau_w / (\tfrac{1}{2}\rho_e u_e^2)` is the skin friction
coefficient.  The :math:`-M_e^2` term is the compressible (von Kármán)
correction.

Edge velocity from TSD
~~~~~~~~~~~~~~~~~~~~~~~

The boundary-layer edge velocity is related to the local wall Mach number
:math:`M_e` (from the TSD surface solution) via the isentropic relation:

.. math::
   :label: ue

   \frac{u_e}{u_\infty} = \frac{M_e}{M_\infty}
       \left(
           \frac{1 + \tfrac{\gamma-1}{2} M_\infty^2}
                {1 + \tfrac{\gamma-1}{2} M_e^2}
       \right)^{1/2}.

This converts the inviscid wall Mach number provided by TSD into a physical
edge velocity for the IBL solver.

Laminar region — Thwaites (1949)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In the laminar region an exact quadrature of the von Kármán equation yields
the **Thwaites formula**:

.. math::
   :label: thwaites

   \theta^2(s) = \frac{0.45\,\nu}{u_e^6(s)}
                 \int_0^s u_e^5(s')\, ds',

where :math:`\nu` is the kinematic viscosity (chord-normalised:
:math:`\nu = 1/Re`).  This is integrated numerically using the trapezoidal
rule from the stagnation point.

The **Thwaites parameter**
:math:`\lambda = (\theta^2/\nu)\,du_e/ds`
correlates with the shape factor :math:`H` and skin friction via
White's polynomial fits:

.. math::

   \ell(\lambda) &= (0.09 + \lambda)^{0.62},\\
   H(\lambda)    &= 2.0 + 4.14\,z - 83.5\,z^2 + 854\,z^3
                    - 3337\,z^4 + 4576\,z^5,
                    \quad z = 0.25 - \lambda.

Separation is indicated by :math:`\lambda < -0.09` (where
:math:`\ell \to 0`).

Transition — Michel's criterion (1951)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Natural transition from laminar to turbulent flow is predicted when the
momentum-thickness Reynolds number satisfies:

.. math::
   :label: michel

   Re_\theta \equiv \frac{u_e \theta}{\nu} > 2.9 \times 10^{-4}\, Re_x^{\,0.4},
   \qquad Re_x = \frac{u_e\, x}{\nu}.

The first :math:`s`-station where :eq:`michel` is satisfied is taken as the
transition point :math:`s_{\rm tr}`.  A user-specified ``x_tr_forced``
overrides this criterion and fixes the transition location.

Turbulent region — Head (1958)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In the turbulent region the von Kármán equation :eq:`vk` is supplemented by
**Head's entrainment equation**, which closes the system by relating
:math:`H` to a second integral quantity.

Define the **entrainment shape factor**

.. math::

   H_1 = \frac{\delta - \delta^*}{\theta},

so that the entrainment velocity :math:`v_e = d(\delta u_e)/ds` can be
written as :math:`v_e = u_e\,F(H_1)`, where Head's entrainment function is

.. math::
   :label: head_F

   F(H_1) =
   \begin{cases}
   0.0306\,(H_1 - 3.0)^{-0.6169}, & H_1 > 3.3,\\
   0.0306\,(0.3)^{-0.6169},        & H_1 \le 3.3.
   \end{cases}

The entrainment ODE is

.. math::
   :label: head_ode

   \frac{d(\theta H_1)}{ds}
   + \frac{1}{u_e}\frac{du_e}{ds}\,\theta H_1 = F(H_1),

and the closure relation between :math:`H` and :math:`H_1` (from curve fits
to experimental data) is

.. math::

   H_1 =
   \begin{cases}
   3.3 + 0.8234\,(H - 1.1)^{-1.287}, & H \le 1.6,\\
   3.3 + 1.5501\,(H - 0.6778)^{-3.064}, & H > 1.6.
   \end{cases}

Equations :eq:`vk` and :eq:`head_ode` form a coupled ODE system in
:math:`(\theta, H_1)` integrated with the ``scipy`` IVP solver, starting
from initial conditions at the transition point set by matching the laminar
values of :math:`\theta` and :math:`H`.

Skin friction — Ludwieg-Tillmann (1950)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The turbulent skin friction coefficient is given by the empirical
Ludwieg-Tillmann formula:

.. math::
   :label: lt

   c_f = \frac{0.246}{10^{0.678 H}}\,Re_\theta^{-0.268},

where :math:`Re_\theta = u_e \theta / \nu`.  This formula is accurate for
:math:`1.2 \lesssim H \lesssim 2.4` and
:math:`10^3 \lesssim Re_\theta \lesssim 10^6`.

Displacement thickness
~~~~~~~~~~~~~~~~~~~~~~

After the IVP is solved, the displacement thickness is recovered from the
definition:

.. math::

   \delta^* = H \cdot \theta.

This is the quantity fed back to the TSD solver as the wall-slope correction.

Friction drag
~~~~~~~~~~~~~

The integrated friction drag coefficient is computed by a trapezoidal
quadrature over both surfaces:

.. math::

   c_{D,f} = \int_0^{s_{\rm TE,u}} c_f^{\rm upper}\, ds
            + \int_0^{s_{\rm TE,l}} c_f^{\rm lower}\, ds,

with arc-length :math:`s` converting the chordwise integral to surface length.

----

Viscous-Inviscid Coupling
--------------------------

Concept: displacement body
~~~~~~~~~~~~~~~~~~~~~~~~~~~

The IBL displacement thickness :math:`\delta^*` modifies the effective
shape seen by the inviscid flow.  For an **upper surface**:

.. math::

   Y_{\rm eff}^+(x) = Y_{\rm airfoil}^+(x) + \delta^*(x).

This raises the effective upper surface by :math:`\delta^*`, thickening the
body.  For the **lower surface** the sign is reversed:

.. math::

   Y_{\rm eff}^-(x) = Y_{\rm airfoil}^-(x) - \delta^*(x).

Wall-slope correction
~~~~~~~~~~~~~~~~~~~~~~

Differentiating with respect to :math:`x` and normalising by
:math:`\delta = (t/c)_{\max}`, the wall BC slopes in :eq:`wall_bc` become

.. math::
   :label: slope_correction

   \frac{d\tilde{Y}^+}{dx} = \frac{dY^+/dx + d\delta^*_u/dx}{\delta},
   \qquad
   \frac{d\tilde{Y}^-}{dx} = \frac{dY^-/dx - d\delta^*_l/dx}{\delta}.

This is implemented by updating the arrays ``FXU`` and ``FXL`` in the
Fortran module before each warm-started TSD solve.

Coupling algorithm
~~~~~~~~~~~~~~~~~~~

The outer loop executes the following steps :math:`N_{\rm outer}` times:

.. list-table::
   :widths: 8 92
   :header-rows: 0

   * - **1.**
     - Solve TSD to convergence (warm-start from previous potential field
       :math:`\varphi^{(k-1)}`).
   * - **2.**
     - Extract wall Mach numbers :math:`M_e^u(x)`, :math:`M_e^l(x)`.
   * - **3.**
     - Apply Gaussian smoothing (σ = ``mach_smooth_sigma`` mesh points)
       to spread TSD shocks over several cells, preventing the IBL ODE
       from seeing a step change in :math:`du_e/ds`.
   * - **4.**
     - Run IBL on both surfaces: Thwaites → Michel → Head; obtain
       :math:`\delta^{*(k)}_u(x)` and :math:`\delta^{*(k)}_l(x)`.
   * - **5.**
     - Clip :math:`\delta^* \le \delta^*_{\max}` and smooth
       :math:`d\delta^*/dx` (σ = ``slope_smooth_sigma`` mesh points).
   * - **6.**
     - Compute slope correction :eq:`slope_correction` and apply
       under-relaxation:

       .. math::

          \text{FXU}^{(k)} = \omega_k\!\left[\frac{dY^+}{dx} +
          \frac{d\delta^{*(k)}_u/dx}{\delta}\right]
          + (1-\omega_k)\,\text{FXU}^{(k-1)},

       where the relaxation factor :math:`\omega_k` decreases linearly
       from 1 to ``coupling_relax_final`` over the outer loop.
   * - **7.**
     - Re-call ``SETBC`` to update the Fortran wall BC arrays
       ``FXUBC`` / ``FXLBC``.

Trailing-edge correction
~~~~~~~~~~~~~~~~~~~~~~~~~

Near the trailing edge the IBL δ\* tends to blow up as the boundary layer
approaches separation.  This numerical blow-up would generate an unrealistically
large wall-slope correction.  The **TE correction** (``use_te_correction=True``)
blends the IBL displacement thickness to a physically motivated extrapolation
over the region :math:`x \in [x_{\rm blend}, 1]`:

.. math::

   \delta^*_{\rm corrected}(x) =
   (1 - w)\,\delta^*_{\rm IBL}(x) + w\,\delta^*_{\rm model}(x),
   \qquad
   w = \left(\frac{x - x_{\rm blend}}{1 - x_{\rm blend}}\right)^2,

where :math:`x_{\rm blend}` is detected as the onset of the blow-up, and
:math:`\delta^*_{\rm model}` is a smooth extrapolation pinned to the IBL value
at :math:`x_{\rm blend}`.  The parameter ``te_relax`` controls how strongly
the model value is blended in.

Divergence Check Correction (DCC)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

For high Mach numbers or large angles of attack, the initial cold TSD solve
(at the full target AoA) may produce a diverged shock configuration.  The DCC
strategy detects this and restarts the coupling from :math:`\alpha = 0`,
ramping the angle of attack up over the outer iterations:

.. math::

   \alpha^{(k)} = t(k)\,\alpha_{\rm target},
   \qquad
   t(k) = \min\!\left(\frac{5k}{N_{\rm outer}},\; 1\right).

This gives the TSD-IBL system time to build up a well-conditioned flow field
before approaching the target condition.

----

References
----------

* Murman, E.M., Bailey, F.R., and Johnson, M.L.
  "TSFOIL — A Computer Code for Two-Dimensional Transonic Calculations,
  Including Wind-Tunnel Wall Effects and Wave Drag Evaluation."
  *NASA SP-347*, 1975.

* Cole, J.D.
  "Problems in Transonic Flow."
  PhD thesis, California Institute of Technology, 1951.

* Krupp, J.A. and Murman, E.M.
  "Computation of Transonic Flows Past Lifting Airfoils and Slender Bodies."
  *AIAA Journal*, 10(7), 880-886, 1971.

* Thwaites, B.
  "Approximate Calculation of the Laminar Boundary Layer."
  *Aeronautical Quarterly*, 1(3), 245-280, 1949.

* Michel, R.
  "Étude de la Transition sur les Profils d'Aile — Établissement d'un Critère
  de Détermination de Point de Transition et Calcul de la Traînée de Profil
  en Incompressible."
  *ONERA Rep. 1/1578A*, 1951.

* Head, M.R.
  "Entrainment in the Turbulent Boundary Layer."
  *ARC R&M 3152*, 1958.

* Ludwieg, H. and Tillmann, W.
  "Investigations of the Wall-Shearing Stress in Turbulent Boundary Layers."
  *NACA TM 1285*, 1950.

* White, F.M. *Viscous Fluid Flow*, 3rd ed. McGraw-Hill, 2006.

* Drela, M.
  "XFOIL: An Analysis and Design System for Low Reynolds Number Airfoils."
  *Lecture Notes in Engineering*, 54, 1-12, 1989.
  *(Reference for displacement-body coupling approach.)*
