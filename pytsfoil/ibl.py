'''
Integral Boundary Layer (IBL) method for 2D airfoil flows.

Given edge velocity distributions derived from wall Mach numbers via isentropic
relations, integrates the boundary layer equations using:

  - Thwaites' method           (laminar region)
  - Michel's criterion         (laminar-turbulent transition)
  - Head's entrainment method  (turbulent region)

Coupling with pyTSFoil:
  - Input:  wall Mach mau/mal on grid xx, plus EMACH and REYNLD
  - Output: displacement thickness delta_star for effective-body (wall-slope)
            correction in pyTSFoil's wall BC; cf for friction drag estimation

References:
  Thwaites, B. "Approximate Calculation of the Laminar Boundary Layer."
      ARC R&M 1314, 1949.
  Head, M. R. "Entrainment in the Turbulent Boundary Layer."
      ARC R&M 3152, 1958.
  Michel, R. "Étude de la Transition sur les Profils d'Aile."
      ONERA Rep. 1/1578A, 1951.
  Ludwieg, H. & Tillmann, W. "Investigations of the Wall-Shearing Stress in
      Turbulent Boundary Layers." NACA TM 1285, 1950.
  White, F. M. Viscous Fluid Flow, 3rd ed. McGraw-Hill, 2006.
'''

import warnings
import numpy as np
from scipy.integrate import solve_ivp
from scipy.interpolate import interp1d
from typing import Dict, Optional, Tuple, Any


class IBL(object):
    '''
    Integral Boundary Layer for 2D airfoil flows.

    Usage with pyTSFoil::

        ibl = IBL(Re=pytsfoil.config['REYNLD'], M_inf=pytsfoil.config['EMACH'])
        upper = ibl.run(
            xx  = pytsfoil.mesh['xx'],
            mach = pytsfoil.data_summary['mau'],
        )
        lower = ibl.run(
            xx  = pytsfoil.mesh['xx'],
            mach = pytsfoil.data_summary['mal'],
        )
        delta_star_upper = upper['delta_star']   # wall-slope correction input
        cd_friction = ibl.friction_drag(upper, lower)   # friction drag coefficient
    '''

    def __init__(self, Re: float, M_inf: float, gamma: float = 1.4):
        '''
        Parameters
        ----------
        Re : float
            Chord-based Reynolds number (REYNLD from pyTSFoil).
        M_inf : float
            Freestream Mach number (EMACH from pyTSFoil).
        gamma : float
            Specific heat ratio, default 1.4.
        '''
        self.Re    = Re
        self.M_inf = M_inf
        self.gamma = gamma
        self.nu    = 1.0 / Re  # chord-normalised kinematic viscosity: nu / (u_inf * c)

    # ------------------------------------------------------------------
    # Public API
    # ------------------------------------------------------------------

    def run(self,
            xx:           np.ndarray,
            mach:         np.ndarray,
            yy:           Optional[np.ndarray] = None,
            x_tr_forced:  Optional[float] = None,
            ) -> Dict[str, Any]:
        '''
        Run IBL on a single surface from leading edge (x=0) to trailing edge (x=1).

        Parameters
        ----------
        xx          : (n,)  x/c coordinates 0 → 1, from pyTSFoil mesh['xx']
        mach        : (n,)  edge Mach number at each station (mau or mal)
        yy          : (n,)  surface y/c coordinates; improves arc-length accuracy
                            for thick airfoils. If None, arc length ≈ x (flat plate).
        x_tr_forced : float or None
                            Forced transition x/c location. If given, Michel's
                            criterion is skipped and transition is fixed at this
                            chordwise position.

        Returns
        -------
        result: Dict[str, Any]
            - 's'            (n,)  arc-length / c from leading edge
            - 'ue'           (n,)  edge velocity ratio u_e / u_inf
            - 'theta'        (n,)  momentum thickness / c
            - 'delta_star'   (n,)  displacement thickness / c
            - 'H'            (n,)  shape factor H = delta_star / theta
            - 'cf'           (n,)  local skin friction coefficient
            - 'x_tr'         float transition x/c location (1.0 if fully laminar)
            - 'i_tr'         int   grid index of transition station
            - 'laminar_mask' (n,)  bool array, True where flow is laminar
        '''
        s  = self._arc_length(xx, yy)
        ue = np.maximum(self._mach_to_ue(mach), 1e-8)

        theta, H, cf, delta_star, _ = self._thwaites(s, ue)
        if x_tr_forced is not None:
            i_tr = int(np.clip(np.searchsorted(xx, x_tr_forced), 1, len(xx) - 1))
        else:
            i_tr = self._michel_transition(s, ue, theta)
        theta, H, cf, delta_star    = self._head(s, ue, mach, theta, H, i_tr)

        lam_mask          = np.zeros(len(s), dtype=bool)
        lam_mask[:i_tr]   = True

        return {
            's':            s,
            'ue':           ue,
            'theta':        theta,
            'delta_star':   delta_star,
            'H':            H,
            'cf':           cf,
            'x_tr':         float(xx[i_tr]) if i_tr < len(xx) else 1.0,
            'i_tr':         i_tr,
            'laminar_mask': lam_mask,
        }

    def wall_slope_correction(self,
                              xx:         np.ndarray,
                              delta_star: np.ndarray,
                              s:          np.ndarray,
                              upper:      bool = True,
                              ) -> np.ndarray:
        '''
        Wall-slope correction d(δ*)/dx for viscous-inviscid interaction.

        The displacement thickness modifies the effective airfoil geometry::

            y_eff_upper(x) = y_airfoil(x) + δ*_upper(x)
            y_eff_lower(x) = y_airfoil(x) - δ*_lower(x)

        In pyTSFoil's thin-airfoil wall BC  (phi_y = dY/dx * phi_x), add
        this correction to dY/dx to account for the viscous displacement.

        Parameters
        ----------
        xx         : (n,) x/c grid
        delta_star : (n,) displacement thickness from run()
        s          : (n,) arc-length from run()
        upper      : True for upper surface (+), False for lower (-)

        Returns
        -------
        (n,) array  ±d(δ*)/dx
        '''
        d_dstar_ds = np.gradient(delta_star, s)
        dx_ds      = np.gradient(xx, s)
        sign       = +1.0 if upper else -1.0
        return sign * d_dstar_ds * dx_ds

    def friction_drag(self,
                      upper: Dict[str, Any],
                      lower: Dict[str, Any],
                      ) -> float:
        '''
        Skin friction drag coefficient: Cd_f = integral cf ds  (both surfaces).

        Parameters
        ----------
        upper, lower : result dicts from run()

        Returns
        -------
        float  Cd_f (non-dimensionalised by chord and dynamic pressure)
        '''
        return (np.trapezoid(upper['cf'], upper['s'])
                + np.trapezoid(lower['cf'], lower['s']))

    @staticmethod
    def smooth_mach(mach: np.ndarray, sigma: float) -> np.ndarray:
        '''
        Gaussian-smooth a wall Mach distribution, replacing M=0 with a small
        floor first so that stagnation points do not pollute the smoothed field.

        Parameters
        ----------
        mach  : (n,) raw wall Mach array
        sigma : Gaussian smoothing width in grid points

        Returns
        -------
        (n,) smoothed Mach array
        '''
        from scipy.ndimage import gaussian_filter1d
        return gaussian_filter1d(np.maximum(mach, 0.05), sigma=sigma)

    @staticmethod
    def clip_and_smooth_slope(arr: np.ndarray,
                              sigma: float,
                              max_val: float) -> np.ndarray:
        '''
        Clip a wall-slope correction to ±max_val, then Gaussian-smooth it.

        Parameters
        ----------
        arr     : (n,) slope correction array d(δ*)/dx
        sigma   : Gaussian smoothing width in grid points
        max_val : symmetric clip bound

        Returns
        -------
        (n,) clipped and smoothed array
        '''
        from scipy.ndimage import gaussian_filter1d
        arr = np.clip(arr, -max_val, max_val)
        return gaussian_filter1d(arr, sigma=sigma)

    @staticmethod
    def repair_dstar(xx: np.ndarray,
                     dstar: np.ndarray,
                     outlier_sigma: float = 5.0) -> Tuple[np.ndarray, int]:
        '''
        Detect trailing-edge blow-up in a δ* (or θ) distribution and replace
        the outlier tail with linear extrapolation from the last stable points.

        The Head ODE can diverge near the trailing edge when the edge-velocity
        gradient is large.  This method identifies the first point in the rear
        half of the surface where \|dδ\*/dx\| exceeds ``outlier_sigma`` times the
        median \|dδ\*/dx\| over the first 70 % of the surface, then linearly
        extrapolates from the two preceding points to the trailing edge.

        Parameters
        ----------
        xx            : (n,) x/c grid
        dstar         : (n,) δ* or θ distribution from run()
        outlier_sigma : detection threshold (multiple of upstream median gradient)

        Returns
        -------
        dstar_repaired  : (n,) repaired array (values clipped to ≥ 0)
        i_break         : index of the first outlier point (or None if no outliers)
        '''
        n = len(dstar)
        if n < 6:
            return dstar.copy(), n-1
        grad = np.gradient(dstar, xx)
        i_ref = max(int(0.7 * n), 4)
        ref = np.median(np.abs(grad[:i_ref])) + 1e-10
        threshold = outlier_sigma * ref
        i_search = max(int(0.5 * n), 2)
        i_break = None
        for i in range(i_search, n):
            if abs(grad[i]) > threshold:
                i_break = i
                break
        if i_break is None or i_break < 2:
            return dstar.copy(), n-1
        result = dstar.copy()
        dx = xx[i_break - 1] - xx[i_break - 2]
        if abs(dx) < 1e-12:
            return dstar.copy(), n-1
        slope = (dstar[i_break - 1] - dstar[i_break - 2]) / dx
        for i in range(i_break, n):
            result[i] = dstar[i_break - 1] + slope * (xx[i] - xx[i_break - 1])
        return np.maximum(result, 0.0), i_break

    @staticmethod
    def smooth_correction(x, y, D, d_end='min', shape="quadratic",
                    d_min=None, k=5, robust=False) -> Tuple[np.ndarray, bool]:
        """
        Smooth correction d(x) added to y(x), pinned at the left end and
        matching a target slope (and optionally a target value) at the right end.

        Always enforced:
            d(x[0])         = 0          # left endpoint unchanged
            (y + d)'(x[-1]) = D          # corrected curve has slope D at the right end

        The correction is a polynomial blend in the normalized coordinate
        s = (x - x0) / L, with L = x[-1] - x0. Since ds/dx = 1/L is constant, the
        right-end slope constraint holds exactly on any monotonic grid. The slope
        the correction must supply, expressed in s, is m = g * L with
        g = D - y'(x[-1]).

        Two regimes
        -----------
        1. d_end is None  -> amplitude is *not* prescribed; the original blend
        menu (`shape`) is used:
            "linear"    d = m*s            (constant slope; min bending energy)
            "quadratic" d = m*s**2 / 2     (d = d' = 0 at left)            [default]
            "cubic"     d = m*s**3 / 3     (d = d' = d'' = 0 at left)

        2. d_end is given -> a cubic Hermite blend pins the right-end *value* too:
            d(0) = 0, d'(0) = 0, d(x[-1]) = d_end, (y+d)'(x[-1]) = D
            d(s) = (3*A - m)*s**2 + (m - 2*A)*s**3,   A = d_end
        Because d' = 0 at the left, the curve eases in flatly and rises to d_end.

        Monotonicity (only meaningful in regime 2, where amplitude is fixed)
        -------------------------------------------------------------------
        The cubic is monotone on [0, 1] iff  m * (3*A - m) >= 0, i.e. d_end must
        share the sign of the end slope and satisfy |d_end| >= |m| / 3. When this
        holds, d is monotone and max|d| occurs at the right end (max|d| = |d_end|),
        so constraining the maximum is the same as setting d_end. If
        `enforce_monotonic` is True and the condition fails, a ValueError reports
        the feasible bound instead of silently returning a non-monotone curve.

        Parameters
        ----------
        x, y : ndarray
            Baseline curve; x monotonic, possibly non-uniform; same shape.
        D : float
            Target slope of (y + d) at the right endpoint.
        d_end : float, optional
            Target value of d at the right endpoint (= the maximum of d when the
            result is monotone). If None, amplitude is left free (regime 1).
        shape : {"linear", "quadratic", "cubic"}, optional
            Blend used only when d_end is None.
        d_min : float, optional
            Minimum value of d_end when d_end='min'.
        k : int, optional
            Trailing points used for the end-slope fit when robust=True.
        robust : bool, optional
            If True, estimate y'(x[-1]) from a quadratic fit over the last k points
            (better for noisy data); otherwise use np.gradient (2nd-order one-sided).

        Returns
        -------
        d : ndarray
            The correction, same shape as x. The corrected curve is y + d.
        success : bool
            True if the monotonicity condition is satisfied or not enforced; False
            if the condition is violated but not enforced (in which case a warning
            is printed and the non-monotone curve is returned).
        """
        x = np.asarray(x, float)
        y = np.asarray(y, float)
        L = x[-1] - x[0]

        # slope of the baseline at the right endpoint
        if robust:                                    # noisy data: local quadratic fit
            p = np.polyfit(x[-k:], y[-k:], 2)
            yp_end = np.polyval(np.polyder(p), x[-1])
        else:                                         # clean data: one-sided difference
            yp_end = np.gradient(y, x)[-1]

        g = D - yp_end                                # slope d must add at x[-1]
        m = g * L                                     # same slope in the s-coordinate
        s = (x - x[0]) / L                            # normalized coordinate in [0, 1]

        success = True
        if d_end is None:
            # --- regime 1: amplitude free, pick a blend shape ---
            if shape == "linear":
                d = m * s
            elif shape == "quadratic":
                d = 0.5 * m * s**2
            elif shape == "cubic":
                d = (m / 3.0) * s**3
            else:
                raise ValueError("shape must be 'linear', 'quadratic', or 'cubic'")
        else:
            # --- regime 2: cubic Hermite pins value (+ slope) at the right end ---
            if d_end == "min":
                # Smallest-magnitude monotone end value. Feasibility requires
                # sign(d_end)=sign(m) and |d_end| >= |m|/3, so the minimum is m/3.
                # (At this bound the cubic collapses to d = (m/3)*s**3, i.e. it is
                #  exactly the shape="cubic" blend — the critically-monotone case.)
                A = m / 3.0
                A = max(A, d_min) if d_min is not None else A
            else:
                A = float(d_end)
                if m * (3.0 * A - m) < 0.0:
                    bound = m / 3.0
                    rel = ">=" if m > 0 else "<="
                    print(
                        "[Warning] Monotonic correction infeasible: with end slope D=%g "
                        "(m = g*L = %g), d_end must satisfy d_end %s %g."
                        % (D, m, rel, bound)
                    )
                    success = False
                    
            d = (3.0 * A - m) * s**2 + (m - 2.0 * A) * s**3

        return d, success

    @staticmethod
    def correction_dstar(xx: np.ndarray, yy: np.ndarray,
                dstar: np.ndarray, AoA: float,
                te_relax: float = 0.3,
                x_blend_start: float = 0.9,
                min_delta: float = -0.01,
                upper: bool = True
                ) -> np.ndarray:
        '''
        Trailing-edge correction for displacement thickness δ*.

        Adjusts δ* near the trailing edge so that the effective upper/lower surface
        (y_eff = y ± δ*) has its slope at x = 1 equal to tan(AoA).

        Parameters
        ----------
        xx: (n,)
            x/c grid (shared for upper and lower surfaces)
        yy: (n,)
            upper/lower surface y/c coordinates
        dstar: (n,)
            δ* from IBL.run()
        AoA: float
            angle of attack (degrees)
        te_relax: float
            Under-relaxation factor for the δ* update (0 < relax ≤ 1).
            It reduces the magnitude of the TE-correction to the δ*,
            providing a systematic bias/relaxation to the TE slope correction.
            Empirically, values around 0.5 together with `NWEDGE=2` achieves good results.
        x_blend_start : float
            x/c where the correction ramp begins (default 0.8), i.e.,
            the location to start blending the TE correction with the IBL solution.
        upper : bool
            True for upper surface correction, False for lower surface.

        Returns
        -------
        dstar_corr: (n,)
            corrected upper/lower displacement thickness

        Notes
        -----
        The correction Δ is added to the effective surface definition:
        
        - y_cor = y_eff + Δ;
        - dy_cor = dy_eff + dΔ (denote d = d(·)/dx);
        - dy_cor[x=1] = target_slope;
        - dΔ = dy_cor - dy_eff
            
        where Δ is zero before x_blend_start and ramps to the correction
        needed to achieve target_slope at TE.
        
        So, we can have dΔ as a smooth function, where dΔ is zero before x_blend_start
        and ramps to `target_slope - dy_eff[x=1]` at TE.
        
        - y_eff = y ± δ*
        - y_cor = y_eff + Δ = y ± δ* + Δ
        - dstar_corr = δ* ± Δ
        '''
        if upper:
            y_eff = yy + dstar
            target_slope = np.tan(np.deg2rad(AoA))
        else:
            y_eff = - yy + dstar
            target_slope = - np.tan(np.deg2rad(AoA))

        mask = xx >= x_blend_start
        _x = xx[mask]
        _y = y_eff[mask]
        _delta, success = IBL.smooth_correction(_x, _y, target_slope, d_min=-dstar[-1]*0.5)
        if not success:
            return dstar

        delta = np.zeros_like(xx)
        delta[mask] = _delta * te_relax
        delta = np.clip(delta, min_delta, None)

        if upper:
            dstar_corr = dstar + delta
        else:
            dstar_corr = dstar + delta
            
        return dstar_corr


    # ------------------------------------------------------------------
    # Private API
    # ------------------------------------------------------------------

    def _mach_to_ue(self, M_e: np.ndarray) -> np.ndarray:
        '''
        Calculate edge velocity from Mach number via isentropic relation:
        
        u_e / u_inf = (M_e / M_inf) * sqrt( (1 + (g-1)/2 * M_inf^2)
                                            / (1 + (g-1)/2 * M_e^2  ) )
        '''
        g  = self.gamma
        m0 = self.M_inf
        return (M_e / m0) * np.sqrt(
            (1.0 + 0.5*(g-1.0)*m0**2) /
            (1.0 + 0.5*(g-1.0)*M_e**2)
        )

    @staticmethod
    def _arc_length(xx: np.ndarray,
                    yy: Optional[np.ndarray]) -> np.ndarray:
        '''
        Calculate arc length s along the surface from leading edge, using the
        x/c coordinates and optionally the y/c coordinates for improved accuracy on thick airfoils.
        If yy is None, assumes a flat plate and uses s ≈ x
        '''
        if yy is not None:
            ds = np.sqrt(np.diff(xx)**2 + np.diff(yy)**2)
        else:
            ds = np.abs(np.diff(xx))
        return np.concatenate([[0.0], np.cumsum(ds)])

    def _thwaites(self, s: np.ndarray, ue: np.ndarray):
        '''
        Calculate laminar boundary layer properties using Thwaites' method (1949).
        
        Closed-form Thwaites integral:
            θ²(s) = 0.45 ν / u_e^6 * ∫₀ˢ u_e^5 ds
        '''
        nu = self.nu

        integrand = ue**5
        integral  = np.zeros_like(s)
        for i in range(1, len(s)):
            integral[i] = (integral[i-1]
                           + 0.5*(integrand[i] + integrand[i-1])*(s[i]-s[i-1]))

        theta2 = np.maximum(0.45 * nu * integral / ue**6, 1e-30)
        theta  = np.sqrt(theta2)

        lam    = np.clip(theta**2 / nu * np.gradient(ue, s), -0.09, 0.25)
        l, H   = self._thwaites_correlations(lam)
        cf     = 2.0 * nu * l / (ue * theta + 1e-30)
        cf[0]  = cf[1]   # remove stagnation singularity (theta→0 at s=0)
        dstar  = H * theta
        return theta, H, cf, dstar, lam

    @staticmethod
    def _thwaites_correlations(lam: np.ndarray):
        '''
        White (2006) polynomial fits to Thwaites' table, valid -0.09 ≤ λ ≤ 0.25.

        l(λ): dimensionless wall shear  →  cf = 2 ν l / (u_e θ)
        H(λ): shape factor              →  δ* = H θ
        '''
        l = 0.22 + 1.402*lam + 0.018*lam / (0.107 + lam)
        z = 0.25 - lam
        H = (2.0 + 4.14*z - 83.5*z**2 + 854.0*z**3
             - 3337.0*z**4 + 4576.0*z**5)
        return l, np.clip(H, 1.0, 10.0)

    def _michel_transition(self, s: np.ndarray, ue: np.ndarray,
                           theta: np.ndarray) -> int:
        '''
        Calculate laminar-turbulent transition location using Michel's criterion (1951).
        
        Transition when Re_θ ≥ 1.174 · (1 + 22400/Re_x) · Re_x^0.46.

        Returns the first grid index where the criterion is exceeded,
        or len(s) if the boundary layer stays laminar to the trailing edge.
        '''
        nu      = self.nu
        Re_th   = ue * theta / nu
        Re_x    = np.maximum(ue * s / (nu + 1e-30), 1.0)
        Re_tr   = 1.174 * (1.0 + 22400.0 / Re_x) * Re_x**0.46

        exceed       = Re_th >= Re_tr
        exceed[:5]   = False  # ignore leading-edge singularity region

        idx = int(np.argmax(exceed))
        return idx if exceed[idx] else len(s)

    # ------------------------------------------------------------------
    # Turbulent: Head (1958)
    # ------------------------------------------------------------------

    def _head(self,
              s:         np.ndarray,
              ue:        np.ndarray,
              mach:      np.ndarray,
              theta_lam: np.ndarray,
              H_lam:     np.ndarray,
              i_tr:      int,
              ) -> Tuple[np.ndarray, np.ndarray, np.ndarray, np.ndarray]:
        '''
        Head's entrainment method integrated from the transition station i_tr.

        Compressible von Kármán momentum integral (reference form):
            dθ/ds = cf/2 - (2 + H - Me²) · θ/u_e · du_e/ds

        The -Me² term is the compressibility correction to the effective pressure
        gradient; it reduces the streamtube-spreading rate and is O(0.5) at
        Me ≈ 0.7, so it is significant for transonic flow from pyTSFoil.

        Head's entrainment ODE (second equation, unchanged):
            d(H₁θ)/ds = 0.0306 · (H₁ - 3)^{-0.6169}

        Skin friction via Ludwieg-Tillmann:  cf = 0.246 · 10^{-0.678H} · Re_θ^{-0.268}
        Auxiliary relation H₁(H): Head (1958) two-branch formula.

        Note on second-equation choice: the modern XFOIL/Drela approach uses the
        kinetic-energy integral equation with H* instead of Head's H₁ ODE. Head's
        method is simpler and sufficient for attached/mildly separated flow. A full
        H* formulation would also include a turbulence lag equation for Cτ.
        '''
        nu     = self.nu
        theta  = theta_lam.copy()
        H      = H_lam.copy()
        due_ds = np.gradient(ue, s)

        # Laminar cf over full array (overwritten in turbulent region below)
        lam_l = np.clip(theta_lam**2 / nu * due_ds, -0.09, 0.25)
        l, _  = self._thwaites_correlations(lam_l)
        cf    = 2.0 * nu * l / (ue * theta_lam + 1e-30)
        cf[0] = cf[1]   # remove stagnation singularity (theta→0 at s=0)

        if i_tr >= len(s) - 1:
            return theta, H, cf, H * theta

        # Turbulent domain
        s_t    = s[i_tr:]
        ue_t   = ue[i_tr:]
        due_t  = due_ds[i_tr:]
        me_t   = mach[i_tr:]          # edge Mach for compressibility correction

        _ue_f  = interp1d(s_t, ue_t,  kind='linear',
                          fill_value=(ue_t[0],  ue_t[-1]),  bounds_error=False)
        _due_f = interp1d(s_t, due_t, kind='linear',
                          fill_value=(due_t[0], due_t[-1]), bounds_error=False)
        _me_f  = interp1d(s_t, me_t,  kind='linear',
                          fill_value=(me_t[0],  me_t[-1]),  bounds_error=False)

        th0   = float(theta_lam[i_tr])
        H0    = 1.4                         # canonical turbulent starting value
        H1_0  = self._H_to_H1(H0)
        y0    = [th0, H1_0 * th0]

        def rhs(t, y):
            th, H1th = y
            th    = max(th, 1e-10)
            H1    = max(H1th / th, 3.01)
            _H    = self._H1_to_H(H1)
            _ue   = float(_ue_f(t))
            _due  = float(_due_f(t))
            _Me   = float(_me_f(t))
            _cf   = self._ludwieg_tillmann(_H, _ue * th / nu)
            # Compressible von Kármán: (2 + H - Me²) instead of (2 + H)
            dth   = 0.5*_cf - (_H + 2.0 - _Me**2)*th/(_ue+1e-30)*_due
            dH1th = 0.0306 * (H1 - 3.0)**(-0.6169)
            return [dth, dH1th]

        sol = solve_ivp(rhs, [s_t[0], s_t[-1]], y0,
                        method='RK45', t_eval=s_t,
                        rtol=1e-5, atol=1e-9)

        if sol.success:
            th_t, H1th_t = sol.y[0], sol.y[1]
        else:
            warnings.warn("Head ODE did not converge; falling back to forward Euler.")
            th_t, H1th_t = self._head_euler(s_t, ue_t, due_t, me_t, th0, H1_0, nu)

        th_t   = np.maximum(th_t, 1e-10)
        H1_t   = np.maximum(H1th_t / th_t, 3.01)
        H_t    = np.vectorize(self._H1_to_H)(H1_t)
        cf_t   = np.vectorize(self._ludwieg_tillmann)(H_t, ue_t * th_t / nu)

        theta[i_tr:] = th_t
        H[i_tr:]     = H_t
        cf[i_tr:]    = cf_t
        return theta, H, cf, H * theta

    @staticmethod
    def _head_euler(s, ue, due_ds, me, theta0, H1_0, nu):
        '''Forward-Euler fallback for Head's ODE (used when RK45 fails).'''
        n     = len(s)
        th    = np.zeros(n);  th[0]   = theta0
        H1th  = np.zeros(n);  H1th[0] = H1_0 * theta0
        for i in range(n - 1):
            ds    = s[i+1] - s[i]
            H1    = max(H1th[i] / th[i], 3.01)
            _H    = IBL._H1_to_H(H1)
            _cf   = IBL._ludwieg_tillmann(_H, ue[i] * th[i] / nu)
            th[i+1]   = max(th[i] + ds*(0.5*_cf
                            - (_H + 2.0 - me[i]**2)*th[i]/(ue[i]+1e-30)*due_ds[i]),
                            1e-10)
            H1th[i+1] = H1th[i] + ds * 0.0306*(H1 - 3.0)**(-0.6169)
        return th, H1th

    @staticmethod
    def _H_to_H1(H: float) -> float:
        '''
        Calculate Head's auxiliary variable H₁ from shape factor H (Head 1958).
        '''
        
        H = max(H, 1.05)
        if H < 1.6:
            return 3.3 + 0.8234 * (H - 1.1)**(-1.287)
        return 3.3 + 1.5501 * (H - 0.6778)**(-3.064)

    @staticmethod
    def _H1_to_H(H1: float) -> float:
        '''
        Calculate shape factor H from Head's auxiliary variable H₁ (Head 1958).
        '''
        H1    = max(H1, 3.01)
        dH1   = max(H1 - 3.3, 1e-6)
        # Branch 1: H < 1.6
        H_b1  = 1.1 + (0.8234 / dH1)**(1.0 / 1.287)
        if H_b1 <= 1.6:
            return max(H_b1, 1.05)
        # Branch 2: H ≥ 1.6
        return max(0.6778 + (1.5501 / dH1)**(1.0 / 3.064), 1.05)

    @staticmethod
    def _ludwieg_tillmann(H: float, Re_theta: float) -> float:
        '''
        Calculate turbulent skin friction coefficient using Ludwieg-Tillmann (1950) formula:
        
        cf = 0.246 · 10^{-0.678 H} · Re_θ^{-0.268}
        '''
        return 0.246 * 10.0**(-0.678 * H) * max(Re_theta, 10.0)**(-0.268)
