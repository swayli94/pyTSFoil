"""
MAE composite correction formula (Rusak 1993, JFM 248, Section 6).

Additive composite:
    cp(x) = cp*(s) + (rho*/rho_inf) * phi_{0x*} * [cp_tsd(x) - cp_common(s)]

where:
    s           = x / R_c
    cp*(s)      = inner surface pressure
    rho*/rho_inf(s) = inner density ratio
    phi_{0x*}(s)    = inner axial velocity (0 at stagnation, 1 far away)
    cp_tsd(x)   = -2 * U * cpfact   (linear TSD formula, NOT truncated by EMACH1)
    cp_common(s) = 0.635776 / (gamma+1)^{1/3} * s^{-1/3}  (TSD nose singularity)

The correction is applied independently to upper and lower surfaces.
"""

import numpy as np
from typing import Tuple, Dict

# Pre-computed constant: 3^{2/3} * cot^{2/3}(alpha3) where alpha3=80.40878 deg
# From Rusak (1993) Table 1 / Eq. (101) simplified with s = x/R_c
_COMMON_COEF = 0.635776


def compute_ma_isentropic(cp: np.ndarray, minf: float, gamma: float = 1.4) -> np.ndarray:
    """
    Compute Mach number from pressure coefficient via inverted isentropic relation.
    
    Parameters
    ----------
    cp : ndarray
        Pressure coefficient.
    minf : float
        Freestream Mach number.
    gamma : float
        Ratio of specific heats.

    Returns
    -------
    ma : ndarray
        Mach number corresponding to the given cp via the isentropic relation.
        Guaranteed to be >= 0.
    """
    A       = 1.0 + 0.5 * (gamma - 1.0) * minf ** 2
    denom   = np.maximum(1.0 + 0.5 * gamma * minf ** 2 * cp, 1e-6)
    ratio   = A / denom ** ((gamma - 1.0) / gamma)
    ma_sq   = (2.0 / (gamma - 1.0)) * (ratio - 1.0)
    ma      = np.sqrt(np.maximum(ma_sq, 0.0))
    return ma


def apply_composite_correction(
        x_surface: np.ndarray,
        cp_tsd: np.ndarray,
        minf: float,
        R_c: float,
        inner_tables: dict,
        x_blend_range: tuple = (0.02, 0.10),
        gamma: float = 1.4,
        debug: bool = False) -> Tuple[np.ndarray, np.ndarray, Dict[str, np.ndarray]]:
    """
    Apply MAE composite correction to one surface (upper OR lower).
    
    `cp = cp_inner + (rho*/rho_inf) * phi_{0x*} * (cp_tsd - cp_common)`

    Call separately for upper and lower surfaces.

    Parameters
    ----------
    x_surface : ndarray, shape (n,)
        x-coordinates of surface mesh points (chord-normalised, 0-1).
    cp_tsd : ndarray, shape (n,)
        Surface pressure coefficient of the raw TSD solution at x_surface,
        i.e., the outer solution without any correction.
    minf : float
        Freestream Mach number.
    R_c : float
        Parabolic nose curvature radius R_c = 2*h^2*delta^2*c.
    inner_tables : dict
        Output of solve_inner_problem: keys 's', 'cp_star', 'rho_ratio', 'phi_ox'.
    x_blend_range : tuple of float
        Range of x over which to blend from composite back to raw TSD
    gamma : float
        Ratio of specific heats.

    Returns
    -------
    cp_composite : ndarray
        Corrected surface pressure coefficient.
    ma_composite : ndarray
        Mach number back-computed from cp_composite via isentropic relation.
        Guaranteed to be >= 0.
    """
    s_tab = inner_tables['s']
    s_max = s_tab[-1]

    s_full = x_surface / R_c  # can be negative for x<0 (outside LE)
    s_b0 = x_blend_range[0] / R_c
    s_b1 = x_blend_range[1] / R_c
    '''
    # Note for the blend range (`x_pure_inner` and `x_blend_range`):
    
    - For s in [0, s_min], the inner solution (`cp_inner`) is fully active.
      Because the TSD formula is numerically unstable near s=0 (singularity),
      we set the bracket `(cp_tsd - cp_common)` to 0 in this region, allowing the inner solution to dominate.
    - For s in [s_min, s_b0], we apply the full composite correction (`cp_composite`),
      which smoothly transitions from the inner solution to the outer TSD solution (the nose region).
    - For s in [s_b0, s_b1], we smoothly blend from `cp_composite` to the raw TSD solution
      (`cp_tsd` solution in the full field).
    - For s > s_b1, we use the raw TSD solution (`cp_tsd`) without any correction,
      as the inner solution's influence diminishes and the TSD solution becomes more accurate.
    
    - The inner solution (`cp_inner`) is interpolated from the inner tables ('cp_star')
      and dominates near the stagnation point.
    - The composite solution (`cp_composite`) is a linear superposition of the inner solution and the outer TSD solution,
      modulated by the density ratio (`rho_ratio`) and axial velocity factor (`phi_ox`).
      `cp_composite = cp_inner + rho_ratio * phi_ox * bracket`.
    
    - Theoretically, at s ~ 5-10 the inner solution has phi_ox -> 1 and cp* -> 0,
      so the composite reduces to `(cp_tsd - cp_common)`. Because cp_common = C*s^{-1/3} is still O(0.2) at s = 10,
      leaving the formula active beyond this point introduces a systematic negative bias across the airfoil.
      Blending to cp_tsd eliminates this bias. So, s_b1 = 10.
    - However, in practice, we found that the raw TSD solution is not accurate in a larger region beyond s=10,
      the composite correction can still provide a significant improvement up to s ~ 20-30, and blending back
      to TSD at s=10 can introduce an artificial positive bump in the Cp distribution.
    - Therefore, we provide a user-controllable `x_blend_range` to allow the composite correction to be active
      over a wider range, while still ensuring a smooth transition to the raw TSD solution at larger s.
    '''
    
    # Interpolate inner tables at s_full (clamp to table range for extrapolation)
    s_min = 0.01
    s_clamp = np.clip(s_full, max(s_tab[0], 1e-6), s_max)
    cp_inner  = np.interp(s_clamp, s_tab, inner_tables['cp_star'])
    rho_ratio = np.interp(s_clamp, s_tab, inner_tables['rho_ratio'])
    phi_ox    = np.interp(s_clamp, s_tab, inner_tables['phi_ox'])

    # Physical bounds
    phi_ox    = np.clip(phi_ox,    0.0, 1.0)
    rho_ratio = np.clip(rho_ratio, 0.0, 2.0)

    # Cap at physical stagnation Cp: the inner problem is solved at M_inf=1 so its
    # stagnation Cp (≈1.276) exceeds the outer limit at M_inf < 1.  Clamping ensures
    # Ma_composite >= 0 and matches the physical stagnation constraint.
    cp_stagnation = (2.0 / (gamma * minf ** 2)) * (
        (1.0 + 0.5 * (gamma - 1.0) * minf ** 2) ** (gamma / (gamma - 1.0)) - 1.0)

    # Common part: TSD nose singularity (s^{-1/3})
    # Protected at s=0 by a minimum s_min; the bracket (cp_tsd - cp_common) is
    # O(1) there because cp_tsd ~ s^{-1/3} with the same coefficient.
    cp_common = _COMMON_COEF / (gamma + 1.0) ** (1.0 / 3.0) * s_clamp ** (-1.0 / 3.0)
    cp_common = np.minimum(cp_common, cp_stagnation)

    # For s < s_min the subtraction cp_tsd - cp_common is numerically unstable;
    # setting the bracket to 0 lets the internal cp* dominate (physically correct).
    bracket = cp_tsd - cp_common
    cp_composite_original = cp_inner + rho_ratio * phi_ox * bracket

    '''
    Note for the correctness of linear superposition in the composite formula:
    
    The matched asymptotic expansion (MAE) composite:
    `f_comp = f_inner + (rho*/rho_inf) * phi_{0x*} * (f_tsd - f_common)`
    is valid for a quantity `f` when three conditions are jointly satisfied:

    - Matching principle. Both the inner (full nonlinear) and outer (TSD) solutions converge
      to the same physical value f_common in the overlap region. This is a necessary condition
      for any f, and it holds for both Cp and Ma.

    - Consistent singularity structure at the outer leading edge. In the outer TSD solution,
      Cp diverges as x → 0 (the classical leading-edge square-root singularity, `Cp ~ A/sqrt(x)`).
      The amplitude of this singularity is precisely `(rho*/rho_inf)*phi_{0x*}`,
      which is the scaling factor in the composite formula. The formula therefore works for Cp
      because the factor (cp_tsd - cp_common) eliminates the divergent outer singularity,
      leaving a smooth correction that the finite inner solution can supply.

    - Linear outer mapping. In the TSD outer region, Cp = -2*phi_x is a linear map from the velocity potential.
      This linearity propagates the additive structure of phi (which is the quantity that is truly additive in MAE)
      directly into Cp. The scaling factor `(rho*/rho_inf)*phi_{0x*}` is itself derived from the amplitude of `phi_x`
      at the outer leading edge, so the formula is internally consistent.

    However, Ma violates condition 2 and 3 simultaneously:

    - In the outer TSD solution, Ma approaches a finite subsonic value as x → 0;
      it carries no outer singularity of the same type as Cp. The factor `(rho*/rho_inf)*phi_{0x*}` was derived for
      the divergent Cp singularity and does not have the same physical meaning when applied to a bounded quantity.
    - The mapping phi → Ma is nonlinear even in the outer TSD region (Ma = Q/a involves a ratio of two
      velocity-dependent quantities), so the additive structure of `phi` does not transfer to Ma via a simple linear operator.
    '''

    # Smooth blend back to raw TSD for s in [s_b0, s_b1].
    if s_b1 > s_b0:
        t = np.clip((s_full - s_b0) / (s_b1 - s_b0), 0.0, 1.0)
        w = t * t * (3.0 - 2.0 * t)     # smoothstep (C1)
    else:
        w = 1.0  # no blending, full TSD everywhere
    cp_composite = (1.0 - w) * cp_composite_original + w * cp_tsd

    # Safety fallback for s > table range
    mask_inner = s_clamp < s_min
    mask_tsd = (x_surface < 0.0) | (s_full > s_max)
    cp_composite = np.where(mask_inner, cp_inner, cp_composite)  # pure inner solution for s < s_min
    cp_composite = np.where(mask_tsd, cp_tsd, cp_composite) # pure TSD solution for s < 0 or s > s_max


    cp_composite = np.minimum(cp_composite, cp_stagnation)
        
    ma_composite = compute_ma_isentropic(cp_composite, minf, gamma)
    
    debug_info = {}
    if debug:
        debug_info = {
            's_full': s_full,
            'cp_tsd': cp_tsd,
            'cp_inner': cp_inner,
            'cp_common': cp_common,
            'rho_ratio': rho_ratio,
            'phi_ox': phi_ox,
            'bracket': bracket,
            'rho_phi_bracket': rho_ratio * phi_ox * bracket,
            'cp_composite_original': cp_composite_original,
            'cp_composite_blended': cp_composite,
            'blend_weight': w,
            'Rc': R_c,
            'x_smin': s_min * R_c,
            'x_sb0': x_blend_range[0],
            'x_sb1': x_blend_range[1],
            'x_smax': R_c * s_max,
        }

    return cp_composite, ma_composite, debug_info
