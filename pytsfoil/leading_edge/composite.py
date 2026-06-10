"""
MAE composite correction formula (Rusak 1993, JFM 248, Section 6).

Additive composite:
    cp(x) = cp*(s) + (rho*/rho_inf) * phi_{0x*} * [cp_TSD(x) - cp_common(s)]

where:
    s           = x / R_c
    cp*(s)      = inner surface pressure
    rho*/rho_inf(s) = inner density ratio
    phi_{0x*}(s)    = inner axial velocity (0 at stagnation, 1 far away)
    cp_TSD(x)   = -2 * U * cpfact   (linear TSD formula, NOT truncated by EMACH1)
    cp_common(s) = 0.635776 / (gamma+1)^{1/3} * s^{-1/3}  (TSD nose singularity)

The correction is applied independently to upper and lower surfaces.
"""

import numpy as np

# Pre-computed constant: 3^{2/3} * cot^{2/3}(alpha3) where alpha3=80.40878 deg
# From Rusak (1993) Table 1 / Eq. (101) simplified with s = x/R_c
_COMMON_COEF = 0.635776


def apply_composite_correction(
        x_surface: np.ndarray,
        u_surface: np.ndarray,
        cpfact: float,
        minf: float,
        R_c: float,
        inner_tables: dict,
        gamma: float = 1.4) -> tuple:
    """
    Apply MAE composite correction to one surface (upper OR lower).

    Call separately for upper and lower surfaces.

    Parameters
    ----------
    x_surface : ndarray, shape (n,)
        x-coordinates of surface mesh points (chord-normalised, 0..1).
    u_surface : ndarray, shape (n,)
        Perturbation velocity U = dP/dx at surface from Fortran P field.
        Must be the RAW value — NOT truncated by EMACH1 (Ma≥0 clamp).
    cpfact : float
        TSD scaling factor delta^{2/3} (or Spreiter/Krupp variant).
    minf : float
        Freestream Mach number.
    R_c : float
        Parabolic nose curvature radius R_c = 2*h^2*delta^2*c.
    inner_tables : dict
        Output of solve_inner_problem: keys 's', 'cp_star', 'rho_ratio', 'phi_ox'.
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

    # Interpolate inner tables at s_full (clamp to table range for extrapolation)
    s_clamp = np.clip(s_full, s_tab[0], s_max)
    cp_star   = np.interp(s_clamp, s_tab, inner_tables['cp_star'])
    rho_ratio = np.interp(s_clamp, s_tab, inner_tables['rho_ratio'])
    phi_ox    = np.interp(s_clamp, s_tab, inner_tables['phi_ox'])

    # Physical bounds
    phi_ox    = np.clip(phi_ox,    0.0, 1.0)
    rho_ratio = np.clip(rho_ratio, 0.0, 2.0)

    # Outer TSD Cp — linear formula, bypasses EMACH1 Ma truncation
    cp_tsd = -2.0 * u_surface * cpfact

    # Common part: TSD nose singularity (s^{-1/3})
    # Protected at s=0 by a minimum s_min; the bracket (cp_tsd - cp_common) is
    # O(1) there because cp_tsd ~ s^{-1/3} with the same coefficient.
    s_safe  = np.maximum(s_full, 1e-6)
    cp_common = _COMMON_COEF / (gamma + 1.0) ** (1.0 / 3.0) * s_safe ** (-1.0 / 3.0)

    # For s < s_min the subtraction cp_tsd - cp_common is numerically unstable;
    # setting the bracket to 0 lets the internal cp* dominate (physically correct).
    s_min   = 0.01
    bracket = np.where(s_full >= s_min, cp_tsd - cp_common, 0.0)

    cp_composite = cp_star + rho_ratio * phi_ox * bracket

    # Smooth blend back to raw TSD for s in [s_b0, s_b1].  At s ~ 5–10 the inner
    # solution has phi_ox -> 1 and cp* -> 0, so the composite reduces to
    # (cp_tsd - cp_common).  Because cp_common = C*s^{-1/3} is still O(0.2) at
    # s = 10, leaving the formula active beyond this point introduces a systematic
    # negative bias across the airfoil.  Blending to cp_tsd eliminates this bias.
    s_b0 = 5.0
    s_b1 = 10.0
    t = np.clip((s_full - s_b0) / (s_b1 - s_b0), 0.0, 1.0)
    w = t * t * (3.0 - 2.0 * t)                # smoothstep (C1)
    cp_composite = (1.0 - w) * cp_composite + w * cp_tsd

    # Safety fallback for s > table range
    far_mask = s_full > s_max
    cp_composite = np.where(far_mask, cp_tsd, cp_composite)

    # For points with x < 0 (upstream of LE): no correction
    cp_composite = np.where(x_surface < 0.0, cp_tsd, cp_composite)

    # Cap at physical stagnation Cp: the inner problem is solved at M_inf=1 so its
    # stagnation Cp (≈1.276) exceeds the outer limit at M_inf < 1.  Clamping ensures
    # Ma_composite >= 0 and matches the physical stagnation constraint.
    cp_stagnation = (2.0 / (gamma * minf ** 2)) * (
        (1.0 + 0.5 * (gamma - 1.0) * minf ** 2) ** (gamma / (gamma - 1.0)) - 1.0)
    cp_composite = np.minimum(cp_composite, cp_stagnation)

    # Back-compute Ma from cp_composite via inverted isentropic relation:
    #   cp = (2/(gamma*M^2)) * ((p/p_inf) - 1)
    #   p/p_inf = ((1+(gamma-1)/2*M_inf^2) / (1+(gamma-1)/2*Ma^2))^(gamma/(gamma-1))
    #
    # Solving for Ma:
    #   Ma^2 = (2/(gamma-1)) * (A / (1 + gamma/2 * M_inf^2 * cp)^((gamma-1)/gamma) - 1)
    # where A = 1 + (gamma-1)/2 * M_inf^2
    A       = 1.0 + 0.5 * (gamma - 1.0) * minf ** 2
    denom   = np.maximum(1.0 + 0.5 * gamma * minf ** 2 * cp_composite, 1e-6)
    ratio   = A / denom ** ((gamma - 1.0) / gamma)
    ma_sq   = (2.0 / (gamma - 1.0)) * (ratio - 1.0)
    ma_composite = np.sqrt(np.maximum(ma_sq, 0.0))

    return cp_composite, ma_composite
