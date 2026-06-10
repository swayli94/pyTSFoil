"""
Singularity subtraction surface corrections for TSD outer solution.

Implements steps D and E from TSD_singularity_subtraction.md:
  D. Body-BC regularisation: subtract phi_s,y(x,0+) from FXU/FXL so the
     numerical unknown phi_r = phi_1 - phi_s has a bounded surface slope.
  E. Post-processing restoration: add phi_s,x(x,0) back to PX so the
     composite correction receives the full phi_tot,x with the correct
     x^{-1/3} singularity structure.

Reference: Rusak (1993) JFM 248; TSD_singularity_subtraction.md §3.
"""

import numpy as np

# Must match composite.py: Cp_common = _COMMON_COEF / (gamma+1)^{1/3} * s^{-1/3}
_COMMON_COEF = 0.635776


def _window(r: np.ndarray, r1: float, r2: float) -> np.ndarray:
    """Smooth window: chi=1 at r<=r1, chi=0 at r>=r2 (C1 smoothstep)."""
    t = np.clip((r - r1) / (r2 - r1), 0.0, 1.0)
    return 1.0 - t * t * (3.0 - 2.0 * t)


def compute_surface_corrections(
        x_foil: np.ndarray,
        h: float,
        delta: float,
        R_c: float,
        cpfact: float,
        gamma: float = 1.4,
        r1: float = 2.0,
        r2: float = 8.0,
) -> tuple:
    """
    Surface corrections for singularity subtraction (steps D and E).

    Parameters
    ----------
    x_foil  : chord-normalised x-coordinates of airfoil mesh points (len=nfoil)
    h       : nose shape constant; half-thickness ~ 2h*sqrt(x) near LE
    delta   : maximum thickness ratio (used to normalise FXU/FXL)
    R_c     : parabolic nose radius  (= 2*h^2 for c=1)
    cpfact  : TSD pressure scaling  delta^{2/3} (Krupp/Cole/Spreiter)
    gamma   : ratio of specific heats
    r1, r2  : window transition limits in units of R_c (default 2, 8)

    Returns
    -------
    phi_sy_upper : ndarray, shape (nfoil,)
        Step D — body-BC correction.
        Upper surface: FXU_modified = FXU - phi_sy_upper  (removes x^{-1/2} spike).
        Lower surface: FXL_modified = FXL + phi_sy_upper  (symmetric thickness).
    phi_sx_surface : ndarray, shape (nfoil,)
        Step E — velocity restoration for post-processing.
        Add to PX before computing Mach/Cp: U_tot = U_r + phi_sx_surface.
        Value is negative (decelerating flow near stagnation).
    """
    # r = scaled distance from LE; chi = 1 inside matching zone, 0 outside
    with np.errstate(divide='ignore', invalid='ignore'):
        r = np.where(x_foil > 0.0, x_foil / R_c, 0.0)
    chi = _window(r, r1, r2)

    # ── Step D: body-BC singular part  phi_s,y(x, 0+) = chi * h/(delta*sqrt(x))
    # Near LE: FXU ~ h/(delta*sqrt(x)), so FXU_modified = FXU - phi_sy_upper → bounded.
    with np.errstate(divide='ignore', invalid='ignore'):
        phi_sy_upper = np.where(
            x_foil > 0.0,
            chi * h / (delta * np.sqrt(np.maximum(x_foil, 1e-15))),
            0.0,
        )

    # ── Step E: velocity restoration  phi_s,x(x, 0) = -cp_common / (2*cpfact)
    # cp_common = _COMMON_COEF/(gamma+1)^{1/3} * (x/R_c)^{-1/3}  (positive, stagnation side)
    # u_s = phi_s,x = -cp_common/(2*cpfact) < 0
    with np.errstate(divide='ignore', invalid='ignore'):
        r_safe = np.where(x_foil > 0.0, np.maximum(r, 1e-9), np.inf)
        cp_common = _COMMON_COEF / (gamma + 1.0) ** (1.0 / 3.0) * r_safe ** (-1.0 / 3.0)
    phi_sx_surface = np.where(
        x_foil > 0.0,
        chi * (-cp_common) / (2.0 * cpfact),
        0.0,
    )

    return phi_sy_upper.astype(np.float32), phi_sx_surface.astype(np.float32)
