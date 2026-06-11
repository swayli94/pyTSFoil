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


def calculate_phi_ry(
        fxu: np.ndarray,
        fxl: np.ndarray,
        fx_camber: np.ndarray,
        phi_sy: np.ndarray,
        x_foil: np.ndarray,
        x_blend_range: tuple = (0.002, 0.010),
    ) -> tuple:
    """
    Calculate the regularised residual BCs phi_ry for the Step D LE closure.

    - `phi_ry_upper = FXU - phi_sy`
    - `phi_ry_lower = FXL + phi_sy`

    This formula removes the leading-order x^{-1/2} singularity from the raw residual BCs, 
    but it has some numerical issues near x=0.
    
    - At x=0, `phi_sy` is set to zero to avoid numerical issues, but FXU/FXL are not.
    - The `phi_sy` is not a perfect match to the actual singularity in FXU/FXL,
      so the subtraction is not exact. The `phi_ry` distribution is not perfect near the LE.
    - Theoretically, `phi_ry` should approach the slope of camber `FX_CAMBER` near the LE.
    
    To mitigate these issues, this function applies a closure to smoothly transition the `phi_ry` values from their raw computed values to the slope of camber in the region of
    `x in [0, x_blending]`.
    
    Parameters
    ----------
    fxu, fxl    : surface slopes (nfoil,)
    fx_camber   : camber slope (nfoil,)
    phi_sy      : Step-D correction from compute_surface_corrections (nfoil,)
    x_foil      : chord-normalised x-coordinates (nfoil,); x[0] = 0 assumed
    x_blending  : length of blending region from LE (default 0.05)

    Returns
    -------
    fxu_modified, fxl_modified : (nfoil,)
    """
    phi_ry_upper = fxu - phi_sy
    phi_ry_lower = fxl + phi_sy
    
    # Smooth transition weight: smoothstep (C1)
    t = np.clip((x_foil-x_blend_range[0]) / (x_blend_range[1] - x_blend_range[0]), 0.0, 1.0)
    w = t * t * (3.0 - 2.0 * t)
    
    # Blend phi_ry towards fx_camber near the LE to mitigate numerical issues
    phi_ry_upper = (1.0 - w) * fx_camber + w * phi_ry_upper
    phi_ry_lower = (1.0 - w) * fx_camber + w * phi_ry_lower

    return phi_ry_upper, phi_ry_lower


def compute_phi_s_2d(
        X_1d: np.ndarray,
        Y_1d: np.ndarray,
        R_c: float,
        cpfact: float,
        gamma: float = 1.4,
        r1: float = 2.0,
        r2: float = 8.0,
) -> np.ndarray:
    """Compute the 2D singular potential phi_s on the full outer mesh.

    Uses the asymptotic form f(xi) ~ C*xi^{2/3} (valid for xi >> 1, i.e. near Y=0).
    This gives phi_s(X, Y) = A * X^{2/3} * chi(r), independent of Y, where the
    window chi provides the spatial confinement and A is determined by matching
    to the cp_common formula.

    The returned array has Fortran layout PHI_S(J, I) = phi_s(Y_J, X_I), shape
    (len(Y_1d), len(X_1d)).  Pass directly to tsf.solver_data.phi_s.

    Parameters
    ----------
    X_1d   : 1D array of outer grid X-coordinates, length NI
    Y_1d   : 1D array of outer grid Y-coordinates, length NJ
    R_c    : parabolic nose radius (= 2*h^2 for c=1)
    cpfact : TSD pressure scaling delta^{2/3}
    gamma  : ratio of specific heats
    r1, r2 : window transition limits in units of R_c (default 2, 8)

    Returns
    -------
    phi_s_2d : ndarray, shape (NJ, NI), float32
        PHI_S(J, I) = phi_s at grid point (Y_J, X_I).
    """
    # Amplitude coefficient: from -2*A*(2/3)*X^{-1/3}*cpfact = cp_common(X)
    # => A = -3/(4) * (COMMON_COEF * R_c^{1/3} / (gamma+1)^{1/3}) / cpfact
    A = (-3.0 / 4.0) * (_COMMON_COEF * R_c ** (1.0 / 3.0) / (gamma + 1.0) ** (1.0 / 3.0)) / cpfact

    # 2D grid: shape (NJ, NI) — Fortran layout PHI_S(J, I)
    XX, YY = np.meshgrid(X_1d, Y_1d, indexing='xy')   # YY rows, XX cols → (NJ, NI)

    # Window: r = sqrt(X^2 + Y^2) / R_c; chi = 1 inside, 0 outside
    r = np.sqrt(np.maximum(XX, 0.0) ** 2 + YY ** 2) / R_c
    chi = _window(r, r1, r2)

    # phi_s = A * X^{2/3} * chi  (zero where X <= 0)
    # np.maximum clamps negatives to 0 before the fractional power to avoid
    # nan from (-x)^(2/3) in float arithmetic (np.where evaluates both branches)
    phi_s = A * np.maximum(XX, 0.0) ** (2.0 / 3.0) * chi

    return phi_s.astype(np.float32)
