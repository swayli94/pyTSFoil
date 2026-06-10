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


def apply_step_d_le_closure(
        fxu: np.ndarray,
        fxl: np.ndarray,
        phi_sy_upper: np.ndarray,
        x_foil: np.ndarray,
        method: str = 'sqrt_fit',
        n_fit: int = 6,
) -> tuple:
    """
    Step D LE closure: return regularized residual BCs phi_ry for the solver.

    phi_ry_upper = FXU - phi_sy_upper  (upper: removes x^{-1/2} spike)
    phi_ry_lower = FXL + phi_sy_upper  (lower: symmetric thickness)

    Near x=0, phi_ry has the theoretical form
        phi_ry(x) ≈ c1/delta + c2/delta * sqrt(x) + O(x)
    because FXU and phi_sy both diverge as x^{-1/2} and cancel, leaving a
    finite intercept c1/delta and a sqrt(x) correction.  The three closure
    methods extrapolate this series back to x=0:

      'sqrt_fit'  (default) — fit phi_ry = A + B*sqrt(x) with n_fit points
                              and use A as the x=0 value.  Mesh-convergent and
                              numerically stable: the sqrt-basis is well-scaled
                              and eliminates the O(sqrt(x)) bias present in
                              both constant and linear methods.
      'linear'              — fit phi_ry = A + B*x with n_fit points and use
                              A as x=0.  Linear-in-x basis diverges in slope as
                              mesh refines (d phi_ry/dx ~ x^{-1/2} → ∞), so
                              the intercept A is mesh-convergent only because
                              the large slope * small x terms cancel; less
                              stable than 'sqrt_fit' on coarse meshes.
      'constant'            — use phi_ry[i_eff] directly (no fitting).  Fastest
                              but retains an O(sqrt(x[i_eff])) bias.

    Parameters
    ----------
    fxu, fxl      : surface slopes FXU/delta, FXL/delta (nfoil,)
    phi_sy_upper  : Step-D correction from compute_surface_corrections (nfoil,)
    x_foil        : chord-normalised x-coordinates (nfoil,); x[0] = 0 assumed
    method        : 'sqrt_fit' | 'linear' | 'constant'
    n_fit         : number of points from effective zone used for fitting

    Returns
    -------
    fxu_modified, fxl_modified : float32 arrays (nfoil,)
    """
    phi_ry_upper = fxu.astype(np.float64) - phi_sy_upper.astype(np.float64)
    phi_ry_lower = fxl.astype(np.float64) + phi_sy_upper.astype(np.float64)

    effective = phi_sy_upper > 0.0
    if not effective.any():
        return phi_ry_upper.astype(np.float32), phi_ry_lower.astype(np.float32)

    i_eff = int(np.argmax(effective))
    if i_eff == 0:
        return phi_ry_upper.astype(np.float32), phi_ry_lower.astype(np.float32)

    i_end = min(i_eff + n_fit, len(phi_ry_upper))
    n_pts = i_end - i_eff

    if method == 'constant' or n_pts < 2:
        val_u = phi_ry_upper[i_eff]
        val_l = phi_ry_lower[i_eff]
    elif method == 'linear':
        # Fit phi_ry = A + B*x; evaluate at x=0 → A
        x_fit = x_foil[i_eff:i_end]
        A_u, _ = _fit_intercept(x_fit, phi_ry_upper[i_eff:i_end])
        A_l, _ = _fit_intercept(x_fit, phi_ry_lower[i_eff:i_end])
        val_u, val_l = A_u, A_l
    elif method == 'sqrt_fit':
        # Fit phi_ry = A + B*sqrt(x); evaluate at x=0 → A
        sx_fit = np.sqrt(x_foil[i_eff:i_end])
        A_u, _ = _fit_intercept(sx_fit, phi_ry_upper[i_eff:i_end])
        A_l, _ = _fit_intercept(sx_fit, phi_ry_lower[i_eff:i_end])
        val_u, val_l = A_u, A_l
    else:
        raise ValueError(f"apply_step_d_le_closure: unknown method '{method}'")

    phi_ry_upper[:i_eff] = val_u
    phi_ry_lower[:i_eff] = val_l

    return phi_ry_upper.astype(np.float32), phi_ry_lower.astype(np.float32)


def _fit_intercept(x: np.ndarray, y: np.ndarray):
    """Least-squares linear fit y = A + B*x; return (A, B)."""
    n = len(x)
    if n < 2:
        return float(y[0]), 0.0
    sx  = float(np.sum(x));  sx2 = float(np.sum(x ** 2))
    sy  = float(np.sum(y));  sxy = float(np.sum(x * y))
    det = n * sx2 - sx ** 2
    if abs(det) < 1e-30:
        return float(y[0]), 0.0
    A = (sx2 * sy - sx * sxy) / det
    B = (n  * sxy - sx * sy)  / det
    return A, B


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
    phi_s = np.where(XX > 0.0, A * XX ** (2.0 / 3.0) * chi, 0.0)

    return phi_s.astype(np.float32)
