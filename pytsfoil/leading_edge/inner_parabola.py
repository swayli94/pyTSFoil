"""
Inner region solver for MAE leading-edge correction (Rusak 1993, JFM 248).

Solves the full nonlinear potential equation at M_inf=1 in parabolic
coordinates (mu, eta) via vectorised red-black SOR with near-optimal omega.

Coordinate system (normalised by h*c^(1/2)):
    x_inner = (mu^2 - eta^2) / 2
    y_inner = mu * eta
    Wing surface : eta = sqrt(2)    Neumann   phi_eta = 0
    Symmetry axis: mu  = 0          Neumann   phi_mu  = 0
    Far field    : mu  = mu_T  and  eta = eta_T    Dirichlet phi = X_ff

Governing equation (conservation flux form in parabolic coordinates):
    d/dmu(rho * phi_mu) + d/deta(rho * phi_eta) = 0
    rho = [1 + (gamma-1)/2 * (1 - Q^2)]^(1/(gamma-1))
    Q^2 = (phi_mu^2 + phi_eta^2) / (mu^2 + eta^2)

Array layout: phi[i, j], shape (M+2, N+2)
    i = 0        ghost  (mu = -dmu  — encodes phi_mu(0) = 0 via phi[0]=phi[2])
    i = 1..M     interior               (mu = 0, dmu, ..., (M-1)*dmu)
    i = M+1      far-field Dirichlet    (mu = mu_T)
    j = 0        ghost  (eta = eta_w - deta — encodes phi_eta(eta_w) = 0)
    j = 1..N     interior               (eta = eta_w, ..., eta_w+(N-1)*deta)
    j = N+1      far-field Dirichlet    (eta = eta_T)
"""

import numpy as np
import hashlib
import os


# ────────────────────────────────────────────────────────────────────────────────
# Low-level helpers
# ────────────────────────────────────────────────────────────────────────────────

def _compute_rho(phi, MU, ETA, dmu, deta, gamma, M, N):
    """Density at every grid point from central-difference velocities.

    Ghost cells are given the same density as their physical mirror images
    so that face averages at the Neumann boundaries are correct.
    """
    # phi_mu : central differences for i = 1..M; one-sided at boundaries
    pm = np.empty_like(phi)
    pm[1:M + 1, :] = (phi[2:M + 2, :] - phi[0:M, :]) / (2.0 * dmu)
    pm[0, :]       = pm[2, :]     # symmetry ghost (mu = 0 → phi_mu = 0 there)
    pm[M + 1, :]   = pm[M, :]    # far-field (Dirichlet): one-sided outward

    # phi_eta : central differences for j = 1..N
    pe = np.empty_like(phi)
    pe[:, 1:N + 1] = (phi[:, 2:N + 2] - phi[:, 0:N]) / (2.0 * deta)
    pe[:, 0]       = pe[:, 2]    # wall ghost
    pe[:, N + 1]   = pe[:, N]   # far-field

    r2     = np.maximum(MU ** 2 + ETA ** 2, 1e-8)
    vel_sq = (pm ** 2 + pe ** 2) / r2
    # Clip vel_sq to ensure subsonic density (avoids clipping instability)
    vel_sq = np.minimum(vel_sq, 0.999)
    arg    = np.maximum(1.0 + 0.5 * (gamma - 1.0) * (1.0 - vel_sq), 1e-6)
    rho    = arg ** (1.0 / (gamma - 1.0))

    # Fix ghost cell densities by symmetry/reflection so face averages are correct
    rho[0, :]     = rho[2, :]     # mu = 0 symmetry
    rho[:, 0]     = rho[:, 2]     # wall reflection
    return rho


def _apply_bc(phi, X_ff, M, N):
    """Enforce all boundary conditions in-place on phi."""
    phi[0, :]     = phi[2, :]         # mu = 0 symmetry ghost
    phi[:, 0]     = phi[:, 2]         # wall eta = eta_w ghost
    phi[M + 1, :] = X_ff[M + 1, :]   # far-field Dirichlet mu = mu_T
    phi[:, N + 1] = X_ff[:, N + 1]   # far-field Dirichlet eta = eta_T


# ────────────────────────────────────────────────────────────────────────────────
# Red-black SOR core
# ────────────────────────────────────────────────────────────────────────────────

def _sor_sweep(phi, rho, X_ff, dmu, deta, M, N, omega, colour):
    """
    One red or black SOR sweep in-place.

    colour = 0 → update points where (i+j) % 2 == 0
    colour = 1 → update points where (i+j) % 2 == 1
    """
    idmu2  = 1.0 / dmu  ** 2
    ideta2 = 1.0 / deta ** 2

    # Face-centred densities (shape (M+2, N+2) each)
    rho_e = np.zeros_like(phi); rho_e[:-1, :] = 0.5 * (rho[:-1, :] + rho[1:,  :])
    rho_w = np.zeros_like(phi); rho_w[1:,  :] = 0.5 * (rho[:-1, :] + rho[1:,  :])
    rho_n = np.zeros_like(phi); rho_n[:, :-1] = 0.5 * (rho[:, :-1] + rho[:, 1:])
    rho_s = np.zeros_like(phi); rho_s[:, 1:]  = 0.5 * (rho[:, :-1] + rho[:, 1:])

    # Build index arrays for interior points (i = 1..M, j = 1..N)
    i_all = np.arange(1, M + 1)
    j_all = np.arange(1, N + 1)
    II, JJ = np.meshgrid(i_all, j_all, indexing='ij')   # (M, N) each
    mask   = (II + JJ) % 2 == colour
    I = II[mask]
    J = JJ[mask]

    # Neighbour values with BC substitutions baked in:
    # West:  i-1 = 0 at i=1 → phi[0,:] = phi[2,:] via _apply_bc
    # South: j-1 = 0 at j=1 → phi[:,0] = phi[:,2] via _apply_bc
    phi_e = phi[I + 1, J]
    phi_w = phi[I - 1, J]
    phi_n = phi[I, J + 1]
    phi_s = phi[I, J - 1]

    rho_e_p = rho_e[I, J]
    rho_w_p = rho_w[I, J]
    rho_n_p = rho_n[I, J]
    rho_s_p = rho_s[I, J]

    denom = (rho_e_p + rho_w_p) * idmu2 + (rho_n_p + rho_s_p) * ideta2
    phi_star = (
        (rho_e_p * phi_e + rho_w_p * phi_w) * idmu2 +
        (rho_n_p * phi_n + rho_s_p * phi_s) * ideta2
    ) / np.maximum(denom, 1e-12)

    phi[I, J] = (1.0 - omega) * phi[I, J] + omega * phi_star


def _solve_sor(gamma, M, N, mu_T, eta_T, omega, max_iter, tol):
    """
    Red-black SOR for the nonlinear potential equation.

    Returns (phi, mu_arr, eta_arr, rho).
    """
    eta_w = np.sqrt(2.0)
    dmu   = mu_T / M
    deta  = (eta_T - eta_w) / N

    mu_arr  = np.linspace(-dmu,         mu_T, M + 2)
    eta_arr = np.linspace(eta_w - deta, eta_T, N + 2)

    MU, ETA = np.meshgrid(mu_arr, eta_arr, indexing='ij')
    X_ff    = 0.5 * (MU ** 2 - ETA ** 2)

    phi = X_ff.copy()
    _apply_bc(phi, X_ff, M, N)

    residual = 1.0
    for it in range(max_iter):
        rho    = _compute_rho(phi, MU, ETA, dmu, deta, gamma, M, N)
        phi_bk = phi[1:M + 1, 1:N + 1].copy()

        _sor_sweep(phi, rho, X_ff, dmu, deta, M, N, omega, colour=0)  # red
        _apply_bc(phi, X_ff, M, N)
        rho = _compute_rho(phi, MU, ETA, dmu, deta, gamma, M, N)

        _sor_sweep(phi, rho, X_ff, dmu, deta, M, N, omega, colour=1)  # black
        _apply_bc(phi, X_ff, M, N)

        residual = float(np.max(np.abs(phi[1:M + 1, 1:N + 1] - phi_bk)))
        if residual < tol:
            print(f"  Inner SOR: converged at iter {it + 1}, residual={residual:.2e}")
            break
    else:
        print(f"  Inner SOR: max_iter={max_iter} reached, residual={residual:.2e}")

    rho = _compute_rho(phi, MU, ETA, dmu, deta, gamma, M, N)
    return phi, mu_arr, eta_arr, rho


# ────────────────────────────────────────────────────────────────────────────────
# Table extraction
# ────────────────────────────────────────────────────────────────────────────────

def _extract_tables(phi, rho, mu_arr, eta_arr, gamma):
    """
    Extract surface tables from converged phi.

    Surface is at j=1 (eta = sqrt(2)).  s = (mu^2 - 2)/4, s=0 at stagnation.
    Only interior points i=1..M are used (excludes Dirichlet far-field i=M+1).
    """
    M   = len(mu_arr) - 2
    dmu = mu_arr[2] - mu_arr[1]

    # Wall values at interior points only (i=1..M)
    mu_wall  = mu_arr[1:M + 1]
    rho_wall = rho[1:M + 1, 1]
    s_all    = (mu_wall ** 2 - 2.0) / 4.0

    # Physical region: s >= 0 (mu >= sqrt(2))
    mask  = s_all >= 0.0
    s         = s_all[mask]
    rho_s     = rho_wall[mask]
    mu_s      = mu_wall[mask]

    # Inner surface Cp (isentropic at M_inf = 1, rho_inf = 1)
    cp_star = (2.0 / gamma) * (rho_s ** gamma - 1.0)

    # phi_{0x*} = mu * phi_mu / (mu^2 + 2) at the wall
    # Central differences for i=1..M using ghost for i=1 and Dirichlet for i=M
    phi_mu_cd         = np.empty(M)
    phi_mu_cd[0]      = (phi[2, 1] - phi[0, 1]) / (2.0 * dmu)       # i=1
    phi_mu_cd[1:M - 1] = (phi[3:M + 1, 1] - phi[1:M - 1, 1]) / (2.0 * dmu)  # i=2..M-1
    phi_mu_cd[M - 1]  = (phi[M + 1, 1] - phi[M - 1, 1]) / (2.0 * dmu)       # i=M

    phi_ox_all = mu_wall * phi_mu_cd / (mu_wall ** 2 + 2.0)
    phi_ox     = phi_ox_all[mask]

    # Prepend exact stagnation point (s=0, mu=sqrt(2)) which the uniform grid cannot resolve.
    # Values: phi_ox=0 (velocity zero), rho=rho_max=(1+(gamma-1)/2)^(1/(gamma-1)), cp*=2/gamma*(rho_max^gamma-1)
    rho_max = (1.0 + 0.5 * (gamma - 1.0)) ** (1.0 / (gamma - 1.0))
    cp_stag = (2.0 / gamma) * (rho_max ** gamma - 1.0)
    s         = np.concatenate([[0.0], s])
    cp_star   = np.concatenate([[cp_stag], cp_star])
    rho_s     = np.concatenate([[rho_max], rho_s])
    phi_ox    = np.concatenate([[0.0], phi_ox])

    return s, cp_star, rho_s, phi_ox


# ────────────────────────────────────────────────────────────────────────────────
# Public API
# ────────────────────────────────────────────────────────────────────────────────

def solve_inner_problem(h: float, c: float,
                        gamma: float = 1.4,
                        M: int = 40, N: int = 40,
                        mu_T: float = 40.0, eta_T: float = 40.0,
                        omega: float = 1.85,
                        max_iter: int = 2000,
                        tol: float = 1e-6,
                        cache_dir: str | None = None) -> dict:
    """
    Solve the Rusak (1993) inner-region problem and return surface tables.

    Uses vectorised red-black SOR with near-optimal omega (≈1.85 for 40×40 grid).
    Typical convergence: < 300 iterations to tol=1e-6.

    The result is universal — independent of M_inf and alpha — and is cached
    on disk by geometry hash so each unique airfoil shape is solved only once.

    Parameters
    ----------
    h         : nose shape constant  ct(x/c) ~ 2h*(cx)^{1/2}
    c         : chord length (1.0 for normalised airfoils)
    gamma     : ratio of specific heats
    M, N      : grid resolution in mu and eta (40×40 from Rusak 1993)
    mu_T, eta_T : far-field extents
    omega     : SOR over-relaxation factor (1.85 near-optimal for M=N=40)
    max_iter  : maximum SOR iterations
    tol       : convergence tolerance on max |Δφ|
    cache_dir : directory for .npz cache files; None disables caching

    Returns
    -------
    dict with keys 's', 'cp_star', 'rho_ratio', 'phi_ox'
    """
    geom_hash = hashlib.md5(np.array([h, c]).tobytes()).hexdigest()[:12]

    if cache_dir is not None:
        os.makedirs(cache_dir, exist_ok=True)
        cache_file = os.path.join(cache_dir, f"inner_{geom_hash}.npz")
        if os.path.exists(cache_file):
            data = np.load(cache_file)
            return {k: data[k] for k in data.files}

    print(f"  Computing inner region solution "
          f"(M={M}, N={N}, mu_T={mu_T}, eta_T={eta_T}, omega={omega}) ...")

    phi, mu_arr, eta_arr, rho = _solve_sor(
        gamma=gamma, M=M, N=N,
        mu_T=mu_T, eta_T=eta_T,
        omega=omega, max_iter=max_iter, tol=tol
    )

    s, cp_star, rho_ratio, phi_ox = _extract_tables(
        phi, rho, mu_arr, eta_arr, gamma
    )

    tables = {
        's':         s,
        'cp_star':   cp_star,
        'rho_ratio': rho_ratio,
        'phi_ox':    phi_ox,
    }

    if cache_dir is not None:
        np.savez(cache_file, **tables)
        print(f"  Inner tables cached → {cache_file}")

    return tables
