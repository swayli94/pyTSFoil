"""
3D TSD Numba kernels — Stage 1-3 (rectangular wing, single symmetric plane + wingtip).

Changes vs stage_1_2:
  - compute_phi_zz_rhs_station: k=nk-1 uses Dirichlet BC (P[k+1] = 0) instead of
    Neumann ghost, so the outer boundary is a spanwise far-field with phi=0.
  - thomas_column_sweep_3d: add apply_wall_bc flag so off-wing stations (k > k_tip)
    skip the airfoil wall and Kutta BCs.

Index convention (0-based throughout):
  i  = chordwise   (0 … ni-1)
  j  = normal/wall (0 … nj-1)
  k  = spanwise    (0 … nk-1)
  P has shape (nk, nj, ni).

Spanwise BCs:
  k=0      : symmetric root (phi_z=0, ghost P[-1] = P[1])
  k=nk-1   : spanwise far-field (phi=0 Dirichlet, P[nk] = 0 phantom)
"""

import numpy as np
from numba import njit


# ---------------------------------------------------------------------------
# 3-D Thomas column sweep  (explicit phi_zz, optional wall BC)
# ---------------------------------------------------------------------------

@njit(cache=True)
def thomas_column_sweep_3d(
    i_col,
    P_k,            # float64[nj, ni], k-th spanwise slice, updated in-place
    P_prev_saved,   # float64[nj], snapshot of P_k[:, i_col-1] before update
    EMU_prev,       # float64[nj], EMU from column i_col-1
    emu_cur,        # float64[nj], OUTPUT: EMU for current column
    # --- finite-difference coefficients ---
    C1, CXL, CXC, CXR,
    CXXL, CXXC, CXXR,
    CYYC, CYYD, CYYU,
    CYYBUC, CYYBUU,
    CYYBLC, CYYBLD,
    # --- boundary condition arrays ---
    FXUBC, FXLBC, PJUMP_k,
    # --- mesh ---
    X, XDIFF,
    # --- solver parameters ---
    eps, wi, ak, rtk,
    # --- index bounds (0-based) ---
    ile, ite, jup, jlow, jbot, jtop, jmin, jmax, im2,
    # --- CFS params ---
    eps_ampl, cfs_triggered, ite_lo, ite_hi, sonvel, beta_sonic,
    # --- phi_zz explicit treatment ---
    phi_zz_diag,    # float64 scalar: added to DIAG (0.0 for explicit-only)
    phi_zz_rhs,     # float64[nj]: full phi_zz at this column (lagged from P_old)
    # --- wing-surface flag ---
    apply_wall_bc,  # bool: False for k > k_tip (off-wing stations)
):
    """
    Single-column Murman-Cole SLOR sweep with explicit spanwise coupling.

    When apply_wall_bc=False the wall/Kutta BC section is skipped so that
    off-wing stations see a plain 2-D Laplace equation (no airfoil source).
    """
    nj = P_k.shape[0]

    dx   = X[i_col] - X[i_col - 1]
    epsx = eps / (dx * dx)
    if cfs_triggered and ite_lo <= i_col <= ite_hi:
        epsx *= eps_ampl

    # ------------------------------------------------------------------ #
    # 1. VC = AK/dx − (γ+1)/2 · φ_x                                      #
    # ------------------------------------------------------------------ #
    VC = np.empty(nj, dtype=np.float64)
    for j in range(jbot, jtop + 1):
        VC[j] = (C1[i_col]
                 - CXL[i_col] * P_prev_saved[j]
                 - CXC[i_col] * P_k[j, i_col]
                 - CXR[i_col] * P_k[j, i_col + 1])
        emu_cur[j] = 0.0
    for j in range(jbot, jtop + 1):
        if VC[j] < 0.0:
            emu_cur[j] = VC[j]

    # ------------------------------------------------------------------ #
    # 2. Assemble tridiagonal                                              #
    # ------------------------------------------------------------------ #
    DIAG = np.empty(nj, dtype=np.float64)
    SUP  = np.empty(nj, dtype=np.float64)
    SUB  = np.empty(nj, dtype=np.float64)
    RHS  = np.empty(nj, dtype=np.float64)

    for j in range(jbot, jtop + 1):
        DIAG[j] = ((emu_cur[j] - VC[j]) * CXXC[i_col] * wi
                   + EMU_prev[j] * CXXR[i_col - 1]
                   - CYYC[j]
                   + phi_zz_diag)
        SUP[j] = CYYD[j]
        SUB[j] = CYYU[j]

    # ------------------------------------------------------------------ #
    # 3. RHS: −residual(φ_xx)                                             #
    # ------------------------------------------------------------------ #
    for j in range(jbot, jtop + 1):
        RHS[j] = -(VC[j] - emu_cur[j]) * (
            CXXL[i_col] * P_k[j, i_col - 1]
            - CXXC[i_col] * P_k[j, i_col]
            + CXXR[i_col] * P_k[j, i_col + 1]
        )
    for j in range(jbot, jtop + 1):
        RHS[j] -= EMU_prev[j] * (
            CXXL[i_col - 1] * P_k[j, im2]
            - CXXC[i_col - 1] * P_k[j, i_col - 1]
            + CXXR[i_col - 1] * P_k[j, i_col]
        )

    # ------------------------------------------------------------------ #
    # 4. RHS: −residual(φ_yy)                                             #
    # ------------------------------------------------------------------ #
    for j in range(jbot + 1, jtop):
        RHS[j] -= (CYYD[j] * P_k[j - 1, i_col]
                   - CYYC[j] * P_k[j, i_col]
                   + CYYU[j] * P_k[j + 1, i_col])

    RHS[jbot] -= (-CYYC[jbot] * P_k[jbot, i_col]
                  + CYYU[jbot] * P_k[jbot + 1, i_col])
    if jbot != jmin:
        RHS[jbot] -= CYYD[jbot] * P_k[jbot - 1, i_col]

    RHS[jtop] -= (CYYD[jtop] * P_k[jtop - 1, i_col]
                  - CYYC[jtop] * P_k[jtop, i_col])
    if jtop != jmax:
        RHS[jtop] -= CYYU[jtop] * P_k[jtop + 1, i_col]

    # ------------------------------------------------------------------ #
    # 4b. RHS: explicit phi_zz neighbor contribution (full residual)      #
    # ------------------------------------------------------------------ #
    for j in range(jbot, jtop + 1):
        RHS[j] -= phi_zz_rhs[j]

    # ------------------------------------------------------------------ #
    # 5. Boundary conditions (wall + Kutta) — wing stations only          #
    # ------------------------------------------------------------------ #
    if apply_wall_bc:
        if ile <= i_col <= ite:
            j = jup
            DIAG[j] += CYYC[j] - CYYBUC
            SUP[j]   = 0.0
            SUB[j]   = CYYBUU
            RHS[j]  += (CYYD[j] * P_k[j - 1, i_col]
                        - CYYC[j] * P_k[j, i_col]
                        + CYYU[j] * P_k[j + 1, i_col]
                        - (-CYYBUC * P_k[j, i_col]
                           + CYYBUU * P_k[j + 1, i_col]
                           + FXUBC[i_col]))
            j = jlow
            DIAG[j] += CYYC[j] - CYYBLC
            SUP[j]   = CYYBLD
            SUB[j]   = 0.0
            RHS[j]  += (CYYD[j] * P_k[j - 1, i_col]
                        - CYYC[j] * P_k[j, i_col]
                        + CYYU[j] * P_k[j + 1, i_col]
                        - (-CYYBLC * P_k[j, i_col]
                           + CYYBLD * P_k[j - 1, i_col]
                           + FXLBC[i_col]))
        elif i_col > ite:
            RHS[jlow] += CYYU[jlow] * PJUMP_k[i_col]
            RHS[jup]  -= CYYD[jup]  * PJUMP_k[i_col]

    # ------------------------------------------------------------------ #
    # 6. BCEND: supersonic freestream far-field BC  (only if ak <= 0)    #
    # ------------------------------------------------------------------ #
    if ak <= 0.0:
        dfacl = -CYYD[jbot] * rtk * XDIFF[i_col]
        dfacu = -CYYU[jtop] * rtk * XDIFF[i_col]
        rfacl = dfacl * (P_k[0,    i_col] - P_k[0,    i_col - 1])
        rfacu = dfacu * (P_k[jmax, i_col] - P_k[jmax, i_col - 1])
        DIAG[jbot] += dfacl
        DIAG[jtop] += dfacu
        RHS[jbot] -= rfacl
        if jbot > jmin:
            RHS[jbot] += CYYD[jbot] * P_k[jbot - 1, i_col]
        RHS[jtop] -= rfacu
        if jtop < jmax:
            RHS[jtop] += CYYU[jtop] * P_k[jtop + 1, i_col]

    # ------------------------------------------------------------------ #
    # 7. Artificial dissipation                                           #
    # ------------------------------------------------------------------ #
    for j in range(jbot, jtop + 1):
        DIAG[j] -= epsx
        RHS[j]  -= epsx * (P_k[j, i_col - 1] - P_prev_saved[j])

    # ------------------------------------------------------------------ #
    # 8. CFS sonic penalty                                                #
    # ------------------------------------------------------------------ #
    if cfs_triggered and ite_lo <= i_col <= ite_hi:
        alpha_sonic = eps * beta_sonic / (dx * dx)
        for j in range(jbot, jtop + 1):
            p_sonic  = P_k[j, i_col - 1] + sonvel * dx
            DIAG[j] += alpha_sonic
            RHS[j]  -= alpha_sonic * (P_k[j, i_col] - p_sonic)

    # ------------------------------------------------------------------ #
    # 9. Thomas algorithm                                                 #
    # ------------------------------------------------------------------ #
    SAVE = np.empty(nj, dtype=np.float64)
    dnom       = 1.0 / DIAG[jbot]
    SAVE[jbot] = SUB[jbot] * dnom
    RHS[jbot]  = RHS[jbot] * dnom

    for j in range(jbot + 1, jtop + 1):
        denom   = DIAG[j] - SUP[j] * SAVE[j - 1]
        dnom    = 1.0 / denom
        SAVE[j] = SUB[j] * dnom
        RHS[j]  = (RHS[j] - SUP[j] * RHS[j - 1]) * dnom
        if abs(RHS[j]) < 1.0e-30:
            RHS[j] = 0.0

    for j in range(jtop - 1, jbot - 1, -1):
        RHS[j] -= SAVE[j] * RHS[j + 1]
        if abs(RHS[j]) < 1.0e-30:
            RHS[j] = 0.0

    # ------------------------------------------------------------------ #
    # 10. Update P_k in-place, return max |correction|                   #
    # ------------------------------------------------------------------ #
    max_err = 0.0
    for j in range(jbot, jtop + 1):
        P_k[j, i_col] += RHS[j]
        a = abs(RHS[j])
        if a > max_err:
            max_err = a

    return max_err


# ---------------------------------------------------------------------------
# Spanwise Laplacian — full residual, with Dirichlet far-field at k=nk-1
# ---------------------------------------------------------------------------

@njit(cache=True)
def compute_phi_zz_rhs_station(P_old, k, inv_dz2, nk, jbot, jtop):
    """
    Full (lagged) phi_zz residual at station k.

    Boundary treatment:
      k=0      : symmetric root (Neumann ghost P[-1] = P[1])
                 phi_zz[0] = 2*(P[1] - P[0]) / dz²
      0<k<nk-1 : central difference
                 phi_zz[k] = (P[k-1] - 2P[k] + P[k+1]) / dz²
      k=nk-1   : far-field Dirichlet (P[nk] = 0 phantom)
                 phi_zz[nk-1] = (P[nk-2] - 2P[nk-1]) / dz²

    The FULL residual (including the -2P[k]/dz² diagonal term) is returned so
    that RHS -= phi_zz_rhs gives a fixed point exactly where phi_zz = 0 at the
    root/symmetric case, and at the correct 3D solution for the wingtip case.
    No convergence floor exists because phi_zz_full → 0 at the fixed point.
    """
    nj = P_old.shape[1]
    ni = P_old.shape[2]
    result = np.zeros((nj, ni), dtype=np.float64)

    if nk == 1:
        return result

    for j in range(jbot, jtop + 1):
        for i in range(ni):
            if k == 0:
                # Neumann ghost P[-1] = P[1] → phi_zz[0] = 2*(P[1]-P[0])/dz²
                result[j, i] = 2.0 * inv_dz2 * (P_old[1, j, i] - P_old[0, j, i])
            elif k == nk - 1:
                # Dirichlet P[nk] = 0 → phi_zz[nk-1] = (P[nk-2] - 2*P[nk-1] + 0)/dz²
                result[j, i] = inv_dz2 * (P_old[nk - 2, j, i]
                                           - 2.0 * P_old[nk - 1, j, i])
            else:
                result[j, i] = inv_dz2 * (P_old[k - 1, j, i]
                                           - 2.0 * P_old[k, j, i]
                                           + P_old[k + 1, j, i])

    return result
