"""
3D TSD Numba kernels — Stage 1 (rectangular wing, no sweep/taper/dihedral).

Two kernels:
  thomas_column_sweep_3d  — 2D Thomas sweep + explicit phi_ηη source term
  compute_phi_zz_station  — spanwise Laplacian phi_ηη[nj, ni] at station k

Index convention (0-based throughout):
  i  = chordwise   (0 … ni-1)
  j  = normal/wall (0 … nj-1)
  k  = spanwise    (0 … nk-1)
  P has shape (nk, nj, ni); a single-station slice P[k] is (nj, ni).
"""

import numpy as np
from numba import njit


# ---------------------------------------------------------------------------
# 3-D Thomas column sweep
# ---------------------------------------------------------------------------

@njit(cache=True)
def thomas_column_sweep_3d(
    i_col,
    P_k,            # float64[nj, ni], k-th spanwise slice, updated in-place
    P_prev_saved,   # float64[nj], snapshot of P_k[:, i_col-1] before it was updated
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
    # --- CFS params (not used in stage 1, kept for API compatibility) ---
    eps_ampl, cfs_triggered, ite_lo, ite_hi, sonvel, beta_sonic,
    # --- phi_ηη explicit contribution (NEW) ---
    phi_zz_contrib,  # float64[nj], lagged spanwise Laplacian at this column
    # --- wall BC flag (False for k > k_tip) ---
    apply_wall_bc,   # bool
):
    """
    Single-column Murman-Cole SLOR sweep for the 3-D TSD equation.

    Identical to the 2-D sweep (stage_0/solver2d_py.py) with two changes:
      1. phi_zz_contrib[j] is subtracted from RHS (explicit spanwise coupling).
      2. apply_wall_bc selects whether wall/Kutta BCs fire (False outside wing span).

    Returns max |delta_P| for convergence tracking.
    """
    nj = P_k.shape[0]

    dx   = X[i_col] - X[i_col - 1]
    epsx = eps / (dx * dx)
    if cfs_triggered and ite_lo <= i_col <= ite_hi:
        epsx *= eps_ampl

    # ------------------------------------------------------------------ #
    # 1. VC = AK/dx − (γ+1)/2 · φ_x   (type-dependent coefficient)      #
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
    # 2. Assemble tridiagonal coefficients                                #
    # ------------------------------------------------------------------ #
    DIAG = np.empty(nj, dtype=np.float64)
    SUP  = np.empty(nj, dtype=np.float64)
    SUB  = np.empty(nj, dtype=np.float64)
    RHS  = np.empty(nj, dtype=np.float64)

    for j in range(jbot, jtop + 1):
        DIAG[j] = ((emu_cur[j] - VC[j]) * CXXC[i_col] * wi
                   + EMU_prev[j] * CXXR[i_col - 1]
                   - CYYC[j])
        SUP[j] = CYYD[j]
        SUB[j] = CYYU[j]

    # ------------------------------------------------------------------ #
    # 3. RHS: −residual(φ_xx)                                            #
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
    # 4. RHS: −residual(φ_yy)                                            #
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
    # 4b. RHS: −residual(φ_ηη) — explicit spanwise coupling (3-D only)  #
    # ------------------------------------------------------------------ #
    for j in range(jbot, jtop + 1):
        RHS[j] -= phi_zz_contrib[j]

    # ------------------------------------------------------------------ #
    # 5. Boundary conditions                                              #
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
    # 8. CFS sonic-penalty (not active in stage 1 — kept for completeness)
    # ------------------------------------------------------------------ #
    if cfs_triggered and ite_lo <= i_col <= ite_hi:
        alpha_sonic = eps * beta_sonic / (dx * dx)
        for j in range(jbot, jtop + 1):
            p_sonic   = P_k[j, i_col - 1] + sonvel * dx
            DIAG[j]  += alpha_sonic
            RHS[j]   -= alpha_sonic * (P_k[j, i_col] - p_sonic)

    # ------------------------------------------------------------------ #
    # 9. Thomas algorithm                                                 #
    # ------------------------------------------------------------------ #
    SAVE = np.empty(nj, dtype=np.float64)
    dnom        = 1.0 / DIAG[jbot]
    SAVE[jbot]  = SUB[jbot] * dnom
    RHS[jbot]   = RHS[jbot] * dnom

    for j in range(jbot + 1, jtop + 1):
        denom  = DIAG[j] - SUP[j] * SAVE[j - 1]
        dnom   = 1.0 / denom
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
# Spanwise Laplacian:  phi_ηη at station k  (uses P from previous iteration)
# ---------------------------------------------------------------------------

@njit(cache=True)
def compute_phi_zz_station_gs(
    P,       # float64[nk, nj, ni], current field (P[k-1] already updated)
    P_old,   # float64[nk, nj, ni], field from start of iteration (fully lagged)
    k,
    eta,
    nk, nj, ni,
    jbot, jtop,
):
    """
    Gauss-Seidel phi_zz: uses P[k-1] (updated this sweep) and P_old[k+1] (lagged).

    For k=0: both neighbours from P_old (k+1 not yet swept; k-1 is symmetry ghost).
    For k>0: lower neighbour P[k-1] is the post-SYOR value from this sweep;
             upper neighbour and centre from P_old.  This forward-GS stencil
             creates negative feedback (stabilising) for the elliptic operator.
    """
    result = np.zeros((nj, ni), dtype=np.float64)

    if k == 0:
        h_u = eta[1] - eta[0]
        coeff = 2.0 / (h_u * h_u)
        for j in range(jbot, jtop + 1):
            for i in range(ni):
                # k=1 not yet updated → use P_old[1]
                result[j, i] = coeff * (P_old[1, j, i] - P_old[0, j, i])

    elif k == nk - 1:
        h_d = eta[k] - eta[k - 1]
        coeff = 1.0 / (h_d * h_d)
        for j in range(jbot, jtop + 1):
            for i in range(ni):
                result[j, i] = coeff * (P[k - 1, j, i] - 2.0 * P_old[k, j, i])
                # P[k+1] = 0 Dirichlet
    else:
        h_d = eta[k] - eta[k - 1]
        h_u = eta[k + 1] - eta[k]
        cd  = 2.0 / (h_d * (h_d + h_u))
        cc  = 2.0 / (h_d * h_u)
        cu  = 2.0 / (h_u * (h_d + h_u))
        for j in range(jbot, jtop + 1):
            for i in range(ni):
                result[j, i] = (cd * P[k - 1, j, i]
                                - cc * P_old[k,     j, i]
                                + cu * P_old[k + 1, j, i])
    return result


@njit(cache=True)
def compute_phi_zz_station(
    P_old,   # float64[nk, nj, ni], full 3-D field from previous iteration
    k,       # int, current spanwise station
    eta,     # float64[nk], spanwise coordinates
    nk, nj, ni,
    jbot, jtop,
):
    """
    Central finite-difference approximation of φ_ηη at spanwise station k.

    Boundary treatment:
      k = 0      — symmetry plane: ghost cell P[k=-1] = P[k=+1]
                   → one-sided: phi_zz ≈ 2*(P[1] - P[0]) / h_u²
      0 < k < nk−1 — interior: standard non-uniform central difference
      k = nk−1  — spanwise far-field Dirichlet φ = 0
                   → P[k+1] = 0 phantom
    Returns phi_zz[nj, ni] for station k (full slice).
    """
    result = np.zeros((nj, ni), dtype=np.float64)

    if k == 0:
        h_u = eta[1] - eta[0]
        coeff = 2.0 / (h_u * h_u)
        for j in range(jbot, jtop + 1):
            for i in range(ni):
                result[j, i] = coeff * (P_old[1, j, i] - P_old[0, j, i])

    elif k == nk - 1:
        h_d = eta[k] - eta[k - 1]
        coeff = 1.0 / (h_d * h_d)
        for j in range(jbot, jtop + 1):
            for i in range(ni):
                # P[k+1] = 0  (Dirichlet far field)
                result[j, i] = coeff * (P_old[k - 1, j, i]
                                        - 2.0 * P_old[k, j, i])
    else:
        h_d = eta[k] - eta[k - 1]
        h_u = eta[k + 1] - eta[k]
        cd  = 2.0 / (h_d * (h_d + h_u))
        cc  = 2.0 / (h_d * h_u)
        cu  = 2.0 / (h_u * (h_d + h_u))
        for j in range(jbot, jtop + 1):
            for i in range(ni):
                result[j, i] = (cd * P_old[k - 1, j, i]
                                - cc * P_old[k, j, i]
                                + cu * P_old[k + 1, j, i])
    return result
