"""
3D TSD Numba kernels — Stage 1 (rectangular wing, both ends symmetric).

Key improvement over stage_1_1_fail: phi_zz is treated semi-implicitly.
The self-coupling term (-cc * P[k]) is moved to DIAG (left-hand side),
eliminating the O(1/dz²) convergence floor that made explicit phi_zz
unable to reach CVERGE.

Index convention (0-based throughout):
  i  = chordwise   (0 … ni-1)
  j  = normal/wall (0 … nj-1)
  k  = spanwise    (0 … nk-1)
  P has shape (nk, nj, ni).

Spanwise BC: both k=0 and k=nk-1 are symmetric planes (phi_z=0).
  Ghost: P[-1] = P[1],  P[nk] = P[nk-2].
"""

import numpy as np
from numba import njit


# ---------------------------------------------------------------------------
# 3-D Thomas column sweep  (semi-implicit phi_zz)
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
    # --- phi_zz semi-implicit treatment ---
    phi_zz_diag,   # float64 scalar: cc = 2/dz², added to DIAG (Jacobian preconditioning)
    phi_zz_rhs,    # float64[nj]: FULL phi_zz at this column (lagged from P_old)
):
    """
    Single-column Murman-Cole SLOR sweep with semi-implicit spanwise coupling.

    phi_zz_rhs[j] = phi_zz evaluated at P_old (lagged Jacobi):
      = cd*P_old[k-1,j,i] - cc*P_old[k,j,i] + cu*P_old[k+1,j,i]

    This FULL residual is subtracted from RHS.  Simultaneously, phi_zz_diag=cc
    is added to DIAG as a Jacobian preconditioner (the derivative of phi_zz
    w.r.t. P[k,j,i]), making the diagonal more dominant → unconditional stability
    and no oscillation floor.  The fixed point is exactly where phi_zz=0.
    """
    nj = P_k.shape[0]

    dx   = X[i_col] - X[i_col - 1]
    epsx = eps / (dx * dx)
    if cfs_triggered and ite_lo <= i_col <= ite_hi:
        epsx *= eps_ampl

    # ------------------------------------------------------------------ #
    # 1.  VC = AK/dx − (γ+1)/2 · φ_x   (type-dependent coefficient)     #
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
    # 2.  Assemble tridiagonal: DIAG includes implicit phi_zz_diag        #
    # ------------------------------------------------------------------ #
    DIAG = np.empty(nj, dtype=np.float64)
    SUP  = np.empty(nj, dtype=np.float64)
    SUB  = np.empty(nj, dtype=np.float64)
    RHS  = np.empty(nj, dtype=np.float64)

    for j in range(jbot, jtop + 1):
        DIAG[j] = ((emu_cur[j] - VC[j]) * CXXC[i_col] * wi
                   + EMU_prev[j] * CXXR[i_col - 1]
                   - CYYC[j]
                   + phi_zz_diag)  # implicit phi_zz: -cc*P[k] → +cc to DIAG
        SUP[j] = CYYD[j]
        SUB[j] = CYYU[j]

    # ------------------------------------------------------------------ #
    # 3.  RHS: −residual(φ_xx)                                            #
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
    # 4.  RHS: −residual(φ_yy)                                            #
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
    # 4b. RHS: explicit neighbor contribution of phi_zz                   #
    #     phi_zz_rhs[j] = cd*P[k-1][j,i] + cu*P[k+1][j,i]  (lagged)    #
    # ------------------------------------------------------------------ #
    for j in range(jbot, jtop + 1):
        RHS[j] -= phi_zz_rhs[j]

    # ------------------------------------------------------------------ #
    # 5.  Boundary conditions (wall + Kutta)                              #
    # ------------------------------------------------------------------ #
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
    # 6.  BCEND: supersonic freestream far-field BC  (only if ak <= 0)   #
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
    # 7.  Artificial dissipation                                          #
    # ------------------------------------------------------------------ #
    for j in range(jbot, jtop + 1):
        DIAG[j] -= epsx
        RHS[j]  -= epsx * (P_k[j, i_col - 1] - P_prev_saved[j])

    # ------------------------------------------------------------------ #
    # 8.  CFS sonic penalty                                               #
    # ------------------------------------------------------------------ #
    if cfs_triggered and ite_lo <= i_col <= ite_hi:
        alpha_sonic = eps * beta_sonic / (dx * dx)
        for j in range(jbot, jtop + 1):
            p_sonic  = P_k[j, i_col - 1] + sonvel * dx
            DIAG[j] += alpha_sonic
            RHS[j]  -= alpha_sonic * (P_k[j, i_col] - p_sonic)

    # ------------------------------------------------------------------ #
    # 9.  Thomas algorithm                                                #
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
# Spanwise Laplacian: explicit neighbor contribution for implicit phi_zz
# ---------------------------------------------------------------------------

@njit(cache=True)
def compute_phi_zz_rhs_station(P_old, k, inv_dz2, nk, jbot, jtop):
    """
    Compute the full (lagged) phi_zz residual at station k.

    phi_zz[k] = cd*P[k-1] - cc*P[k] + cu*P[k+1]   (uniform grid, dz)

    This returns the FULL phi_zz (including the -cc*P[k] diagonal term), so
    that when it is subtracted from RHS, the solver's fixed point is exactly
    where phi_zz = 0 (i.e., the z-uniform 2-D solution for a rectangular wing).

    DIAG separately receives +cc (phi_zz_diag) as the Jacobian correction that
    makes the tridiagonal diagonally dominant — this improves stability without
    changing the fixed point.

    Ghost cells (symmetric BCs at both ends):
      P[-1]  = P[1]      (symmetry at k=0)
      P[nk]  = P[nk-2]  (symmetry at k=nk-1)

    Returns float64[nj, ni] for station k (full slice), or zeros if nk==1.
    """
    nj = P_old.shape[1]
    ni = P_old.shape[2]
    result = np.zeros((nj, ni), dtype=np.float64)

    if nk == 1:
        return result  # single station: no spanwise coupling

    for j in range(jbot, jtop + 1):
        for i in range(ni):
            if k == 0:
                # ghost P[-1] = P[1]:
                # phi_zz[0] = (P[1] - 2P[0] + P[1]) / dz² = 2(P[1]-P[0])/dz²
                result[j, i] = 2.0 * inv_dz2 * (P_old[1, j, i] - P_old[0, j, i])
            elif k == nk - 1:
                # ghost P[nk] = P[nk-2]:
                # phi_zz[nk-1] = 2*(P[nk-2]-P[nk-1])/dz²
                result[j, i] = 2.0 * inv_dz2 * (P_old[nk - 2, j, i] - P_old[nk - 1, j, i])
            else:
                # standard central difference
                result[j, i] = inv_dz2 * (P_old[k - 1, j, i]
                                           - 2.0 * P_old[k, j, i]
                                           + P_old[k + 1, j, i])

    return result
