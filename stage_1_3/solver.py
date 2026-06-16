"""
3-D TSD SLOR solver — Stage 1-3 (rectangular wing, single symmetric plane + wingtip).

Geometry:
  - k=0        : symmetric root plane  (phi_z=0, ghost P[-1] = P[1])
  - k=1..k_tip : wing stations         (airfoil wall + Kutta BCs applied)
  - k_tip+1..nk-2 : off-wing stations  (no airfoil; plain 2-D Laplace + z-coupling)
  - k=nk-1     : spanwise far-field    (phi=0 Dirichlet, reset P[nk-1]=0 each iter)

phi_zz treatment: explicit full residual (phi_zz_diag = 0).
  - Full phi_zz in RHS: phi_zz_rhs = (P[k-1] - 2P[k] + P[k+1]) / dz²
  - The -2P[k]/dz² self-coupling term IS included (unlike stage_1_1_fail which only
    had the neighbor terms).  This eliminates the convergence floor: at the fixed
    point P* the SYOR delta_P → 0 because the full phi_zz residual cancels the
    2-D residual exactly.
  - phi_zz_diag = 0: DIAG unchanged from the 2-D operator.

Equation (non-conservative, no sweep/taper/dihedral, xi_z=0):
  [1 - M∞² - (γ+1)M∞² φ_x] φ_xx  +  φ_yy  +  φ_zz  =  0
"""

import numpy as np
from .numba_kernels import thomas_column_sweep_3d, compute_phi_zz_rhs_station


# ---------------------------------------------------------------------------
# Spanwise grid
# ---------------------------------------------------------------------------

def build_spanwise_grid(half_span, nk, z_max_factor=3.0):
    """
    Uniform grid from z=0 to z_max = z_max_factor * half_span with nk stations.

    Returns
    -------
    eta   : float64[nk], spanwise coordinates
    k_tip : int, index of the station nearest to half_span
    dz    : float, uniform spacing
    """
    z_max = z_max_factor * half_span
    eta   = np.linspace(0.0, z_max, nk)
    dz    = eta[1] - eta[0] if nk > 1 else float(z_max)
    k_tip = int(np.argmin(np.abs(eta - half_span)))
    return eta, k_tip, dz


# ---------------------------------------------------------------------------
# Main 3-D SLOR solver
# ---------------------------------------------------------------------------

def solve_3d(
    P,                       # float64[nk, nj, ni], initial potential (in-place)
    # --- chordwise + normal FD coefficients (same for all k) ---
    C1, CXL, CXC, CXR,
    CXXL, CXXC, CXXR,
    CYYC, CYYD, CYYU,
    CYYBUC, CYYBUU, CYYBLC, CYYBLD,
    # --- wing-surface BC (applied for k=0..k_tip only) ---
    FXUBC, FXLBC,           # float64[ni]
    PJUMP_3d,               # float64[nk, ni]
    THETA,                  # float64[nj, ni]
    # --- far-field BC arrays (x/y, same for all k) ---
    VUP, VDOWN, VTOP, VBOT,
    DUP, DDOWN, DTOP, DBOT,
    # --- mesh ---
    X, XDIFF,
    dz,                     # float, uniform spanwise spacing
    # --- flow parameters ---
    AK, EPS, WI, SONVEL, VOL,
    # --- far-field state (per-station, mutated in-place) ---
    CIRCFF_3d,              # float64[nk]
    DUB_3d,                 # float64[nk]
    # --- Kutta extrapolation coefficients ---
    CJUP, CJUP1, CJLOW, CJLOW1,
    # --- index bounds (0-based) ---
    imax, jmax,
    iup, idown, ile, ite,
    jup, jlow, jbot, jtop,
    k_tip,                  # int, last wing station index
    # --- solver control ---
    MAXIT=8000,
    CVERGE=1e-5,
    DVERGE=10.0,
    IPRTER=500,
    WCIRC=1.0,
    NDUB=25,
    FLAG_CFS=False,
    BETA_SONIC=100.0,
    EPS_AMPL=200.0,
    ITER_START_CFS=100,
    use_phi_zz=True,
    iter_ramp_phizz=0,   # >0: ramp phi_zz scale from 0→1 over this many iters
    print_info=True,
):
    """
    3-D SLOR solver for rectangular wing with single symmetric plane + wingtip.
    Updates P in-place.  Returns (converged, n_iter, error_history).
    """
    nk, nj, ni = P.shape
    jmin = 0
    rtk  = np.sqrt(abs(AK)) if AK <= 0.0 else 0.0

    if use_phi_zz and nk > 1:
        inv_dz2     = 1.0 / (dz * dz)
        phi_zz_diag = 0.0   # explicit: full phi_zz in RHS, no diagonal preconditioning
    else:
        inv_dz2     = 0.0
        phi_zz_diag = 0.0

    zero_nj   = np.zeros(nj, dtype=np.float64)
    zero_bc   = np.zeros(ni, dtype=np.float64)   # zero FXUBC/FXLBC for off-wing

    # Initialise CIRCTE from the initial P field (avoid large dcirc at iter 1
    # when warm-starting; cold start from P=0 gives CIRCTE=0 anyway).
    CIRCTE_3d = np.zeros(nk)
    for k in range(k_tip + 1):
        pup  = CJUP  * P[k, jup,  ite] - CJUP1  * P[k, jup  + 1, ite]
        plow = CJLOW * P[k, jlow, ite] - CJLOW1 * P[k, jlow - 1, ite]
        CIRCTE_3d[k] = pup - plow

    im2_init = iup - 1
    if AK < 0.0:
        im2_init = iup - 2

    cfs_triggered = False
    ite_lo = ite - 5
    ite_hi = ite + 5

    error_history = []
    converged     = False

    for iteration in range(1, MAXIT + 1):

        # ---------------------------------------------------------------- #
        # Step 1: save lagged field for phi_zz                             #
        # ---------------------------------------------------------------- #
        P_old = P.copy()

        # ---------------------------------------------------------------- #
        # Step 2: RECIRC — Kutta / PJUMP for wing stations; zero for rest  #
        # ---------------------------------------------------------------- #
        dcirc     = np.zeros(nk)
        wcirc_eff = 0.02 if cfs_triggered else WCIRC

        for k in range(nk):
            if k <= k_tip:
                pup  = (CJUP  * P[k, jup,  ite]
                        - CJUP1  * P[k, jup  + 1, ite])
                plow = (CJLOW * P[k, jlow, ite]
                        - CJLOW1 * P[k, jlow - 1, ite])
                cteold        = CIRCTE_3d[k]
                CIRCTE_3d[k]  = pup - plow
                CIRCFF_3d[k]  = ((1.0 - wcirc_eff) * CIRCFF_3d[k]
                                  + CIRCTE_3d[k] * wcirc_eff)
                dcirc[k]      = CIRCTE_3d[k] - cteold

                if imax > ite:
                    factor = (CIRCFF_3d[k] - CIRCTE_3d[k]) / (X[imax] - 1.0)
                    for iw in range(ite, imax + 1):
                        PJUMP_3d[k, iw] = CIRCTE_3d[k] + (X[iw] - 1.0) * factor
            else:
                # Off-wing: zero circulation, zero PJUMP
                dcirc[k]       = -CIRCTE_3d[k]
                CIRCTE_3d[k]   = 0.0
                CIRCFF_3d[k]   = 0.0
                PJUMP_3d[k, :] = 0.0

        # ---------------------------------------------------------------- #
        # Step 3: SYOR-3D — column sweep for each spanwise station          #
        # ---------------------------------------------------------------- #
        error = 0.0

        for k in range(nk):
            is_wing = (k <= k_tip)

            if use_phi_zz and nk > 1:
                phi_zz_rhs_k = compute_phi_zz_rhs_station(
                    P_old, k, inv_dz2, nk, jbot, jtop)
                if iter_ramp_phizz > 0:
                    scale = min(float(iteration) / iter_ramp_phizz, 1.0)
                    phi_zz_rhs_k = phi_zz_rhs_k * scale
            else:
                phi_zz_rhs_k = None

            EMU_prev     = np.zeros(nj, dtype=np.float64)
            emu_cur      = np.zeros(nj, dtype=np.float64)

            if AK <= 0.0:
                EMU_prev[:] = C1[1]

            P_prev_saved = P[k, :, iup - 1].copy()
            im2          = im2_init

            fxu = FXUBC if is_wing else zero_bc
            fxl = FXLBC if is_wing else zero_bc

            for i in range(iup, idown + 1):
                P_cur_before = P[k, :, i].copy()

                rhs_col = (phi_zz_rhs_k[:, i]
                           if phi_zz_rhs_k is not None
                           else zero_nj)

                col_err = thomas_column_sweep_3d(
                    i, P[k], P_prev_saved, EMU_prev, emu_cur,
                    C1, CXL, CXC, CXR,
                    CXXL, CXXC, CXXR,
                    CYYC, CYYD, CYYU,
                    CYYBUC, CYYBUU, CYYBLC, CYYBLD,
                    fxu, fxl, PJUMP_3d[k],
                    X, XDIFF,
                    EPS, WI, AK, rtk,
                    ile, ite, jup, jlow, jbot, jtop, jmin, jmax, im2,
                    EPS_AMPL, cfs_triggered, ite_lo, ite_hi, SONVEL, BETA_SONIC,
                    phi_zz_diag,
                    rhs_col,
                    is_wing,
                )
                error = max(error, col_err)

                P_prev_saved = P_cur_before
                EMU_prev[:]  = emu_cur[:]
                im2          = i - 1

                if AK <= 0.0 and i == idown - 1:
                    P[k, :, idown + 1] = P[k, :, idown - 1]

        # ---------------------------------------------------------------- #
        # Step 4: theta correction AFTER SYOR (subsonic, 2-D only)        #
        # In 3-D mode (use_phi_zz) the explicit phi_zz drives CIRCTE at   #
        # the wingtip and theta would amplify that change, causing         #
        # divergence.  Skip theta entirely in 3-D; far-field is propagated #
        # through RECIRC → CIRCFF → RESET each iteration instead.         #
        # ---------------------------------------------------------------- #
        if AK >= 0.0 and not (use_phi_zz and nk > 1):
            for k in range(nk):
                if abs(dcirc[k]) > 1e-14:
                    P[k, jbot:jtop + 1, iup:idown + 1] += (
                        dcirc[k] * THETA[jbot:jtop + 1, iup:idown + 1])

        # ---------------------------------------------------------------- #
        # Step 5: CFS detection (mid-wing station k = k_tip // 2)         #
        # ---------------------------------------------------------------- #
        k_cfs = k_tip // 2
        if FLAG_CFS and not cfs_triggered and iteration >= ITER_START_CFS:
            for i in range(iup, idown + 1):
                if abs(X[i] - 1.0) <= 0.05:
                    u_local = (P[k_cfs, jup, i] - P[k_cfs, jup, i - 1]) * XDIFF[i]
                    if u_local > SONVEL:
                        cfs_triggered = True
                        break
        elif FLAG_CFS and cfs_triggered:
            still_super = False
            for i in range(iup, idown + 1):
                if abs(X[i] - 1.0) <= 0.05:
                    u_local = (P[k_cfs, jup, i] - P[k_cfs, jup, i - 1]) * XDIFF[i]
                    if u_local > SONVEL:
                        still_super = True
                        break
            if not still_super:
                cfs_triggered = False

        # ---------------------------------------------------------------- #
        # Step 6: REDUB                                                     #
        # ---------------------------------------------------------------- #
        if iteration % NDUB == 0:
            for k in range(k_tip + 1):
                if abs(CIRCFF_3d[k]) >= 1e-4:
                    DUB_3d[k] = VOL

        # ---------------------------------------------------------------- #
        # Step 7: RESET x/y far-field Dirichlet BCs per station            #
        # ---------------------------------------------------------------- #
        for k in range(nk):
            circ = CIRCFF_3d[k]   # = 0 for k > k_tip
            dub  = DUB_3d[k]      # = 0 for k > k_tip
            for j in range(jmax + 1):
                P[k, j, 0]    = circ * VUP[j]   + dub * DUP[j]
                P[k, j, imax] = circ * VDOWN[j] + dub * DDOWN[j]
            for i in range(imax + 1):
                P[k, 0,    i] = circ * VBOT[i] + dub * DBOT[i]
                P[k, jmax, i] = circ * VTOP[i] + dub * DTOP[i]

        # Spanwise far-field Dirichlet: enforce phi = 0 at k = nk-1
        P[nk - 1, :, :] = 0.0

        # ---------------------------------------------------------------- #
        # Convergence / divergence                                          #
        # ---------------------------------------------------------------- #
        error_history.append(error)

        if print_info and (iteration == 1 or iteration % IPRTER == 0):
            k_mid = k_tip // 2
            print(f"  iter={iteration:6d}  error={error:.4e}"
                  f"  CIRCFF[k={k_mid}]={CIRCFF_3d[k_mid]:.5f}")

        if error <= CVERGE:
            converged = True
            if print_info:
                print(f"  Converged after {iteration} iterations"
                      f"  (error={error:.4e})")
            break
        if error >= DVERGE:
            if print_info:
                print(f"  Diverged at iter {iteration}  (error={error:.4e})")
            break

    return converged, iteration, np.array(error_history)


# ---------------------------------------------------------------------------
# Helper: assemble 3-D state from 2-D extract_arrays dict
# ---------------------------------------------------------------------------

def build_3d_state(d, nk, half_span, z_max_factor=3.0):
    """
    Given the 2-D coefficient dict from stage_0/solver2d_py.extract_arrays(),
    assemble the 3-D solver inputs for a rectangular wing with wingtip.

    Parameters
    ----------
    d            : dict from extract_arrays()
    nk           : int, number of spanwise stations
    half_span    : float, physical half-span (wing extends from z=0 to half_span)
    z_max_factor : float, computational z_max = z_max_factor * half_span
    """
    ni = d['imax'] + 1
    nj = d['jmax'] + 1

    eta, k_tip, dz = build_spanwise_grid(half_span, nk, z_max_factor)

    P         = np.zeros((nk, nj, ni), dtype=np.float64)
    PJUMP_3d  = np.zeros((nk, ni),     dtype=np.float64)
    CIRCFF_3d = np.zeros(nk,           dtype=np.float64)
    DUB_3d    = np.zeros(nk,           dtype=np.float64)

    out = {
        'P'         : P,
        'PJUMP_3d'  : PJUMP_3d,
        'CIRCFF_3d' : CIRCFF_3d,
        'DUB_3d'    : DUB_3d,
        'eta'       : eta,
        'dz'        : dz,
        'nk'        : nk,
        'k_tip'     : k_tip,
        'half_span' : half_span,
    }
    for key in (
        'C1', 'CXL', 'CXC', 'CXR',
        'CXXL', 'CXXC', 'CXXR',
        'CYYC', 'CYYD', 'CYYU',
        'CYYBUC', 'CYYBUU', 'CYYBLC', 'CYYBLD',
        'FXUBC', 'FXLBC', 'THETA',
        'VUP', 'VDOWN', 'VTOP', 'VBOT',
        'DUP', 'DDOWN', 'DTOP', 'DBOT',
        'X', 'XDIFF',
        'AK', 'EPS', 'WI', 'SONVEL', 'VOL',
        'CJUP', 'CJUP1', 'CJLOW', 'CJLOW1',
        'imax', 'jmax',
        'iup', 'idown', 'ile', 'ite',
        'jup', 'jlow', 'jbot', 'jtop',
        'MAXIT', 'CVERGE', 'DVERGE', 'IPRTER', 'WCIRC',
        'FLAG_CFS', 'BETA_SONIC', 'EPS_AMPL', 'ITER_START_CFS',
    ):
        out[key] = d[key]

    return out
