"""
3-D TSD SLOR solver — Stage 1 (rectangular wing, no sweep/taper/dihedral).

Equation (non-conservative, physical coordinates):
  [1 − M∞² − (γ+1)M∞² φ_x] φ_xx  +  φ_yy  +  φ_zz  =  0

Algorithm (per iteration):
  1. Save P_old (for phi_zz and convergence)
  2. RECIRC: update Kutta / PJUMP from P_old; store dcirc[k]
  3. Compute phi_zz_k (lagged, from P_old) for each station
  4. SYOR-3D: k-loop → column sweep per station (phi_zz added to RHS)
  5. Theta correction: P += dcirc[k] * THETA  (after SYOR, subsonic only)
  6. REDUB
  7. RESET far-field Dirichlet BCs + spanwise far-field P[nk-1]=0
  8. Convergence check

Note on ordering: steps 2-3-4-5 must be in this sequence. In particular,
phi_zz must use P_old (before the column sweep), and the theta circulation
correction must come AFTER the sweep — matching the 2-D SOLVE() flow.
"""

import numpy as np
from .numba_kernels import (thomas_column_sweep_3d,
                            compute_phi_zz_station,
                            compute_phi_zz_station_gs)


# ---------------------------------------------------------------------------
# Spanwise grid helper
# ---------------------------------------------------------------------------

def build_spanwise_grid(half_span, nk, z_max_factor=3.0):
    """
    Uniform spanwise grid from z=0 to z_max = z_max_factor * half_span.

    Returns
    -------
    eta   : float64[nk]
    k_tip : int, index closest to half_span
    """
    z_max = z_max_factor * half_span
    eta   = np.linspace(0.0, z_max, nk)
    k_tip = int(np.argmin(np.abs(eta - half_span)))
    return eta, k_tip


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
    # --- wing-surface BC (same for all k ≤ k_tip) ---
    FXUBC, FXLBC,           # float64[ni]
    PJUMP_3d,               # float64[nk, ni], Kutta wake jump per station
    THETA,                  # float64[nj, ni], circulation correction field
    # --- far-field BC arrays (same for all k) ---
    VUP, VDOWN, VTOP, VBOT,
    DUP, DDOWN, DTOP, DBOT,
    # --- mesh ---
    X, XDIFF,
    eta,                    # float64[nk], spanwise coordinates
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
    k_tip,                  # last spanwise index inside the wing (controls wall BCs)
    # --- solver control ---
    MAXIT=1000,
    CVERGE=1e-5,
    DVERGE=10.0,
    IPRTER=100,
    WCIRC=1.0,
    NDUB=25,
    FLAG_CFS=False,
    BETA_SONIC=100.0,
    EPS_AMPL=200.0,
    ITER_START_CFS=100,
    use_phi_zz=True,        # set False to force strip-theory (phi_zz = 0)
    phi_zz_relax=1.0,       # under-relaxation factor for phi_zz (0–1); <1 for stability
    k_tip_phizz=None,       # last k that receives phi_zz (None → same as k_tip)
    z_implicit=False,       # reserved (not yet implemented)
    phi_zz_update_freq=1,   # freeze phi_zz for N iterations (outer-Picard stability).
                            # 1 = update every iteration (Gauss-Seidel, old behaviour);
                            # >1 = frozen phi_zz (pure Jacobi) updated every N iters.
                            # Larger values allow the 2-D SLOR to converge against a
                            # fixed source, eliminating the O(1/dz²) oscillation floor.
                            # Convergence is checked at every update point and requires
                            # both max|ΔP| < CVERGE and max|Δphi_zz| < CVERGE.
    print_info=True,
):
    """
    3-D SLOR solver.  Updates P[nk, nj, ni] in-place.
    Returns (converged, n_iter, error_history).
    """
    nk, nj, ni = P.shape
    jmin  = 0
    rtk   = np.sqrt(abs(AK)) if AK <= 0.0 else 0.0

    # Frozen phi_zz storage for phi_zz_update_freq > 1 (outer-Picard approach).
    # phi_zz_change tracks max|Δphi_zz| at each update point; it enters the
    # convergence criterion together with the SLOR residual.
    phi_zz_frozen  = None
    phi_zz_change  = 0.0
    if use_phi_zz and phi_zz_update_freq > 1:
        phi_zz_frozen = np.zeros((nk, nj, ni), dtype=np.float64)

    # Initialize CIRCTE from the initial P field so that dcirc ≈ 0 at iter 1.
    # This is critical for warm starts: without this, the theta correction at
    # iter 1 sees dcirc = CIRCTE − 0 (huge), which amplifies P spuriously.
    CIRCTE_3d = np.zeros(nk)
    for k in range(min(k_tip + 1, nk)):
        pup  = (CJUP  * P[k, jup,  ite]
                - CJUP1  * P[k, jup  + 1, ite])
        plow = (CJLOW * P[k, jlow, ite]
                - CJLOW1 * P[k, jlow - 1, ite])
        CIRCTE_3d[k] = pup - plow

    im2_init = iup - 1
    if AK < 0.0:
        im2_init = iup - 2

    cfs_triggered = False
    ite_lo = ite - 5
    ite_hi = ite + 5

    error_history = []
    converged     = False
    ZERO_BC       = np.zeros(ni, dtype=np.float64)

    for iteration in range(1, MAXIT + 1):

        # ---------------------------------------------------------------- #
        # Step 1: save lagged field for phi_zz                             #
        # ---------------------------------------------------------------- #
        P_old = P.copy()

        # ---------------------------------------------------------------- #
        # Step 2: RECIRC — update Kutta / PJUMP, store dcirc[k]           #
        # (do NOT modify P here; theta correction happens after SYOR)      #
        # ---------------------------------------------------------------- #
        dcirc = np.zeros(nk)
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
                    factor = ((CIRCFF_3d[k] - CIRCTE_3d[k])
                              / (X[imax] - 1.0))
                    for iw in range(ite, imax + 1):
                        PJUMP_3d[k, iw] = (CIRCTE_3d[k]
                                           + (X[iw] - 1.0) * factor)
            else:
                # outside wing: zero circulation
                dcirc[k]       = -CIRCTE_3d[k]
                CIRCTE_3d[k]   = 0.0
                CIRCFF_3d[k]   = 0.0
                PJUMP_3d[k, :] = 0.0

        k_zz_max = k_tip_phizz if k_tip_phizz is not None else k_tip

        # ---------------------------------------------------------------- #
        # Step 3: update / freeze phi_zz                                  #
        #                                                                  #
        # freq=1  (Gauss-Seidel): recompute phi_zz every iteration using  #
        #   the already-swept P[k-1] — negative feedback, stable.         #
        # freq>1  (outer-Picard): phi_zz is frozen for N iterations from  #
        #   the Jacobi stencil of P_old.  At each update point we track   #
        #   max|Δphi_zz| to use as the outer-convergence criterion.       #
        # ---------------------------------------------------------------- #
        if use_phi_zz and phi_zz_frozen is not None:
            if iteration == 1 or iteration % phi_zz_update_freq == 0:
                phi_zz_change = 0.0
                for k in range(k_zz_max + 1):
                    arr = compute_phi_zz_station(
                        P_old, k, eta, nk, nj, ni, jbot, jtop)
                    if phi_zz_relax != 1.0:
                        arr = arr * phi_zz_relax
                    # Zero phi_zz near the trailing edge (columns ite-2 … end).
                    # phi_zz at i=ite directly perturbs P[jup/jlow, ite], which
                    # shifts CIRCTE every iteration and causes runaway drift
                    # through the theta correction.  Excluding this region keeps
                    # the Kutta-condition dynamics identical to the 2-D solver.
                    arr[:, ite - 2:] = 0.0
                    ch = float(np.max(np.abs(arr - phi_zz_frozen[k])))
                    if ch > phi_zz_change:
                        phi_zz_change = ch
                    phi_zz_frozen[k] = arr

        # ---------------------------------------------------------------- #
        # Step 4: SYOR-3D — column sweep for each spanwise station         #
        # ---------------------------------------------------------------- #
        error = 0.0
        zero_slice = np.zeros((nj, ni), dtype=np.float64)

        for k in range(nk):
            is_wing = (k <= k_tip)

            if use_phi_zz and k <= k_zz_max:
                if phi_zz_frozen is not None:
                    phi_zz_k = phi_zz_frozen[k]          # frozen (outer-Picard)
                else:
                    arr = compute_phi_zz_station_gs(      # Gauss-Seidel (freq=1)
                        P, P_old, k, eta, nk, nj, ni, jbot, jtop)
                    if phi_zz_relax != 1.0:
                        arr = arr * phi_zz_relax
                    phi_zz_k = arr
            else:
                phi_zz_k = zero_slice

            EMU_prev = np.zeros(nj, dtype=np.float64)
            emu_cur  = np.zeros(nj, dtype=np.float64)

            if AK <= 0.0:
                EMU_prev[:] = C1[1]

            P_prev_saved = P[k, :, iup - 1].copy()
            im2 = im2_init

            for i in range(iup, idown + 1):
                P_cur_before = P[k, :, i].copy()

                col_err = thomas_column_sweep_3d(
                    i, P[k], P_prev_saved, EMU_prev, emu_cur,
                    C1, CXL, CXC, CXR,
                    CXXL, CXXC, CXXR,
                    CYYC, CYYD, CYYU,
                    CYYBUC, CYYBUU, CYYBLC, CYYBLD,
                    FXUBC if is_wing else ZERO_BC,
                    FXLBC if is_wing else ZERO_BC,
                    PJUMP_3d[k],
                    X, XDIFF,
                    EPS, WI, AK, rtk,
                    ile, ite, jup, jlow, jbot, jtop, jmin, jmax, im2,
                    EPS_AMPL, cfs_triggered, ite_lo, ite_hi, SONVEL, BETA_SONIC,
                    phi_zz_k[:, i],
                    is_wing,
                )
                error = max(error, col_err)

                P_prev_saved = P_cur_before
                EMU_prev[:]  = emu_cur[:]
                im2 = i - 1

                if AK <= 0.0 and i == idown - 1:
                    P[k, :, idown + 1] = P[k, :, idown - 1]

        # ---------------------------------------------------------------- #
        # Step 5: theta correction AFTER SYOR (subsonic only)              #
        # ---------------------------------------------------------------- #
        if AK >= 0.0:
            for k in range(nk):
                if abs(dcirc[k]) > 1e-14:
                    P[k, jbot:jtop + 1, iup:idown + 1] += (
                        dcirc[k]
                        * THETA[jbot:jtop + 1, iup:idown + 1])

        # ---------------------------------------------------------------- #
        # Step 5.5a: CFS detection (check mid-span wing station)           #
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
            for k in range(nk):
                if abs(CIRCFF_3d[k]) >= 1e-4:
                    DUB_3d[k] = VOL

        # ---------------------------------------------------------------- #
        # Step 7: RESET far-field Dirichlet BCs per station                 #
        # ---------------------------------------------------------------- #
        for k in range(nk):
            circ = CIRCFF_3d[k]
            dub  = DUB_3d[k]
            for j in range(jmax + 1):
                P[k, j, 0]    = circ * VUP[j]   + dub * DUP[j]
                P[k, j, imax] = circ * VDOWN[j] + dub * DDOWN[j]
            for i in range(imax + 1):
                P[k, 0,    i] = circ * VBOT[i] + dub * DBOT[i]
                P[k, jmax, i] = circ * VTOP[i] + dub * DTOP[i]

        # Spanwise far-field Dirichlet: φ = 0 at k = nk-1
        P[nk - 1, :, :] = 0.0

        # ---------------------------------------------------------------- #
        # Convergence / divergence check                                   #
        #                                                                  #
        # With frozen phi_zz (outer-Picard), convergence is declared only #
        # at update points and requires both the SLOR residual AND the     #
        # phi_zz change to be below CVERGE.  Between updates we still     #
        # append the SLOR residual to error_history for the convergence   #
        # plot, but do NOT declare convergence (phi_zz may still move).   #
        # ---------------------------------------------------------------- #
        at_update = (phi_zz_frozen is not None and
                     (iteration == 1 or iteration % phi_zz_update_freq == 0))

        if at_update:
            check_error = max(error, phi_zz_change)
        else:
            check_error = error

        error_history.append(check_error)

        if print_info and (iteration == 1 or iteration % IPRTER == 0):
            k_mid  = k_tip // 2
            circ_m = CIRCFF_3d[k_mid]
            if phi_zz_frozen is not None:
                print(f"  iter={iteration:5d}  SLOR={error:.3e}"
                      f"  phi_zz_Δ={phi_zz_change:.3e}"
                      f"  CIRCFF[k={k_mid}]={circ_m:.5f}")
            else:
                print(f"  iter={iteration:5d}  error={error:.4e}"
                      f"  CIRCFF[k={k_mid}]={circ_m:.5f}")

        # Convergence: for frozen phi_zz, only check at update points.
        if phi_zz_frozen is None or at_update:
            if check_error <= CVERGE:
                converged = True
                if print_info:
                    print(f"  Converged after {iteration} iterations"
                          f"  (error={check_error:.4e})")
                break
            if check_error >= DVERGE:
                if print_info:
                    print(f"  Diverged at iter {iteration}"
                          f"  (error={check_error:.4e})")
                break

    return converged, iteration, np.array(error_history)


# ---------------------------------------------------------------------------
# Helper: build 3-D initial state from 2-D extract_arrays dict
# ---------------------------------------------------------------------------

def build_3d_state(d, nk, half_span, z_max_factor=3.0):
    """
    Given the 2-D coefficient dict from stage_0/solver2d_py.extract_arrays()
    and spanwise parameters, assemble the 3-D solver inputs.
    """
    ni = d['imax'] + 1
    nj = d['jmax'] + 1

    eta, k_tip = build_spanwise_grid(half_span, nk, z_max_factor)

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
        'k_tip'     : k_tip,
        'nk'        : nk,
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
