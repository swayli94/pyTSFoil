"""
2D TSD solver in Python/Numba  — Stage 0 validation.

Equivalent to TSFOIL's SOLVE() + SYOR().  Validates the
thomas_column_sweep kernel before extending it to 3D.

Index convention (everywhere in this file):
  - All arrays use 0-based Python indexing.
  - Fortran 1-based index I  →  Python 0-based i  =  I - 1.
  - P has shape (nj, ni) where nj = JMAX, ni = IMAX.
"""

import numpy as np
from numba import njit


# ---------------------------------------------------------------------------
# Core Numba kernel: one column of the Murman-Cole SLOR sweep
# ---------------------------------------------------------------------------

@njit(cache=True)
def thomas_column_sweep(
    i_col,          # int, 0-based column index
    P,              # float64[nj, ni], updated in-place at column i_col
    P_prev_saved,   # float64[nj], snapshot of P[:, i_col-1] BEFORE it was updated
    EMU_prev,       # float64[nj], EMU from column i_col-1 (Murman upwinding)
    emu_cur,        # float64[nj], OUTPUT: EMU for current column i_col
    # --- finite-difference coefficients (0-based) ---
    C1,             # float64[ni]
    CXL, CXC, CXR,            # float64[ni]  phi_x coefficients
    CXXL, CXXC, CXXR,         # float64[ni]  phi_xx coefficients
    CYYC, CYYD, CYYU,         # float64[nj]  phi_yy coefficients
    CYYBUC, CYYBUU,            # float64 scalars – upper wall BC
    CYYBLC, CYYBLD,            # float64 scalars – lower wall BC
    # --- boundary condition arrays (0-based) ---
    FXUBC,          # float64[ni]
    FXLBC,          # float64[ni]
    PJUMP,          # float64[ni]
    # --- mesh ---
    X,              # float64[ni]
    XDIFF,          # float64[ni], XDIFF[i] = 1/(X[i]-X[i-1])
    # --- solver parameters ---
    eps, wi, ak, rtk,
    # --- index bounds (0-based) ---
    ile, ite, jup, jlow, jbot, jtop, jmin, jmax, im2,
    # --- CFS correction flags ---
    eps_ampl, cfs_triggered, ite_lo, ite_hi, sonvel, beta_sonic,
):
    """
    Single-column Murman-Cole SLOR sweep (equivalent to one I-iteration in SYOR).

    Updates P[:, i_col] in-place via Thomas algorithm.
    Returns max |delta_P| for convergence tracking.
    """
    nj = P.shape[0]

    # --- artificial dissipation factor (EPS / dx^2) ---
    dx = X[i_col] - X[i_col - 1]
    epsx = eps / (dx * dx)
    if cfs_triggered and ite_lo <= i_col <= ite_hi:
        epsx *= eps_ampl

    # ------------------------------------------------------------------ #
    # 1. VC = AK/dx - (gamma+1)/2 * phi_x  ≈  1 - M^2 (similarity form) #
    #    Supersonic  : VC < 0 → EMU = VC   (upwind bias)                 #
    #    Subsonic    : VC > 0 → EMU = 0    (centred)                     #
    # ------------------------------------------------------------------ #
    VC = np.empty(nj, dtype=np.float64)
    for j in range(jbot, jtop + 1):
        VC[j] = (C1[i_col]
                 - CXL[i_col] * P_prev_saved[j]
                 - CXC[i_col] * P[j, i_col]
                 - CXR[i_col] * P[j, i_col + 1])
        emu_cur[j] = 0.0
    for j in range(jbot, jtop + 1):
        if VC[j] < 0.0:
            emu_cur[j] = VC[j]

    # ------------------------------------------------------------------ #
    # 2. Assemble tridiagonal system  DIAG·dP + SUP·dP[j-1] + SUB·dP[j+1] = RHS
    #    (SUP = sub-diagonal acting on P[j-1], SUB = super-diagonal on P[j+1])
    # ------------------------------------------------------------------ #
    DIAG = np.empty(nj, dtype=np.float64)
    SUP  = np.empty(nj, dtype=np.float64)   # coefficient for P[j-1]  (CYYD in Fortran)
    SUB  = np.empty(nj, dtype=np.float64)   # coefficient for P[j+1]  (CYYU in Fortran)
    RHS  = np.empty(nj, dtype=np.float64)

    for j in range(jbot, jtop + 1):
        DIAG[j] = ((emu_cur[j] - VC[j]) * CXXC[i_col] * wi
                   + EMU_prev[j] * CXXR[i_col - 1]
                   - CYYC[j])
        SUP[j] = CYYD[j]
        SUB[j] = CYYU[j]

    # ------------------------------------------------------------------ #
    # 3. RHS = -residual(phi_xx) — current column (Murman-Cole)          #
    # ------------------------------------------------------------------ #
    for j in range(jbot, jtop + 1):
        RHS[j] = -(VC[j] - emu_cur[j]) * (
            CXXL[i_col] * P[j, i_col - 1]
            - CXXC[i_col] * P[j, i_col]
            + CXXR[i_col] * P[j, i_col + 1]
        )

    # RHS: Murman upwinding from previous column
    for j in range(jbot, jtop + 1):
        RHS[j] -= EMU_prev[j] * (
            CXXL[i_col - 1] * P[j, im2]
            - CXXC[i_col - 1] * P[j, i_col - 1]
            + CXXR[i_col - 1] * P[j, i_col]
        )

    # ------------------------------------------------------------------ #
    # 4. RHS: -residual(phi_yy)                                          #
    # ------------------------------------------------------------------ #
    # interior rows
    for j in range(jbot + 1, jtop):
        RHS[j] -= (CYYD[j] * P[j - 1, i_col]
                   - CYYC[j] * P[j, i_col]
                   + CYYU[j] * P[j + 1, i_col])

    # bottom boundary row (j = jbot)
    RHS[jbot] -= (-CYYC[jbot] * P[jbot, i_col]
                  + CYYU[jbot] * P[jbot + 1, i_col])
    if jbot != jmin:
        RHS[jbot] -= CYYD[jbot] * P[jbot - 1, i_col]

    # top boundary row (j = jtop)
    RHS[jtop] -= (CYYD[jtop] * P[jtop - 1, i_col]
                  - CYYC[jtop] * P[jtop, i_col])
    if jtop != jmax:
        RHS[jtop] -= CYYU[jtop] * P[jtop + 1, i_col]

    # ------------------------------------------------------------------ #
    # 5. Boundary conditions                                              #
    # ------------------------------------------------------------------ #
    if ile <= i_col <= ite:
        # --- upper airfoil wall ---
        j = jup
        DIAG[j] += CYYC[j] - CYYBUC
        SUP[j]   = 0.0
        SUB[j]   = CYYBUU
        RHS[j]  += (CYYD[j] * P[j - 1, i_col]
                    - CYYC[j] * P[j, i_col]
                    + CYYU[j] * P[j + 1, i_col]
                    - (-CYYBUC * P[j, i_col]
                       + CYYBUU * P[j + 1, i_col]
                       + FXUBC[i_col]))
        # --- lower airfoil wall ---
        j = jlow
        DIAG[j] += CYYC[j] - CYYBLC
        SUP[j]   = CYYBLD
        SUB[j]   = 0.0
        RHS[j]  += (CYYD[j] * P[j - 1, i_col]
                    - CYYC[j] * P[j, i_col]
                    + CYYU[j] * P[j + 1, i_col]
                    - (-CYYBLC * P[j, i_col]
                       + CYYBLD * P[j - 1, i_col]
                       + FXLBC[i_col]))
    elif i_col > ite:
        # --- Kutta wake slice ---
        RHS[jlow] += CYYU[jlow] * PJUMP[i_col]
        RHS[jup]  -= CYYD[jup]  * PJUMP[i_col]

    # ------------------------------------------------------------------ #
    # 6. BCEND: supersonic freestream far-field BC  (only if ak <= 0)    #
    # ------------------------------------------------------------------ #
    if ak <= 0.0:
        dfacl = -CYYD[jbot] * rtk * XDIFF[i_col]
        dfacu = -CYYU[jtop] * rtk * XDIFF[i_col]
        rfacl = dfacl * (P[0,    i_col] - P[0,    i_col - 1])
        rfacu = dfacu * (P[jmax, i_col] - P[jmax, i_col - 1])
        DIAG[jbot] += dfacl
        DIAG[jtop] += dfacu
        RHS[jbot] -= rfacl
        if jbot > jmin:
            RHS[jbot] += CYYD[jbot] * P[jbot - 1, i_col]
        RHS[jtop] -= rfacu
        if jtop < jmax:
            RHS[jtop] += CYYU[jtop] * P[jtop + 1, i_col]

    # ------------------------------------------------------------------ #
    # 7. Artificial dissipation  (EPS / dx^2)                            #
    # ------------------------------------------------------------------ #
    for j in range(jbot, jtop + 1):
        DIAG[j] -= epsx
        RHS[j]  -= epsx * (P[j, i_col - 1] - P_prev_saved[j])

    # ------------------------------------------------------------------ #
    # 8. CFS sonic-penalty correction at trailing-edge columns            #
    # ------------------------------------------------------------------ #
    if cfs_triggered and ite_lo <= i_col <= ite_hi:
        alpha_sonic = eps * beta_sonic / (dx * dx)
        for j in range(jbot, jtop + 1):
            p_sonic    = P[j, i_col - 1] + sonvel * dx
            DIAG[j]   += alpha_sonic
            RHS[j]    -= alpha_sonic * (P[j, i_col] - p_sonic)

    # ------------------------------------------------------------------ #
    # 9. Thomas algorithm  (forward elimination + back substitution)      #
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
    # 10. Update P and return max |correction|                            #
    # ------------------------------------------------------------------ #
    max_err = 0.0
    for j in range(jbot, jtop + 1):
        P[j, i_col] += RHS[j]
        a = abs(RHS[j])
        if a > max_err:
            max_err = a

    return max_err


# ---------------------------------------------------------------------------
# Outer SLOR loop  (equivalent to Fortran SOLVE)
# ---------------------------------------------------------------------------

def solve_2d_py(
    P,                      # float64[nj, ni], initial potential (modified in-place)
    # --- finite-difference coefficients ---
    C1, CXL, CXC, CXR,
    CXXL, CXXC, CXXR,
    CYYC, CYYD, CYYU,
    CYYBUC, CYYBUU, CYYBLC, CYYBLD,
    # --- boundary arrays ---
    FXUBC, FXLBC, PJUMP,
    THETA,                  # float64[nj, ni], circulation correction field
    # --- far-field BC arrays ---
    VUP, VDOWN, VTOP, VBOT,
    DUP, DDOWN, DTOP, DBOT,
    # --- mesh ---
    X, XDIFF,
    # --- flow parameters ---
    AK, EPS, WI, SONVEL, VOL,
    # --- far-field state (mutable scalars passed as 1-elem arrays) ---
    CIRCFF_arr,             # float64[1]  – mutated in-place
    DUB_arr,                # float64[1]  – mutated in-place
    # --- Kutta/circulation extrapolation coefficients ---
    CJUP, CJUP1, CJLOW, CJLOW1,
    # --- index bounds (0-based) ---
    imax, jmax,
    iup, idown, ile, ite,
    jup, jlow, jbot, jtop,
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
    print_info=True,
):
    """
    Full 2D SLOR solver — Python equivalent of Fortran SOLVE().
    Updates P in-place.  Returns (converged, n_iter, error_history).
    """
    nj, ni = P.shape
    jmin = 0
    imin = 0

    rtk = np.sqrt(abs(AK)) if AK <= 0.0 else 0.0

    CIRCFF  = CIRCFF_arr[0]
    DUB     = DUB_arr[0]
    CIRCTE  = 0.0

    cfs_triggered = False
    ite_lo = ite - 5
    ite_hi = ite + 5

    # im2 initialisation (tracks the "two columns back" index)
    im2_init = iup - 1
    if AK < 0.0:
        im2_init = iup - 2

    error_history = []
    converged     = False

    for iteration in range(1, MAXIT + 1):

        # -------------------------------------------------------------- #
        # RECIRC: update trailing-edge circulation jump and PJUMP         #
        # -------------------------------------------------------------- #
        cteold  = CIRCTE
        pup     = CJUP  * P[jup,  ite] - CJUP1  * P[jup  + 1, ite]
        plow    = CJLOW * P[jlow, ite] - CJLOW1 * P[jlow - 1, ite]
        CIRCTE  = pup - plow
        circo   = CIRCFF
        wcirc_eff = 0.02 if cfs_triggered else WCIRC   # Fortran: if (CFS_TRIGGERED) WCIRC = 0.02
        CIRCFF  = (1.0 - wcirc_eff) * circo + CIRCTE * wcirc_eff
        dcirc   = CIRCTE - cteold

        if imax > ite:
            factor = (CIRCFF - CIRCTE) / (X[imax] - 1.0)
            for iw in range(ite, imax + 1):
                PJUMP[iw] = CIRCTE + (X[iw] - 1.0) * factor

        # -------------------------------------------------------------- #
        # SYOR: column sweep  iup → idown                                 #
        # -------------------------------------------------------------- #
        EMU_prev = np.zeros(nj, dtype=np.float64)
        emu_cur  = np.zeros(nj, dtype=np.float64)

        if AK <= 0.0:
            EMU_prev[:] = C1[1]          # C1(2) in Fortran 1-based

        P_prev_saved = P[:, iup - 1].copy()  # snapshot of col before iup
        im2 = im2_init

        error = 0.0

        for i in range(iup, idown + 1):
            # save P[:, i] BEFORE Thomas update (needed for next column's VC)
            P_cur_before = P[:, i].copy()

            col_err = thomas_column_sweep(
                i, P, P_prev_saved, EMU_prev, emu_cur,
                C1, CXL, CXC, CXR,
                CXXL, CXXC, CXXR,
                CYYC, CYYD, CYYU,
                CYYBUC, CYYBUU, CYYBLC, CYYBLD,
                FXUBC, FXLBC, PJUMP,
                X, XDIFF,
                EPS, WI, AK, rtk,
                ile, ite, jup, jlow, jbot, jtop, jmin, jmax, im2,
                EPS_AMPL, cfs_triggered, ite_lo, ite_hi, SONVEL, BETA_SONIC,
            )
            error = max(error, col_err)

            P_prev_saved = P_cur_before   # old P[:, i] → POLD for column i+1
            EMU_prev[:]  = emu_cur[:]
            im2 = i - 1

            # supersonic freestream: mirror at idown boundary
            if AK <= 0.0 and i == idown - 1:
                P[:, idown + 1] = P[:, idown - 1]

        # -------------------------------------------------------------- #
        # Circulation correction  P += dcirc * THETA  (subsonic only)    #
        # -------------------------------------------------------------- #
        if AK >= 0.0:
            for i in range(iup, idown + 1):
                for j in range(jbot, jtop + 1):
                    P[j, i] += dcirc * THETA[j, i]

        # -------------------------------------------------------------- #
        # REDUB: update doublet strength every NDUB iterations            #
        # -------------------------------------------------------------- #
        if iteration % NDUB == 0:
            if abs(CIRCFF) >= 1e-4:
                DUB = VOL           # lifting case: DUB = model volume
            # non-lifting double-integral update omitted for brevity
            # (negligible for AoA > 0 cases used in Stage 0)

        # -------------------------------------------------------------- #
        # RESET: update far-field Dirichlet BCs                           #
        # -------------------------------------------------------------- #
        for j in range(jmax + 1):
            P[j, 0]    = CIRCFF * VUP[j]   + DUB * DUP[j]
            P[j, imax] = CIRCFF * VDOWN[j] + DUB * DDOWN[j]
        for i in range(imax + 1):
            P[0,    i] = CIRCFF * VBOT[i] + DUB * DBOT[i]
            P[jmax, i] = CIRCFF * VTOP[i] + DUB * DTOP[i]

        error_history.append(error)

        # -------------------------------------------------------------- #
        # CFS detection (optional)                                        #
        # -------------------------------------------------------------- #
        if FLAG_CFS and not cfs_triggered and iteration >= ITER_START_CFS:
            for i in range(iup, idown + 1):
                if abs(X[i] - 1.0) <= 0.05:
                    u_local = (P[jup, i] - P[jup, i - 1]) * XDIFF[i]
                    if u_local > SONVEL:
                        cfs_triggered = True
                        break
        elif FLAG_CFS and cfs_triggered:
            still_super = False
            for i in range(iup, idown + 1):
                if abs(X[i] - 1.0) <= 0.05:
                    u_local = (P[jup, i] - P[jup, i - 1]) * XDIFF[i]
                    if u_local > SONVEL:
                        still_super = True
                        break
            if not still_super:
                cfs_triggered = False

        # -------------------------------------------------------------- #
        # Convergence / divergence check                                  #
        # -------------------------------------------------------------- #
        if print_info and (iteration == 1 or iteration % IPRTER == 0):
            print(f"  iter={iteration:5d}  error={error:.4e}  CIRCFF={CIRCFF:.5f}")

        if error <= CVERGE:
            converged = True
            if print_info:
                print(f"  Converged after {iteration} iterations  (error={error:.4e})")
            break
        if error >= DVERGE:
            if print_info:
                print(f"  Diverged at iteration {iteration}  (error={error:.4e})")
            break

    CIRCFF_arr[0] = CIRCFF
    DUB_arr[0]    = DUB

    return converged, iteration, np.array(error_history)


# ---------------------------------------------------------------------------
# Helper: extract all arrays from tsfoil_fortran module to numpy (float64)
# ---------------------------------------------------------------------------

def extract_arrays(tsf, pytsfoil_obj):
    """
    Pull every array needed by solve_2d_py from the live Fortran module.

    Returns a dict keyed by name.  All arrays are 0-based float64 copies.
    The Fortran module uses 1-based indexing; here we slice out only the
    live [1:IMAX+1] / [1:JMAX+1] ranges and shift to 0-based.
    """
    imax = int(tsf.common_data.imax)
    jmax = int(tsf.common_data.jmax)
    ni = imax      # 0-based columns: 0 … imax-1
    nj = jmax      # 0-based rows:    0 … jmax-1

    def f1d_i(arr):
        """Fortran 1-D array over I: slice [0:imax] → 0-based [0:imax-1]."""
        return np.array(arr[:imax], dtype=np.float64)

    def f1d_j(arr):
        """Fortran 1-D array over J: slice [0:jmax] → 0-based."""
        return np.array(arr[:jmax], dtype=np.float64)

    def f2d(arr):
        """Fortran P(NMP_plus2, NMP_plus1): slice to (nj, ni) 0-based."""
        return np.array(arr[:jmax, :imax], dtype=np.float64)

    d = {}

    # mesh
    d['imax'] = imax - 1          # 0-based last index
    d['jmax'] = jmax - 1
    d['iup']  = int(tsf.common_data.iup)  - 1
    d['idown']= int(tsf.common_data.idown)- 1
    d['ile']  = int(tsf.common_data.ile)  - 1
    d['ite']  = int(tsf.common_data.ite)  - 1
    d['jup']  = int(tsf.common_data.jup)  - 1
    d['jlow'] = int(tsf.common_data.jlow) - 1
    d['jbot'] = int(tsf.common_data.jbot) - 1
    d['jtop'] = int(tsf.common_data.jtop) - 1

    # flow scalars
    d['AK']      = float(tsf.common_data.ak)
    d['EPS']     = float(tsf.common_data.eps)
    d['WI']      = float(tsf.solver_data.wi)
    d['SONVEL']  = float(tsf.solver_data.sonvel)
    d['VOL']     = float(tsf.common_data.vol)
    d['WCIRC']   = float(tsf.common_data.wcirc)
    d['MAXIT']   = int(tsf.common_data.maxit)
    d['CVERGE']  = float(tsf.common_data.cverge)
    d['DVERGE']  = float(tsf.common_data.dverge)
    d['IPRTER']  = int(tsf.common_data.iprter)
    d['FLAG_CFS']= bool(tsf.common_data.flag_cfs)
    d['BETA_SONIC']     = float(tsf.common_data.beta_sonic)
    d['EPS_AMPL']       = float(tsf.common_data.eps_ampl)
    d['ITER_START_CFS'] = int(tsf.common_data.iter_start_cfs)

    # circulation extrapolation coefficients
    d['CJUP']   = float(tsf.solver_data.cjup)
    d['CJUP1']  = float(tsf.solver_data.cjup1)
    d['CJLOW']  = float(tsf.solver_data.cjlow)
    d['CJLOW1'] = float(tsf.solver_data.cjlow1)

    # far-field state (will be mutated)
    d['CIRCFF_arr'] = np.array([float(tsf.solver_data.circff)], dtype=np.float64)
    d['DUB_arr']    = np.array([float(tsf.solver_data.dub)],    dtype=np.float64)

    # x-direction coefficients
    d['C1']   = f1d_i(tsf.solver_data.c1)
    d['CXL']  = f1d_i(tsf.solver_data.cxl)
    d['CXC']  = f1d_i(tsf.solver_data.cxc)
    d['CXR']  = f1d_i(tsf.solver_data.cxr)
    d['CXXL'] = f1d_i(tsf.solver_data.cxxl)
    d['CXXC'] = f1d_i(tsf.solver_data.cxxc)
    d['CXXR'] = f1d_i(tsf.solver_data.cxxr)

    # y-direction coefficients
    d['CYYC'] = f1d_j(tsf.solver_data.cyyc)
    d['CYYD'] = f1d_j(tsf.solver_data.cyyd)
    d['CYYU'] = f1d_j(tsf.solver_data.cyyu)

    # airfoil-wall special coefficients (scalars)
    d['CYYBUC'] = float(tsf.solver_data.cyybuc)
    d['CYYBUU'] = float(tsf.solver_data.cyybuu)
    d['CYYBLC'] = float(tsf.solver_data.cyyblc)
    d['CYYBLD'] = float(tsf.solver_data.cyybld)

    # boundary condition arrays
    d['FXUBC'] = f1d_i(tsf.solver_data.fxubc)
    d['FXLBC'] = f1d_i(tsf.solver_data.fxlbc)
    d['PJUMP'] = f1d_i(tsf.solver_data.pjump)

    # circulation correction field
    d['THETA'] = f2d(tsf.solver_data.theta)

    # far-field BC arrays
    d['VUP']   = f1d_j(tsf.solver_data.vup)
    d['VDOWN'] = f1d_j(tsf.solver_data.vdown)
    d['VTOP']  = f1d_i(tsf.solver_data.vtop)
    d['VBOT']  = f1d_i(tsf.solver_data.vbot)
    d['DUP']   = f1d_j(tsf.solver_data.dup)
    d['DDOWN'] = f1d_j(tsf.solver_data.ddown)
    d['DTOP']  = f1d_i(tsf.solver_data.dtop)
    d['DBOT']  = f1d_i(tsf.solver_data.dbot)

    # mesh coordinate arrays
    d['X']     = f1d_i(tsf.common_data.x)
    d['XDIFF'] = f1d_i(tsf.common_data.xdiff)

    # initial potential (will be zeroed for cold start)
    d['P_fortran'] = f2d(tsf.solver_data.p)

    return d


# ---------------------------------------------------------------------------
# Convenience: compute surface Cp from Python P field
# ---------------------------------------------------------------------------

def surface_cp_from_P(P, d, pytsfoil_obj):
    """
    Isentropic Cp on upper/lower airfoil surfaces from Python potential P.
    Returns (cpu, cpl, xx_foil) in physical x coordinates.
    """
    ile   = d['ile']
    ite   = d['ite']
    jup   = d['jup']
    jlow  = d['jlow']
    XDIFF = d['XDIFF']

    CJUP   = d['CJUP']
    CJUP1  = d['CJUP1']
    CJLOW  = d['CJLOW']
    CJLOW1 = d['CJLOW1']

    emach = float(pytsfoil_obj.config['EMACH'])
    delta = float(pytsfoil_obj.airfoil['t_max'])
    ak    = d['AK']
    gam1  = 2.4    # gamma + 1 = 2.4 for air

    # AK = (1 - M^2) / delta^(2/3) / M  (Krupp SIMDEF=3)
    # local Mach from phi_x  (EMACH1 equivalence)
    # ARG = 1 - delta^(2/3) * (AK - GAM1 * phi_x) * M
    delrt2 = delta ** (2.0 / 3.0)
    simdef = pytsfoil_obj.config['SIMDEF']

    def phi_x(j, i):
        """Central phi_x at (j, i) — same as Fortran PX."""
        return 0.5 * (XDIFF[i + 1] * (P[j, i + 1] - P[j, i])
                      + XDIFF[i]   * (P[j, i]     - P[j, i - 1]))

    def emach1(u):
        ak_loc = ak - gam1 * u
        if simdef == 1:
            arg = 1.0 - delrt2 * ak_loc
        elif simdef == 2:
            arg = 1.0 - delrt2 * ak_loc * emach ** (4.0 / 3.0)
        else:   # 3 = Krupp (default)
            arg = 1.0 - delrt2 * ak_loc * emach
        return np.sqrt(max(arg, 0.0))

    n_foil = ite - ile + 1
    cpu  = np.zeros(n_foil)
    cpl  = np.zeros(n_foil)

    for k, i in enumerate(range(ile, ite + 1)):
        uu = (CJUP  * phi_x(jup,      i)
              - CJUP1 * phi_x(jup + 1, i))
        ul = (CJLOW  * phi_x(jlow,     i)
              - CJLOW1 * phi_x(jlow - 1, i))
        cpu[k] = pytsfoil_obj._cp_isentropic(emach1(uu), emach)
        cpl[k] = pytsfoil_obj._cp_isentropic(emach1(ul), emach)

    xx_foil = d['X'][ile: ite + 1]
    return cpu, cpl, xx_foil
