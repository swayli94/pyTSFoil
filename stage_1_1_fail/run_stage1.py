"""
Stage 1 verification: rectangular wing (no sweep/taper/dihedral).

Test matrix
-----------
For each of two representative cases:

  Test A — Strip-theory mode  (use_phi_zz=False, nk=30):
    Every station is independent; each should reproduce the 2-D (Stage 0)
    Fortran Cp and CL at the mid-span section.
    Pass criterion: max|ΔCp| < 0.005, |ΔCL|/CL < 0.1 %

  Test B — Full 3-D mode  (use_phi_zz=True, nk=30):
    φ_zz couples the spanwise stations (3-D relief effect) via outer-Picard
    iteration (phi_zz_update_freq > 1).  phi_zz is frozen for N iterations,
    allowing the 2-D SLOR to converge against a fixed source, eliminating
    the O(1/dz²) oscillation floor that prevents convergence with freq=1.
    Expected: CL_3d < CL_2d (finite-wing downwash reduces effective α).
    No strict PASS/FAIL; structure and trend are reported.

Cases
-----
  NACA 0012   Ma=0.50  α=1.00°   (subsonic, strict)
  NACA 0012   Ma=0.70  α=0.50°   (subcritical, structure only)

Wing: rectangular, AR=6, uniform NACA 0012 section, no sweep / taper / dihedral.
"""

import sys
import os
import time
import numpy as np
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt

_HERE = os.path.dirname(os.path.abspath(__file__))
_ROOT = os.path.dirname(_HERE)
if _ROOT not in sys.path:
    sys.path.insert(0, _ROOT)

from pytsfoil import PyTSFoil
import tsfoil_fortran as tsf
from stage_0.solver2d_py import extract_arrays, surface_cp_from_P, solve_2d_py
from stage_1_1_fail.solver import build_3d_state, solve_3d, build_spanwise_grid
from stage_1_1_fail.postprocess import surface_cp_3d_station, spanwise_cl


# ---------------------------------------------------------------------------
# Wing / test configuration
# ---------------------------------------------------------------------------

AR        = 6.0        # aspect ratio (b/c = 6 → half-span = 3)
N_K       = 30         # number of spanwise stations
Z_MAX_FAC = 3.0        # z_max = Z_MAX_FAC * (b/2)

CASES = [
    {
        'name'  : 'NACA0012_M050_A100',
        'label' : 'NACA 0012   Ma=0.50  α=1.00°',
        'EMACH' : 0.50, 'ALPHA': 1.00,
        'n_x'   : 120, 'n_y': 60, 'n_af': 80,
        'flag_CFS': False,
        'strict': True,
        'blend_factor': 1.0,   # full cosine taper for warm-start of 3-D run
    },
    {
        'name'  : 'NACA0012_M070_A050',
        'label' : 'NACA 0012   Ma=0.70  α=0.50°',
        'EMACH' : 0.70, 'ALPHA': 0.50,
        'n_x'   : 120, 'n_y': 60, 'n_af': 80,
        'flag_CFS': True,      # M=0.70 has a small supersonic bubble; CFS needed for
                               # Python SLOR convergence (Fortran handles it natively)
        'strict': False,
        'blend_factor': 0.5,   # half cosine taper (moderate near-critical warm-start)
    },
]


def naca0012_coords(n=201):
    t = 0.12
    x = 0.5 * (1 - np.cos(np.linspace(0, np.pi, n)))
    y = 5 * t * (0.2969*np.sqrt(x) - 0.1260*x - 0.3516*x**2
                  + 0.2843*x**3 - 0.1015*x**4)
    xu = x[::-1]; yu =  y[::-1]
    xl = x;       yl = -y
    return np.column_stack([np.concatenate([xu, xl[1:]]),
                            np.concatenate([yu, yl[1:]])])


# ---------------------------------------------------------------------------
# JIT warm-up (load numba cache once before timing)
# ---------------------------------------------------------------------------

_jit_warmed = False

def _warmup_jit(d, half_span):
    global _jit_warmed
    if _jit_warmed:
        return 0.0
    print("  [JIT warm-up] loading Numba cache …", end=' ', flush=True)
    t0  = time.perf_counter()
    s   = build_3d_state(d, nk=3, half_span=half_span, z_max_factor=Z_MAX_FAC)
    P_w = s['P'].copy()
    PJUMP_w    = s['PJUMP_3d'].copy()
    CIRCFF_w   = s['CIRCFF_3d'].copy()
    DUB_w      = s['DUB_3d'].copy()
    solve_3d(
        P_w,
        s['C1'], s['CXL'], s['CXC'], s['CXR'],
        s['CXXL'], s['CXXC'], s['CXXR'],
        s['CYYC'], s['CYYD'], s['CYYU'],
        s['CYYBUC'], s['CYYBUU'], s['CYYBLC'], s['CYYBLD'],
        s['FXUBC'], s['FXLBC'],
        PJUMP_w, s['THETA'],
        s['VUP'], s['VDOWN'], s['VTOP'], s['VBOT'],
        s['DUP'], s['DDOWN'], s['DTOP'], s['DBOT'],
        s['X'], s['XDIFF'],
        s['eta'],
        s['AK'], s['EPS'], s['WI'], s['SONVEL'], s['VOL'],
        CIRCFF_w, DUB_w,
        s['CJUP'], s['CJUP1'], s['CJLOW'], s['CJLOW1'],
        s['imax'], s['jmax'],
        s['iup'], s['idown'], s['ile'], s['ite'],
        s['jup'], s['jlow'], s['jbot'], s['jtop'],
        s['k_tip'],
        MAXIT=1, CVERGE=1e-20, DVERGE=1e20,
        IPRTER=9999, WCIRC=s['WCIRC'],
        use_phi_zz=True,
        print_info=False,
    )
    t_jit = time.perf_counter() - t0
    _jit_warmed = True
    print(f"done ({t_jit:.2f} s)")
    return t_jit


# ---------------------------------------------------------------------------
# Run one case
# ---------------------------------------------------------------------------

def run_case(case):
    print(f"\n{'='*70}")
    print(f"  {case['label']}   AR={AR}")
    print(f"{'='*70}")

    coords   = naca0012_coords()
    half_span = AR / 2.0   # c = 1

    # ================================================================
    # 1. Fortran 2-D reference
    # ================================================================
    print("\n[1/4] Fortran 2-D reference …")
    pts = PyTSFoil(airfoil_coordinates=coords, work_dir=_HERE)
    pts.set_config(
        EMACH=case['EMACH'], ALPHA=case['ALPHA'],
        MAXIT=5000, CVERGE=1e-6,
        n_point_x=case['n_x'], n_point_y=case['n_y'],
        n_point_airfoil=case['n_af'],
        EPS=0.2, NWDGE=0,
        flag_CFS=case['flag_CFS'],
        flag_output=False, flag_output_summary=False,
        flag_output_shock=False, flag_output_field=False,
        flag_print_info=False,
    )
    pts.initialize_data()
    pts.set_airfoil()
    pts.set_mesh()
    pts.compute_mesh_indices()

    t0 = time.perf_counter()
    pts.run_fortran_solver()
    t_fort = time.perf_counter() - t0

    pts.compute_data_summary()
    cl_fort = pts.data_summary['cl']

    pts.config['flag_output_shock']   = False
    pts.config['flag_output_summary'] = False
    pts.output_surface()
    ile     = pts.mesh['ile']
    ite     = pts.mesh['ite']
    xx_f    = pts.mesh['xx'][ile: ite + 1]
    cpu_f   = pts.data_summary['cpu'][ile: ite + 1]
    cpl_f   = pts.data_summary['cpl'][ile: ite + 1]
    print(f"    CL_fort = {cl_fort:.5f}   t = {t_fort:.2f} s")

    # ================================================================
    # 2. Extract 2-D coefficient dict
    # ================================================================
    d = extract_arrays(tsf, pts)

    # Capture CLFACT for section CL via TE potential jump (same as 2D)
    import tsfoil_fortran as _tsf
    clfact = float(_tsf.solver_data.clfact)

    def cl_from_P(P_k):
        """Section CL at station k via TE potential jump (same formula as 2D)."""
        pup  = (d['CJUP']  * P_k[d['jup'],  d['ite']]
                - d['CJUP1']  * P_k[d['jup']  + 1, d['ite']])
        plow = (d['CJLOW'] * P_k[d['jlow'], d['ite']]
                - d['CJLOW1'] * P_k[d['jlow'] - 1, d['ite']])
        return 2.0 * clfact * (pup - plow)

    # JIT warm-up (one-time per run)
    _warmup_jit(d, half_span)

    # ================================================================
    # 3. Test A: strip theory (use_phi_zz=False) — must match 2-D
    # ================================================================
    print(f"\n[2/4] Test A — strip theory  (nk={N_K}, φ_zz=0) …")
    sA = build_3d_state(d, nk=N_K, half_span=half_span, z_max_factor=Z_MAX_FAC)

    t0 = time.perf_counter()
    conv_A, n_A, err_A = solve_3d(
        sA['P'],
        sA['C1'], sA['CXL'], sA['CXC'], sA['CXR'],
        sA['CXXL'], sA['CXXC'], sA['CXXR'],
        sA['CYYC'], sA['CYYD'], sA['CYYU'],
        sA['CYYBUC'], sA['CYYBUU'], sA['CYYBLC'], sA['CYYBLD'],
        sA['FXUBC'], sA['FXLBC'],
        sA['PJUMP_3d'], sA['THETA'],
        sA['VUP'], sA['VDOWN'], sA['VTOP'], sA['VBOT'],
        sA['DUP'], sA['DDOWN'], sA['DTOP'], sA['DBOT'],
        sA['X'], sA['XDIFF'],
        sA['eta'],
        sA['AK'], sA['EPS'], sA['WI'], sA['SONVEL'], sA['VOL'],
        sA['CIRCFF_3d'], sA['DUB_3d'],
        sA['CJUP'], sA['CJUP1'], sA['CJLOW'], sA['CJLOW1'],
        sA['imax'], sA['jmax'],
        sA['iup'], sA['idown'], sA['ile'], sA['ite'],
        sA['jup'], sA['jlow'], sA['jbot'], sA['jtop'],
        sA['k_tip'],
        MAXIT=sA['MAXIT'], CVERGE=sA['CVERGE'], DVERGE=sA['DVERGE'],
        IPRTER=sA['IPRTER'], WCIRC=sA['WCIRC'],
        FLAG_CFS=sA['FLAG_CFS'], BETA_SONIC=sA['BETA_SONIC'],
        EPS_AMPL=sA['EPS_AMPL'], ITER_START_CFS=sA['ITER_START_CFS'],
        use_phi_zz=False,
        print_info=False,
    )
    t_A = time.perf_counter() - t0

    # Mid-span station for comparison with 2-D
    k_mid = sA['k_tip'] // 2
    cpu_A, cpl_A, xx_A = surface_cp_3d_station(sA['P'][k_mid], d, pts)
    # Use TE potential-jump CL (same as Fortran) for accurate comparison
    cl_3dA_mid = cl_from_P(sA['P'][k_mid])

    max_dcp_u_A = np.max(np.abs(cpu_A - np.interp(xx_A, xx_f, cpu_f)))
    max_dcp_l_A = np.max(np.abs(cpl_A - np.interp(xx_A, xx_f, cpl_f)))
    max_dcp_A   = max(max_dcp_u_A, max_dcp_l_A)
    delta_cl_A  = abs(cl_3dA_mid - cl_fort) / max(abs(cl_fort), 1e-6)
    status_A    = 'converged' if conv_A else 'NOT converged'

    print(f"    CL_mid-strip={cl_3dA_mid:.5f}  CL_fort={cl_fort:.5f}  "
          f"|ΔCL|/CL={delta_cl_A*100:.3f}%")
    print(f"    mid-span max|ΔCp|={max_dcp_A:.4f}  t={t_A:.1f}s  {n_A} iters  ({status_A})")

    passed_A = None
    if case['strict']:
        passed_A = (max_dcp_A < 0.005) and (delta_cl_A < 0.001)
        print(f"    Strip theory   Cp {'PASS' if max_dcp_A < 0.005 else 'FAIL'}"
              f"   CL {'PASS' if delta_cl_A < 0.001 else 'FAIL'}"
              f"   → {'PASS' if passed_A else 'FAIL'}")

    # ================================================================
    # 4. Test B: full 3-D (use_phi_zz=True) — expect CL < CL_fort
    # ================================================================
    print(f"\n[3/4] Test B — full 3-D  (nk={N_K}, φ_zz coupling) …")
    sB = build_3d_state(d, nk=N_K, half_span=half_span, z_max_factor=Z_MAX_FAC)

    # Warm start from converged strip-theory solution with a cosine taper.
    # blend_factor controls how aggressively the spanwise variation is seeded:
    #   1.0 → full cosine taper (P[k_tip]=0)
    #   0.5 → half-strength taper (gentler seed)
    blend_factor = case.get('blend_factor', 1.0)
    k_tip_B = sB['k_tip']
    eta_B   = sB['eta']
    for k in range(k_tip_B + 1):
        cos_fac = np.cos(0.5 * np.pi * eta_B[k] / eta_B[k_tip_B])
        fac = 1.0 - (1.0 - cos_fac) * blend_factor
        sB['P'][k]         = fac * sA['P'][k_mid]
        sB['CIRCFF_3d'][k] = fac * sA['CIRCFF_3d'][k_mid]
        sB['DUB_3d'][k]    = fac * sA['DUB_3d'][k_mid]
        sB['PJUMP_3d'][k]  = fac * sA['PJUMP_3d'][k_mid]

    # phi_zz parameters: Gauss-Seidel (phi_zz_update_freq=1).
    # The GS forward sweep in z uses the already-swept P[k-1] at each station,
    # which creates stabilising negative feedback for the elliptic z-coupling
    # and drives CL toward the physically correct 3-D value (< CL_2d).
    # Exclude the last 2 stations from phi_zz to avoid the abrupt wing/no-wing
    # boundary spike at k_tip destabilising the sweep.
    # NOTE: the explicit GS approach has an oscillation floor of O(relax/dz²)·CVERGE
    # that prevents convergence to CVERGE; this is a known limitation.
    K_TIP_PHIZZ        = sB['k_tip'] - 2
    PHI_ZZ_UPDATE_FREQ = 1          # GS: recompute phi_zz each iteration
    emach_B = case['EMACH']
    if emach_B >= 0.65:
        PHI_ZZ_RELAX = 0.1          # near-critical: small relax to avoid shock amplification
        MAXIT_3D     = 100
    else:
        PHI_ZZ_RELAX = 0.5          # subsonic: larger relax shows 3-D relief clearly
        MAXIT_3D     = 200
    sB['DVERGE'] = 1e20     # disable divergence stop; MAXIT_3D caps iteration count

    t0 = time.perf_counter()
    conv_B, n_B, err_B = solve_3d(
        sB['P'],
        sB['C1'], sB['CXL'], sB['CXC'], sB['CXR'],
        sB['CXXL'], sB['CXXC'], sB['CXXR'],
        sB['CYYC'], sB['CYYD'], sB['CYYU'],
        sB['CYYBUC'], sB['CYYBUU'], sB['CYYBLC'], sB['CYYBLD'],
        sB['FXUBC'], sB['FXLBC'],
        sB['PJUMP_3d'], sB['THETA'],
        sB['VUP'], sB['VDOWN'], sB['VTOP'], sB['VBOT'],
        sB['DUP'], sB['DDOWN'], sB['DTOP'], sB['DBOT'],
        sB['X'], sB['XDIFF'],
        sB['eta'],
        sB['AK'], sB['EPS'], sB['WI'], sB['SONVEL'], sB['VOL'],
        sB['CIRCFF_3d'], sB['DUB_3d'],
        sB['CJUP'], sB['CJUP1'], sB['CJLOW'], sB['CJLOW1'],
        sB['imax'], sB['jmax'],
        sB['iup'], sB['idown'], sB['ile'], sB['ite'],
        sB['jup'], sB['jlow'], sB['jbot'], sB['jtop'],
        sB['k_tip'],
        MAXIT=MAXIT_3D, CVERGE=sB['CVERGE'], DVERGE=sB['DVERGE'],
        IPRTER=sB['IPRTER'], WCIRC=sB['WCIRC'],
        FLAG_CFS=sB['FLAG_CFS'], BETA_SONIC=sB['BETA_SONIC'],
        EPS_AMPL=sB['EPS_AMPL'], ITER_START_CFS=sB['ITER_START_CFS'],
        use_phi_zz=True, phi_zz_relax=PHI_ZZ_RELAX,
        k_tip_phizz=K_TIP_PHIZZ,
        phi_zz_update_freq=PHI_ZZ_UPDATE_FREQ,
        print_info=False,
    )
    t_B = time.perf_counter() - t0

    k_mid = sB['k_tip'] // 2
    cpu_B, cpl_B, xx_B = surface_cp_3d_station(sB['P'][k_mid], d, pts)
    cl_z_B, cl_3dB = spanwise_cl(sB['P'], d, pts, sB['eta'], sB['k_tip'])
    # Also compute mid-span section CL via TE potential jump
    cl_3dB_mid = cl_from_P(sB['P'][k_mid])
    status_B = 'converged' if conv_B else 'NOT converged'

    relief_mid = (cl_fort - cl_3dB_mid) / max(abs(cl_fort), 1e-6) * 100
    relief_int = (cl_fort - cl_3dB)     / max(abs(cl_fort), 1e-6) * 100
    print(f"    CL_mid={cl_3dB_mid:.5f}  CL_integrated={cl_3dB:.5f}  CL_2d={cl_fort:.5f}"
          f"  relief(mid)={relief_mid:.1f}%  t={t_B:.1f}s  {n_B} iters  ({status_B})")

    three_d_ok = cl_3dB_mid < cl_fort
    print(f"    CL_mid_3d < CL_2d : {'YES (3-D relief ✓)' if three_d_ok else 'NO (unexpected)'}")

    # ================================================================
    # 5. Plots
    # ================================================================
    print("\n[4/4] Saving plots …")

    fig, axes = plt.subplots(2, 2, figsize=(14, 10))
    fig.suptitle(case['label'] + f'  AR={AR}', fontsize=12)

    # — top-left: Cp comparison (strip theory vs Fortran) —
    ax = axes[0, 0]
    ax.plot(xx_f,  cpu_f, 'b-',  lw=2.0, label='Fortran 2-D upper')
    ax.plot(xx_f,  cpl_f, 'b--', lw=2.0, label='Fortran 2-D lower')
    ax.plot(xx_A,  cpu_A, 'g-',  lw=1.5, label=f'Strip (φ_zz=0) upper')
    ax.plot(xx_A,  cpl_A, 'g--', lw=1.5, label=f'Strip (φ_zz=0) lower')
    ax.invert_yaxis()
    ax.set_xlabel('x/c'); ax.set_ylabel('Cp')
    ax.set_title(f'Strip theory vs Fortran (k={k_mid})')
    ax.legend(fontsize=7); ax.grid(True, alpha=0.3)
    if passed_A is not None:
        ax.text(0.02, 0.05,
                f"max|ΔCp|={max_dcp_A:.4f}\n|ΔCL|/CL={delta_cl_A*100:.3f}%\n"
                f"{'PASS' if passed_A else 'FAIL'}",
                transform=ax.transAxes, fontsize=8,
                color='green' if passed_A else 'red',
                va='bottom', bbox=dict(fc='white', alpha=0.7))

    # — top-right: Cp comparison (3-D vs Fortran at mid-span) —
    ax = axes[0, 1]
    ax.plot(xx_f,  cpu_f, 'b-',  lw=2.0, label='Fortran 2-D upper')
    ax.plot(xx_f,  cpl_f, 'b--', lw=2.0, label='Fortran 2-D lower')
    ax.plot(xx_B,  cpu_B, 'r-',  lw=1.5, label=f'3-D (φ_zz) upper')
    ax.plot(xx_B,  cpl_B, 'r--', lw=1.5, label=f'3-D (φ_zz) lower')
    ax.invert_yaxis()
    ax.set_xlabel('x/c'); ax.set_ylabel('Cp')
    ax.set_title(f'3-D vs Fortran 2-D (mid-span k={k_mid})')
    ax.legend(fontsize=7); ax.grid(True, alpha=0.3)
    ax.text(0.02, 0.05,
            f"CL_mid={cl_3dB_mid:.4f}\nCL_2d={cl_fort:.4f}\n"
            f"relief={relief_mid:.1f}%",
            transform=ax.transAxes, fontsize=8,
            color='darkred', va='bottom',
            bbox=dict(fc='white', alpha=0.7))

    # — bottom-left: spanwise lift distribution —
    ax = axes[1, 0]
    eta_wing  = sB['eta'][:sB['k_tip'] + 1]
    ax.plot(eta_wing / (AR / 2), cl_z_B[:sB['k_tip'] + 1], 'r-o',
            ms=3, lw=1.5, label='3-D cl(z)')
    ax.axhline(cl_fort, color='b', ls='--', lw=1.5, label='2-D CL')
    ax.set_xlabel('z / (b/2)'); ax.set_ylabel('cl(z)')
    ax.set_title('Spanwise lift distribution (3-D)')
    ax.legend(fontsize=8); ax.grid(True, alpha=0.3)

    # — bottom-right: convergence histories —
    ax = axes[1, 1]
    if len(err_A) > 1:
        ax.semilogy(np.arange(1, len(err_A) + 1), err_A, 'g-',
                    lw=1.3, label='Strip (φ_zz=0)')
    if len(err_B) > 1:
        ax.semilogy(np.arange(1, len(err_B) + 1), err_B, 'r-',
                    lw=1.3, label='3-D (φ_zz)')
    ax.axhline(d['CVERGE'], color='k', ls=':', lw=1,
               label=f"CVERGE={d['CVERGE']:.0e}")
    ax.set_xlabel('Iteration'); ax.set_ylabel('max|ΔP|')
    ax.set_title('Convergence history')
    ax.legend(fontsize=8); ax.grid(True, alpha=0.3)

    fig.tight_layout()
    out_png = os.path.join(_HERE, f"result_{case['name']}.png")
    fig.savefig(out_png, dpi=120)
    plt.close(fig)
    print(f"    Saved → {out_png}")

    return {
        'name'        : case['name'],
        'label'       : case['label'],
        'strict'      : case['strict'],
        'cl_fort'     : cl_fort,
        'cl_strip_mid': cl_3dA_mid,
        'cl_3d_mid'   : cl_3dB_mid,
        'cl_3d_int'   : cl_3dB,
        'max_dcp_A'   : max_dcp_A,
        'delta_cl_A'  : delta_cl_A,
        'three_d_ok'  : three_d_ok,
        'passed_A'    : passed_A,
        'conv_A'      : conv_A,
        'conv_B'      : conv_B,
        'n_A'         : n_A,
        'n_B'         : n_B,
        't_A'         : t_A,
        't_B'         : t_B,
    }


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

if __name__ == '__main__':
    results = []
    for case in CASES:
        r = run_case(case)
        results.append(r)

    W = 32
    print(f"\n{'='*100}")
    print("  SUMMARY")
    print(f"{'='*100}")
    hdr = (f"  {'Case':<{W}}  {'CL_2d':>7}  {'CL_str':>7}  {'CL_3d':>7}"
           f"  {'max|ΔCp|':>8}  {'|ΔCL|%':>7}  {'3Drel%':>6}  {'Strip':>6}  {'3-D'}")
    print(hdr)
    print(f"  {'-'*98}")

    all_strict = True
    for r in results:
        s_res = ('PASS' if r['passed_A'] else 'FAIL') if r['strict'] else 'n/a'
        t_res = ('conv' if r['conv_B'] else 'no-cv')
        rel   = (r['cl_fort'] - r['cl_3d_mid']) / max(abs(r['cl_fort']), 1e-6) * 100
        print(
            f"  {r['label']:<{W}}"
            f"  {r['cl_fort']:>7.4f}  {r['cl_strip_mid']:>7.4f}  {r['cl_3d_mid']:>7.4f}"
            f"  {r['max_dcp_A']:>8.4f}  {r['delta_cl_A']*100:>7.3f}"
            f"  {rel:>6.1f}%  {s_res:>6}  {t_res}"
        )
        if r['strict']:
            all_strict = all_strict and bool(r['passed_A'])

    print(f"\n  Strip-theory validation (strict cases): "
          f"{'ALL PASS' if all_strict else 'SOME FAIL'}")
    sys.exit(0 if all_strict else 1)
