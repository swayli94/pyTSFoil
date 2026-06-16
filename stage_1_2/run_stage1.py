"""
Stage 1 verification: rectangular wing, both ends symmetric (phi_z=0).

Simplified geometry compared to stage_1_1_fail:
  - Span = 1, uniform sections, no sweep / taper / dihedral.
  - k=0 AND k=nk-1 are both symmetric planes → no wingtip.
  - Exact 3-D solution = 2-D solution (infinite periodic wing in z).

Key improvement: semi-implicit phi_zz (self-coupling term moved to DIAG)
eliminates the convergence floor; Test B should converge to CVERGE.

Tests
-----
  Test A — strip theory (use_phi_zz=False):
    Each station independent → must reproduce Fortran 2-D Cp and CL.
    Pass criterion: max|ΔCp| < 0.005, |ΔCL|/CL < 0.1%

  Test B — full 3-D (use_phi_zz=True, semi-implicit):
    phi_zz couples stations; exact answer is z-uniform (= 2-D) since
    the wing is infinite in z after ghost-cell symmetry.
    Pass criterion:
      (a) converges to CVERGE   ← primary check (was not possible with explicit)
      (b) |CL_3d - CL_2d| / CL_2d < 0.5%   ← physics check

Cases
-----
  NACA 0012  Ma=0.50  α=1.00°  (subsonic, strict)
  NACA 0012  Ma=0.70  α=0.50°  (near-critical, structure only)
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
from stage_0.solver2d_py import extract_arrays, surface_cp_from_P
from stage_1_2.solver import build_3d_state, solve_3d
from stage_1_2.postprocess import surface_cp_3d_station, spanwise_cl


# ---------------------------------------------------------------------------
# Test configuration
# ---------------------------------------------------------------------------

FULL_SPAN = 1.0     # wing span (z from 0 to 1, both ends symmetric)
N_K       = 30      # number of spanwise stations

CASES = [
    {
        'name'    : 'NACA0012_M050_A100',
        'label'   : 'NACA 0012  Ma=0.50  α=1.00°',
        'EMACH'   : 0.50, 'ALPHA': 1.00,
        'n_x'     : 120, 'n_y': 60, 'n_af': 80,
        'flag_CFS': False,
        'strict'  : True,
    },
    {
        'name'    : 'NACA0012_M070_A050',
        'label'   : 'NACA 0012  Ma=0.70  α=0.50°',
        'EMACH'   : 0.70, 'ALPHA': 0.50,
        'n_x'     : 120, 'n_y': 60, 'n_af': 80,
        'flag_CFS': True,
        'strict'  : False,
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
# JIT warm-up
# ---------------------------------------------------------------------------

_jit_warmed = False

def _warmup_jit(d):
    global _jit_warmed
    if _jit_warmed:
        return 0.0
    print("  [JIT warm-up] loading Numba cache …", end=' ', flush=True)
    t0 = time.perf_counter()
    s  = build_3d_state(d, nk=3, full_span=FULL_SPAN)
    P_w = s['P'].copy()
    solve_3d(
        P_w,
        s['C1'], s['CXL'], s['CXC'], s['CXR'],
        s['CXXL'], s['CXXC'], s['CXXR'],
        s['CYYC'], s['CYYD'], s['CYYU'],
        s['CYYBUC'], s['CYYBUU'], s['CYYBLC'], s['CYYBLD'],
        s['FXUBC'], s['FXLBC'],
        s['PJUMP_3d'], s['THETA'],
        s['VUP'], s['VDOWN'], s['VTOP'], s['VBOT'],
        s['DUP'], s['DDOWN'], s['DTOP'], s['DBOT'],
        s['X'], s['XDIFF'],
        s['dz'],
        s['AK'], s['EPS'], s['WI'], s['SONVEL'], s['VOL'],
        s['CIRCFF_3d'], s['DUB_3d'],
        s['CJUP'], s['CJUP1'], s['CJLOW'], s['CJLOW1'],
        s['imax'], s['jmax'],
        s['iup'], s['idown'], s['ile'], s['ite'],
        s['jup'], s['jlow'], s['jbot'], s['jtop'],
        MAXIT=1, CVERGE=1e-20, DVERGE=1e20, IPRTER=9999,
        WCIRC=s['WCIRC'], use_phi_zz=True, print_info=False,
    )
    t_jit = time.perf_counter() - t0
    _jit_warmed = True
    print(f"done ({t_jit:.2f} s)")
    return t_jit


def _cl_from_P_k(P_k, d, clfact):
    """Section CL at one station via trailing-edge potential jump."""
    pup  = (d['CJUP']  * P_k[d['jup'],  d['ite']]
            - d['CJUP1']  * P_k[d['jup']  + 1, d['ite']])
    plow = (d['CJLOW'] * P_k[d['jlow'], d['ite']]
            - d['CJLOW1'] * P_k[d['jlow'] - 1, d['ite']])
    return 2.0 * clfact * (pup - plow)


# ---------------------------------------------------------------------------
# Run one case
# ---------------------------------------------------------------------------

def run_case(case):
    print(f"\n{'='*70}")
    print(f"  {case['label']}   span={FULL_SPAN}  nk={N_K}")
    print(f"{'='*70}")

    coords = naca0012_coords()

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
    ile   = pts.mesh['ile']
    ite   = pts.mesh['ite']
    xx_f  = pts.mesh['xx'][ile: ite + 1]
    cpu_f = pts.data_summary['cpu'][ile: ite + 1]
    cpl_f = pts.data_summary['cpl'][ile: ite + 1]
    print(f"    CL_fort = {cl_fort:.5f}   t = {t_fort:.2f} s")

    # ================================================================
    # 2. Extract 2-D coefficient dict
    # ================================================================
    d = extract_arrays(tsf, pts)
    clfact = float(tsf.solver_data.clfact)

    _warmup_jit(d)

    # ================================================================
    # 3. Test A: strip theory (phi_zz=0) — must match 2-D
    # ================================================================
    print(f"\n[2/4] Test A — strip theory  (nk={N_K}, φ_zz=0) …")
    sA = build_3d_state(d, nk=N_K, full_span=FULL_SPAN)

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
        sA['dz'],
        sA['AK'], sA['EPS'], sA['WI'], sA['SONVEL'], sA['VOL'],
        sA['CIRCFF_3d'], sA['DUB_3d'],
        sA['CJUP'], sA['CJUP1'], sA['CJLOW'], sA['CJLOW1'],
        sA['imax'], sA['jmax'],
        sA['iup'], sA['idown'], sA['ile'], sA['ite'],
        sA['jup'], sA['jlow'], sA['jbot'], sA['jtop'],
        MAXIT=sA['MAXIT'], CVERGE=sA['CVERGE'], DVERGE=sA['DVERGE'],
        IPRTER=sA['IPRTER'], WCIRC=sA['WCIRC'],
        FLAG_CFS=sA['FLAG_CFS'], BETA_SONIC=sA['BETA_SONIC'],
        EPS_AMPL=sA['EPS_AMPL'], ITER_START_CFS=sA['ITER_START_CFS'],
        use_phi_zz=False, print_info=False,
    )
    t_A = time.perf_counter() - t0

    k_mid  = N_K // 2
    cpu_A, cpl_A, xx_A = surface_cp_3d_station(sA['P'][k_mid], d, pts)
    cl_A_mid = _cl_from_P_k(sA['P'][k_mid], d, clfact)

    max_dcp_u = np.max(np.abs(cpu_A - np.interp(xx_A, xx_f, cpu_f)))
    max_dcp_l = np.max(np.abs(cpl_A - np.interp(xx_A, xx_f, cpl_f)))
    max_dcp_A = max(max_dcp_u, max_dcp_l)
    delta_cl_A = abs(cl_A_mid - cl_fort) / max(abs(cl_fort), 1e-6)
    status_A   = 'converged' if conv_A else 'NOT converged'

    print(f"    CL_strip={cl_A_mid:.5f}  CL_fort={cl_fort:.5f}"
          f"  |ΔCL|/CL={delta_cl_A*100:.3f}%")
    print(f"    max|ΔCp|={max_dcp_A:.4f}  t={t_A:.1f}s  {n_A} iters  ({status_A})")

    passed_A = None
    if case['strict']:
        passed_A = (max_dcp_A < 0.005) and (delta_cl_A < 0.001)
        print(f"    Strip-theory  Cp {'PASS' if max_dcp_A < 0.005 else 'FAIL'}"
              f"  CL {'PASS' if delta_cl_A < 0.001 else 'FAIL'}"
              f"  → {'PASS' if passed_A else 'FAIL'}")

    # ================================================================
    # 4. Test B: full 3-D implicit phi_zz — must converge AND CL ≈ CL_2d
    # ================================================================
    print(f"\n[3/4] Test B — full 3-D, implicit φ_zz  (nk={N_K}) …")
    sB = build_3d_state(d, nk=N_K, full_span=FULL_SPAN)

    # Warm start from strip-theory solution (exact answer for infinite wing).
    # phi_zz = 0 everywhere at start → solver should converge quickly.
    for k in range(N_K):
        sB['P'][k]         = sA['P'][k_mid].copy()
        sB['CIRCFF_3d'][k] = sA['CIRCFF_3d'][k_mid]
        sB['DUB_3d'][k]    = sA['DUB_3d'][k_mid]
        sB['PJUMP_3d'][k]  = sA['PJUMP_3d'][k_mid].copy()

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
        sB['dz'],
        sB['AK'], sB['EPS'], sB['WI'], sB['SONVEL'], sB['VOL'],
        sB['CIRCFF_3d'], sB['DUB_3d'],
        sB['CJUP'], sB['CJUP1'], sB['CJLOW'], sB['CJLOW1'],
        sB['imax'], sB['jmax'],
        sB['iup'], sB['idown'], sB['ile'], sB['ite'],
        sB['jup'], sB['jlow'], sB['jbot'], sB['jtop'],
        MAXIT=sB['MAXIT'], CVERGE=sB['CVERGE'], DVERGE=sB['DVERGE'],
        IPRTER=sB['IPRTER'], WCIRC=sB['WCIRC'],
        FLAG_CFS=sB['FLAG_CFS'], BETA_SONIC=sB['BETA_SONIC'],
        EPS_AMPL=sB['EPS_AMPL'], ITER_START_CFS=sB['ITER_START_CFS'],
        use_phi_zz=True, print_info=False,
    )
    t_B = time.perf_counter() - t0

    cpu_B, cpl_B, xx_B = surface_cp_3d_station(sB['P'][k_mid], d, pts)
    cl_z_B, cl_B_mean  = spanwise_cl(sB['P'], d, pts, sB['eta'])
    cl_B_mid = _cl_from_P_k(sB['P'][k_mid], d, clfact)
    delta_cl_B = abs(cl_B_mid - cl_fort) / max(abs(cl_fort), 1e-6)
    status_B   = 'converged' if conv_B else 'NOT converged'

    print(f"    CL_mid={cl_B_mid:.5f}  CL_mean={cl_B_mean:.5f}  CL_2d={cl_fort:.5f}"
          f"  |ΔCL|/CL={delta_cl_B*100:.3f}%  t={t_B:.1f}s  {n_B} iters  ({status_B})")

    passed_B = None
    if case['strict']:
        passed_B = conv_B and (delta_cl_B < 0.005)
        print(f"    Full 3-D  converged={'PASS' if conv_B else 'FAIL'}"
              f"  |ΔCL|/CL={'PASS' if delta_cl_B < 0.005 else 'FAIL'}"
              f"  → {'PASS' if passed_B else 'FAIL'}")

    # ================================================================
    # 5. Plots
    # ================================================================
    print("\n[4/4] Saving plots …")

    fig, axes = plt.subplots(2, 2, figsize=(14, 10))
    fig.suptitle(case['label'] + f'  span={FULL_SPAN}  nk={N_K}', fontsize=12)

    ax = axes[0, 0]
    ax.plot(xx_f, cpu_f, 'b-',  lw=2.0, label='Fortran 2-D upper')
    ax.plot(xx_f, cpl_f, 'b--', lw=2.0, label='Fortran 2-D lower')
    ax.plot(xx_A, cpu_A, 'g-',  lw=1.5, label='Strip (φ_zz=0) upper')
    ax.plot(xx_A, cpl_A, 'g--', lw=1.5, label='Strip (φ_zz=0) lower')
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

    ax = axes[0, 1]
    ax.plot(xx_f, cpu_f, 'b-',  lw=2.0, label='Fortran 2-D upper')
    ax.plot(xx_f, cpl_f, 'b--', lw=2.0, label='Fortran 2-D lower')
    ax.plot(xx_B, cpu_B, 'r-',  lw=1.5, label='3-D implicit φ_zz upper')
    ax.plot(xx_B, cpl_B, 'r--', lw=1.5, label='3-D implicit φ_zz lower')
    ax.invert_yaxis()
    ax.set_xlabel('x/c'); ax.set_ylabel('Cp')
    ax.set_title(f'3-D (implicit φ_zz) vs Fortran 2-D (k={k_mid})')
    ax.legend(fontsize=7); ax.grid(True, alpha=0.3)
    conv_str = 'CONV' if conv_B else 'NO-CV'
    ax.text(0.02, 0.05,
            f"CL_mid={cl_B_mid:.4f}  CL_2d={cl_fort:.4f}\n"
            f"|ΔCL|/CL={delta_cl_B*100:.3f}%  {conv_str}",
            transform=ax.transAxes, fontsize=8,
            color='darkred', va='bottom',
            bbox=dict(fc='white', alpha=0.7))

    ax = axes[1, 0]
    ax.plot(sB['eta'], cl_z_B, 'r-o', ms=3, lw=1.5, label='3-D cl(z)')
    ax.axhline(cl_fort, color='b', ls='--', lw=1.5, label='2-D CL')
    ax.set_xlabel('z'); ax.set_ylabel('cl(z)')
    ax.set_title('Spanwise lift distribution (3-D)')
    ax.legend(fontsize=8); ax.grid(True, alpha=0.3)

    ax = axes[1, 1]
    if len(err_A) > 1:
        ax.semilogy(np.arange(1, len(err_A) + 1), err_A, 'g-',
                    lw=1.3, label='Strip (φ_zz=0)')
    if len(err_B) > 1:
        ax.semilogy(np.arange(1, len(err_B) + 1), err_B, 'r-',
                    lw=1.3, label='3-D implicit φ_zz')
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
        'name'       : case['name'],
        'label'      : case['label'],
        'strict'     : case['strict'],
        'cl_fort'    : cl_fort,
        'cl_strip'   : cl_A_mid,
        'cl_3d_mid'  : cl_B_mid,
        'cl_3d_mean' : cl_B_mean,
        'max_dcp_A'  : max_dcp_A,
        'delta_cl_A' : delta_cl_A,
        'delta_cl_B' : delta_cl_B,
        'passed_A'   : passed_A,
        'passed_B'   : passed_B,
        'conv_A'     : conv_A,
        'conv_B'     : conv_B,
        'n_A'        : n_A,
        'n_B'        : n_B,
        't_A'        : t_A,
        't_B'        : t_B,
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
           f"  {'|ΔCp|max':>8}  {'ΔCL_A%':>7}  {'ΔCL_B%':>7}  {'Strip':>6}  {'3-D'}")
    print(hdr)
    print(f"  {'-'*98}")

    all_strict = True
    for r in results:
        s_A = ('PASS' if r['passed_A'] else 'FAIL') if r['strict'] else 'n/a'
        s_B = ('PASS' if r['passed_B'] else 'FAIL') if r['strict'] else ('conv' if r['conv_B'] else 'no-cv')
        print(
            f"  {r['label']:<{W}}"
            f"  {r['cl_fort']:>7.4f}  {r['cl_strip']:>7.4f}  {r['cl_3d_mid']:>7.4f}"
            f"  {r['max_dcp_A']:>8.4f}  {r['delta_cl_A']*100:>7.3f}"
            f"  {r['delta_cl_B']*100:>7.3f}  {s_A:>6}  {s_B}"
        )
        if r['strict']:
            all_strict = all_strict and bool(r['passed_A']) and bool(r['passed_B'])

    print(f"\n  Strict-case validation: "
          f"{'ALL PASS' if all_strict else 'SOME FAIL'}")
    sys.exit(0 if all_strict else 1)
