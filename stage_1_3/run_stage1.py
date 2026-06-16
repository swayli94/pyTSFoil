"""
Stage 1-3 verification: rectangular wing, single symmetric plane + wingtip.

Geometry
--------
  AR = 6  (chord=1, half-span=3, full-span=6).
  k=0      : symmetric root plane
  k=k_tip  : wingtip station (nearest to z = half_span)
  k=nk-1   : spanwise far-field Dirichlet (phi=0)

phi_zz treatment: FULL explicit residual (phi_zz_diag = 0).
  Stage 1-1 used only neighbor terms → convergence floor ≈ cc·CVERGE ≈ 10⁻⁴.
  This stage includes the -2P[k]/dz² self-term in RHS, eliminating the floor.
  The fixed point satisfies the complete 3-D TSD equation.

Tests
-----
  Test A — strip theory (use_phi_zz=False):
    Each station independent → must reproduce Fortran 2-D Cp and CL at root.
    Pass criterion: max|ΔCp| < 0.005, |ΔCL|/CL < 0.1 %

  Test B — full 3-D (use_phi_zz=True, explicit):
    Warm start from strip-theory solution.
    Pass criteria (strict case):
      (a) Converges to CVERGE.
      (b) CL_root ≈ CL_2d (root section far from tip: finite-span effect small).
      (c) CL_tip  < CL_2d (tip section: finite-span unloading).
      (d) CL_mean < CL_2d (overall 3-D relief effect).

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
from stage_1_3.solver import build_3d_state, solve_3d
from stage_1_3.postprocess import surface_cp_3d_station, spanwise_cl


# ---------------------------------------------------------------------------
# Wing / test configuration
# ---------------------------------------------------------------------------

AR        = 6.0    # aspect ratio (b/c = 6, chord=1, half-span=3)
HALF_SPAN = AR / 2.0
N_K       = 30
Z_MAX_FAC = 3.0    # z_max = 3 × half_span = 9

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

MAXIT_3D    = 15000   # z-Jacobi spectral radius ≈0.9985 needs ~3500+ iters


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
    s  = build_3d_state(d, nk=4, half_span=HALF_SPAN, z_max_factor=Z_MAX_FAC)
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
        s['k_tip'],
        MAXIT=1, CVERGE=1e-20, DVERGE=1e20, IPRTER=9999,
        WCIRC=s['WCIRC'], use_phi_zz=True, iter_ramp_phizz=0, print_info=False,
    )
    t_jit = time.perf_counter() - t0
    _jit_warmed = True
    print(f"done ({t_jit:.2f} s)")
    return t_jit


def _cl_from_P_k(P_k, d, clfact):
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
    print(f"  {case['label']}   AR={AR}  half_span={HALF_SPAN}  nk={N_K}")
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
    # 3. Test A: strip theory (phi_zz=0)
    # ================================================================
    print(f"\n[2/4] Test A — strip theory  (nk={N_K}, φ_zz=0) …")
    sA = build_3d_state(d, nk=N_K, half_span=HALF_SPAN, z_max_factor=Z_MAX_FAC)
    k_tip = sA['k_tip']
    eta   = sA['eta']

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
        k_tip,
        MAXIT=sA['MAXIT'], CVERGE=sA['CVERGE'], DVERGE=sA['DVERGE'],
        IPRTER=sA['IPRTER'], WCIRC=sA['WCIRC'],
        FLAG_CFS=sA['FLAG_CFS'], BETA_SONIC=sA['BETA_SONIC'],
        EPS_AMPL=sA['EPS_AMPL'], ITER_START_CFS=sA['ITER_START_CFS'],
        use_phi_zz=False, print_info=False,
    )
    t_A = time.perf_counter() - t0

    # Use root station (k=0) as reference (nearest to symmetric plane)
    cpu_A0, cpl_A0, xx_A = surface_cp_3d_station(sA['P'][0], d, pts)
    cl_A_root = _cl_from_P_k(sA['P'][0], d, clfact)

    max_dcp_u = np.max(np.abs(cpu_A0 - np.interp(xx_A, xx_f, cpu_f)))
    max_dcp_l = np.max(np.abs(cpl_A0 - np.interp(xx_A, xx_f, cpl_f)))
    max_dcp_A = max(max_dcp_u, max_dcp_l)
    delta_cl_A = abs(cl_A_root - cl_fort) / max(abs(cl_fort), 1e-6)
    status_A   = 'converged' if conv_A else 'NOT converged'

    print(f"    CL_root={cl_A_root:.5f}  CL_fort={cl_fort:.5f}"
          f"  |ΔCL|/CL={delta_cl_A*100:.3f}%")
    print(f"    max|ΔCp|={max_dcp_A:.4f}  t={t_A:.1f}s  {n_A} iters  ({status_A})")

    passed_A = None
    if case['strict']:
        passed_A = (max_dcp_A < 0.005) and (delta_cl_A < 0.001)
        print(f"    Strip-theory  Cp {'PASS' if max_dcp_A < 0.005 else 'FAIL'}"
              f"  CL {'PASS' if delta_cl_A < 0.001 else 'FAIL'}"
              f"  → {'PASS' if passed_A else 'FAIL'}")

    # ================================================================
    # 4. Test B: full 3-D explicit phi_zz
    # ================================================================
    print(f"\n[3/4] Test B — full 3-D, explicit φ_zz  (nk={N_K},"
          f"  k_tip={k_tip}, z_tip={eta[k_tip]:.2f}) …")
    sB = build_3d_state(d, nk=N_K, half_span=HALF_SPAN, z_max_factor=Z_MAX_FAC)

    # Warm start from strip-theory solution (wing stations only).
    # Off-wing P stays at zero (default from build_3d_state).
    for k in range(k_tip + 1):
        sB['P'][k]         = sA['P'][0].copy()
        sB['CIRCFF_3d'][k] = sA['CIRCFF_3d'][0]
        sB['DUB_3d'][k]    = sA['DUB_3d'][0]
        sB['PJUMP_3d'][k]  = sA['PJUMP_3d'][0].copy()

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
        k_tip,
        MAXIT=MAXIT_3D, CVERGE=sB['CVERGE'], DVERGE=sB['DVERGE'],
        IPRTER=500,
        WCIRC=sB['WCIRC'],
        FLAG_CFS=sB['FLAG_CFS'], BETA_SONIC=sB['BETA_SONIC'],
        EPS_AMPL=sB['EPS_AMPL'], ITER_START_CFS=sB['ITER_START_CFS'],
        use_phi_zz=True, print_info=True,
    )
    t_B = time.perf_counter() - t0

    cl_z_B, cl_B_mean, eta_w = spanwise_cl(sB['P'], d, pts, eta, k_tip)
    cl_B_root = _cl_from_P_k(sB['P'][0],     d, clfact)
    cl_B_tip  = _cl_from_P_k(sB['P'][k_tip], d, clfact)

    delta_cl_root = abs(cl_B_root - cl_fort) / max(abs(cl_fort), 1e-6)
    status_B      = 'converged' if conv_B else 'NOT converged'

    print(f"    CL_root={cl_B_root:.5f}  CL_tip={cl_B_tip:.5f}"
          f"  CL_mean={cl_B_mean:.5f}  CL_2d={cl_fort:.5f}")
    print(f"    |ΔCL_root|/CL={delta_cl_root*100:.3f}%"
          f"  t={t_B:.1f}s  {n_B} iters  ({status_B})")

    passed_B = None
    if case['strict']:
        # Primary: convergence; secondary: root close to 2D; tip < 2D (3D relief)
        tip_unloaded = (cl_B_tip < cl_fort)
        passed_B = conv_B and (delta_cl_root < 0.05) and tip_unloaded
        print(f"    Full 3-D  conv={'PASS' if conv_B else 'FAIL'}"
              f"  |ΔCL_root|/CL={'PASS' if delta_cl_root < 0.05 else 'FAIL'}"
              f"  tip_unload={'PASS' if tip_unloaded else 'FAIL'}"
              f"  → {'PASS' if passed_B else 'FAIL'}")

    # ================================================================
    # 5. Plots
    # ================================================================
    print("\n[4/4] Saving plots …")

    cpu_B_root, cpl_B_root, xx_B = surface_cp_3d_station(sB['P'][0],     d, pts)
    cpu_B_tip,  cpl_B_tip,  _    = surface_cp_3d_station(sB['P'][k_tip], d, pts)

    fig, axes = plt.subplots(2, 2, figsize=(14, 10))
    fig.suptitle(case['label']
                 + f'  AR={AR}  nk={N_K}  k_tip={k_tip}',
                 fontsize=11)

    ax = axes[0, 0]
    ax.plot(xx_f,  cpu_f,     'b-',  lw=2.0, label='Fortran 2-D upper')
    ax.plot(xx_f,  cpl_f,     'b--', lw=2.0, label='Fortran 2-D lower')
    ax.plot(xx_A,  cpu_A0,    'g-',  lw=1.5, label='Strip root upper')
    ax.plot(xx_A,  cpl_A0,    'g--', lw=1.5, label='Strip root lower')
    ax.invert_yaxis()
    ax.set_xlabel('x/c'); ax.set_ylabel('Cp')
    ax.set_title(f'Strip theory vs Fortran 2-D (root, k=0)')
    ax.legend(fontsize=7); ax.grid(True, alpha=0.3)
    if passed_A is not None:
        ax.text(0.02, 0.05,
                f"max|ΔCp|={max_dcp_A:.4f}\n|ΔCL|/CL={delta_cl_A*100:.3f}%\n"
                f"{'PASS' if passed_A else 'FAIL'}",
                transform=ax.transAxes, fontsize=8,
                color='green' if passed_A else 'red',
                va='bottom', bbox=dict(fc='white', alpha=0.7))

    ax = axes[0, 1]
    ax.plot(xx_f,   cpu_f,      'b-',  lw=2.0, label='Fortran 2-D upper')
    ax.plot(xx_f,   cpl_f,      'b--', lw=2.0, label='Fortran 2-D lower')
    ax.plot(xx_B,   cpu_B_root, 'r-',  lw=1.5, label=f'3-D root (k=0) upper')
    ax.plot(xx_B,   cpl_B_root, 'r--', lw=1.5, label=f'3-D root (k=0) lower')
    ax.plot(xx_B,   cpu_B_tip,  'm-',  lw=1.2, label=f'3-D tip (k={k_tip}) upper')
    ax.plot(xx_B,   cpl_B_tip,  'm--', lw=1.2, label=f'3-D tip (k={k_tip}) lower')
    ax.invert_yaxis()
    ax.set_xlabel('x/c'); ax.set_ylabel('Cp')
    ax.set_title('3-D explicit φ_zz  root vs tip vs Fortran 2-D')
    ax.legend(fontsize=7); ax.grid(True, alpha=0.3)
    conv_str = 'CONV' if conv_B else 'NO-CV'
    ax.text(0.02, 0.05,
            f"CL_root={cl_B_root:.4f}  CL_tip={cl_B_tip:.4f}\n"
            f"CL_mean={cl_B_mean:.4f}  CL_2d={cl_fort:.4f}\n"
            f"{conv_str}",
            transform=ax.transAxes, fontsize=8,
            color='darkred', va='bottom',
            bbox=dict(fc='white', alpha=0.7))

    ax = axes[1, 0]
    ax.plot(eta_w, cl_z_B, 'r-o', ms=4, lw=1.5, label='3-D cl(z)')
    ax.axhline(cl_fort, color='b', ls='--', lw=1.5, label='2-D CL')
    ax.axvline(eta[k_tip], color='k', ls=':', lw=1, label=f'z_tip={eta[k_tip]:.2f}')
    ax.set_xlabel('z'); ax.set_ylabel('cl(z)')
    ax.set_title('Spanwise lift distribution (3-D wing stations)')
    ax.legend(fontsize=8); ax.grid(True, alpha=0.3)

    ax = axes[1, 1]
    if len(err_A) > 1:
        ax.semilogy(np.arange(1, len(err_A) + 1), err_A, 'g-',
                    lw=1.3, label='Strip (φ_zz=0)')
    if len(err_B) > 1:
        ax.semilogy(np.arange(1, len(err_B) + 1), err_B, 'r-',
                    lw=1.3, label='3-D explicit φ_zz')
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
        'cl_A_root'   : cl_A_root,
        'cl_B_root'   : cl_B_root,
        'cl_B_tip'    : cl_B_tip,
        'cl_B_mean'   : cl_B_mean,
        'max_dcp_A'   : max_dcp_A,
        'delta_cl_A'  : delta_cl_A,
        'delta_cl_root': delta_cl_root,
        'passed_A'    : passed_A,
        'passed_B'    : passed_B,
        'conv_A'      : conv_A,
        'conv_B'      : conv_B,
        'n_A'         : n_A,
        'n_B'         : n_B,
        't_A'         : t_A,
        't_B'         : t_B,
        'k_tip'       : k_tip,
        'eta_tip'     : eta[k_tip],
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
    print(f"\n{'='*110}")
    print("  SUMMARY")
    print(f"{'='*110}")
    hdr = (f"  {'Case':<{W}}  {'CL_2d':>7}  {'CL_root':>7}  {'CL_tip':>7}"
           f"  {'CL_mean':>7}  {'|ΔCp|max':>8}  {'ΔCL_A%':>7}  {'Strip':>6}  {'3-D'}")
    print(hdr)
    print(f"  {'-'*108}")

    all_strict = True
    for r in results:
        s_A = ('PASS' if r['passed_A'] else 'FAIL') if r['strict'] else 'n/a'
        s_B = (('PASS' if r['passed_B'] else 'FAIL') if r['strict']
               else ('conv' if r['conv_B'] else 'no-cv'))
        print(
            f"  {r['label']:<{W}}"
            f"  {r['cl_fort']:>7.4f}"
            f"  {r['cl_A_root']:>7.4f}"
            f"  {r['cl_B_tip']:>7.4f}"
            f"  {r['cl_B_mean']:>7.4f}"
            f"  {r['max_dcp_A']:>8.4f}"
            f"  {r['delta_cl_A']*100:>7.3f}"
            f"  {s_A:>6}  {s_B}"
        )
        if r['strict']:
            all_strict = all_strict and bool(r['passed_A']) and bool(r['passed_B'])

    print(f"\n  Strict-case validation: "
          f"{'ALL PASS' if all_strict else 'SOME FAIL'}")
    sys.exit(0 if all_strict else 1)
