"""
Stage 0 verification: Python/Numba SLOR vs Fortran TSFOIL.

Test matrix
-----------
Subsonic — strict PASS/FAIL (both solvers must converge to same solution):
  RAE 2822   Ma=0.50  α=0.50°   (converges ~2678 Fortran / ~2489 Python iters)
  NACA 0012  Ma=0.50  α=1.00°   (converges ~2439 Fortran / ~2350 Python iters)

Transonic — structure & timing only (CFS=True; PASS/FAIL not applied):
  RAE 2822   Ma=0.70  α=0.50°
  RAE 2822   Ma=0.80  α=1.00°
  NACA 0012  Ma=0.70  α=0.50°
  NACA 0012  Ma=0.80  α=1.25°

Pass criteria (subsonic only):
  max |ΔCp| < 0.005   (surface)
  |ΔCL| / CL_ref < 0.001
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
from stage_0.solver2d_py import solve_2d_py, extract_arrays, surface_cp_from_P


# ---------------------------------------------------------------------------
# Test case table
# ---------------------------------------------------------------------------

TEST_CASES = [
    # ---- subsonic: strict validation ----
    {
        'name'   : 'RAE2822_M050_A050',
        'label'  : 'RAE 2822   Ma=0.50  α=0.50°',
        'dat'    : os.path.join(_ROOT, 'example', 'rae2822', 'rae2822.dat'),
        'EMACH'  : 0.50, 'ALPHA': 0.50,
        'n_x'   : 120, 'n_y': 60, 'n_af': 80,
        'flag_CFS': False,
        'strict' : True,
    },
    {
        'name'   : 'NACA0012_M050_A100',
        'label'  : 'NACA 0012  Ma=0.50  α=1.00°',
        'dat'    : None,
        'EMACH'  : 0.50, 'ALPHA': 1.00,
        'n_x'   : 120, 'n_y': 60, 'n_af': 80,
        'flag_CFS': False,
        'strict' : True,
    },
    # ---- transonic: structure + timing ----
    {
        'name'   : 'RAE2822_M070_A050',
        'label'  : 'RAE 2822   Ma=0.70  α=0.50°',
        'dat'    : os.path.join(_ROOT, 'example', 'rae2822', 'rae2822.dat'),
        'EMACH'  : 0.70, 'ALPHA': 0.50,
        'n_x'   : 120, 'n_y': 60, 'n_af': 80,
        'flag_CFS': True,
        'strict' : False,
    },
    {
        'name'   : 'RAE2822_M080_A100',
        'label'  : 'RAE 2822   Ma=0.80  α=1.00°',
        'dat'    : os.path.join(_ROOT, 'example', 'rae2822', 'rae2822.dat'),
        'EMACH'  : 0.80, 'ALPHA': 1.00,
        'n_x'   : 120, 'n_y': 60, 'n_af': 80,
        'flag_CFS': True,
        'strict' : False,
    },
    {
        'name'   : 'NACA0012_M070_A050',
        'label'  : 'NACA 0012  Ma=0.70  α=0.50°',
        'dat'    : None,
        'EMACH'  : 0.70, 'ALPHA': 0.50,
        'n_x'   : 120, 'n_y': 60, 'n_af': 80,
        'flag_CFS': True,
        'strict' : False,
    },
    {
        'name'   : 'NACA0012_M080_A125',
        'label'  : 'NACA 0012  Ma=0.80  α=1.25°',
        'dat'    : None,
        'EMACH'  : 0.80, 'ALPHA': 1.25,
        'n_x'   : 120, 'n_y': 60, 'n_af': 80,
        'flag_CFS': True,
        'strict' : False,
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
# Numba JIT warmup  (called once before any timed Python solve)
# ---------------------------------------------------------------------------

_jit_warmed = False

def _warmup_jit(d, nj, ni):
    """Run 1 iteration of solve_2d_py to load JIT cache from disk."""
    global _jit_warmed
    if _jit_warmed:
        return 0.0
    P_w = np.zeros((nj, ni), dtype=np.float64)
    t0  = time.perf_counter()
    solve_2d_py(
        P_w,
        d['C1'], d['CXL'], d['CXC'], d['CXR'],
        d['CXXL'], d['CXXC'], d['CXXR'],
        d['CYYC'], d['CYYD'], d['CYYU'],
        d['CYYBUC'], d['CYYBUU'], d['CYYBLC'], d['CYYBLD'],
        d['FXUBC'], d['FXLBC'], d['PJUMP'],
        d['THETA'],
        d['VUP'], d['VDOWN'], d['VTOP'], d['VBOT'],
        d['DUP'], d['DDOWN'], d['DTOP'], d['DBOT'],
        d['X'], d['XDIFF'],
        d['AK'], d['EPS'], d['WI'], d['SONVEL'], d['VOL'],
        np.array([0.0]), np.array([0.0]),
        d['CJUP'], d['CJUP1'], d['CJLOW'], d['CJLOW1'],
        d['imax'], d['jmax'],
        d['iup'], d['idown'], d['ile'], d['ite'],
        d['jup'], d['jlow'], d['jbot'], d['jtop'],
        MAXIT=1, CVERGE=1e-20, DVERGE=1e20,
        IPRTER=9999, WCIRC=d['WCIRC'],
        FLAG_CFS=d['FLAG_CFS'],
        BETA_SONIC=d['BETA_SONIC'],
        EPS_AMPL=d['EPS_AMPL'],
        ITER_START_CFS=d['ITER_START_CFS'],
        print_info=False,
    )
    t_jit = time.perf_counter() - t0
    _jit_warmed = True
    return t_jit


# ---------------------------------------------------------------------------
# Run one test case
# ---------------------------------------------------------------------------

def run_case(case):
    print(f"\n{'='*62}")
    print(f"  {case['label']}   [{'strict' if case['strict'] else 'structure'}]")
    print(f"{'='*62}")

    coords = (np.loadtxt(case['dat'], skiprows=1)
              if case['dat'] is not None else naca0012_coords())

    # ================================================================
    # 1. Fortran reference
    # ================================================================
    print("\n[1/3] Fortran TSFOIL ...")

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

    t0      = time.perf_counter()
    pts.run_fortran_solver()
    t_fort  = time.perf_counter() - t0

    pts.compute_data_summary()
    cl_fort = pts.data_summary['cl']

    pts.config['flag_output_shock']   = False
    pts.config['flag_output_summary'] = False
    pts.output_surface()
    ile      = pts.mesh['ile']
    ite      = pts.mesh['ite']
    xx_f     = pts.mesh['xx'][ile: ite + 1]
    cpu_fort = pts.data_summary['cpu'][ile: ite + 1]
    cpl_fort = pts.data_summary['cpl'][ile: ite + 1]

    print(f"    CL = {cl_fort:.5f}   t = {t_fort:.2f}s")

    # ================================================================
    # 2. Python / Numba SLOR
    # ================================================================
    print("[2/3] Python / Numba SLOR ...")

    d   = extract_arrays(tsf, pts)
    ni  = d['imax'] + 1
    nj  = d['jmax'] + 1

    # JIT warmup (load compiled cache, one-time cost)
    t_jit = _warmup_jit(d, nj, ni)
    if t_jit > 0:
        print(f"    JIT cache load: {t_jit:.3f}s  (one-time)")

    P_py       = np.zeros((nj, ni), dtype=np.float64)
    CIRCFF_arr = np.array([0.0], dtype=np.float64)
    DUB_arr    = np.array([0.0], dtype=np.float64)

    t0 = time.perf_counter()
    converged, n_iter, err_hist = solve_2d_py(
        P_py,
        d['C1'], d['CXL'], d['CXC'], d['CXR'],
        d['CXXL'], d['CXXC'], d['CXXR'],
        d['CYYC'], d['CYYD'], d['CYYU'],
        d['CYYBUC'], d['CYYBUU'], d['CYYBLC'], d['CYYBLD'],
        d['FXUBC'], d['FXLBC'], d['PJUMP'],
        d['THETA'],
        d['VUP'], d['VDOWN'], d['VTOP'], d['VBOT'],
        d['DUP'], d['DDOWN'], d['DTOP'], d['DBOT'],
        d['X'], d['XDIFF'],
        d['AK'], d['EPS'], d['WI'], d['SONVEL'], d['VOL'],
        CIRCFF_arr, DUB_arr,
        d['CJUP'], d['CJUP1'], d['CJLOW'], d['CJLOW1'],
        d['imax'], d['jmax'],
        d['iup'], d['idown'], d['ile'], d['ite'],
        d['jup'], d['jlow'], d['jbot'], d['jtop'],
        MAXIT=d['MAXIT'], CVERGE=d['CVERGE'], DVERGE=d['DVERGE'],
        IPRTER=d['IPRTER'], WCIRC=d['WCIRC'],
        FLAG_CFS=d['FLAG_CFS'],
        BETA_SONIC=d['BETA_SONIC'], EPS_AMPL=d['EPS_AMPL'],
        ITER_START_CFS=d['ITER_START_CFS'],
        print_info=False,
    )
    t_py = time.perf_counter() - t0

    status_str = 'converged' if converged else 'NOT converged'
    print(f"    CL_py = {2.0 * float(tsf.solver_data.clfact) * (d['CJUP'] * P_py[d['jup'], d['ite']] - d['CJUP1'] * P_py[d['jup']+1, d['ite']] - d['CJLOW'] * P_py[d['jlow'], d['ite']] + d['CJLOW1'] * P_py[d['jlow']-1, d['ite']]):.5f}"
          f"   t = {t_py:.2f}s   {n_iter} iters  ({status_str})")
    print(f"    Speed ratio  Python/Fortran = {t_py/t_fort:.2f}×"
          f"   ({t_py/n_iter*1000:.2f} ms/iter Python,"
          f"  ~{t_fort/n_iter*1000:.2f} ms/iter Fortran est.)")

    pup_te  = d['CJUP']  * P_py[d['jup'],  d['ite']] - d['CJUP1']  * P_py[d['jup']  + 1, d['ite']]
    plow_te = d['CJLOW'] * P_py[d['jlow'], d['ite']] - d['CJLOW1'] * P_py[d['jlow'] - 1, d['ite']]
    clfact  = float(tsf.solver_data.clfact)
    cl_py   = 2.0 * clfact * (pup_te - plow_te)
    cpu_py, cpl_py, xx_py = surface_cp_from_P(P_py, d, pts)

    # ================================================================
    # 3. Compare
    # ================================================================
    print("[3/3] Comparison ...")

    cpu_fi = np.interp(xx_py, xx_f, cpu_fort)
    cpl_fi = np.interp(xx_py, xx_f, cpl_fort)
    max_dcp_u = np.max(np.abs(cpu_py - cpu_fi))
    max_dcp_l = np.max(np.abs(cpl_py - cpl_fi))
    max_dcp   = max(max_dcp_u, max_dcp_l)
    delta_cl  = abs(cl_py - cl_fort) / max(abs(cl_fort), 1e-6)

    print(f"    max|ΔCp| = {max_dcp:.4f}  (upper {max_dcp_u:.4f} / lower {max_dcp_l:.4f})")
    print(f"    |ΔCL|/CL = {delta_cl*100:.3f}%")

    passed = None
    if case['strict']:
        passed = (max_dcp < 0.005) and (delta_cl < 0.001)
        print(f"    Cp {'PASS' if max_dcp < 0.005 else 'FAIL'}  "
              f"CL {'PASS' if delta_cl < 0.001 else 'FAIL'}  "
              f"→  {'PASS' if passed else 'FAIL'}")

    # ================================================================
    # 4. Plot: Cp comparison + convergence history
    # ================================================================
    fig, axes = plt.subplots(1, 2, figsize=(13, 5))
    fig.suptitle(case['label'] +
                 (f'  [PASS]' if passed is True else
                  f'  [FAIL]' if passed is False else
                  f'  [structure]'), fontsize=12)

    ax = axes[0]
    ax.plot(xx_f,  cpu_fort, 'b-',  lw=2.0, label='Fortran upper')
    ax.plot(xx_f,  cpl_fort, 'b--', lw=2.0, label='Fortran lower')
    py_label = 'Python' + ('' if converged else ' (not conv.)')
    ax.plot(xx_py, cpu_py,  'r-',  lw=1.5, label=f'{py_label} upper')
    ax.plot(xx_py, cpl_py,  'r--', lw=1.5, label=f'{py_label} lower')
    ax.invert_yaxis()
    ax.set_xlabel('x/c'); ax.set_ylabel('Cp')
    ax.set_title('Surface Cp')
    ax.legend(fontsize=8); ax.grid(True, alpha=0.3)

    ax = axes[1]
    if len(err_hist) > 1:
        ax.semilogy(np.arange(1, len(err_hist)+1), err_hist, 'r-', lw=1.3,
                    label='Python SLOR')
    ax.axhline(d['CVERGE'], color='k', ls=':', lw=1, label=f"CVERGE={d['CVERGE']:.0e}")
    ax.set_xlabel('Iteration'); ax.set_ylabel('max|ΔP| per sweep')
    ax.set_title('Python convergence history')
    ax.legend(fontsize=8); ax.grid(True, alpha=0.3)

    # timing annotation
    txt = (f"Fortran : {t_fort:.2f}s  (MAXIT={d['MAXIT']})\n"
           f"Python  : {t_py:.2f}s  ({n_iter} iters, {status_str})\n"
           f"Ratio   : {t_py/t_fort:.2f}×\n"
           f"ms/iter : {t_py/n_iter*1000:.2f} (Py) / ~{t_fort/n_iter*1000:.2f} (Ft)")
    axes[1].text(0.98, 0.97, txt, transform=axes[1].transAxes,
                 fontsize=7, va='top', ha='right',
                 bbox=dict(boxstyle='round', fc='wheat', alpha=0.7))

    fig.tight_layout()
    out_png = os.path.join(_HERE, f"result_{case['name']}.png")
    fig.savefig(out_png, dpi=120)
    plt.close(fig)
    print(f"    Saved → {out_png}")

    return {
        'name'     : case['name'],
        'label'    : case['label'],
        'strict'   : case['strict'],
        'cl_fort'  : cl_fort,
        'cl_py'    : cl_py,
        'delta_cl' : delta_cl,
        'max_dcp'  : max_dcp,
        'converged': converged,
        'n_iter'   : n_iter,
        'passed'   : passed,
        't_fort'   : t_fort,
        't_py'     : t_py,
    }


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

if __name__ == '__main__':
    results = []
    for case in TEST_CASES:
        r = run_case(case)
        results.append(r)

    W = 36
    print(f"\n{'='*90}")
    print("  SUMMARY")
    print(f"{'='*90}")
    hdr = (f"  {'Case':<{W}}  {'CL_f':>8}  {'CL_py':>8}  {'ΔCL%':>6}"
           f"  {'maxΔCp':>7}  {'t_ft(s)':>8}  {'t_py(s)':>8}  {'ratio':>6}  {'Result'}")
    print(hdr)
    print(f"  {'-'*88}")

    strict_all = True
    for r in results:
        if r['strict']:
            res_str = 'PASS' if r['passed'] else 'FAIL'
            strict_all = strict_all and r['passed']
        else:
            res_str = 'conv' if r['converged'] else 'no-conv'
        print(f"  {r['label']:<{W}}  {r['cl_fort']:>8.5f}  {r['cl_py']:>8.5f}"
              f"  {r['delta_cl']*100:>6.3f}%  {r['max_dcp']:>7.4f}"
              f"  {r['t_fort']:>8.2f}  {r['t_py']:>8.2f}"
              f"  {r['t_py']/r['t_fort']:>5.1f}×  {res_str}")

    print(f"\n  Strict validation (subsonic): {'ALL PASS' if strict_all else 'SOME FAIL'}")
    sys.exit(0 if strict_all else 1)
