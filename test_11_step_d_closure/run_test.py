"""
Test 11: Step D LE closure — full singularity subtraction (Steps A+C+D+E).

Adds Step D (body-BC regularisation) with three LE-closure extrapolation methods:
  'constant'  — phi_ry[0] = phi_ry[i_eff]   (first effective point, O(sqrt(x)) bias)
  'linear'    — fit phi_ry = A + B*x,         extrapolate intercept A to x=0
  'sqrt_fit'  — fit phi_ry = A + B*sqrt(x),   extrapolate intercept A to x=0
                (default; matches theoretical near-LE behaviour phi_ry ~ c1+c2*sqrt(x))

Modes compared per case:
  baseline           — no LE correction
  composite          — Task 6 composite only
  sing_sub           — A+C+E only (Task 9)
  sing_D_<method>    — A+C+D+E with each closure method
  full               — A+C+E + composite
  full_D_<method>    — A+C+D+E + composite with each closure method

Figures:
  _bc.png      : FXU / phi_sy / phi_ry with all three closure methods
  _cp.png      : Cp full-chord for all modes
  summary.png  : nMa0 and rmCp bar charts

Key findings documented in doc/improve-progress-2.md §11.
"""

import os
import sys
import copy
import numpy as np
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt

path      = os.path.abspath(os.path.dirname(__file__))
path_root = os.path.abspath(os.path.join(path, '..'))
sys.path.insert(0, path_root)

from pytsfoil import PyTSFoil
from airfoil_database.utils import load_airfoil_database_from_json
from pytsfoil.leading_edge import compute_surface_corrections, apply_step_d_le_closure

try:
    import tsfoil_fortran as tsf
except ImportError:
    tsf = None

GAMMA     = 1.4
fname_db  = os.path.join(path_root, 'airfoil_database', 'airfoil_database.json')
path_figs = os.path.join(path, 'figures')
os.makedirs(path_figs, exist_ok=True)

BASE_CFG = {
    'CVERGE':  1e-5,  'DVERGE':  10.0,  'EPS':    0.5,
    'IPRTER':  100,   'MAXIT':   9999,  'RIGF':   0.0,
    'SIMDEF':  3,     'WCIRC':   1.0,   'WE':     [1.8, 1.9, 1.95],
    'NWDGE':   0,     'WCONST':  4.0,   'IFLAP':  0,
    'DELFLP':  0.0,   'FLPLOC':  0.77,
    'n_point_x': 200, 'n_point_y': 80,  'n_point_airfoil': 100,
    'flag_output': False, 'flag_output_summary': False,
    'flag_output_shock': False, 'flag_output_field': False,
    'flag_print_info': False,
}

D_METHODS = ('constant', 'linear', 'sqrt_fit')

MODES_BASE = {
    'baseline':  {'apply_singularity_subtraction': False, 'apply_step_d': False, 'apply_le_correction': False},
    'composite': {'apply_singularity_subtraction': False, 'apply_step_d': False, 'apply_le_correction': True},
    'sing_sub':  {'apply_singularity_subtraction': True,  'apply_step_d': False, 'apply_le_correction': False},
    'full':      {'apply_singularity_subtraction': True,  'apply_step_d': False, 'apply_le_correction': True},
}
# step-D variants are added dynamically per method

COLOURS = {
    'baseline':  'steelblue',
    'composite': 'forestgreen',
    'sing_sub':  'gray',
    'full':      'crimson',
    'constant':  'orange',
    'linear':    'gold',
    'sqrt_fit':  'darkorange',
}
COLOURS_D = {
    'sing_D_constant': 'orange',
    'sing_D_linear':   'gold',
    'sing_D_sqrt_fit': 'darkorange',
    'full_D_constant': 'mediumpurple',
    'full_D_linear':   'violet',
    'full_D_sqrt_fit': 'darkviolet',
}
ALL_COLOURS = {**COLOURS, **COLOURS_D}

STYLES_D = {
    'sing_D_constant': '-.',
    'sing_D_linear':   '-.',
    'sing_D_sqrt_fit': '-.',
    'full_D_constant': '-',
    'full_D_linear':   '-',
    'full_D_sqrt_fit': '-',
}


# ── helpers ────────────────────────────────────────────────────────────────────

def _rmse_cp(ts, airfoil):
    from scipy.interpolate import interp1d
    xx  = ts.mesh['xx']
    f_u = interp1d(xx, ts.data_summary['cpu'], kind='linear', fill_value='extrapolate')
    f_l = interp1d(xx, ts.data_summary['cpl'], kind='linear', fill_value='extrapolate')
    return float(np.sqrt(0.5 * (
        np.mean((f_u(airfoil['xu']) - airfoil['cpu']) ** 2) +
        np.mean((f_l(airfoil['xl']) - airfoil['cpl']) ** 2))))


def _n_ma0(ts):
    ile = ts.mesh['ile']; ite = ts.mesh['ite']
    return int(np.sum(ts.data_summary['mal'][ile:ite + 1] == 0.0))


def _build_coords(airfoil):
    from cst_modeling.section import cst_foil
    x, yu, yl, _, _ = cst_foil(201, airfoil['cst_u'], airfoil['cst_l'],
                                 x=None, t=airfoil['tmax'], tail=0.0)
    return np.column_stack((np.concatenate([x[::-1], x[1:]]),
                             np.concatenate([yu[::-1], yl[1:]])))


def _run_mode(airfoil, coords, extra_cfg):
    cfg = copy.deepcopy(BASE_CFG)
    cfg.update({'ALPHA': airfoil['AoA'], 'EMACH': airfoil['Ma'], 'REYNLD': airfoil['Re']})
    cfg.update(extra_cfg)
    ts = PyTSFoil(airfoil_coordinates=coords, work_dir=path)
    ts.set_config(**cfg)
    ts.run()
    return ts


# ── per-case run ───────────────────────────────────────────────────────────────

def run_case(airfoil):
    idx = airfoil['entry_index']
    Ma  = airfoil['Ma']
    AoA = airfoil['AoA']
    print(f"  Case {idx}  Ma={Ma:.3f}  AoA={AoA:.2f}°", flush=True)

    coords = _build_coords(airfoil)
    results = {}

    # Base modes
    for mode, extra in MODES_BASE.items():
        ts = _run_mode(airfoil, coords, extra)
        results[mode] = {
            'xx':      ts.mesh['xx'],
            'cpu':     ts.data_summary['cpu'],
            'cpl':     ts.data_summary['cpl'],
            'n_ma0':   _n_ma0(ts),
            'rmse_cp': _rmse_cp(ts, airfoil),
        }
        print(f"    {mode:12s}  nMa0={results[mode]['n_ma0']:3d}  rmCp={results[mode]['rmse_cp']:.4f}")

    # Step-D modes — one solve per method
    for m in D_METHODS:
        for prefix, le in (('sing_D', False), ('full_D', True)):
            key = f'{prefix}_{m}'
            extra = {
                'apply_singularity_subtraction': True,
                'apply_step_d': True,
                'step_d_method': m,
                'apply_le_correction': le,
            }
            ts = _run_mode(airfoil, coords, extra)
            results[key] = {
                'xx':      ts.mesh['xx'],
                'cpu':     ts.data_summary['cpu'],
                'cpl':     ts.data_summary['cpl'],
                'n_ma0':   _n_ma0(ts),
                'rmse_cp': _rmse_cp(ts, airfoil),
            }
            print(f"    {key:22s}  nMa0={results[key]['n_ma0']:3d}  rmCp={results[key]['rmse_cp']:.4f}")

    # ── BC diagnostics from baseline state ────────────────────────────────────
    ts_b   = _run_mode(airfoil, coords, {'apply_singularity_subtraction': False,
                                          'apply_step_d': False, 'apply_le_correction': False})
    nfoil  = ts_b.mesh['nfoil']
    x_foil = ts_b.mesh['xx_airfoil'][:nfoil]
    R_c    = ts_b.airfoil.get('R_c')
    h      = ts_b.airfoil.get('h_nose')
    delta  = float(tsf.common_data.delta) if tsf else airfoil['tmax']
    cpfact = ts_b._cpfact
    gamma  = ts_b._gamma

    diag = {'x_foil': x_foil, 'R_c': R_c}
    if tsf is not None and R_c is not None and h is not None and R_c > 0:
        fxu = np.array(tsf.common_data.fxu[:nfoil], dtype=float)
        fxl = np.array(tsf.common_data.fxl[:nfoil], dtype=float)
        phi_sy, _ = compute_surface_corrections(x_foil, h, delta, R_c, cpfact, gamma)
        phi_sy = phi_sy.astype(float)

        diag['fxu'] = fxu
        diag['fxl'] = fxl
        diag['phi_sy'] = phi_sy
        diag['phi_ry_raw'] = fxu - phi_sy

        for m in D_METHODS:
            ry_u, _ = apply_step_d_le_closure(fxu, fxl, phi_sy, x_foil, method=m)
            diag[f'phi_ry_{m}'] = ry_u.astype(float)

    _plot_bc(idx, Ma, AoA, diag)
    _plot_cp(idx, Ma, AoA, airfoil, results)

    return idx, results


# ── plotting ───────────────────────────────────────────────────────────────────

def _plot_bc(idx, Ma, AoA, diag):
    """FXU, phi_sy, and phi_ry for each closure method."""
    xf  = diag.get('x_foil')
    R_c = diag.get('R_c') or 1e-3
    if xf is None:
        return
    mask = xf <= min(0.12, 25 * R_c)

    fig, axes = plt.subplots(1, 2, figsize=(14, 5))
    fig.suptitle(
        f"Case {idx} | Ma={Ma:.3f}, AoA={AoA:.2f}°"
        f" — Step-D BC closure methods  (R_c={R_c:.4f})", fontsize=11)

    ax = axes[0]
    if 'fxu' in diag:
        ax.plot(xf[mask], diag['fxu'][mask], 'b-',  lw=2, label='FXU (upper body slope/δ)')
        ax.plot(xf[mask], diag['fxl'][mask], 'r-',  lw=2, label='FXL (lower body slope/δ)')
        ax.plot(xf[mask],  diag['phi_sy'][mask], 'g--', lw=2, label='φ_s,y upper')
        ax.plot(xf[mask], -diag['phi_sy'][mask], 'g--', lw=2)
    ax.axhline(0, color='gray', lw=0.5)
    ax.set(title='FXU/FXL vs φ_s,y', xlabel='x/c', ylabel='slope/δ')
    ax.set_xlim(-0.001, xf[mask].max()); ax.legend(fontsize=8); ax.grid()

    ax = axes[1]
    if 'phi_ry_raw' in diag:
        ax.plot(xf[mask], diag['phi_ry_raw'][mask], 'k--', lw=1.5,
                label='φ_r,y raw (= FXU − φ_s,y, spike at x=0)')
        clrs = {'constant': 'orange', 'linear': 'gold', 'sqrt_fit': 'darkorange'}
        lws  = {'constant': 1.5,      'linear': 2.0,    'sqrt_fit': 2.5}
        for m in D_METHODS:
            key = f'phi_ry_{m}'
            if key in diag:
                ax.plot(xf[mask], diag[key][mask], color=clrs[m], lw=lws[m],
                        label=f'φ_r,y after closure ({m})')
    ax.axhline(0, color='gray', lw=0.5)
    ax.set(title='Residual BC φ_r,y — three closure methods',
           xlabel='x/c', ylabel='residual slope/δ')
    ax.set_xlim(-0.001, xf[mask].max()); ax.legend(fontsize=8); ax.grid()

    fig.tight_layout()
    fig.savefig(os.path.join(path_figs, f'case_{idx:04d}_bc.png'), dpi=100)
    plt.close(fig)


def _plot_cp(idx, Ma, AoA, airfoil, results):
    """Cp: reference modes (left) and step-D modes by method (right)."""
    fig, axes = plt.subplots(1, 2, figsize=(16, 5))
    fig.suptitle(f"Case {idx} | Ma={Ma:.3f}, AoA={AoA:.2f}° — Cp comparison", fontsize=11)

    ref_order = ('baseline', 'composite', 'sing_sub', 'full')
    ref_cols  = {'baseline': 'steelblue', 'composite': 'forestgreen',
                 'sing_sub': 'gray', 'full': 'crimson'}
    ref_styl  = {'baseline': '--', 'composite': ':', 'sing_sub': '-.', 'full': '-'}

    ax = axes[0]
    for m in ref_order:
        r = results[m]
        n0 = r['n_ma0']; rm = r['rmse_cp']
        ax.plot(results['baseline']['xx'], r['cpu'],
                color=ref_cols[m], ls=ref_styl[m], lw=1.5,
                label=f"{m}  nMa0={n0}  rmCp={rm:.4f}")
        ax.plot(results['baseline']['xx'], r['cpl'],
                color=ref_cols[m], ls=ref_styl[m], lw=1.5)
    ax.plot(airfoil['xu'], airfoil['cpu'], 'k^', ms=3, markevery=5, label='RANS')
    ax.plot(airfoil['xl'], airfoil['cpl'], 'kv', ms=3, markevery=5)
    ax.invert_yaxis()
    ax.set(title='Reference modes', xlabel='x/c', ylabel='Cp', xlim=(-.05, 1.05))
    ax.legend(fontsize=7); ax.grid()

    ax = axes[1]
    sing_cols  = {'constant': 'orange',      'linear': 'goldenrod', 'sqrt_fit': 'darkorange'}
    full_cols  = {'constant': 'mediumpurple', 'linear': 'orchid',   'sqrt_fit': 'darkviolet'}
    for prefix, cols, label_pfx in (
            ('sing_D', sing_cols, 'sing_D'),
            ('full_D', full_cols, 'full_D')):
        for m in D_METHODS:
            key = f'{prefix}_{m}'
            r = results[key]
            n0 = r['n_ma0']; rm = r['rmse_cp']
            xx = r['xx']
            ax.plot(xx, r['cpu'], color=cols[m], ls='-.', lw=1.5,
                    label=f"{label_pfx}_{m}  nMa0={n0}  rm={rm:.4f}")
            ax.plot(xx, r['cpl'], color=cols[m], ls='-.', lw=1.5)
    # Add baseline for reference
    ax.plot(results['baseline']['xx'], results['baseline']['cpu'], 'b--', lw=1, alpha=0.4, label='baseline')
    ax.plot(results['baseline']['xx'], results['baseline']['cpl'], 'b--', lw=1, alpha=0.4)
    ax.plot(airfoil['xu'], airfoil['cpu'], 'k^', ms=3, markevery=5, label='RANS')
    ax.plot(airfoil['xl'], airfoil['cpl'], 'kv', ms=3, markevery=5)
    ax.invert_yaxis()
    ax.set(title='Step-D modes (all methods)', xlabel='x/c', ylabel='Cp', xlim=(-.05, 1.05))
    ax.legend(fontsize=6); ax.grid()

    fig.tight_layout()
    fig.savefig(os.path.join(path_figs, f'case_{idx:04d}_cp.png'), dpi=100)
    plt.close(fig)


def _plot_summary(all_results, cases):
    """Bar-chart summary across cases."""
    n = len(cases)
    x_pos = np.arange(n)
    keys_plot = (['baseline', 'full'] +
                 [f'full_D_{m}' for m in D_METHODS])
    cols_plot = {
        'baseline':       'steelblue',
        'full':           'crimson',
        'full_D_constant':'mediumpurple',
        'full_D_linear':  'orchid',
        'full_D_sqrt_fit':'darkviolet',
    }
    width = 0.15
    offsets = {k: (i - len(keys_plot)/2 + 0.5) * width
               for i, k in enumerate(keys_plot)}

    fig, axes = plt.subplots(1, 2, figsize=(14, 5))
    fig.suptitle("Task 11: full_D closure methods vs baseline/full", fontsize=11)
    for ax, metric, ylabel, title in zip(
            axes,
            ['n_ma0', 'rmse_cp'],
            ['nMa0 (lower=better)', 'rmCp (lower=better)'],
            ['nMa0 across cases', 'rmCp vs RANS']):
        for k in keys_plot:
            vals = [all_results[idx][k][metric] for idx in cases]
            ax.bar(x_pos + offsets[k], vals, width,
                   label=k, color=cols_plot[k], alpha=0.85)
        ax.set(title=title, xlabel='Case', ylabel=ylabel)
        ax.set_xticks(x_pos); ax.set_xticklabels([str(i) for i in cases])
        ax.legend(fontsize=7); ax.grid(axis='y')
    fig.tight_layout()
    fig.savefig(os.path.join(path_figs, 'summary.png'), dpi=100)
    plt.close(fig)


# ── main ────────────────────────────────────────────────────────────────────────

def main():
    print("Loading airfoil database...")
    db     = load_airfoil_database_from_json(fname_db)
    entries = list(db.values())

    CASE_INDICES = [0, 1, 3]
    cases = sorted([e for e in entries if e['entry_index'] in CASE_INDICES],
                   key=lambda a: a['entry_index'])

    all_results = {}
    hdr = (f"{'idx':>4}  {'Ma':>5}  {'AoA':>5}  "
           f"{'rmBase':>7}  {'rmFull':>7}  "
           f"{'rmFD_cst':>9}  {'rmFD_lin':>9}  {'rmFD_sqt':>9}")
    print(f"\nRunning {len(cases)} cases ...\n")
    print(hdr); print('-' * len(hdr))

    for airfoil in cases:
        idx, res = run_case(airfoil)
        all_results[idx] = res
        rm = {k: res[k]['rmse_cp'] for k in res}
        print(f"{idx:>4}  {airfoil['Ma']:>5.3f}  {airfoil['AoA']:>5.2f}  "
              f"{rm['baseline']:>7.4f}  {rm['full']:>7.4f}  "
              f"{rm['full_D_constant']:>9.4f}  "
              f"{rm['full_D_linear']:>9.4f}  "
              f"{rm['full_D_sqrt_fit']:>9.4f}")
        print()

    _plot_summary(all_results, [a['entry_index'] for a in cases])
    print(f"\nFigures saved to {path_figs}/")
    print("  case_XXXX_bc.png  — residual BC for each closure method")
    print("  case_XXXX_cp.png  — Cp for all modes")
    print("  summary.png       — bar chart: full_D methods vs baseline/full")


if __name__ == '__main__':
    main()
