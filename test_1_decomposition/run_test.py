"""
Test 1: TSD decomposition — thickness, camber, angle-of-attack contributions.

For each Mach number one 4×6 figure is produced:

  Col 1 — Thickness effect  : AoA=0, no camber,       t_scale = 0.1 / 0.5 / 1.0
  Col 2 — Camber effect     : AoA=0, t_scale=0.1,     camber_scale = 0.1 / 0.5 / 1.0
  Col 3 — AoA effect        : t_scale=0.1, no camber, AoA = 0° / 1° / 2°
  Col 4 — AoA + Camber      : t_scale=0.1, full cam,  AoA = 0° / 1° / 2°
  Col 5 — LE Radius effect  : t_scale=0.5, no camber, AoA = 0° / 1° / 2°
                               each subplot overlays ref (blue) and half-r_LE (orange)
  Col 6 — Full airfoil (ref): t_scale=1.0, full cam,  AoA = 0° / 1° / 2°

Rows 1-3: Mach-number surface distributions (upper solid, lower dashed).
Row  4  : Geometry illustration.
           Cols 1-2 — overlay of the 3 geometries + single 0° inflow line.
           Cols 3-4,6 — single geometry + 3 inflow lines at 0°/1°/2°.
           Col 5 — ref + half-r_LE geometries + 3 inflow lines at 0°/1°/2°.
"""

import os
import sys
import copy
import time
import traceback
import numpy as np
import matplotlib.pyplot as plt
import multiprocessing as mp

path      = os.path.abspath(os.path.dirname(__file__))
path_root = os.path.abspath(os.path.join(path, '..'))
sys.path.insert(0, path_root)

from cst_modeling.section import cst_foil
from pytsfoil import PyTSFoil

# ── airfoil (RAE2822-like CST) ────────────────────────────────────────────────
CST_U = np.array([ 0.12829643,  0.12670863,  0.16065898,  0.14942386,  0.15102884,
                   0.22416928,  0.16078175,  0.20998555,  0.18608795,  0.21052324])
CST_L = np.array([-0.12927128, -0.13176061, -0.17044964, -0.07045476, -0.33888064,
                   0.00991923, -0.20070721, -0.03536713, -0.04397496,  0.06436195])
T_MAX  = 0.15
N_FOIL = 201

MACH_LIST = [0.70, 0.75, 0.80]

# ── column definitions ────────────────────────────────────────────────────────
# Each case is (t_scale, camber_scale, aoa_deg).
# geo_mode = 'vary_geom'     → row-4 overlays 3 shapes + single 0° inflow line.
# geo_mode = 'vary_aoa'      → row-4 shows 1 shape + 3 inflow lines at 0°/1°/2°.
# geo_mode = 'vary_aoa_2geom'→ row-4 shows ref+half-LE shapes + 3 inflow lines.
# le_variants = True         → each Mach subplot overlays ref and half-r_LE results.
COLUMNS = [
    {
        'title':      'Thickness effect',
        'cases':      [(0.1, 0.0, 0.0), (0.5, 0.0, 0.0), (1.0, 0.0, 0.0)],
        'row_labels': ['t×0.1', 't×0.5', 't×1.0'],
        'geo_mode':   'vary_geom',
    },
    {
        'title':      'Camber effect',
        'cases':      [(0.1, 0.1, 0.0), (0.1, 0.5, 0.0), (0.1, 1.0, 0.0)],
        'row_labels': ['c×0.1', 'c×0.5', 'c×1.0'],
        'geo_mode':   'vary_geom',
    },
    {
        'title':      'AoA effect',
        'cases':      [(0.1, 0.0, 0.0), (0.1, 0.0, 1.0), (0.1, 0.0, 2.0)],
        'row_labels': ['α=0°', 'α=1°', 'α=2°'],
        'geo_mode':   'vary_aoa',
    },
    {
        'title':      'AoA + Camber',
        'cases':      [(0.1, 1.0, 0.0), (0.1, 1.0, 1.0), (0.1, 1.0, 2.0)],
        'row_labels': ['α=0°', 'α=1°', 'α=2°'],
        'geo_mode':   'vary_aoa',
    },
    {
        'title':      'LE Radius effect',
        'cases':      [(0.5, 0.0, 0.0), (0.5, 0.0, 1.0), (0.5, 0.0, 2.0)],
        'row_labels': ['α=0°', 'α=1°', 'α=2°'],
        'geo_mode':   'vary_aoa_2geom',
        'le_variants': True,
    },
    {
        'title':      'Full airfoil (ref)',
        'cases':      [(1.0, 1.0, 0.0), (1.0, 1.0, 1.0), (1.0, 1.0, 2.0)],
        'row_labels': ['α=0°', 'α=1°', 'α=2°'],
        'geo_mode':   'vary_aoa',
    },
]

# colors for the three rows when overlaying geometry or inflow lines
ROW_COLORS = ['tab:blue', 'tab:green', 'tab:red']

# ── solver config ─────────────────────────────────────────────────────────────
BASE_CFG = {
    'REYNLD':  6.5e6,
    'CVERGE':  1e-5,
    'DVERGE':  10.0,
    'EPS':     0.5,
    'IPRTER':  100,
    'MAXIT':   9999,
    'RIGF':    0.0,
    'SIMDEF':  3,
    'WCIRC':   1.0,
    'WE':      [1.8, 1.9, 1.95],
    'NWDGE':   0,
    'WCONST':  4.0,
    'IFLAP':   0,
    'DELFLP':  0.0,
    'FLPLOC':  0.77,
    'n_point_x':        200,
    'n_point_y':         80,
    'n_point_airfoil':  100,
    'flag_output':          False,
    'flag_output_summary':  False,
    'flag_output_shock':    False,
    'flag_output_field':    False,
    'flag_print_info':      False,
}

path_figs = os.path.join(path, 'figures')
os.makedirs(path_figs, exist_ok=True)


# ── geometry ──────────────────────────────────────────────────────────────────

def _base_components(cst_u, cst_l):
    x, yu, yl, tmax, radius_LE = cst_foil(N_FOIL, cst_u, cst_l, t=T_MAX)
    return x, 0.5 * (yu + yl), 0.5 * (yu - yl), tmax, radius_LE


def _make_coords(x, camber, half_thk, t_scale, camber_scale):
    thk = half_thk * t_scale
    cam = camber  * camber_scale
    yu  = cam + thk
    yl  = cam - thk
    xx  = np.concatenate([x[::-1], x[1:]])
    yy  = np.concatenate([yu[::-1], yl[1:]])
    return np.column_stack([xx, yy])


# ── solver worker (must be top-level for multiprocessing pickling) ─────────────

def _run_one(job: dict) -> dict:
    key          = job['key']
    t_scale      = job['t_scale']
    camber_scale = job['camber_scale']
    ma           = job['ma']
    aoa          = job['aoa']
    x            = job['x']
    camber       = job['camber']
    half_thk     = job['half_thk']

    coords = _make_coords(x, camber, half_thk, t_scale, camber_scale)
    cfg = copy.deepcopy(BASE_CFG)
    cfg['EMACH'] = ma
    cfg['ALPHA'] = aoa

    t0 = time.time()
    try:
        ts = PyTSFoil(airfoil_coordinates=coords, work_dir=path)
        ts.set_config(**cfg)
        ts.run()

        ile = ts.mesh['ile']
        ite = ts.mesh['ite']
        mau = ts.data_summary['mau']
        mal = ts.data_summary['mal']

        return {
            'success':      True,
            'key':          key,
            't_scale':      t_scale,
            'camber_scale': camber_scale,
            'ma':           ma,
            'aoa':          aoa,
            'elapsed':      time.time() - t0,
            'cl':           ts.data_summary['cl'],
            'cd':           ts.data_summary['cd'],
            'cm':           ts.data_summary['cm'],
            'n_ma0_upper':  int(np.sum(mau[ile:ite + 1] == 0.0)),
            'n_ma0_lower':  int(np.sum(mal[ile:ite + 1] == 0.0)),
            'xx':           ts.mesh['xx'].copy(),
            'mau':          mau.copy(),
            'mal':          mal.copy(),
        }
    except Exception:
        return {
            'success': False,
            'key':     key,
            'ma':      ma,
            'aoa':     aoa,
            'error':   traceback.format_exc(),
        }


# ── plotting helpers ──────────────────────────────────────────────────────────

def _mach_ax(ax, r, row_label, r2=None):
    """Fill one Mach-distribution subplot.

    If r2 is provided the subplot overlays two results:
      r  (blue)   — reference geometry
      r2 (orange) — half-r_LE geometry
    """
    ax.text(0.03, 0.97, row_label, transform=ax.transAxes, fontsize=8,
            va='top', ha='left',
            bbox=dict(boxstyle='round,pad=0.2', fc='white', ec='gray', alpha=0.7))

    if r['success']:
        lbl_ref = 'Ref' if r2 is not None else None
        ax.plot(r['xx'], r['mau'], color='tab:blue', ls='-',  lw=1.0, label=lbl_ref)
        ax.plot(r['xx'], r['mal'], color='tab:blue', ls='--', lw=1.0)
        ax.axhline(1.0, color='k', lw=0.5, ls=':')
        ax.axhline(0.0, color='k', lw=0.5, ls=':')
        ax.axhline(r['ma'], color='k', lw=0.5, ls='--')
    else:
        ax.text(0.5, 0.5, 'FAILED', transform=ax.transAxes,
                ha='center', va='center', color='red', fontsize=9)

    if r2 is not None:
        if r2['success']:
            ax.plot(r2['xx'], r2['mau'], color='tab:orange', ls='-',  lw=1.0, label='½r_LE')
            ax.plot(r2['xx'], r2['mal'], color='tab:orange', ls='--', lw=1.0)
        else:
            ax.text(0.5, 0.3, 'FAILED(½r)', transform=ax.transAxes,
                    ha='center', va='center', color='darkorange', fontsize=7)

    if r2 is None:
        if r['success']:
            info = (f"CL={r['cl']:.4f}  CD={r['cd']:.5f}\n"
                    f"Ma0_u={r['n_ma0_upper']}  Ma0_l={r['n_ma0_lower']}")
            ax.text(0.97, 0.97, info, transform=ax.transAxes, fontsize=6,
                    va='top', ha='right', family='monospace')
    else:
        lines = []
        if r['success']:
            lines.append(f"Ref  CL={r['cl']:.4f} CD={r['cd']:.5f}")
        if r2 is not None and r2['success']:
            lines.append(f"½r   CL={r2['cl']:.4f} CD={r2['cd']:.5f}")
        if lines:
            ax.text(0.97, 0.97, '\n'.join(lines), transform=ax.transAxes, fontsize=5,
                    va='top', ha='right', family='monospace')
        ax.legend(fontsize=5, loc='lower right', framealpha=0.6)

    ax.set_xlim(-0.1, 1.1)
    ax.set_ylim(-0.05, 1.55)
    ax.grid(alpha=0.3)
    ax.tick_params(labelsize=7)


def _geo_vary_geom(ax, x, camber, half_thk, cases, row_labels):
    """Geometry subplot: overlay 3 shapes + single 0° inflow line (cols 1-2)."""
    for i, (t_s, c_s, _) in enumerate(cases):
        coords = _make_coords(x, camber, half_thk, t_s, c_s)
        ax.plot(coords[:, 0], coords[:, 1],
                color=ROW_COLORS[i], lw=1.0, label=row_labels[i])
    x_ref = np.array([-0.1, 1.1])
    ax.plot(x_ref, np.zeros(2), 'k--', lw=0.8, label='Inflow 0°')


def _geo_vary_aoa(ax, x, camber, half_thk, cases, row_labels):
    """Geometry subplot: single shape + 3 inflow lines (cols 3-4, 6)."""
    t_s, c_s, _ = cases[0]
    coords = _make_coords(x, camber, half_thk, t_s, c_s)
    ax.plot(coords[:, 0], coords[:, 1], 'k-', lw=1.2, label='Geometry')
    x_ref = np.array([-0.1, 1.1])
    for i, (_, _, aoa) in enumerate(cases):
        y_ref = np.tan(np.deg2rad(aoa)) * (x_ref - 1.0)
        ax.plot(x_ref, y_ref, ls='--', color=ROW_COLORS[i], lw=0.9,
                label=row_labels[i])


def _geo_vary_aoa_2geom(ax, x, camber, half_thk, x2, camber2, half_thk2, cases, row_labels):
    """Geometry subplot: ref (solid) + half-r_LE (dashed) shapes + 3 inflow lines (col 5)."""
    t_s, c_s, _ = cases[0]
    coords_ref = _make_coords(x,  camber,  half_thk,  t_s, c_s)
    coords_hle = _make_coords(x2, camber2, half_thk2, t_s, c_s)
    ax.plot(coords_ref[:, 0], coords_ref[:, 1], 'k-',  lw=1.2, label='Ref')
    ax.plot(coords_hle[:, 0], coords_hle[:, 1], 'k--', lw=1.2, label='½r_LE')
    x_ref = np.array([-0.1, 1.1])
    for i, (_, _, aoa) in enumerate(cases):
        y_ref = np.tan(np.deg2rad(aoa)) * (x_ref - 1.0)
        ax.plot(x_ref, y_ref, ls='--', color=ROW_COLORS[i], lw=0.9,
                label=row_labels[i])


# ── main figure ───────────────────────────────────────────────────────────────

def _plot_figure(ma, x, camber, half_thk, results_by_key,
                 x_hle=None, camber_hle=None, half_thk_hle=None):
    n_cols = len(COLUMNS)
    fig, axes = plt.subplots(
        4, n_cols, figsize=(4 * n_cols, 13),
        gridspec_kw={'height_ratios': [1, 1, 1, 0.65]},
    )
    fig.suptitle(f'TSD Decomposition  —  Ma = {ma:.2f}', fontsize=13)

    for col_idx, col in enumerate(COLUMNS):
        is_dual = col.get('le_variants', False)

        # rows 0-2: Mach distributions ----------------------------------------
        for row_idx in range(3):
            ax = axes[row_idx, col_idx]
            t_s, c_s, aoa = col['cases'][row_idx]

            if is_dual:
                r  = results_by_key.get((ma, t_s, c_s, aoa, 'ref'), {'success': False})
                r2 = results_by_key.get((ma, t_s, c_s, aoa, 'hle'), {'success': False})
                _mach_ax(ax, r, col['row_labels'][row_idx], r2=r2)
            else:
                r = results_by_key.get((ma, t_s, c_s, aoa), {'success': False})
                _mach_ax(ax, r, col['row_labels'][row_idx])

            if row_idx == 0:
                ax.set_title(col['title'], fontsize=9, pad=4)
            if col_idx == 0:
                ax.set_ylabel('Mach', fontsize=8)

            # suppress x-tick labels on rows 0 and 1
            ax.tick_params(labelbottom=(row_idx == 2))

        # row 3: geometry illustration ----------------------------------------
        ax = axes[3, col_idx]
        if col['geo_mode'] == 'vary_geom':
            _geo_vary_geom(ax, x, camber, half_thk, col['cases'], col['row_labels'])
        elif col['geo_mode'] == 'vary_aoa_2geom':
            _geo_vary_aoa_2geom(ax, x, camber, half_thk,
                                x_hle, camber_hle, half_thk_hle,
                                col['cases'], col['row_labels'])
        else:
            _geo_vary_aoa(ax, x, camber, half_thk, col['cases'], col['row_labels'])

        ax.set_xlim(-0.1, 1.1)
        ax.set_ylim(-0.1, 0.1)
        ax.set_xlabel('x/c', fontsize=8)
        ax.legend(fontsize=6, loc='upper right', framealpha=0.6)
        ax.grid(alpha=0.3)
        ax.tick_params(labelsize=7)
        if col_idx == 0:
            ax.set_ylabel('y/c', fontsize=8)

    fig.tight_layout()
    out = os.path.join(path_figs, f'decomposition_Ma{ma:.2f}.png')
    fig.savefig(out, dpi=150, bbox_inches='tight')
    plt.close(fig)


# ── summary table ─────────────────────────────────────────────────────────────

def _print_summary(results_by_key, fname):
    hdr = (f'{"Ma":>5}  {"t_sc":>5}  {"c_sc":>5}  {"AoA":>5}  {"geom":>6}  '
           f'{"CL":>9}  {"CD":>9}  {"CM":>9}  '
           f'{"Ma0_u":>5}  {"Ma0_l":>5}  {"t(s)":>6}\n')
    sep = '=' * len(hdr.rstrip())
    with open(fname, 'w') as f:
        f.write(sep + '\n')
        f.write(hdr)
        f.write('-' * len(hdr.rstrip()) + '\n')
        for key in sorted(results_by_key):
            r = results_by_key[key]
            if len(key) == 5:
                ma, t_s, c_s, aoa, geom_tag = key
            else:
                ma, t_s, c_s, aoa = key
                geom_tag = 'ref'
            if not r['success']:
                f.write(f"  {ma:.2f}  {t_s:.2f}  {c_s:.2f}  {aoa:.1f}  {geom_tag:>6}  FAILED\n")
            else:
                f.write(f"  {ma:.2f}  {t_s:.2f}  {c_s:.2f}  {aoa:.1f}  {geom_tag:>6}  "
                        f"{r['cl']:>9.5f}  {r['cd']:>9.6f}  {r['cm']:>9.5f}  "
                        f"{r['n_ma0_upper']:>5d}  {r['n_ma0_lower']:>5d}  "
                        f"{r['elapsed']:>6.1f}\n")
        f.write(sep + '\n')


# ── entry point ───────────────────────────────────────────────────────────────

if __name__ == '__main__':

    x, camber, half_thk, tmax, radius_LE = _base_components(CST_U, CST_L)
    print(f"Airfoil: tmax={tmax:.4f}, radius_LE={radius_LE:.4f}")

    # half-LE-radius variant: scale first CST param by 1/sqrt(2) → r_LE halved (r_LE ∝ A0²)
    _hle_scale = 1.0 / np.sqrt(2.0)
    _cst_u_hle = CST_U.copy(); _cst_u_hle[0] *= _hle_scale
    _cst_l_hle = CST_L.copy(); _cst_l_hle[0] *= _hle_scale
    x_hle, camber_hle, half_thk_hle, tmax_hle, radius_LE_hle = _base_components(_cst_u_hle, _cst_l_hle)
    print(f"Half-LE airfoil: tmax={tmax_hle:.4f}, radius_LE={radius_LE_hle:.4f}")

    # collect unique cases — avoid duplicates
    # regular columns use 4-tuple keys (ma, t_s, c_s, aoa)
    # le_variants column uses 5-tuple keys (ma, t_s, c_s, aoa, geom_tag)
    seen = set()
    jobs = []
    for ma in MACH_LIST:
        for col in COLUMNS:
            is_dual = col.get('le_variants', False)
            for t_s, c_s, aoa in col['cases']:
                if is_dual:
                    for geom_tag, gx, gc, ght in [
                        ('ref', x,     camber,     half_thk),
                        ('hle', x_hle, camber_hle, half_thk_hle),
                    ]:
                        key = (ma, t_s, c_s, aoa, geom_tag)
                        if key not in seen:
                            seen.add(key)
                            jobs.append({
                                'key':          key,
                                'ma':           ma,
                                't_scale':      t_s,
                                'camber_scale': c_s,
                                'aoa':          aoa,
                                'x':            gx,
                                'camber':       gc,
                                'half_thk':     ght,
                            })
                else:
                    key = (ma, t_s, c_s, aoa)
                    if key not in seen:
                        seen.add(key)
                        jobs.append({
                            'key':          key,
                            'ma':           ma,
                            't_scale':      t_s,
                            'camber_scale': c_s,
                            'aoa':          aoa,
                            'x':            x,
                            'camber':       camber,
                            'half_thk':     half_thk,
                        })

    n_proc = min(mp.cpu_count(), len(jobs))
    print(f"Running {len(jobs)} unique cases with {n_proc} workers ...")

    with mp.Pool(n_proc) as pool:
        raw = pool.map(_run_one, jobs)

    results_by_key = {r['key']: r for r in raw}

    n_ok = sum(1 for r in raw if r['success'])
    print(f"Done — {n_ok}/{len(raw)} succeeded.")

    for ma in MACH_LIST:
        _plot_figure(ma, x, camber, half_thk, results_by_key,
                     x_hle=x_hle, camber_hle=camber_hle, half_thk_hle=half_thk_hle)

    _print_summary(results_by_key, os.path.join(path, 'summary.txt'))
    print("Summary written.")
