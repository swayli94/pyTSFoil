"""
Test 1: TSD leading-edge M=0 region decomposition.

Decomposes the contributions of thickness, camber, and angle of attack
to the leading-edge Ma=0 region in the TSD solution.

For each (Ma, AoA) combination, four cases are run:
  - thickness_only : AoA=0, original thickness, no camber
  - camber_only    : AoA=0, original camber + small reference thickness
  - aoa_only       : flat plate + small reference thickness
  - full           : original geometry, non-zero AoA

camber_only and aoa_only require delta>0 for TSD scaling.  They are run with
the smallest thickness-scale (fraction of original half-thickness) that
achieves convergence, sweeping [0.5, 0.3, 0.2, 0.1, 0.05, 0.02, 0.01].

No MAE leading-edge correction is applied.
"""

import os
import sys
import copy
import time
import numpy as np
import matplotlib.pyplot as plt
import multiprocessing as mp

path      = os.path.abspath(os.path.dirname(__file__))
path_root = os.path.abspath(os.path.join(path, '..'))
sys.path.insert(0, path_root)

from cst_modeling.section import cst_foil
from pytsfoil import PyTSFoil

# ── airfoil (RAE2822) ─────────────────────────────────────────────────────────
CST_U = np.array([ 0.12829643,  0.12670863,  0.16065898,  0.14942386,  0.15102884,
                   0.22416928,  0.16078175,  0.20998555,  0.18608795,  0.21052324])
CST_L = np.array([-0.12927128, -0.13176061, -0.17044964, -0.07045476, -0.33888064,
                   0.00991923, -0.20070721, -0.03536713, -0.04397496,  0.06436195])
T_MAX = 0.15 # RAE2822 original maximum thickness = 0.1211

# ── sweep parameters ──────────────────────────────────────────────────────────
AOA_LIST  = [0.0, 1.0, 2.0, 3.0]   # degrees
MACH_LIST = [0.70, 0.75, 0.80]
N_FOIL    = 201

# Thickness-scale candidates for camber_only / aoa_only (fraction of original half-thk)
T_SCALE_CANDIDATES = [0.1]

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


# ── airfoil geometry builders ─────────────────────────────────────────────────

def _base_components(cst_u, cst_l):
    """Return (x, camber, half_thk, tmax) for the original CST airfoil."""
    x, yu, yl, tmax, _ = cst_foil(N_FOIL, cst_u, cst_l, t=T_MAX)
    return x, 0.5 * (yu + yl), 0.5 * (yu - yl), tmax


def _make_coords(x, camber, half_thk, variant, t_scale=1.0):
    """
    Build closed-loop coordinate array for a geometry variant.

    variant:
      'thickness_only' — symmetric, full thickness, no camber
      'camber_only'    — camber line + t_scale * half_thk as symmetric thickness
      'aoa_only'       — flat plate + t_scale * half_thk as symmetric thickness
      'full'           — original camber + full thickness
    t_scale: fraction of original half_thk to use as symmetric thickness
             (only applies to camber_only / aoa_only)
    """
    thk = half_thk * t_scale

    if variant in ('thickness_only', 'thickness_half'):
        yu = thk
        yl = -thk
    elif variant == 'camber_only':
        yu = camber + thk
        yl = camber - thk
    elif variant == 'aoa_only':
        yu = thk
        yl = -thk
    elif variant == 'full':
        yu = camber + half_thk
        yl = camber - half_thk
    else:
        raise ValueError(f'Unknown variant: {variant}')

    xx = np.concatenate([x[::-1], x[1:]])
    yy = np.concatenate([yu[::-1], yl[1:]])
    return np.column_stack([xx, yy])


# ── single worker ─────────────────────────────────────────────────────────────

def _solve(coords, ma, aoa):
    """Run one PyTSFoil solve; return ts object or raise."""
    cfg = copy.deepcopy(BASE_CFG)
    cfg['EMACH'] = ma
    cfg['ALPHA'] = aoa
    ts = PyTSFoil(airfoil_coordinates=coords, work_dir=path)
    ts.set_config(**cfg)
    ts.run()
    return ts


def _extract(ts, ma, aoa, variant, t_scale, elapsed):
    ile = ts.mesh['ile']
    ite = ts.mesh['ite']
    mal = ts.data_summary['mal']
    mau = ts.data_summary['mau']
    return {
        'success':      True,
        'ma':           ma,
        'aoa':          aoa,
        'variant':      variant,
        't_scale':      t_scale,
        'elapsed':      elapsed,
        'cl':           ts.data_summary['cl'],
        'cd':           ts.data_summary['cd'],
        'cm':           ts.data_summary['cm'],
        'n_ma0_upper':  int(np.sum(mau[ile:ite + 1] == 0.0)),
        'n_ma0_lower':  int(np.sum(mal[ile:ite + 1] == 0.0)),
        'xx':           ts.mesh['xx'].copy(),
        'mau':          mau.copy(),
        'mal':          mal.copy(),
        'cpu':          ts.data_summary['cpu'].copy(),
        'cpl':          ts.data_summary['cpl'].copy(),
    }


def _run_one(job: dict) -> dict:
    """
    Run one (Ma, AoA, variant) case.

    For thickness_only / full: single solve with t_scale=1.0.
    For camber_only / aoa_only: retry across T_SCALE_CANDIDATES (smallest first),
    report result for the first scale that converges.
    """
    ma      = job['ma']
    aoa     = job['aoa']
    variant = job['variant']
    x       = job['x']
    camber  = job['camber']
    half_thk = job['half_thk']

    import traceback

    if variant in ('camber_only', 'aoa_only'):
        scales = T_SCALE_CANDIDATES
    elif variant == 'thickness_half':
        scales = [0.5]
    else:
        scales = [1.0]

    t0 = time.time()
    last_err = ''
    for t_scale in scales:
        coords = _make_coords(x, camber, half_thk, variant, t_scale)
        try:
            ts = _solve(coords, ma, aoa)
            return _extract(ts, ma, aoa, variant, t_scale, time.time() - t0)
        except Exception:
            last_err = traceback.format_exc()
            continue

    return {
        'success': False,
        'ma': ma, 'aoa': aoa, 'variant': variant, 't_scale': None,
        'error': last_err,
    }


# ── plotting ──────────────────────────────────────────────────────────────────

# color per variant; upper → solid, lower → dashed
VARIANT_CONFIG = {
    'thickness_only': {'color': 'b', 'linewidth': 1.5, 'alpha': 1.0, 'label': 'Thickness only'},
    'thickness_half': {'color': 'b', 'linewidth': 1.0, 'alpha': 0.5, 'label': 'Thickness half'},
    'camber_only':    {'color': 'g', 'linewidth': 1.5, 'alpha': 1.0, 'label': 'Camber only'},
    'aoa_only':       {'color': 'r', 'linewidth': 1.0, 'alpha': 1.0, 'label': 'AoA only'},
    'full':           {'color': 'k', 'linewidth': 4.0, 'alpha': 0.5, 'label': 'Full'},
}


def _base_label(r):
    label = VARIANT_CONFIG[r['variant']]['label']
    if r['variant'] in ('camber_only', 'aoa_only') and r.get('t_scale') is not None:
        label += f' (t={r["t_scale"]:.2f})'
    return label


def _plot_mach(ma, aoa, cases_by_variant, x, camber, half_thk):
    """
    Two subplots:
      left  — airfoil geometry for each variant + inflow reference line through (1, 0)
      right — Mach distribution, full chord
    Each variant: solid = upper surface, dashed = lower surface (Mach subplot).
    """
    fig, (ax_geo, ax_mach) = plt.subplots(1, 2, figsize=(14, 5))
    fig.suptitle(f'Mach Distribution  Ma={ma:.2f}, AoA={aoa:.1f}°', fontsize=11)

    # ── geometry subplot ──────────────────────────────────────────────────────
    for variant, r in cases_by_variant.items():
        if not r['success']:
            continue
        color = VARIANT_CONFIG[variant]['color']
        linewidth = VARIANT_CONFIG[variant]['linewidth']
        alpha = VARIANT_CONFIG[variant]['alpha']
        label  = _base_label(r)
        t_scale = r.get('t_scale') or 1.0
        coords = _make_coords(x, camber, half_thk, variant, t_scale)
        ax_geo.plot(coords[:, 0], coords[:, 1], color=color, lw=linewidth, alpha=alpha, label=label)

    # inflow reference line through trailing edge (1, 0) at angle AoA
    aoa_rad = np.deg2rad(aoa)
    x_ref   = np.array([-0.15, 1.25])
    y_ref   = np.tan(aoa_rad) * (x_ref - 1.0)
    ax_geo.plot(x_ref, y_ref, color='red', ls='--', lw=0.5,
                label=f'Inflow dir. (AoA={aoa:.1f}°)')

    # ax_geo.set_aspect('equal')
    ax_geo.set_xlim(-0.15, 1.25)
    ax_geo.set_ylim(-0.2, 0.2)
    ax_geo.set(title='Geometry', xlabel='x/c', ylabel='y/c')
    ax_geo.legend(fontsize=7)
    ax_geo.grid(alpha=0.3)

    # ── Mach subplot ──────────────────────────────────────────────────────────
    for variant, r in cases_by_variant.items():
        if not r['success']:
            continue
        color = VARIANT_CONFIG[variant]['color']
        linewidth = VARIANT_CONFIG[variant]['linewidth']
        alpha = VARIANT_CONFIG[variant]['alpha']
        label = _base_label(r)
        ax_mach.plot(r['xx'], r['mau'], color=color, ls='-',  lw=linewidth, alpha=alpha, label=f'{label} upper')
        ax_mach.plot(r['xx'], r['mal'], color=color, ls='--', lw=linewidth, alpha=alpha, label=f'{label} lower')

    ax_mach.axhline(1.0, color='gray', lw=0.8, ls=':', label='M=1')
    ax_mach.axhline(0.0, color='gray', lw=0.5, ls=':')
    ax_mach.set_xlim(-0.2, 1.2)
    ax_mach.set_ylim(-0.05, 1.5)
    ax_mach.set(title='Full chord', xlabel='x/c', ylabel='Mach')
    ax_mach.legend(fontsize=7, ncol=2)
    ax_mach.grid(alpha=0.3)

    fig.tight_layout()
    fig.savefig(os.path.join(path_figs, f'mach_Ma{ma:.2f}_AoA{aoa:.1f}.png'),
                dpi=120, bbox_inches='tight')
    plt.close(fig)


def _plot_n_ma0_heatmaps(all_results):
    variants = list(VARIANT_CONFIG.keys())

    for surface, surf_key in (('upper', 'n_ma0_upper'), ('lower', 'n_ma0_lower')):
        fig, axes = plt.subplots(1, len(variants), figsize=(5 * len(variants), 4))
        fig.suptitle(f'Ma=0 point count — {surface} surface', fontsize=11)

        for ax, variant in zip(axes, variants):
            data = np.full((len(AOA_LIST), len(MACH_LIST)), np.nan)
            for r in all_results:
                if r['success'] and r['variant'] == variant:
                    i_aoa  = AOA_LIST.index(r['aoa'])
                    i_mach = MACH_LIST.index(r['ma'])
                    data[i_aoa, i_mach] = r[surf_key]

            valid = data[~np.isnan(data)]
            vmax  = max(valid.max(), 1) if len(valid) else 1
            im = ax.imshow(data, aspect='auto', origin='lower',
                           cmap='YlOrRd', vmin=0, vmax=vmax)
            ax.set_xticks(range(len(MACH_LIST)))
            ax.set_xticklabels([f'{m:.2f}' for m in MACH_LIST])
            ax.set_yticks(range(len(AOA_LIST)))
            ax.set_yticklabels([f'{a:.1f}°' for a in AOA_LIST])
            ax.set(xlabel='Mach', ylabel='AoA', title=VARIANT_CONFIG[variant]['label'])
            for ii in range(len(AOA_LIST)):
                for jj in range(len(MACH_LIST)):
                    v = data[ii, jj]
                    txt = f'{int(v)}' if not np.isnan(v) else 'F'
                    ax.text(jj, ii, txt, ha='center', va='center', fontsize=9)
            plt.colorbar(im, ax=ax)

        fig.tight_layout()
        fig.savefig(os.path.join(path_figs, f'n_ma0_{surface}.png'),
                    dpi=120, bbox_inches='tight')
        plt.close(fig)


def print_summary(all_results, fname):
    
    with open(fname, 'w') as f:
        
        f.write('=' * 105 + '\n')

        f.write(f'{"Ma":>5}  {"AoA":>5}  {"Variant":<18}  {"t_scale":>7}  '
                f'{"n_Ma0_u":>7}  {"n_Ma0_l":>7}  '
                f'{"CL":>8}  {"CD":>8}  {"CM":>8}  {"t(s)":>6}\n')
        f.write('-' * 105 + '\n')
        sort_key = lambda r: (r['ma'], r['aoa'], list(VARIANT_CONFIG.keys()).index(r['variant']))
        for r in sorted(all_results, key=sort_key):
            ts_str = f'{r["t_scale"]:.2f}' if r.get('t_scale') is not None else '  —  '
            if not r['success']:
                f.write(f"  {r['ma']:.2f}  {r['aoa']:.1f}  {r['variant']:<18}  {ts_str:>7}  FAILED\n")
                continue
            f.write(f"  {r['ma']:.2f}  {r['aoa']:.1f}  {r['variant']:<18}  {ts_str:>7}  "
                    f"{r['n_ma0_upper']:>7d}  {r['n_ma0_lower']:>7d}  "
                    f"{r['cl']:>8.5f}  {r['cd']:>8.5f}  {r['cm']:>8.5f}  "
                    f"{r['elapsed']:>6.1f}\n")
        f.write('=' * 105 + '\n')


if __name__ == '__main__':

    x, camber, half_thk, tmax = _base_components(CST_U, CST_L)
    print(f"Airfoil: tmax={tmax:.4f}")

    jobs = []
    for ma in MACH_LIST:
        for aoa in AOA_LIST:
            for variant in VARIANT_CONFIG.keys():
                jobs.append({
                    'ma': ma, 'aoa': aoa, 'variant': variant,
                    'x': x, 'camber': camber, 'half_thk': half_thk,
                })

    n_proc = min(mp.cpu_count(), len(jobs))
    print(f"Running {len(jobs)} cases with {n_proc} workers ...")

    with mp.Pool(n_proc) as pool:
        all_results = pool.map(_run_one, jobs)

    for ma in MACH_LIST:
        for aoa in AOA_LIST:
            cases_by_variant = {
                r['variant']: r
                for r in all_results
                if r['ma'] == ma and r['aoa'] == aoa
            }
            _plot_mach(ma, aoa, cases_by_variant, x, camber, half_thk)
            # _plot_cp(ma, aoa, cases_by_variant)

    _plot_n_ma0_heatmaps(all_results)

    fname = os.path.join(path, 'summary.txt')
    print_summary(all_results, fname)
