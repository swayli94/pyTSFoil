"""
Test 2: TSD perturbation-velocity limiter.

Compares the TSD solution with the velocity limiter enabled at different
threshold values (D) against the baseline (no limiter) and RANS reference.

Reports per mode:
  - RMSE Cp vs RANS (upper / lower averaged)
  - n_clipped, max_u_before in the last sweep
  - CL, CD relative change vs baseline

Plots per case:
  1. Cp distribution: baseline / limiter variants / RANS, with shaded bands
     where |uu| (upper) or |ul| (lower) in the baseline exceeds D.
  2. ΔCp = Cp_variant − Cp_baseline + activity markers.
  3. Perturbation velocity |uu|, |ul| with D-thresholds overlaid.
"""

import os
import sys
import copy
import time
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches
import multiprocessing as mp
from scipy.interpolate import interp1d

path       = os.path.abspath(os.path.dirname(__file__))
path_root  = os.path.abspath(os.path.join(path, '..'))
sys.path.append(path_root)

from cst_modeling.section import cst_foil
from pytsfoil import PyTSFoil
from airfoil_database.utils import load_airfoil_database_from_json

# ── paths ─────────────────────────────────────────────────────────────────────
fname_db  = os.path.join(path_root, 'airfoil_database', 'airfoil_database.json')
path_figs = os.path.join(path, 'figures')
os.makedirs(path_figs, exist_ok=True)

# ── baseline config ────────────────────────────────────────────────────────────
BASE_CFG = {
    'CVERGE':  1e-5,
    'DVERGE':  10.0,
    'EPS':     0.5,
    'IPRTER':  100,
    'MAXIT':   9999,
    'RIGF':    1.0,
    'SIMDEF':  3,
    'WCIRC':   1.0,
    'WE':      [1.8, 1.9, 1.95],
    'NWDGE':   1,
    'WCONST':  4.0,
    'IFLAP':   0,
    'DELFLP':  0.0,
    'FLPLOC':  0.77,
    'n_point_x':       200,
    'n_point_y':        80,
    'n_point_airfoil': 100,
    'flag_output':          False,
    'flag_output_summary':  False,
    'flag_output_shock':    False,
    'flag_output_field':    False,
    'flag_print_info':      False,
}

# Modes: (label, vel_lim_enabled, vel_lim_d, vel_lim_elliptic_only)
# D is in Krupp similarity units; SONVEL ≈ 1.1 for typical cases (Ma~0.72, delta~0.12).
# D=5 allows up to ~4× SONVEL, equivalent to |u'/U_inf| ≈ 1.7  (clips LE singularity only).
# D=2 clips more aggressively, entering the physical supersonic region.
# D=100 is a sanity check: should be essentially identical to baseline (n_clipped ≈ 0).
MODES = [
    ('baseline',     False, 5.0,  False),
    ('lim_D5',       True,  5.0,  False),
    ('lim_D3',       True,  3.0,  False),
    ('lim_elliptic', True,  3.0,  True),
]

# Colours / linestyles for each mode in plots
STYLE = {
    'baseline':     dict(color='steelblue',  ls='--', lw=2.0, label='Baseline (no limiter)'),
    'lim_D5':       dict(color='tomato',     ls='-',  lw=1.5, label='Limiter D=5'),
    'lim_D3':       dict(color='darkred',    ls='-',  lw=2.0, label='Limiter D=3'),
    'lim_elliptic': dict(color='purple',     ls=':',  lw=1.5, label='Limiter D=3 (elliptic only)'),
}

N_CASES   = 10
N_PROCESS = 10
GAMMA     = 1.4


def _cp_isentropic(ma, minf, gamma=GAMMA):
    denom = 2.0 + (gamma - 1.0) * ma ** 2
    numer = 2.0 + (gamma - 1.0) * minf ** 2
    return (2.0 / (gamma * minf ** 2)) * ((numer / denom) ** (gamma / (gamma - 1.0)) - 1.0)


def _run_mode(coords, airfoil, label, enabled, d_val, elliptic_only):
    cfg = copy.deepcopy(BASE_CFG)
    cfg.update({
        'ALPHA':  airfoil['AoA'],
        'EMACH':  airfoil['Ma'],
        'REYNLD': airfoil['Re'],
        'vel_lim_enabled':       enabled,
        'vel_lim_d':             d_val,
        'vel_lim_theta':         1.0,
        'vel_lim_elliptic_only': elliptic_only,
    })
    ts = PyTSFoil(airfoil_coordinates=coords, work_dir=path)
    ts.set_config(**cfg)
    ts.run()

    xx  = ts.mesh['xx']
    ile = ts.mesh['ile']
    ite = ts.mesh['ite']

    cpu = ts.data_summary['cpu']
    cpl = ts.data_summary['cpl']
    mau = ts.data_summary['mau']
    mal = ts.data_summary['mal']
    uu  = ts.data_summary['uu']   # upper-surface streamwise perturbation velocity (P_x)
    ul  = ts.data_summary['ul']   # lower-surface streamwise perturbation velocity (P_x)
    cl  = ts.data_summary['cl']
    cm  = ts.data_summary['cm']
    cd  = ts.data_summary['cd']

    # RMSE vs RANS
    fcpu = interp1d(xx, cpu, kind='linear', fill_value='extrapolate')
    fcpl = interp1d(xx, cpl, kind='linear', fill_value='extrapolate')
    rmse_cp = float(np.sqrt(0.5 * (
        np.mean((fcpu(airfoil['xu']) - airfoil['cpu']) ** 2) +
        np.mean((fcpl(airfoil['xl']) - airfoil['cpl']) ** 2)
    )))

    # Ma=0 count on lower surface
    n_ma0 = int(np.sum(mal[ile:ite + 1] == 0.0))

    # Limiter diagnostics (only populated when enabled)
    n_clipped    = ts.data_summary.get('vel_lim_n_clipped',    0)
    max_u_before = ts.data_summary.get('vel_lim_max_u_before', float('nan'))
    n_infeasible = ts.data_summary.get('vel_lim_n_infeasible', 0)

    return {
        'xx': xx, 'ile': ile, 'ite': ite,
        'yy': ts.mesh['yy'],
        'jlow': ts.mesh['jlow'],
        'jup':  ts.mesh['jup'],
        'cpu': cpu, 'cpl': cpl, 'mau': mau, 'mal': mal,
        'uu': uu, 'ul': ul,
        'cl': cl, 'cm': cm, 'cd': cd,
        'rmse_cp': rmse_cp, 'n_ma0': n_ma0,
        'n_clipped': n_clipped,
        'max_u_before': max_u_before,
        'n_infeasible': n_infeasible,
        'd_val': d_val,
        'enabled': enabled,
        'P_field':  ts.data_summary.get('P_field'),
        'clip_map': ts.data_summary.get('vel_lim_clip_map'),
    }


def run_single(airfoil: dict) -> dict:
    """Worker: run all modes for one airfoil."""
    try:
        t0 = time.time()

        cst_u = airfoil['cst_u']
        cst_l = airfoil['cst_l']
        tmax  = airfoil['tmax']
        x, yu, yl, _, _ = cst_foil(201, cst_u, cst_l, x=None, t=tmax, tail=0.0)
        coords = np.column_stack((
            np.concatenate([x[::-1], x[1:]]),
            np.concatenate([yu[::-1], yl[1:]])
        ))

        results = {}
        for (label, enabled, d_val, elliptic) in MODES:
            results[label] = _run_mode(coords, airfoil, label, enabled, d_val, elliptic)

        _plot(airfoil, results)
        _plot_velocity(airfoil, results)
        _plot_field(airfoil, results)

        return {
            'success':     True,
            'entry_index': airfoil['entry_index'],
            'airfoil_id':  airfoil['airfoil_id'],
            'Ma':          airfoil['Ma'],
            'AoA':         airfoil['AoA'],
            'elapsed':     time.time() - t0,
            'results':     results,
        }
    except Exception as e:
        import traceback
        print(f"Error in case {airfoil['entry_index']}: {e}")
        return {
            'success':     False,
            'entry_index': airfoil['entry_index'],
            'error':       traceback.format_exc(),
        }


def _limiter_active_mask(xx, uu_or_ul, ile, ite, d_val):
    """Boolean mask on the airfoil x-grid where |u_pert| > d_val."""
    x_foil = xx[ile:ite + 1]
    u_foil = uu_or_ul[ile:ite + 1]
    return x_foil, np.abs(u_foil) > d_val


def _shade_active(ax, x_foil, active_mask, color, alpha=0.15):
    """Shade vertical bands where active_mask is True."""
    in_band = False
    x_start = None
    for i, (xi, act) in enumerate(zip(x_foil, active_mask)):
        if act and not in_band:
            x_start = xi
            in_band = True
        elif not act and in_band:
            ax.axvspan(x_start, xi, color=color, alpha=alpha)
            in_band = False
    if in_band:
        ax.axvspan(x_start, x_foil[-1], color=color, alpha=alpha)


def _plot_field(airfoil, results):
    """
    2×N_LIM figure: for each limiter mode (with clip_map), show
      Left : P-field contourf with airfoil outline
      Right: same P-field + scatter of clipped nodes (last-sweep clip_map > 0)
    Baseline P-field is shown as grey contour lines underneath for reference.
    """
    idx = airfoil['entry_index']
    Ma  = airfoil['Ma']
    AoA = airfoil['AoA']

    r_base = results['baseline']
    xx     = r_base['xx']
    yy     = r_base['yy']
    ile    = r_base['ile']
    ite    = r_base['ite']
    jlow   = r_base['jlow']
    jup    = r_base['jup']
    P_base = r_base['P_field']   # shape (jmax, imax)

    lim_modes = [(label, r) for label, r in results.items()
                 if r['enabled'] and r['clip_map'] is not None]
    if not lim_modes:
        return

    n_cols = len(lim_modes)
    fig, axes = plt.subplots(2, n_cols, figsize=(8 * n_cols, 10), squeeze=False)
    fig.suptitle(
        f"Case {idx} | {airfoil['airfoil_id']} | Ma={Ma:.2f}, AoA={AoA:.2f}°  —  "
        f"Velocity potential P & clip activity (last sweep)",
        fontsize=11)

    XX, YY = np.meshgrid(xx, yy)  # both shape (jmax, imax)

    # Shared colour scale across all P panels
    p_all = np.concatenate([r_base['P_field'].ravel()] +
                           [r['P_field'].ravel() for _, r in lim_modes])
    vmin, vmax = np.percentile(p_all, 2), np.percentile(p_all, 98)

    # Airfoil x-range mask for clipping plots
    x_foil_mask = (XX[0, :] >= xx[ile]) & (XX[0, :] <= xx[ite])

    for col, (label, r) in enumerate(lim_modes):
        P_lim   = r['P_field']    # shape (jmax, imax)
        clip_map = r['clip_map']  # shape (jmax, imax), counts in last sweep

        sty  = STYLE[label]
        d_val = r['d_val']

        for row, (P_show, title_sfx) in enumerate([(P_lim, 'P field'),
                                                    (P_lim, 'Clip activity')]):
            ax = axes[row, col]

            # Filled contour of P
            cf = ax.contourf(XX, YY, P_show, levels=40, cmap='RdBu_r',
                             vmin=vmin, vmax=vmax)

            # Grey reference contours from baseline
            ax.contour(XX, YY, P_base, levels=20, colors='k',
                       linewidths=0.3, alpha=0.35)

            # Airfoil surface lines (JUP / JLOW rows, ILE..ITE columns)
            ax.plot(xx[ile:ite+1], np.full(ite - ile + 1, yy[jup]),  'w-', lw=1.2)
            ax.plot(xx[ile:ite+1], np.full(ite - ile + 1, yy[jlow]), 'w-', lw=1.2)
            # Leading / trailing edge markers
            for xi in [xx[ile], xx[ite]]:
                ax.axvline(xi, color='white', lw=0.6, ls=':')

            if row == 1:
                # Scatter clipped nodes (clip_map > 0)
                Jc, Ic = np.where(clip_map > 0)
                if len(Jc) > 0:
                    counts = clip_map[Jc, Ic]
                    sc = ax.scatter(xx[Ic], yy[Jc], c=counts, cmap='hot_r',
                                    s=12, zorder=5, linewidths=0,
                                    vmin=1, vmax=max(counts.max(), 2))
                    plt.colorbar(sc, ax=ax, label='clip count (last sweep)', shrink=0.7)
                    ax.set_title(f'{label} | {title_sfx}  '
                                 f'(n_clipped={r["n_clipped"]})', fontsize=9)
                else:
                    ax.set_title(f'{label} | {title_sfx}  (no clips in last sweep)', fontsize=9)
            else:
                plt.colorbar(cf, ax=ax, label='P', shrink=0.7)
                ax.set_title(f'{label} | {title_sfx}  D={d_val}', fontsize=9)

            ax.set(xlabel='x/c', ylabel='y/c')
            # Zoom to region of interest: from LE-upstream to a bit past TE
            ax.set_xlim(max(xx[0], xx[ile] - 0.3), min(xx[-1], xx[ite] + 0.5))
            ax.set_ylim(max(yy[0], yy[jlow] - 0.5), min(yy[-1], yy[jup] + 0.5))
            ax.set_aspect('equal', adjustable='box')

    fig.tight_layout()
    fig.savefig(os.path.join(path_figs, f'case_{idx:04d}_field.png'),
                dpi=110, bbox_inches='tight')
    plt.close(fig)


def _plot(airfoil, results):
    idx = airfoil['entry_index']
    Ma  = airfoil['Ma']
    AoA = airfoil['AoA']

    r_base = results['baseline']
    xx  = r_base['xx']
    ile = r_base['ile']
    ite = r_base['ite']
    x_foil = xx[ile:ite + 1]

    fig, axes = plt.subplots(1, 3, figsize=(18, 5))
    fig.suptitle(
        f"Case {idx} | {airfoil['airfoil_id']} | Ma={Ma:.2f}, AoA={AoA:.2f}°",
        fontsize=11)

    # ── [0] Cp distribution ───────────────────────────────────────────────────
    ax = axes[0]
    for label, r in results.items():
        sty = STYLE[label]
        ax.plot(r['xx'], r['cpu'], lw=sty['lw'], color=sty['color'], ls=sty['ls'])
        ax.plot(r['xx'], r['cpl'], lw=sty['lw'], color=sty['color'], ls=sty['ls'],
                label=sty['label'])

    ax.plot(airfoil['xu'], airfoil['cpu'], 'g^', ms=4, markevery=4, label='RANS upper')
    ax.plot(airfoil['xl'], airfoil['cpl'], 'gv', ms=4, markevery=4, label='RANS lower')

    # Shade limiter-active regions derived from baseline |uu|/|ul|
    for label, r in results.items():
        if not r['enabled']:
            continue
        d_val = r['d_val']
        _, act_u = _limiter_active_mask(xx, r_base['uu'], ile, ite, d_val)
        _, act_l = _limiter_active_mask(xx, r_base['ul'], ile, ite, d_val)
        c = STYLE[label]['color']
        _shade_active(ax, x_foil, act_u | act_l, color=c, alpha=0.10)

    ax.invert_yaxis()
    ax.set(title='Cp Distribution', xlabel='x/c', ylabel='Cp')
    ax.set_xlim(-0.05, 1.05)
    ax.legend(fontsize=6)
    ax.grid()

    # ── [1] ΔCp vs baseline ───────────────────────────────────────────────────
    ax = axes[1]
    for label, r in results.items():
        if label == 'baseline':
            continue
        sty = STYLE[label]
        delta_u = r['cpu'][ile:ite + 1] - r_base['cpu'][ile:ite + 1]
        delta_l = r['cpl'][ile:ite + 1] - r_base['cpl'][ile:ite + 1]
        ax.plot(x_foil, delta_u, lw=sty['lw'], color=sty['color'], ls=sty['ls'],
                label=sty['label'])
        ax.plot(x_foil, delta_l, lw=sty['lw'], color=sty['color'], ls=sty['ls'])

        # Mark where limiter fires in baseline velocity field
        d_val = r['d_val']
        _, act_u = _limiter_active_mask(xx, r_base['uu'], ile, ite, d_val)
        _, act_l = _limiter_active_mask(xx, r_base['ul'], ile, ite, d_val)
        _shade_active(ax, x_foil, act_u | act_l, color=STYLE[label]['color'], alpha=0.12)

    ax.axhline(0, color='gray', lw=0.7, ls=':')
    ax.set(title='ΔCp vs baseline (airfoil region)', xlabel='x/c', ylabel='ΔCp')
    ax.set_xlim(0.0, 1.0)
    ax.legend(fontsize=6)
    ax.grid()

    # ── [2] Mach distribution ─────────────────────────────────────────────────
    ax = axes[2]
    for label, r in results.items():
        sty = STYLE[label]
        ax.plot(r['xx'], r['mau'], lw=sty['lw'], color=sty['color'], ls=sty['ls'])
        ax.plot(r['xx'], r['mal'], lw=sty['lw'], color=sty['color'], ls=sty['ls'],
                label=sty['label'])
    ax.axhline(0, color='k', lw=0.5, ls=':')
    ax.axhline(1, color='k', lw=0.5, ls='--')
    ax.set(title='Mach Distribution', xlabel='x/c', ylabel='Mach')
    ax.set_xlim(-0.05, 1.05)
    ax.legend(fontsize=6)
    ax.grid()

    fig.tight_layout()
    fig.savefig(os.path.join(path_figs, f'case_{idx:04d}_cp.png'), dpi=100, bbox_inches='tight')
    plt.close(fig)


def _plot_velocity(airfoil, results):
    """Plot |uu|, |ul| vs D thresholds, and show where limiter fires."""
    idx = airfoil['entry_index']
    Ma  = airfoil['Ma']
    AoA = airfoil['AoA']

    r_base = results['baseline']
    xx     = r_base['xx']
    ile    = r_base['ile']
    ite    = r_base['ite']
    x_foil = xx[ile:ite + 1]
    uu_foil = r_base['uu'][ile:ite + 1]
    ul_foil = r_base['ul'][ile:ite + 1]

    fig, axes = plt.subplots(1, 2, figsize=(14, 5))
    fig.suptitle(
        f"Case {idx} | {airfoil['airfoil_id']} | Ma={Ma:.2f}, AoA={AoA:.2f}° — "
        f"Perturbation velocity & limiter activity",
        fontsize=11)

    d_values = sorted({r['d_val'] for r in results.values() if r['enabled']})
    d_colors = {2.0: 'darkred', 5.0: 'tomato', 10.0: 'orange'}

    for ax, u_foil, surface in [(axes[0], uu_foil, 'Upper'), (axes[1], ul_foil, 'Lower')]:
        ax.plot(x_foil, np.abs(u_foil), 'steelblue', lw=1.5, label='|u_pert| (baseline)')
        ax.fill_between(x_foil, 0, np.abs(u_foil), alpha=0.15, color='steelblue')

        for d in d_values:
            c = d_colors.get(d, 'gray')
            ax.axhline(d, color=c, lw=1.2, ls='--', label=f'D = {d}')
            # Shade active regions
            active = np.abs(u_foil) > d
            in_band = False
            x_start = None
            for xi, act in zip(x_foil, active):
                if act and not in_band:
                    x_start = xi
                    in_band = True
                elif not act and in_band:
                    ax.axvspan(x_start, xi, color=c, alpha=0.18,
                               label=f'Active (D={d})' if x_start == x_foil[active][0] else '')
                    in_band = False
            if in_band:
                ax.axvspan(x_start, x_foil[-1], color=c, alpha=0.18)

            # Mark active nodes with scatter
            ax.scatter(x_foil[active], np.abs(u_foil)[active],
                       color=c, s=12, zorder=5)

        ax.set(title=f'{surface} surface: |u_pert| (P_x)', xlabel='x/c', ylabel='|P_x|')
        ax.set_xlim(-0.01, 1.01)
        ax.legend(fontsize=7)
        ax.grid()

    fig.tight_layout()
    fig.savefig(os.path.join(path_figs, f'case_{idx:04d}_velocity.png'), dpi=100, bbox_inches='tight')
    plt.close(fig)


def print_summary(all_results, fname):
    mode_labels = [m[0] for m in MODES]

    header_mode = '  '.join(f'{m[:10]:>10}' for m in mode_labels)
    
    with open(fname, 'w') as f:
    
        f.write('\n' + '=' * 110 + '\n')
        f.write(f'{"idx":>4}  {"Ma":>5}  {"AoA":>5}  {"metric":<14}  {header_mode}\n')
        f.write('-' * 110 + '\n')

        for r in all_results:
            if not r['success']:
                f.write(f"  {r['entry_index']:>3}  FAILED: {str(r['error'])[:70]}\n")
                continue

            idx = r['entry_index']
            Ma  = r['Ma']
            AoA = r['AoA']
            res = r['results']
            base = res['baseline']

            rows = {
                'RMSE_Cp':    {m: res[m]['rmse_cp'] for m in mode_labels},
                'CL':         {m: res[m]['cl']       for m in mode_labels},
                'dCL%':       {m: (res[m]['cl'] - base['cl']) / (abs(base['cl']) + 1e-9) * 100
                            for m in mode_labels},
                'n_clipped':  {m: res[m]['n_clipped']    for m in mode_labels},
                'max_u_bef':  {m: res[m]['max_u_before'] for m in mode_labels},
            }

            first = True
            for metric, vals in rows.items():
                prefix = f" {idx:>3}  {Ma:>5.2f}  {AoA:>5.2f}" if first else ' ' * 18
                first = False
                row = '  '.join(
                    f'{vals[m]:>10.4f}' if isinstance(vals[m], float) else f'{vals[m]:>10}'
                    for m in mode_labels
                )
                f.write(f"{prefix}  {metric:<14}  {row}\n")
            f.write('\n')

        f.write('=' * 110 + '\n')


if __name__ == '__main__':
    
    db    = load_airfoil_database_from_json(fname_db)
    cases = list(db.values())[:N_CASES]

    print(f"Running {N_CASES} cases * {len(MODES)} modes with N_PROCESS={N_PROCESS} ...")
    with mp.Pool(N_PROCESS) as pool:
        all_results = pool.map(run_single, cases)

    fname = os.path.join(path_figs, 'summary.txt')
    print_summary(all_results, fname)
