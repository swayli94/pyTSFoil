"""
Test 4: IBL-TSD coupling with perturbation-velocity limiter.

Combines the results of Test 2 (velocity limiter) and Test 3 (IBL coupling).
Runs four modes for each airfoil:

  baseline    - pure inviscid TSD, no IBL, no limiter
  ibl         - IBL-TSD coupling (N_OUTER iterations), no limiter
  ibl_lim_D5  - IBL-TSD + limiter D=5, applied at all nodes
  ibl_lim_ell - IBL-TSD + limiter D=5, applied at elliptic (subsonic) nodes only

Key question: does the velocity limiter suppress IBL-induced divergence in
high-AoA / high-Mach cases where the unconstrained IBL over-corrects the
wall BC and drives TSD toward divergence?

Reports per case:
  RMSE_Cp vs RANS, CL, CD_wave, CD_fric, CD_total, max n_clipped per mode.

Plots per case (3x2 grid, case_{idx:04d}_ibl4.png):
  [0,0] Cp distribution  — all 4 modes + RANS
  [0,1] Wall Mach        — all 4 modes + RANS
  [1,0] CL convergence   — IBL modes vs outer iteration (stability check)
  [1,1] Limiter n_clipped per outer iteration — limiter modes only
  [2,0] δ* and θ         — IBL modes (final iteration)
  [2,1] Skin friction cf — IBL modes (final iteration)
"""

import os
import sys
import copy
import time
import numpy as np
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
import multiprocessing as mp
from scipy.interpolate import interp1d

path      = os.path.abspath(os.path.dirname(__file__))
path_root = os.path.abspath(os.path.join(path, '..'))
sys.path.insert(0, path_root)

from pytsfoil import PyTSFoil, IBL
from airfoil_database.utils import load_airfoil_database_from_json

# ── paths ─────────────────────────────────────────────────────────────────────

fname_db  = os.path.join(path_root, 'airfoil_database', 'airfoil_database.json')
path_figs = os.path.join(path, 'figures')
os.makedirs(path_figs, exist_ok=True)

# ── parameters ────────────────────────────────────────────────────────────────

N_CASES      = 10
N_PROCESS    = 10
N_OUTER      = 10
IBL_RELAX    = 0.5
MAXIT_INNER  = 200
I_OUTER_REPAIR = 3

BASE_CFG = {
    'CVERGE':  1e-5,
    'DVERGE':  10.0,
    'EPS':     0.5,
    'IPRTER':  500,
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
    'vel_lim_theta':        1.0,
}

# (key, use_ibl, vel_lim_enabled, vel_lim_d, vel_lim_elliptic_only)
MODES = [
    ('baseline',    False, False, 5.0, False),
    ('ibl',         True,  False, 5.0, False),
    ('ibl_lim_D5',  True,  True,  5.0, False),
    ('ibl_lim_ell', True,  True,  5.0, True),
]

STYLE = {
    'baseline':    dict(color='#888888', ls='--', lw=1.5, label='Baseline (inviscid)'),
    'ibl':         dict(color='#2ca02c', ls='-',  lw=2.0, label='IBL (no limiter)'),
    'ibl_lim_D5':  dict(color='#d62728', ls='-',  lw=1.8, label='IBL + Lim D=5'),
    'ibl_lim_ell': dict(color='#9467bd', ls=':',  lw=1.8, label='IBL + Lim D=5 (elliptic)'),
}

IBL_MODES = [k for k, use_ibl, *_ in MODES if use_ibl]
LIM_MODES  = [k for k, use_ibl, vel_en, *_ in MODES if use_ibl and vel_en]


# ── worker ────────────────────────────────────────────────────────────────────

def _run_one_mode(coords, airfoil, key, use_ibl, vel_lim_en, vel_lim_d, elliptic):
    """Run a single mode for one airfoil; returns a result dict."""
    EMACH  = float(airfoil['Ma'])
    ALPHA  = float(airfoil['AoA'])
    REYNLD = float(airfoil['Re'])

    cfg = copy.deepcopy(BASE_CFG)
    cfg.update({
        'EMACH':  EMACH,
        'ALPHA':  ALPHA,
        'REYNLD': REYNLD,
        'vel_lim_enabled':       vel_lim_en,
        'vel_lim_d':             vel_lim_d,
        'vel_lim_elliptic_only': elliptic,
    })

    ts = PyTSFoil(airfoil_coordinates=coords, work_dir=path)
    ts.set_config(**cfg)

    if not use_ibl:
        t0 = time.time()
        ts.run()
        cd_wave  = ts.compute_wave_drag()
        elapsed  = time.time() - t0
        xx  = ts.mesh['xx'].copy()
        ile = ts.mesh['ile']
        ite = ts.mesh['ite']
        return {
            'use_ibl':  False,
            'xx': xx, 'ile': ile, 'ite': ite,
            'cpu': ts.data_summary['cpu'].copy(),
            'cpl': ts.data_summary['cpl'].copy(),
            'mau': ts.data_summary['mau'].copy(),
            'mal': ts.data_summary['mal'].copy(),
            'cl':  float(ts.data_summary['cl']),
            'cd':  cd_wave,
            'cd_f': float('nan'),
            'elapsed': elapsed,
            'history': [],
            'upper': None,
            'lower': None,
        }

    # IBL-coupled mode
    ibl_inst = IBL(Re=REYNLD, M_inf=EMACH)
    t0 = time.time()
    history = ts.run_ibl_coupled(
        ibl=ibl_inst,
        n_outer=N_OUTER,
        ibl_relax=IBL_RELAX,
        x_tr_upper=0.0,
        x_tr_lower=0.0,
        maxit_inner=MAXIT_INNER,
        i_outer_repair=I_OUTER_REPAIR,
    )
    cd_wave  = ts.compute_wave_drag()
    elapsed  = time.time() - t0

    xx  = ts.mesh['xx'].copy()
    ile = ts.mesh['ile']
    ite = ts.mesh['ite']
    return {
        'use_ibl':  True,
        'xx': xx, 'ile': ile, 'ite': ite,
        'cpu': ts.data_summary['cpu'].copy(),
        'cpl': ts.data_summary['cpl'].copy(),
        'mau': ts.data_summary['mau'].copy(),
        'mal': ts.data_summary['mal'].copy(),
        'cl':   float(ts.data_summary['cl']),
        'cd':   cd_wave,
        'cd_f': float(ts.data_summary['ibl_cd_f']),
        'elapsed': elapsed,
        'upper': ts.data_summary['ibl_upper'],
        'lower': ts.data_summary['ibl_lower'],
        'history': history,
    }


def run_single(airfoil: dict) -> dict:
    """Worker: run all modes for one airfoil."""
    try:
        t0 = time.time()

        xu    = np.array(airfoil['xu'])
        yu    = np.array(airfoil['yu'])
        xl    = np.array(airfoil['xl'])
        yl    = np.array(airfoil['yl'])

        x_coords = np.concatenate([xu[::-1], xl[1:]])
        y_coords = np.concatenate([yu[::-1], yl[1:]])
        coords   = np.column_stack([x_coords, y_coords])

        results = {}
        for key, use_ibl, vel_en, vel_d, elliptic in MODES:
            results[key] = _run_one_mode(
                coords, airfoil, key, use_ibl, vel_en, vel_d, elliptic)

        # RMSE Cp vs RANS for each mode
        cpu_rans = np.array(airfoil['cpu'])
        cpl_rans = np.array(airfoil['cpl'])
        for key, r in results.items():
            xx = r['xx']
            f_cpu = interp1d(xx, r['cpu'], kind='linear', fill_value='extrapolate')
            f_cpl = interp1d(xx, r['cpl'], kind='linear', fill_value='extrapolate')
            r['rmse_cp'] = float(np.sqrt(0.5 * (
                np.mean((f_cpu(xu) - cpu_rans) ** 2) +
                np.mean((f_cpl(xl) - cpl_rans) ** 2)
            )))

        _plot(airfoil, results)

        return {
            'success':     True,
            'entry_index': airfoil['entry_index'],
            'airfoil_id':  airfoil['airfoil_id'],
            'Ma': float(airfoil['Ma']),
            'AoA': float(airfoil['AoA']),
            'Re':  float(airfoil['Re']),
            'cl_rans': float(airfoil['CL_RANS']),
            'cd_rans': float(airfoil['Cd_RANS']),
            'elapsed': time.time() - t0,
            'results': results,
        }
    except Exception as e:
        import traceback
        print(f"Error in case {airfoil.get('entry_index', '?')}: {e}")
        return {
            'success':     False,
            'entry_index': airfoil.get('entry_index', -1),
            'error':       traceback.format_exc(),
        }


# ── plotting ──────────────────────────────────────────────────────────────────

def _plot(airfoil: dict, results: dict):
    idx = airfoil['entry_index']
    Ma  = float(airfoil['Ma'])
    AoA = float(airfoil['AoA'])
    Re  = float(airfoil['Re'])

    xu = np.array(airfoil['xu'])
    xl = np.array(airfoil['xl'])

    # Use mesh from baseline for common x-axis
    r0   = results['baseline']
    xx   = r0['xx']
    ile  = r0['ile']
    ite  = r0['ite']
    xx_foil = xx[ile:ite + 1]

    fig, axes = plt.subplots(3, 2, figsize=(16, 18))
    fig.suptitle(
        f"Case {idx} | airfoil={airfoil['airfoil_id']}  "
        f"$M_\\infty={Ma:.3f}$, $\\alpha={AoA:.2f}°$, $Re={Re:.1e}$\n"
        f"IBL-TSD coupling + velocity limiter effect",
        fontsize=12, fontweight='bold',
    )

    # ── [0,0] Cp distribution ─────────────────────────────────────────────────
    ax = axes[0, 0]
    ax.plot(xu, airfoil['cpu'], color='k', lw=4.0, alpha=0.4, ls='-',  label='RANS upper')
    ax.plot(xl, airfoil['cpl'], color='k', lw=4.0, alpha=0.4, ls='--', label='RANS lower')
    for key, r in results.items():
        sty = STYLE[key]
        xx_r = r['xx']
        ax.plot(xx_r, r['cpu'], color=sty['color'], lw=sty['lw'], ls=sty['ls'])
        ax.plot(xx_r, r['cpl'], color=sty['color'], lw=sty['lw'], ls=sty['ls'],
                label=sty['label'])
    ax.invert_yaxis()
    ax.set_xlim(-0.02, 1.02)
    ax.set_xlabel('$x/c$')
    ax.set_ylabel('$C_p$')
    ax.set_title('Pressure coefficient $C_p$')
    ax.legend(fontsize=8, ncol=2, loc='lower right')
    ax.grid(True, alpha=0.3)

    # ── [0,1] Wall Mach ───────────────────────────────────────────────────────
    ax = axes[0, 1]
    ax.plot(xu, airfoil['mwu'], color='k', lw=4.0, alpha=0.4, ls='-',  label='RANS upper')
    ax.plot(xl, airfoil['mwl'], color='k', lw=4.0, alpha=0.4, ls='--', label='RANS lower')
    for key, r in results.items():
        sty = STYLE[key]
        xx_r = r['xx']
        ax.plot(xx_r, r['mau'], color=sty['color'], lw=sty['lw'], ls=sty['ls'])
        ax.plot(xx_r, r['mal'], color=sty['color'], lw=sty['lw'], ls=sty['ls'],
                label=sty['label'])
    ax.axhline(1.0, color='k', lw=0.9, ls=':', alpha=0.5, label='Sonic ($M=1$)')
    ax.set_xlim(-0.02, 1.02)
    ax.set_ylim(0, None)
    ax.set_xlabel('$x/c$')
    ax.set_ylabel('Mach number')
    ax.set_title('Wall Mach number')
    ax.legend(fontsize=8, ncol=2)
    ax.grid(True, alpha=0.3)

    # ── [1,0] CL convergence history ──────────────────────────────────────────
    ax = axes[1, 0]
    ax2 = ax.twinx()
    has_any = False
    for key in IBL_MODES:
        r = results[key]
        hist = r['history']
        if not hist:
            continue
        has_any = True
        sty   = STYLE[key]
        iters = np.arange(1, len(hist) + 1)
        cl_h  = np.array([h['cl']   for h in hist])
        cdf_h = np.array([h['cd_f'] for h in hist])
        ax.plot(iters, cl_h,    'o-', color=sty['color'], lw=1.8, ms=5,
                label=f'{sty["label"]} CL')
        ax2.plot(iters, cdf_h * 1e4, 's--', color=sty['color'], lw=1.2, ms=4, alpha=0.6)
    if has_any:
        ax.axhline(float(airfoil['CL_RANS']), color='k', lw=1.0, ls=':', label='CL (RANS)')
    ax.set_xlabel('Outer iteration')
    ax.set_ylabel('$C_L$')
    ax2.set_ylabel('$C_{d,f} \\times 10^4$', color='gray')
    ax2.tick_params(axis='y', labelcolor='gray')
    ax.set_title('Coupling convergence — $C_L$ (solid) / $C_{d,f}$ (dashed, right axis)')
    if has_any:
        ax.set_xticks(np.arange(1, N_OUTER + 1))
    ax.legend(fontsize=7, loc='right')
    ax.grid(True, alpha=0.3)

    # ── [1,1] Airfoil geometry + viscous effective body (ibl, no limiter) ────
    ax = axes[1, 1]
    r_ibl     = results.get('ibl', {})
    upper_ibl = r_ibl.get('upper')
    lower_ibl = r_ibl.get('lower')
    C_IBL_color = STYLE['ibl']['color']

    # Interpolate physical surface y-coords onto mesh x-grid
    yu_foil = np.interp(xx_foil, xu, np.array(airfoil['yu']))
    yl_foil = np.interp(xx_foil, xl, np.array(airfoil['yl']))

    # Airfoil contour
    ax.plot(xx_foil, yu_foil, 'k-', lw=1.8, label='Airfoil geometry')
    ax.plot(xx_foil, yl_foil, 'k-', lw=1.8)
    ax.plot([xx_foil[-1], xx_foil[-1]], [yu_foil[-1], yl_foil[-1]], 'k-', lw=1.8)

    # Effective body ± δ* (ibl mode only)
    if upper_ibl is not None and lower_ibl is not None:
        yu_eff = yu_foil + upper_ibl['delta_star']
        yl_eff = yl_foil - lower_ibl['delta_star']
        ax.plot(xx_foil, yu_eff, color=C_IBL_color, lw=1.6, ls='--',
                label='Effective body (airfoil ± δ*)')
        ax.plot(xx_foil, yl_eff, color=C_IBL_color, lw=1.6, ls='--')
        ax.fill_between(xx_foil, yu_foil, yu_eff, alpha=0.25, color=C_IBL_color,
                        label='δ* region')
        ax.fill_between(xx_foil, yl_eff, yl_foil, alpha=0.25, color=C_IBL_color)

    # Freestream direction: dashed line through the trailing edge at angle AoA
    alpha_rad = np.radians(AoA)
    x_te = xx_foil[-1]
    y_te = (yu_foil[-1] + yl_foil[-1]) / 2
    x_line = np.array([-0.1, 1.1])
    y_line = y_te + (x_line - x_te) * np.tan(alpha_rad)
    ax.plot(x_line, y_line, color='royalblue', ls='--', lw=1.2, alpha=0.85,
            label=f'$U_\\infty$  ($\\alpha={AoA:.1f}°$)')

    ax.set_xlabel('$x/c$')
    ax.set_ylabel('$y/c$')
    ax.set_title('Airfoil geometry + viscous effective body (IBL, no limiter)')
    ax.set_xlim(-0.1, 1.1)
    ax.set_ylim(-0.1, 0.1)
    ax.legend(fontsize=8, loc='upper right')
    ax.grid(True, alpha=0.3)

    # ── [2,0] δ* and θ ────────────────────────────────────────────────────────
    ax = axes[2, 0]
    C_U = '#1f77b4'
    C_L = '#d62728'
    for key in IBL_MODES:
        r     = results[key]
        upper = r['upper']
        lower = r['lower']
        if upper is None:
            continue
        sty = STYLE[key]
        i_tr_u = upper['i_tr']
        i_tr_l = lower['i_tr']
        # δ* (thick lines)
        if i_tr_u > 1:
            ax.plot(xx_foil[:i_tr_u + 1], upper['delta_star'][:i_tr_u + 1],
                    color=sty['color'], lw=2.0, ls='--', alpha=0.6)
        ax.plot(xx_foil[i_tr_u:], upper['delta_star'][i_tr_u:],
                color=sty['color'], lw=2.0, ls='-', label=f'{sty["label"]} δ*')
        # lower δ* (thin lines)
        if i_tr_l > 1:
            ax.plot(xx_foil[:i_tr_l + 1], lower['delta_star'][:i_tr_l + 1],
                    color=sty['color'], lw=1.0, ls='--', alpha=0.6)
        ax.plot(xx_foil[i_tr_l:], lower['delta_star'][i_tr_l:],
                color=sty['color'], lw=1.0, ls='-')
    ax.set_xlabel('$x/c$')
    ax.set_ylabel('Thickness / $c$')
    ax.set_title('Displacement thickness $\\delta^*$ (thick=upper, thin=lower)')
    ax.set_xlim(-0.02, 1.02)
    ax.legend(fontsize=7, loc='upper left')
    ax.grid(True, alpha=0.3)

    # ── [2,1] Skin friction cf ────────────────────────────────────────────────
    ax = axes[2, 1]
    for key in IBL_MODES:
        r     = results[key]
        upper = r['upper']
        lower = r['lower']
        if upper is None:
            continue
        sty = STYLE[key]
        i_tr_u = upper['i_tr']
        i_tr_l = lower['i_tr']
        # upper cf (solid)
        if i_tr_u > 1:
            ax.plot(xx_foil[:i_tr_u + 1], upper['cf'][:i_tr_u + 1],
                    color=sty['color'], lw=1.8, ls='--', alpha=0.6)
        ax.plot(xx_foil[i_tr_u:], upper['cf'][i_tr_u:],
                color=sty['color'], lw=1.8, ls='-', label=f'{sty["label"]} (upper)')
        # lower cf (thin)
        if i_tr_l > 1:
            ax.plot(xx_foil[:i_tr_l + 1], lower['cf'][:i_tr_l + 1],
                    color=sty['color'], lw=1.0, ls='--', alpha=0.6)
        ax.plot(xx_foil[i_tr_l:], lower['cf'][i_tr_l:],
                color=sty['color'], lw=1.0, ls='-')
    ax.set_xlabel('$x/c$')
    ax.set_ylabel('$c_f$')
    ax.set_title('Skin friction (thick=upper, thin=lower, dashed=laminar)')
    ax.set_xlim(-0.02, 1.02)
    ax.set_ylim(-0.0005, None)
    ax.legend(fontsize=7, loc='upper right')
    ax.grid(True, alpha=0.3)

    fig.tight_layout()
    fig.savefig(os.path.join(path_figs, f'case_{idx:04d}_ibl4.png'),
                dpi=120, bbox_inches='tight')
    plt.close(fig)


# ── summary ───────────────────────────────────────────────────────────────────

def _n_clipped_max(r):
    """Maximum n_clipped across outer iterations for a limiter mode, or 0."""
    hist = r.get('history', [])
    if not hist or 'vel_lim_n_clipped' not in hist[0]:
        return 0
    return max(h.get('vel_lim_n_clipped', 0) for h in hist)


def print_summary(all_results, fname):
    mode_keys = [m[0] for m in MODES]
    col_w = 12

    header_modes = '  '.join(f'{k[:col_w]:>{col_w}}' for k in mode_keys)

    with open(fname, 'w') as f:
        f.write('\n' + '=' * 120 + '\n')
        f.write(f'{"idx":>4}  {"Ma":>5}  {"AoA":>5}  {"Re":>8}  '
                f'{"metric":<16}  {header_modes}  {"RANS":>{col_w}}\n')
        f.write('-' * 120 + '\n')

        for rec in all_results:
            if not rec['success']:
                f.write(f"  {rec['entry_index']:>3}  FAILED\n\n")
                continue

            idx = rec['entry_index']
            Ma  = rec['Ma']
            AoA = rec['AoA']
            Re  = rec['Re']
            res = rec['results']

            def fmtf(v):
                return f'{v:>{col_w}.5f}' if isinstance(v, float) and not np.isnan(v) else f'{"N/A":>{col_w}}'
            def fmti(v):
                return f'{int(v):>{col_w}d}' if isinstance(v, (int, np.integer)) else f'{"N/A":>{col_w}}'

            rows = [
                ('RMSE_Cp', [fmtf(res[k]['rmse_cp']) for k in mode_keys],
                 f'{"N/A":>{col_w}}'),
                ('CL',      [fmtf(res[k]['cl'])       for k in mode_keys],
                 fmtf(rec['cl_rans'])),
                ('CD_wave', [fmtf(res[k]['cd'])        for k in mode_keys],
                 f'{"N/A":>{col_w}}'),
                ('CD_fric', [fmtf(res[k]['cd_f'])      for k in mode_keys],
                 f'{"N/A":>{col_w}}'),
                ('CD_total',[fmtf(res[k]['cd'] + (res[k]['cd_f'] if not np.isnan(res[k]['cd_f']) else 0.0))
                             for k in mode_keys],
                 fmtf(rec['cd_rans'])),
                ('n_clipped_max', [fmti(_n_clipped_max(res[k])) for k in mode_keys],
                 f'{"N/A":>{col_w}}'),
                ('CPU_s',        [f'{res[k]["elapsed"]:>{col_w}.2f}' for k in mode_keys],
                 f'{"N/A":>{col_w}}'),
            ]

            first = True
            for metric, vals, rans_val in rows:
                prefix = f" {idx:>3}  {Ma:>5.3f}  {AoA:>5.2f}  {Re:>8.1e}" if first else ' ' * 28
                first = False
                row = '  '.join(vals)
                f.write(f"{prefix}  {metric:<16}  {row}  {rans_val}\n")
            f.write('\n')

        f.write('=' * 120 + '\n')


# ── main ──────────────────────────────────────────────────────────────────────

if __name__ == '__main__':

    db    = load_airfoil_database_from_json(fname_db)
    cases = list(db.values())[:N_CASES]

    print(f"Running {N_CASES} cases with N_PROCESS={N_PROCESS} ...")
    print(f"  Modes: {[m[0] for m in MODES]}")
    print(f"  N_OUTER={N_OUTER}  IBL_RELAX={IBL_RELAX}  forced transition at x=0")

    with mp.Pool(N_PROCESS) as pool:
        all_results = pool.map(run_single, cases)

    n_ok   = sum(1 for r in all_results if r['success'])
    n_fail = len(all_results) - n_ok
    print(f"Done: {n_ok}/{len(all_results)} succeeded, {n_fail} failed.")

    fname = os.path.join(path_figs, 'summary.txt')
    print_summary(all_results, fname)
