"""
Test 6: MAE leading-edge correction verification.

Tests the composite correction on the first 10 database cases and reports:
  1. Ma=0 point count before / after correction (lower surface)
  2. Cp RMSE vs RANS (upper / lower)
  3. CL, CM relative change
  4. Cp/Ma self-consistency (isentropic residual)
"""

import os
import sys
import copy
import time
import numpy as np
import matplotlib.pyplot as plt
import multiprocessing as mp
from scipy.interpolate import interp1d

path       = os.path.abspath(os.path.dirname(__file__))
path_root  = os.path.abspath(os.path.join(path, '..'))
sys.path.append(path_root)

from cst_modeling.section import cst_foil
from pytsfoil import PyTSFoil
from airfoil_database.utils import load_airfoil_database_from_json

try:
    from pytsfoil.leading_edge import solve_inner_problem, apply_composite_correction
except ImportError:
    solve_inner_problem = None
    apply_composite_correction = None

# ── paths ─────────────────────────────────────────────────────────────────────
fname_db  = os.path.join(path_root, 'airfoil_database', 'airfoil_database.json')
path_figs = os.path.join(path, 'figures')
os.makedirs(path_figs, exist_ok=True)

# ── baseline config (same as run_database_2) ──────────────────────────────────
BASE_CFG = {
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
    'n_point_x':       200,
    'n_point_y':        80,
    'n_point_airfoil': 100,
    'flag_output':          False,
    'flag_output_summary':  False,
    'flag_output_shock':    False,
    'flag_output_field':    False,
    'flag_print_info':      False,
}

N_CASES    = 10
N_PROCESS  = 10  # parallel workers
GAMMA      = 1.4


def _cp_isentropic(ma, minf, gamma=GAMMA):
    denom = 2.0 + (gamma - 1.0) * ma ** 2
    numer = 2.0 + (gamma - 1.0) * minf ** 2
    return (2.0 / (gamma * minf ** 2)) * ((numer / denom) ** (gamma / (gamma - 1.0)) - 1.0)


def run_single(airfoil: dict) -> dict:
    """Worker: run baseline and corrected cases for one airfoil."""
    try:
    # if True:
        t0 = time.time()

        cst_u  = airfoil['cst_u']
        cst_l  = airfoil['cst_l']
        tmax   = airfoil['tmax']
        x, yu, yl, _, _ = cst_foil(201, cst_u, cst_l, x=None, t=tmax, tail=0.0)
        coords = np.column_stack((np.concatenate([x[::-1], x[1:]]),
                                   np.concatenate([yu[::-1], yl[1:]])))

        results  = {}
        ts_cache = {}
        for mode in ('baseline', 'corrected'):
            cfg = copy.deepcopy(BASE_CFG)
            cfg.update({'ALPHA': airfoil['AoA'],
                        'EMACH': airfoil['Ma'],
                        'REYNLD': airfoil['Re']})
            if mode == 'corrected':
                cfg['apply_le_correction'] = True

            ts = PyTSFoil(airfoil_coordinates=coords, work_dir=path)
            ts.set_config(**cfg)
            ts.run()
            ts_cache[mode] = ts

            xx   = ts.mesh['xx']
            cpu  = ts.data_summary['cpu']
            cpl  = ts.data_summary['cpl']
            mau  = ts.data_summary['mau']
            mal  = ts.data_summary['mal']
            cl   = ts.data_summary.get('cl_le', ts.data_summary['cl']) if mode == 'corrected' else ts.data_summary['cl']
            cm   = ts.data_summary.get('cm_le', ts.data_summary['cm']) if mode == 'corrected' else ts.data_summary['cm']
            cd   = ts.data_summary['cd']

            # Interpolate to RANS grid
            fcpu = interp1d(xx, cpu, kind='linear', fill_value='extrapolate')
            fcpl = interp1d(xx, cpl, kind='linear', fill_value='extrapolate')
            _cpu = fcpu(airfoil['xu'])
            _cpl = fcpl(airfoil['xl'])
            rmse_cp = float(np.sqrt(0.5 * (np.mean((_cpu - airfoil['cpu'])**2) +
                                            np.mean((_cpl - airfoil['cpl'])**2))))

            # Ma=0 count on lower surface (within airfoil region)
            ile = ts.mesh['ile']
            ite = ts.mesh['ite']
            n_ma0 = int(np.sum(mal[ile:ite+1] == 0.0))

            # Cp/Ma self-consistency RMSE on airfoil
            cp_from_ma = _cp_isentropic(mal[ile:ite+1], airfoil['Ma'])
            cpl_airfoil = cpl[ile:ite+1]
            rmse_self = float(np.sqrt(np.mean((cp_from_ma - cpl_airfoil)**2)))

            results[mode] = {
                'xx': xx, 'cpu': cpu, 'cpl': cpl, 'mau': mau, 'mal': mal,
                'cl': cl, 'cm': cm, 'cd': cd,
                'rmse_cp': rmse_cp,
                'n_ma0': n_ma0,
                'rmse_self': rmse_self,
            }

        # ── correction diagnostics ────────────────────────────────────────────
        diag = None
        if apply_composite_correction is not None and solve_inner_problem is not None:
            ts_b  = ts_cache['baseline']
            R_c   = ts_b.airfoil.get('R_c')
            h     = ts_b.airfoil.get('h_nose')
            if R_c is not None and h is not None and R_c > 0:
                cache_dir = os.path.join(path_root, 'pytsfoil', 'leading_edge',
                                         'inner_tables_cache')
                inner_tables = solve_inner_problem(h=h, c=1.0, gamma=ts_b._gamma,
                                                   cache_dir=cache_dir)
                ile_b  = ts_b.mesh['ile']
                ite_b  = ts_b.mesh['ite']
                x_foil = ts_b.mesh['xx'][ile_b:ite_b + 1]
                cpu_foil = ts_b.data_summary['cpu'][ile_b:ite_b + 1]
                cpl_foil = ts_b.data_summary['cpl'][ile_b:ite_b + 1]
                result = ts_b._adaptive_blend_range()
                _, _, debug_upper = apply_composite_correction(
                    x_foil, cpu_foil, airfoil['Ma'], R_c, inner_tables,
                    x_blend_range=result['x_blend_range_upper'],
                    gamma=ts_b._gamma, debug=True)
                _, _, debug_lower = apply_composite_correction(
                    x_foil, cpl_foil, airfoil['Ma'], R_c, inner_tables,
                    x_blend_range=result['x_blend_range_lower'],
                    gamma=ts_b._gamma, debug=True)

                diag = {
                    'x_foil':  x_foil,
                    'upper':   debug_upper,
                    'lower':   debug_lower,
                    'R_c': R_c, 'h': h,
                }

        ts_b  = ts_cache['baseline']
        nose_R_c = ts_b.airfoil.get('R_c')
        nose_h   = ts_b.airfoil.get('h_nose')
        _plot(airfoil, results, xx, R_c=nose_R_c, h=nose_h)
        if diag is not None:
            _plot_corrections(airfoil, results, diag)

        return {
            'success':      True,
            'entry_index':  airfoil['entry_index'],
            'airfoil_id':   airfoil['airfoil_id'],
            'Ma':           airfoil['Ma'],
            'AoA':          airfoil['AoA'],
            'elapsed':      time.time() - t0,
            'results':      results,
        }
    except Exception as e:
        print(f"Error in case {airfoil['entry_index']}: {e}")
        import traceback
        return {
            'success':     False,
            'entry_index': airfoil['entry_index'],
            'error':       traceback.format_exc(),
        }


def _plot(airfoil, results, xx, R_c=None, h=None):
    idx = airfoil['entry_index']
    Ma  = airfoil['Ma']
    AoA = airfoil['AoA']

    fig, axes = plt.subplots(1, 3, figsize=(18, 5))
    fig.suptitle(f"Case {idx} | {airfoil['airfoil_id']} | Ma={Ma:.2f}, AoA={AoA:.2f}°",
                 fontsize=11)

    r_b = results['baseline']
    r_c = results['corrected']

    ax = axes[0]
    ax.plot(airfoil['xu'], airfoil['yu'], 'g-', lw=1.5, label='Airfoil')
    ax.plot(airfoil['xl'], airfoil['yl'], 'g-', lw=1.5)
    if R_c is not None and h is not None and R_c > 0:
        # Parabolic nose: stored h = h_coef/2 (Rusak convention y_surface = 2h*sqrt(x)),
        # so the coefficient that fits the actual surface is h_coef = 2h.
        h_coef = 2.0 * h
        x_le = np.linspace(0.0, 0.15, 400)
        ax.plot(x_le,  h_coef * np.sqrt(x_le), 'r--', lw=1.2, label=f'$2h\\sqrt{{x}}$  (parabola fit)')
        ax.plot(x_le, -h_coef * np.sqrt(x_le), 'r--', lw=1.2)
        # Osculating circle at LE tip: centre (R_c, 0), radius R_c
        theta = np.linspace(0.0, 2.0 * np.pi, 360)
        ax.plot(R_c + R_c * np.cos(theta), R_c * np.sin(theta),
                'b:', lw=0.9, label=f'LE circle ($R_c$)')
        ax.annotate(f'$R_c$={R_c:.5f}\n$h$={h:.4f}',
                    xy=(0, 0), xytext=(0.04, h_coef * np.sqrt(0.04) * 1.4),
                    fontsize=7, color='navy',
                    arrowprops=dict(arrowstyle='->', color='navy', lw=0.8))
        # Zoom to LE region so the parabola is visible
        y_half = max(h_coef * np.sqrt(0.15) * 1.3, R_c * 2.2)
        ax.set_xlim(-R_c * 0.5, 0.15)
        ax.set_ylim(-y_half, y_half)
        ax.set_aspect('equal', adjustable='box')
        ax.set(title='Geometry — LE zoom', xlabel='x/c', ylabel='y/c')
    else:
        ax.set(title='Geometry', xlabel='x/c', ylabel='y/c')
    ax.legend(fontsize=7)
    ax.grid()

    ax = axes[1]
    ax.plot(xx, r_b['cpu'], 'b--', label='TSD upper')
    ax.plot(xx, r_b['cpl'], 'b--', label='TSD lower')
    ax.plot(xx, r_c['cpu'], 'r-',  label='Corrected upper')
    ax.plot(xx, r_c['cpl'], 'r-',  label='Corrected lower')
    ax.plot(airfoil['xu'], airfoil['cpu'], 'g^', markevery=5, label='RANS upper', ms=4)
    ax.plot(airfoil['xl'], airfoil['cpl'], 'gv', markevery=5, label='RANS lower', ms=4)
    ax.invert_yaxis()
    ax.set(title='Cp Distribution', xlabel='x/c', ylabel='Cp')
    ax.set_xlim(-0.05, 1.05)
    ax.legend(fontsize=7)
    ax.grid()

    ax = axes[2]
    ax.plot(xx, r_b['mau'], 'b--', label='TSD upper')
    ax.plot(xx, r_b['mal'], 'b--', label='TSD lower')
    ax.plot(xx, r_c['mau'], 'r-',  label='Corrected upper')
    ax.plot(xx, r_c['mal'], 'r-',  label='Corrected lower')
    ax.axhline(0, color='k', lw=0.5, ls=':')
    ax.axhline(1, color='k', lw=0.5, ls='--')
    ax.set(title='Mach Distribution', xlabel='x/c', ylabel='Mach')
    ax.set_xlim(-0.05, 1.05)
    ax.legend(fontsize=7)
    ax.grid()

    fig.tight_layout()
    fig.savefig(os.path.join(path_figs, f'case_{idx:04d}.png'), dpi=100, bbox_inches='tight')
    plt.close(fig)


def _plot_corrections(airfoil, results, diag):

    idx = airfoil['entry_index']
    Ma  = airfoil['Ma']
    AoA = airfoil['AoA']

    x_foil = diag['x_foil']
    du     = diag['upper']
    dl     = diag['lower']
    xx     = results['baseline']['xx']

    r_b = results['baseline']
    r_c = results['corrected']

    # find airfoil-region mask in the full xx array (for corrected Cp lookup)
    ile_mask = (xx >= x_foil[0] - 1e-9) & (xx <= x_foil[-1] + 1e-9)
    xx_af    = xx[ile_mask]

    cpu_corr = r_c['cpu'][ile_mask]
    cpl_corr = r_c['cpl'][ile_mask]

    fig, axes = plt.subplots(2, 2, figsize=(14, 9))
    fig.suptitle(
        f"Case {idx} | Ma={Ma:.3f}, AoA={AoA:.2f}° — Composite correction terms",
        fontsize=11)

    x_smin, x_b0, x_b1 = dl['x_smin'], dl['x_sb0'], dl['x_sb1']

    # ── [0,0] Composition of cp_composite on upper surface ───────────────────
    ax = axes[0, 0]
    mask = x_foil <= 0.25
    ax.plot(x_foil[mask], du['cp_inner'][mask],            'y-', lw=2, label='cp_inner')
    ax.plot(x_foil[mask], du['cp_tsd'][mask],               'r-', lw=2, label='cp_tsd')
    ax.plot(x_foil[mask], du['cp_common'][mask],           'g--', lw=2, label='cp_common')
    ax.plot(x_foil[mask], du['rho_phi_bracket'][mask],     'g-', lw=1, label='ρ·φ_ox·bracket (outer match)')
    ax.plot(x_foil[mask], du['cp_composite_original'][mask],'b-', lw=2, label='cp_composite (original)')
    ax.plot(x_foil[mask], du['cp_composite_blended'][mask], 'k--', lw=1.5, label='cp_final (blended composite)')
    mask_af = xx_af <= 0.25
    ax.plot(xx_af[mask_af], cpu_corr[mask_af], 'k^', ms=5, markevery=1,
            label='cp_final (pytsfoil output)')
    ax.axvspan(du['x_sb0'], min(du['x_sb1'], x_foil[-1]), alpha=0.10, color='green',
               label=f'blend region [{x_b0:.3f}, {du['x_sb1']:.3f}]')
    ax.axvline(x_smin, color='purple', ls=':', lw=1, label=f"x_smin={x_smin:.4f}")
    ax.axhline(0, color='gray', lw=0.5)
    ax.invert_yaxis()
    ax.set(title='cp_composite decomposition — upper surface  (x ≤ 25%c)',
           xlabel='x/c', ylabel='Cp')
    ax.set_xlim(-0.005, 0.25)
    ax.legend(fontsize=7); ax.grid()

    # ── [0,1] Composition of cp_composite on lower surface ───────────────────
    ax = axes[0, 1]
    mask2 = x_foil <= 0.25
    ax.plot(x_foil[mask2], dl['cp_inner'][mask2],             'y--', lw=2, label='cp_inner')
    ax.plot(x_foil[mask2], dl['cp_tsd'][mask2],               'r-', lw=2, label='cp_tsd')
    ax.plot(x_foil[mask2], dl['cp_common'][mask2],           'g--', lw=2, label='cp_common')
    ax.plot(x_foil[mask2], dl['rho_phi_bracket'][mask2],      'g-', lw=1, label='ρ·φ_ox·bracket  (outer match)')
    ax.plot(x_foil[mask2], dl['cp_composite_original'][mask2],'b-', lw=2,   label='cp_composite (original)')
    ax.plot(x_foil[mask2], dl['cp_composite_blended'][mask2], 'k--', lw=1.5, label='cp_final (blended composite)')
    
    # pytsfoil output as cross-check
    mask_af2 = xx_af <= 0.25
    ax.plot(xx_af[mask_af2], cpl_corr[mask_af2], 'k^', ms=5, markevery=1,
            label='cp_final (pytsfoil output)')
    ax.axvspan(dl['x_sb0'], min(dl['x_sb1'], x_foil[-1]), alpha=0.10, color='green',
               label=f'blend region [{dl["x_sb0"]:.3f}, {dl["x_sb1"]:.3f}]')
    ax.axvline(x_smin, color='purple', ls=':', lw=1, label=f"x_smin={x_smin:.4f}")
    ax.axhline(0, color='gray', lw=0.5)
    ax.invert_yaxis()
    ax.set(title='cp_composite decomposition — lower surface  (x ≤ 25%c)',
           xlabel='x/c', ylabel='Cp')
    ax.set_xlim(-0.005, 0.25)
    ax.legend(fontsize=7); ax.grid()

    # ── [1,0] Transition functions ────────────────────────────────────────────
    ax = axes[1, 0]
    ax.plot(x_foil, dl['rho_ratio']*dl['phi_ox'], 'b-',  lw=1.5, label='ρ*/ρ_inf*φ_ox')
    ax.plot(x_foil, dl['blend_weight'], 'g-', lw=1.5, label='blend weight w(x)  (smoothstep)')
    ax.axvspan(dl['x_sb0'], min(dl['x_sb1'], x_foil[-1]), alpha=0.10, color='green',
               label=f'blend region [{dl["x_sb0"]:.3f}, {dl["x_sb1"]:.3f}]')
    ax.axvline(x_smin, color='purple', ls=':', lw=1, label=f"x_smin={x_smin:.4f}")
    ax.axhline(0, color='gray', lw=0.5)
    ax.axhline(1, color='gray', lw=0.5, ls='--')
    ax.set(title='Transition functions (same for upper & lower)',
           xlabel='x/c', ylabel='value')
    ax.set_xlim(-0.005, 0.25)
    ax.legend(fontsize=7); ax.grid()

    # ── [1,1] Net correction ΔCp ─────────────────────────────────────────────
    ax = axes[1, 1]
    ax.plot(xx, r_b['cpu'], 'b--', label='TSD upper')
    ax.plot(xx, r_b['cpl'], 'b--', label='TSD lower')
    ax.plot(xx, r_c['cpu'], 'r-',  label='Corrected upper')
    ax.plot(xx, r_c['cpl'], 'r-',  label='Corrected lower')
    ax.plot(airfoil['xu'], airfoil['cpu'], 'g^', markevery=5, label='RANS upper', ms=4)
    ax.plot(airfoil['xl'], airfoil['cpl'], 'gv', markevery=5, label='RANS lower', ms=4)
    ax.axvspan(dl['x_sb0'], min(dl['x_sb1'], x_foil[-1]), alpha=0.10, color='green',
               label=f'blend region (lower)')
    ax.axvspan(du['x_sb0'], min(du['x_sb1'], x_foil[-1]), alpha=0.10, color='red',
               label=f'blend region (lower)')
    ax.invert_yaxis()
    ax.set(title='Cp Distribution', xlabel='x/c', ylabel='Cp')
    ax.set_xlim(-0.005, 0.25)
    ax.legend(fontsize=7); ax.grid()

    fig.tight_layout()
    fig.savefig(os.path.join(path_figs, f'case_{idx:04d}_corrections.png'),
                dpi=100, bbox_inches='tight')
    plt.close(fig)


def print_summary(all_results):
    print()
    print('=' * 80)
    print(f'{"idx":>4}  {"Ma":>5}  {"AoA":>5}  '
          f'{"Ma0_base":>8}  {"Ma0_corr":>8}  '
          f'{"RmCp_base":>9}  {"RmCp_corr":>9}  '
          f'{"dCL%":>7}  {"dCM%":>7}  {"Self_corr":>9}')
    print('-' * 80)
    for r in all_results:
        if not r['success']:
            print(f"  {r['entry_index']:>3}  FAILED: {r['error'][:60]}")
            continue
        b = r['results']['baseline']
        c = r['results']['corrected']
        dcl = (c['cl'] - b['cl']) / (abs(b['cl']) + 1e-9) * 100.0
        dcm = (c['cm'] - b['cm']) / (abs(b['cm']) + 1e-9) * 100.0
        print(f"  {r['entry_index']:>3}  {r['Ma']:>5.2f}  {r['AoA']:>5.2f}  "
              f"{b['n_ma0']:>8d}  {c['n_ma0']:>8d}  "
              f"{b['rmse_cp']:>9.5f}  {c['rmse_cp']:>9.5f}  "
              f"{dcl:>7.2f}  {dcm:>7.2f}  {c['rmse_self']:>9.5f}")
    print('=' * 80)


if __name__ == '__main__':
    db     = load_airfoil_database_from_json(fname_db)
    cases  = list(db.values())[:N_CASES]
    for a in cases:
        a['plot_cp'] = True

    print(f"Running {N_CASES} cases with N_PROCESS={N_PROCESS} ...")
    with mp.Pool(N_PROCESS) as pool:
        all_results = pool.map(run_single, cases)
        
    # all_results = []
    # for airfoil in cases:
    #     result = run_single(airfoil)
    #     all_results.append(result)
    
    print_summary(all_results)
    print(f"Figures saved to {path_figs}/")
