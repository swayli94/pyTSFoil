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
N_PROCESS  = 4   # parallel workers
GAMMA      = 1.4


def _cp_isentropic(ma, minf, gamma=GAMMA):
    denom = 2.0 + (gamma - 1.0) * ma ** 2
    numer = 2.0 + (gamma - 1.0) * minf ** 2
    return (2.0 / (gamma * minf ** 2)) * ((numer / denom) ** (gamma / (gamma - 1.0)) - 1.0)


def run_single(airfoil: dict) -> dict:
    """Worker: run baseline and corrected cases for one airfoil."""
    try:
        t0 = time.time()

        cst_u  = airfoil['cst_u']
        cst_l  = airfoil['cst_l']
        tmax   = airfoil['tmax']
        x, yu, yl, _, _ = cst_foil(201, cst_u, cst_l, x=None, t=tmax, tail=0.0)
        coords = np.column_stack((np.concatenate([x[::-1], x[1:]]),
                                   np.concatenate([yu[::-1], yl[1:]])))

        results = {}
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

        _plot(airfoil, results, xx)

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
        import traceback
        return {
            'success':     False,
            'entry_index': airfoil['entry_index'],
            'error':       traceback.format_exc(),
        }


def _plot(airfoil, results, xx):
    idx = airfoil['entry_index']
    Ma  = airfoil['Ma']
    AoA = airfoil['AoA']

    fig, axes = plt.subplots(1, 3, figsize=(18, 5))
    fig.suptitle(f"Case {idx} | {airfoil['airfoil_id']} | Ma={Ma:.2f}, AoA={AoA:.2f}°",
                 fontsize=11)

    r_b = results['baseline']
    r_c = results['corrected']

    ax = axes[0]
    ax.plot(airfoil['xu'], airfoil['yu'], 'g-')
    ax.plot(airfoil['xl'], airfoil['yl'], 'g-')
    ax.set(title='Geometry', xlabel='x/c', ylabel='y/c')
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

    print_summary(all_results)
    print(f"Figures saved to {path_figs}/")
