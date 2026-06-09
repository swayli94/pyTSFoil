"""
Task 4(3) test: FXL smooth-blend correction (Method 1 of fix_le_mach).

For each of the first N_MAX_RUN database cases, run PyTSFoil under seven
conditions using AoA-aware effective slope threshold |FX - ALPHA_sim|*delta:
  0. baseline   — fix_le_mach=False (reference)
  1. both-1.5   — both surfaces, threshold=1.5
  2. both-0.5   — both surfaces, threshold=0.5
  3. lower-1.5  — lower surface only, threshold=1.5
  4. lower-1.0  — lower surface only, threshold=1.0
  5. lower-0.5  — lower surface only, threshold=0.5
  6. lower-0.3  — lower surface only, threshold=0.3

Metrics collected per case:
  - Ma=0 count on lower surface (LE region)
  - Cp RMSE vs RANS
  - Ma RMSE vs RANS
  - CL, CD, CM (absolute values)
  - delta_CL / delta_CD / delta_CM vs baseline

Results are written to results_fix3.json and comparison figures to figures/.
"""
import os
import sys
path = os.path.abspath(os.path.dirname(__file__))
path_root = os.path.abspath(os.path.join(os.path.dirname(__file__), '..'))
sys.path.append(path)
sys.path.append(path_root)

import json
import copy
import numpy as np
import matplotlib.pyplot as plt
import multiprocessing as mp
from typing import Dict, Any, List
from scipy.interpolate import interp1d
from cst_modeling.foil import cst_foil
from pytsfoil import PyTSFoil
from airfoil_database.utils import (load_airfoil_database_from_json,
                save_results_to_json)

EPS = 0.5
N_AIRFOIL_POINTS = 1001
N_PROCESS = 8
N_MAX_RUN = 10
DPI = 100

VARIANTS = [
    {'label': 'baseline',  'fix_le_mach': False, 'fix_le_mach_threshold': 1.5, 'fix_le_mach_surface': 'both'},
    {'label': 'both-1.5',  'fix_le_mach': True,  'fix_le_mach_threshold': 1.5, 'fix_le_mach_surface': 'both'},
    {'label': 'both-0.3',  'fix_le_mach': True,  'fix_le_mach_threshold': 0.3, 'fix_le_mach_surface': 'both'},
    {'label': 'lower-1.5', 'fix_le_mach': True,  'fix_le_mach_threshold': 1.5, 'fix_le_mach_surface': 'lower'},
    {'label': 'lower-0.3', 'fix_le_mach': True,  'fix_le_mach_threshold': 0.3, 'fix_le_mach_surface': 'lower'},
    {'label': 'upper-1.5', 'fix_le_mach': True,  'fix_le_mach_threshold': 1.5, 'fix_le_mach_surface': 'upper'},
    {'label': 'upper-0.3', 'fix_le_mach': True,  'fix_le_mach_threshold': 0.3, 'fix_le_mach_surface': 'upper'},
]

baseline_config = {
    'EMACH': 0.75,
    'ALPHA': 1.0,
    'REYNLD': 6.5E6,
    'CVERGE': 1e-5,
    'DVERGE': 10.0,
    'EPS': EPS,
    'IPRTER': 100,
    'MAXIT': 9999,
    'RIGF': 0.0,
    'SIMDEF': 3,
    'WCIRC': 1.0,
    'WE': [1.8, 1.9, 1.95],
    'NWDGE': 0,
    'WCONST': 4.0,
    'IFLAP': 0,
    'DELFLP': 0.0,
    'FLPLOC': 0.77,
    'n_point_x': 200,
    'n_point_y': 80,
    'n_point_airfoil': 100,
    'flag_output': False,
    'flag_output_summary': False,
    'flag_output_shock': False,
    'flag_output_field': False,
    'flag_print_info': False,
}

fname_database = os.path.join(path_root, "airfoil_database", "airfoil_database.json")
fname_results  = os.path.join(path, "results_fix3.json")
path_figures   = os.path.join(path, "figures")
os.makedirs(path_figures, exist_ok=True)


def count_le_zeros(mal: np.ndarray, xx: np.ndarray, ile_x: float = 0.0,
                   ite_x: float = 1.0) -> tuple:
    """Count Ma=0 points on lower surface within airfoil x-range."""
    mask = (xx >= ile_x) & (xx <= ite_x)
    mal_af = mal[mask]
    xx_af  = xx[mask]
    n_zero = int(np.sum(mal_af == 0.0))
    if n_zero > 0:
        last_zero_idx = np.where(mal_af == 0.0)[0][-1]
        x_exit = float(xx_af[last_zero_idx])
    else:
        x_exit = 0.0
    return n_zero, x_exit


def run_one_variant(task: Dict[str, Any]) -> Dict[str, Any]:
    """Run a single (airfoil, variant) combination in a worker process."""
    airfoil = task['airfoil']
    variant = task['variant']
    try:
        cst_u = airfoil['cst_u']
        cst_l = airfoil['cst_l']
        tmax  = airfoil['tmax']
        x, yu, yl, _, _ = cst_foil(N_AIRFOIL_POINTS, cst_u, cst_l, x=None, t=tmax, tail=0.0)
        xx_in = np.concatenate((x[::-1], x[1:]))
        yy_in = np.concatenate((yu[::-1], yl[1:]))
        airfoil_coords = np.column_stack((xx_in, yy_in))

        pytsfoil = PyTSFoil(airfoil_coordinates=airfoil_coords, work_dir=path)
        cfg = copy.deepcopy(baseline_config)
        cfg.update({
            'ALPHA':  airfoil['AoA'],
            'EMACH':  airfoil['Ma'],
            'REYNLD': airfoil['Re'],
            'fix_le_mach':           variant['fix_le_mach'],
            'fix_le_mach_threshold': variant['fix_le_mach_threshold'],
            'fix_le_mach_surface':   variant.get('fix_le_mach_surface', 'both'),
        })
        pytsfoil.set_config(**cfg)
        pytsfoil.run()

        xx   = pytsfoil.mesh['xx']
        cpu  = pytsfoil.data_summary['cpu']
        cpl  = pytsfoil.data_summary['cpl']
        mau  = pytsfoil.data_summary['mau']
        mal  = pytsfoil.data_summary['mal']
        uu   = pytsfoil.data_summary['uu']
        ul   = pytsfoil.data_summary['ul']
        vu   = pytsfoil.data_summary['vu']
        vl   = pytsfoil.data_summary['vl']
        cl   = pytsfoil.data_summary['cl']
        cd   = pytsfoil.data_summary['cd']
        cm   = pytsfoil.data_summary['cm']

        fcpu = interp1d(xx, cpu, kind='linear', fill_value='extrapolate')
        fcpl = interp1d(xx, cpl, kind='linear', fill_value='extrapolate')
        fmau = interp1d(xx, mau, kind='linear', fill_value='extrapolate')
        fmal = interp1d(xx, mal, kind='linear', fill_value='extrapolate')

        _cpu = fcpu(airfoil['xu']); _err_u = np.mean((_cpu - airfoil['cpu'])**2)
        _cpl = fcpl(airfoil['xl']); _err_l = np.mean((_cpl - airfoil['cpl'])**2)
        rmse_cp = float(np.sqrt(0.5 * (_err_u + _err_l)))

        _mau = fmau(airfoil['xu']); _err_u = np.mean((_mau - airfoil['mwu'])**2)
        _mal = fmal(airfoil['xl']); _err_l = np.mean((_mal - airfoil['mwl'])**2)
        rmse_mw = float(np.sqrt(0.5 * (_err_u + _err_l)))

        n_zero, x_exit = count_le_zeros(mal, xx)

        return {
            'success': True,
            'entry': airfoil['entry_index'],
            'variant': variant['label'],
            'Ma': airfoil['Ma'],
            'AoA': airfoil['AoA'],
            'cl': cl, 'cd': cd, 'cm': cm,
            'rmse_cp': rmse_cp,
            'rmse_mw': rmse_mw,
            'n_zero_le': n_zero,
            'x_exit': x_exit,
            'xx': xx.tolist(),
            'cpu': cpu.tolist(), 'cpl': cpl.tolist(),
            'mau': mau.tolist(), 'mal': mal.tolist(),
            'uu': uu.tolist(), 'ul': ul.tolist(),
            'vu': vu.tolist(), 'vl': vl.tolist(),
        }
    except Exception as e:
        return {
            'success': False,
            'entry': airfoil['entry_index'],
            'variant': variant['label'],
            'error': str(e),
        }


def build_tasks(airfoils):
    tasks = []
    for af in airfoils:
        for v in VARIANTS:
            tasks.append({'airfoil': af, 'variant': v})
    return tasks


def plot_mach_comparison(entry_idx: int, airfoil: Dict[str, Any],
                         results_by_variant: Dict[str, Dict]) -> None:
    """3×2 figure: Ma / U / V for upper and lower surfaces vs all variants."""
    Ma    = airfoil['Ma']
    AoA   = airfoil['AoA']
    delta = airfoil['tmax']
    # U_critical: ARG=0 in EMACH1 (Krupp) → U_crit = -Ma / (GAM1 * delta^(2/3))
    u_crit = -Ma / (2.4 * delta ** (2.0 / 3.0))
    LE_XLIM = 0.15

    fig, axes = plt.subplots(3, 2, figsize=(14, 15))
    colors = ['#1f77b4', '#ff7f0e', '#2ca02c', '#d62728', '#9467bd', '#8c564b', '#e377c2']

    surfaces = [
        ('upper', 'xu', 'mwu', 'mau', 'uu', 'vu'),
        ('lower', 'xl', 'mwl', 'mal', 'ul', 'vl'),
    ]

    # ── Row 0: Ma distributions ───────────────────────────────────────────────
    for col, (surf, key_x, key_mw, key_m, _, _) in enumerate(surfaces):
        ax = axes[0, col]
        ax.plot(airfoil[key_x], airfoil[key_mw], 'k-', lw=2, label='RANS', zorder=10)
        for i, v in enumerate(VARIANTS):
            r = results_by_variant.get(v['label'])
            if r and r.get('success'):
                xx = np.array(r['xx'])
                m  = np.array(r[key_m])
                mask = (xx >= -0.05) & (xx <= 1.05)
                n0  = r['n_zero_le'] if surf == 'lower' else '—'
                ls  = '-' if v['label'] == 'baseline' else '--'
                ax.plot(xx[mask], m[mask], color=colors[i], ls=ls, lw=1.4,
                        label=f"{v['label']}  (n0={n0})")
        ax.axhline(y=Ma, color='gray', ls=':', lw=1, alpha=0.5, label='M∞')
        ax.set_title(f'{surf.capitalize()} surface — Ma')
        ax.set_xlabel('x/c')
        ax.set_ylabel('Ma')
        ax.set_xlim(-0.05, 1.05)
        ax.legend(fontsize=7)
        ax.grid(alpha=0.4)

    # ── Row 1: U distributions with U_critical ────────────────────────────────
    for col, (surf, _, _, _, key_u, _) in enumerate(surfaces):
        ax = axes[1, col]
        for i, v in enumerate(VARIANTS):
            r = results_by_variant.get(v['label'])
            if r and r.get('success'):
                xx = np.array(r['xx'])
                u  = np.array(r[key_u])
                mask = (xx >= -0.05) & (xx <= 1.05)
                ls  = '-' if v['label'] == 'baseline' else '--'
                ax.plot(xx[mask], u[mask], color=colors[i], ls=ls, lw=1.4,
                        label=v['label'])
        ax.axhline(y=u_crit, color='red', ls='--', lw=2.0,
                   label=f'U_crit = {u_crit:.3f}')
        ax.axhline(y=0.0, color='gray', ls='-', lw=0.7, alpha=0.4)
        ax.set_title(f'{surf.capitalize()} surface — U  (Ma=0 when U < U_crit)')
        ax.set_xlabel('x/c')
        ax.set_ylabel('U = ∂P/∂x')
        ax.set_xlim(-0.05, 1.05)
        ax.legend(fontsize=7)
        ax.grid(alpha=0.4)

    # ── Row 2: V distributions near LE ────────────────────────────────────────
    # V at the surface ≈ CYY × (FX − α_sim); threshold |FX − α_sim|·δ ≤ thr
    # CYY is mesh-dependent, so exact threshold lines in V units are unavailable.
    # x_exit of baseline marks the right boundary of the Ma=0 zone.
    base_r   = results_by_variant.get('baseline')
    x_exit   = base_r['x_exit'] if (base_r and base_r.get('success') and base_r['x_exit'] > 0) else None
    for col, (surf, _, _, _, _, key_v) in enumerate(surfaces):
        ax = axes[2, col]
        for i, v in enumerate(VARIANTS):
            r = results_by_variant.get(v['label'])
            if r and r.get('success'):
                xx = np.array(r['xx'])
                vv = np.array(r[key_v])
                mask = (xx >= -0.01) & (xx <= LE_XLIM)
                ls  = '-' if v['label'] == 'baseline' else '--'
                thr = v['fix_le_mach_threshold']
                sfx = f" thr={thr}" if v['fix_le_mach'] else ''
                ax.plot(xx[mask], vv[mask], color=colors[i], ls=ls, lw=1.4,
                        label=f"{v['label']}{sfx}")
        if x_exit is not None:
            ax.axvline(x=x_exit, color='red', ls=':', lw=1.5,
                       label=f'x_exit(base)={x_exit:.4f}')
        ax.axhline(y=0.0, color='gray', ls='-', lw=0.7, alpha=0.4)
        ax.set_title(f'{surf.capitalize()} surface — V  (x ≤ {LE_XLIM:.0%}, LE region)')
        ax.set_xlabel('x/c')
        ax.set_ylabel('V = ∂P/∂y')
        ax.set_xlim(-0.01, LE_XLIM)
        ax.legend(fontsize=7)
        ax.grid(alpha=0.4)

    fig.suptitle(
        f'Entry {entry_idx}: Ma={Ma:.2f}, AoA={AoA:.2f}°, δ={delta:.3f}, '
        f'U_crit={u_crit:.3f}  — FX smooth-blend variants',
        fontsize=11)
    fig.tight_layout()
    fig.savefig(os.path.join(path_figures, f'entry_{entry_idx}.png'),
                dpi=DPI, bbox_inches='tight')
    plt.close(fig)


if __name__ == '__main__':
    dict_airfoils = load_airfoil_database_from_json(fname_database)
    airfoils = list(dict_airfoils.values())[:N_MAX_RUN]
    print(f"Running {len(airfoils)} cases × {len(VARIANTS)} variants "
          f"= {len(airfoils)*len(VARIANTS)} total jobs")

    tasks = build_tasks(airfoils)
    with mp.Pool(processes=N_PROCESS) as pool:
        raw = pool.map(run_one_variant, tasks)

    # Organize results
    all_results: Dict[int, Dict[str, Dict]] = {}
    for r in raw:
        ei = r['entry']
        all_results.setdefault(ei, {})[r['variant']] = r

    # Save flat list
    with open(fname_results, 'w') as f:
        json.dump(raw, f, indent=2)
    print(f"Results written to {fname_results}")

    # Print comparison table
    header = f"{'Entry':>5} {'Ma':>5} {'AoA':>6} | "
    for v in VARIANTS:
        header += f"{v['label']:>10} "
    print('\nMa=0 count on lower surface:')
    print(header.replace('Ma=0 count','').rstrip())
    print('-' * len(header))
    for af in airfoils:
        ei = af['entry_index']
        row = f"{ei:>5} {af['Ma']:>5.2f} {af['AoA']:>6.2f}° | "
        for v in VARIANTS:
            r = all_results.get(ei, {}).get(v['label'], {})
            n0 = r.get('n_zero_le', 'ERR')
            row += f"{str(n0):>10} "
        print(row)

    print('\nCp RMSE vs RANS:')
    print(header)
    print('-' * len(header))
    for af in airfoils:
        ei = af['entry_index']
        row = f"{ei:>5} {af['Ma']:>5.2f} {af['AoA']:>6.2f}° | "
        for v in VARIANTS:
            r = all_results.get(ei, {}).get(v['label'], {})
            row += f"{r.get('rmse_cp', float('nan')):>10.4f} "
        print(row)

    print('\nCL (vs baseline delta):')
    for af in airfoils:
        ei = af['entry_index']
        base_cl = all_results.get(ei, {}).get('baseline', {}).get('cl', float('nan'))
        row = f"Entry {ei:>2} AoA={af['AoA']:>5.2f}°: baseline CL={base_cl:.4f} |"
        for v in VARIANTS[1:]:
            r = all_results.get(ei, {}).get(v['label'], {})
            delta = r.get('cl', float('nan')) - base_cl
            row += f"  {v['label']} ΔCL={delta:+.4f}"
        print(row)

    # Plots
    for af in airfoils:
        plot_mach_comparison(af['entry_index'], af, all_results[af['entry_index']])
    print(f"Figures saved to {path_figures}/")
