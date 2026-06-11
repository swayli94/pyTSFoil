"""
Test 12: Leading-edge correction ablation study.

Two-panel Cp figure per case.

Sub-plot 1 (no MAE — TSD correction only):
  baseline  — no correction
  only_D    — step D only:     regularise FXU/FXL, iterate phi
  only_DE   — steps D+E:       regularise FXU/FXL, iterate phi, restore phi_s,x
  all_AE    — steps A-E:       full singularity subtraction, iterate phi_r

Sub-plot 2 (with MAE composite correction):
  baseline  — no correction  (same as above, for reference)
  MAE_only  — MAE correction only, no singularity subtraction
  DE_MAE    — steps D+E + MAE
  all_AE_MAE — steps A-E + MAE

Test folder: test_12_correction_components/
Figures: test_12_correction_components/figures/
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

try:
    from cst_modeling.section import cst_foil
    _HAS_CST = True
except ImportError:
    _HAS_CST = False

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

# ── mode definitions ──────────────────────────────────────────────────────────
# Sub-plot 1: no MAE, varying TSD correction level
# Sub-plot 2: with MAE, varying TSD correction level
MODES = {
    # --- sub-plot 1 (no MAE) ---
    'baseline': {
        'apply_singularity_subtraction': False,
        'apply_step_d':  False,
        'apply_step_e':  False,
        'apply_le_correction': False,
    },
    'only_D': {
        'apply_singularity_subtraction': False,
        'apply_step_d':  True,
        'apply_step_e':  False,
        'apply_le_correction': False,
    },
    'only_DE': {
        'apply_singularity_subtraction': False,
        'apply_step_d':  True,
        'apply_step_e':  True,
        'apply_le_correction': False,
    },
    'all_AE': {
        'apply_singularity_subtraction': True,
        'apply_step_d':  True,
        'apply_step_e':  False,   # E is implicit when apply_singularity_subtraction=True
        'apply_le_correction': False,
    },
    # --- sub-plot 2 (with MAE) ---
    'MAE_only': {
        'apply_singularity_subtraction': False,
        'apply_step_d':  False,
        'apply_step_e':  False,
        'apply_le_correction': True,
    },
    'DE_MAE': {
        'apply_singularity_subtraction': False,
        'apply_step_d':  True,
        'apply_step_e':  True,
        'apply_le_correction': True,
    },
    'all_AE_MAE': {
        'apply_singularity_subtraction': True,
        'apply_step_d':  True,
        'apply_step_e':  False,
        'apply_le_correction': True,
    },
}

# Sub-plot groupings
PANEL1_MODES = ('baseline', 'only_D', 'only_DE', 'all_AE')
PANEL2_MODES = ('baseline', 'MAE_only', 'DE_MAE', 'all_AE_MAE')

COLOURS = {
    'baseline':   'steelblue',
    'only_D':     'darkorange',
    'only_DE':    'crimson',
    'all_AE':     'dimgray',
    'MAE_only':   'mediumseagreen',
    'DE_MAE':     'darkviolet',
    'all_AE_MAE': 'black',
}

STYLES = {
    'baseline':   '--',
    'only_D':     '-.',
    'only_DE':    ':',
    'all_AE':     '-',
    'MAE_only':   '-.',
    'DE_MAE':     ':',
    'all_AE_MAE': '-',
}

LABELS = {
    'baseline':   'baseline',
    'only_D':     'D only (phi iter)',
    'only_DE':    'D+E (phi iter)',
    'all_AE':     'A-E (phi_r iter)',
    'MAE_only':   'MAE only',
    'DE_MAE':     'D+E+MAE',
    'all_AE_MAE': 'A-E+MAE',
}

CASE_INDICES = [0, 1, 3]


def _build_coords(airfoil):
    if not _HAS_CST:
        raise ImportError("cst_modeling is required to build airfoil coordinates")
    x, yu, yl, _, _ = cst_foil(201, airfoil['cst_u'], airfoil['cst_l'],
                                x=None, t=airfoil['tmax'], tail=0.0)
    return np.column_stack((np.concatenate([x[::-1], x[1:]]),
                             np.concatenate([yu[::-1], yl[1:]])))


def _run_mode(airfoil, coords, extra_cfg):
    cfg = copy.deepcopy(BASE_CFG)
    cfg.update({'ALPHA': airfoil['AoA'], 'EMACH': airfoil['Ma'],
                'REYNLD': airfoil['Re']})
    cfg.update(extra_cfg)
    ts = PyTSFoil(airfoil_coordinates=coords, work_dir=path)
    ts.set_config(**cfg)
    ts.run()
    return ts


def _n_ma0(ts):
    ile = ts.mesh['ile']
    ite = ts.mesh['ite']
    return int(np.sum(ts.data_summary['mal'][ile:ite + 1] == 0.0))


def run_case(airfoil):
    idx = airfoil['entry_index']
    Ma  = airfoil['Ma']
    AoA = airfoil['AoA']
    print(f"  Case {idx}  Ma={Ma:.3f}  AoA={AoA:.2f}°", flush=True)

    coords  = _build_coords(airfoil)
    results = {}

    for mode, extra in MODES.items():
        ts = _run_mode(airfoil, coords, extra)
        results[mode] = {
            'xx':      ts.mesh['xx'],
            'cpu':     ts.data_summary['cpu'],
            'cpl':     ts.data_summary['cpl'],
            'n_ma0':   _n_ma0(ts),
        }
        print(f"    {mode:12s}  nMa0={results[mode]['n_ma0']:3d}")

    _plot_cp(idx, Ma, AoA, airfoil, results)
    return idx, results


def _add_panel(ax, modes, results, airfoil, title):
    """Draw one Cp panel for the given list of mode keys."""
    for mode in modes:
        r   = results[mode]
        n0  = r['n_ma0']
        lbl = f"{LABELS[mode]}  nMa0={n0}"
        ax.plot(r['xx'], r['cpu'], color=COLOURS[mode], ls=STYLES[mode],
                lw=1.5, label=lbl)
        ax.plot(r['xx'], r['cpl'], color=COLOURS[mode], ls=STYLES[mode], lw=1.5)
    ax.plot(airfoil['xu'], airfoil['cpu'], 'k^', ms=3, markevery=5, label='RANS')
    ax.plot(airfoil['xl'], airfoil['cpl'], 'kv', ms=3, markevery=5)
    ax.invert_yaxis()
    ax.set(title=title, xlabel='x/c', ylabel='Cp', xlim=(-.05, 1.05))
    ax.legend(fontsize=7); ax.grid()


def _plot_cp(idx, Ma, AoA, airfoil, results):
    fig, axes = plt.subplots(1, 2, figsize=(16, 5))
    fig.suptitle(
        f"Case {idx} | Ma={Ma:.3f}, AoA={AoA:.2f}°"
        " — Task 12 correction components", fontsize=11)

    _add_panel(axes[0], PANEL1_MODES, results, airfoil,
               'No MAE — TSD correction level (baseline / D / D+E / A-E)')
    _add_panel(axes[1], PANEL2_MODES, results, airfoil,
               'With MAE — MAE combined with TSD corrections')

    fig.tight_layout()
    fig.savefig(os.path.join(path_figs, f'case_{idx:04d}_cp.png'), dpi=100)
    plt.close(fig)


if __name__ == '__main__':
    
    print("Loading airfoil database...")
    db      = load_airfoil_database_from_json(fname_db)
    entries = list(db.values())

    cases = sorted([e for e in entries if e['entry_index'] in CASE_INDICES],
                   key=lambda a: a['entry_index'])

    for airfoil in cases:
        run_case(airfoil)
