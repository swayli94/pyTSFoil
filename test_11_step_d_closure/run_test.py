"""
Test 11 (revised): Step D's effect on M=0 nodes

Compares three modes:

    baseline  — no corrections
    sing_D    — D (Step D only, no composite/MAE)
    sing_D_MAE — D + MAE (Step D + leading edge correction)

Focus: does Step D reduce the number of M=0 nodes?

BC plot also shows the AoA contribution (-α/δ) to FXU/FXL.
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
from pytsfoil.leading_edge import compute_surface_corrections, calculate_phi_ry

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

MODES = {
    'baseline': {
        'apply_singularity_subtraction': False,
        'apply_step_d': False,
        'apply_le_correction': False
    },
    'sing_D': {
        'apply_singularity_subtraction': True,
        'apply_step_d': True,
        'apply_le_correction': False
    },
    # 'sing_D_MAE': {
    #     'apply_singularity_subtraction': True,
    #     'apply_step_d': True,
    #     'apply_le_correction': True
    # },
}

COLOURS = {'baseline': 'steelblue', 'sing_D': 'gray',   'sing_D_MAE': 'green'}
STYLES  = {'baseline': '-',         'sing_D': '-.',     'sing_D_MAE': '--'}


def _n_ma0(ts):
    ile = ts.mesh['ile']; ite = ts.mesh['ite']
    return int(np.sum(ts.data_summary['mal'][ile:ite + 1] == 0.0))


def _build_coords(airfoil):
    from cst_modeling.section import cst_foil
    t = airfoil['tmax']
    x, yu, yl, _, _ = cst_foil(201, airfoil['cst_u'], airfoil['cst_l'],
                                 x=None, t=t, tail=0.0)
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


def run_case(airfoil):
    idx = airfoil['entry_index']
    Ma  = airfoil['Ma']
    AoA = airfoil['AoA']
    print(f"  Case {idx}  Ma={Ma:.3f}  AoA={AoA:.2f}°", flush=True)

    coords      = _build_coords(airfoil)
    results     = {}
    ts_baseline = None

    for mode, extra in MODES.items():
        ts = _run_mode(airfoil, coords, extra)
        if mode == 'baseline':
            ts_baseline = ts
        results[mode] = {
            'xx':      ts.mesh['xx'],
            'cpu':     ts.data_summary['cpu'],
            'cpl':     ts.data_summary['cpl'],
            'n_ma0':   _n_ma0(ts),
        }

    # ── BC diagnostics from saved baseline run ─────────────────────────────────
    ts_b      = ts_baseline
    nfoil     = ts_b.mesh['nfoil']
    x_foil    = ts_b.mesh['xx_airfoil'][:nfoil]
    R_c       = ts_b.airfoil.get('R_c')
    h         = ts_b.airfoil.get('h_nose')
    delta     = float(tsf.common_data.delta) if tsf else airfoil['tmax']
    cpfact    = ts_b._cpfact
    gamma     = ts_b._gamma
    alpha_rad = np.radians(AoA)

    diag = {'x_foil': x_foil, 'R_c': R_c, 'alpha_rad': alpha_rad, 'delta': delta}
    if tsf is not None and R_c is not None and h is not None and R_c > 0:
        fxu = ts_b.mesh['fxu']
        fxl = ts_b.mesh['fxl']
        fx_camber = ts_b.mesh['fx_camber']
        phi_sy, _ = compute_surface_corrections(x_foil, h, delta, R_c, cpfact, gamma)
        phi_sy    = phi_sy.astype(float)
        phi_ry_upper, phi_ry_lower = calculate_phi_ry(
            fxu, fxl, fx_camber, phi_sy, x_foil)

        diag.update({
            'fxu':        fxu,
            'fxl':        fxl,
            'fx_camber':  fx_camber,
            'phi_sy':     phi_sy,
            'phi_ry_raw_u': fxu - phi_sy,
            'phi_ry_raw_l': fxl + phi_sy,
            'phi_ry_upper': phi_ry_upper.astype(float),
            'phi_ry_lower': phi_ry_lower.astype(float),
        })

    _plot_bc(idx, Ma, AoA, diag)
    _plot_cp(idx, Ma, AoA, airfoil, results)

    return idx, results


def _plot_bc(idx, Ma, AoA, diag):
    """
    FXU/FXL, phi_sy, phi_ry before/after Step D.
    Horizontal lines mark the AoA contribution (-α/δ) so its magnitude
    relative to the LE thickness spike is visible.
    """
    xf       = diag.get('x_foil')
    R_c      = diag.get('R_c') or 1e-3
    delta    = diag.get('delta', 1.0)
    alpha_rad = diag.get('alpha_rad', 0.0)
    if xf is None:
        return
    mask = xf <= 0.1

    # AoA shifts both upper and lower FX by the same constant: -α/δ
    aoa_level = -alpha_rad / delta   # contribution to FXU (and FXL) from AoA

    fig, axes = plt.subplots(1, 2, figsize=(14, 5))
    fig.suptitle(
        f"Case {idx} | Ma={Ma:.3f}, AoA={AoA:.2f}°  "
        f"(R_c={R_c:.4f}, δ={delta:.4f},  −α/δ={aoa_level:.3f})",
        fontsize=11)

    # Left: raw body slopes vs singular part
    ax = axes[0]
    if 'fxu' in diag:
        ax.plot(xf[mask], diag['fxu'][mask], 'b-',  lw=2,   label='FXU (upper)')
        ax.plot(xf[mask], diag['fxl'][mask], 'r-',  lw=2,   label='FXL (lower)')
        ax.plot(xf[mask], diag['fx_camber'][mask], 'k-',  lw=1, label='FX_camber')
        ax.plot(xf[mask],  diag['phi_sy'][mask], 'g--', lw=1.5, label='φ_s,y upper')
        ax.plot(xf[mask], -diag['phi_sy'][mask], 'g--', lw=1.5, label='φ_s,y lower')
        # AoA contribution level — where FX converges far from LE
        ax.axhline(aoa_level, color='purple', ls=':', lw=1.8,
                   label=f'AoA level −α/δ = {aoa_level:.3f}')
    ax.axhline(0, color='gray', lw=0.5)
    ax.set(title='FXU/FXL vs φ_s,y  (AoA level marked)',
           xlabel='x/c', ylabel='slope/δ')
    ax.set_xlim(-0.001, xf[mask].max())
    ax.legend(fontsize=8); ax.grid()

    # Right: residual phi_ry before/after Step D, AoA level for reference
    ax = axes[1]
    if 'phi_ry_raw_u' in diag:
        ax.plot(xf[mask], diag['phi_ry_raw_u'][mask], 'k--', lw=3.0, alpha=0.2,
                label='FXU - φ_s,y')
        ax.plot(xf[mask], diag['phi_ry_raw_l'][mask], 'k--', lw=3.0, alpha=0.2,
                label='FXL + φ_s,y')
        ax.plot(xf[mask], diag['phi_ry_upper'][mask], 'r.-', lw=1.5,
                label=f'φ_r,y (upper, Step D)')
        ax.plot(xf[mask], diag['phi_ry_lower'][mask], 'g--', lw=1.5,
                label=f'φ_r,y (lower, Step D)')
        ax.plot(xf[mask], diag['fx_camber'][mask], 'k-',  lw=0.8, label='FX_camber')
        ax.axhline(aoa_level, color='purple', ls=':', lw=1.8,
                   label=f'AoA level -α/δ = {aoa_level:.3f}')
    ax.axhline(0, color='gray', lw=0.5)
    ax.set(title=f'Residual BC φ_r,y — before/after Step D',
           xlabel='x/c', ylabel='residual slope/δ')
    ax.set_xlim(-0.001, xf[mask].max())
    ax.set_ylim(diag['phi_ry_lower'][mask].min() - 2, 
                diag['phi_ry_upper'][mask].max() + 2)
    ax.legend(fontsize=8); ax.grid()

    fig.tight_layout()
    fig.savefig(os.path.join(path_figs, f'case_{idx:04d}_bc.png'), dpi=100)
    plt.close(fig)


def _plot_cp(idx, Ma, AoA, airfoil, results):
    
    fig, ax = plt.subplots(figsize=(9, 5))
    fig.suptitle(f"Case {idx} | Ma={Ma:.3f}, AoA={AoA:.2f}° — Cp comparison",
                 fontsize=11)

    for mode in MODES:
        r  = results[mode]
        n0 = r['n_ma0']
        ax.plot(r['xx'], r['cpu'], color=COLOURS[mode], ls=STYLES[mode], lw=2.0,
                label=f"{mode}  nMa0={n0}")
        ax.plot(r['xx'], r['cpl'], color=COLOURS[mode], ls=STYLES[mode], lw=1.0)

    ax.plot(airfoil['xu'], airfoil['cpu'], 'k^', ms=4, markevery=5, label='RANS')
    ax.plot(airfoil['xl'], airfoil['cpl'], 'kv', ms=4, markevery=5)
    ax.invert_yaxis()
    ax.set(xlabel='x/c', ylabel='Cp', xlim=(-.05, 1.05))
    ax.legend(fontsize=9); ax.grid()

    fig.tight_layout()
    fig.savefig(os.path.join(path_figs, f'case_{idx:04d}_cp.png'), dpi=100)
    plt.close(fig)


if __name__ == '__main__':
    
    print("Loading airfoil database...")
    db      = load_airfoil_database_from_json(fname_db)
    entries = list(db.values())

    CASE_INDICES = [0, 1, 3]
    cases = sorted([e for e in entries if e['entry_index'] in CASE_INDICES],
                   key=lambda a: a['entry_index'])

    all_results = {}
    for airfoil in cases:
        idx, res = run_case(airfoil)
        all_results[idx] = res