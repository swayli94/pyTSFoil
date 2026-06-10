"""
Test 10: Leading-edge correction smoothness check and improvement.

Analyses correction terms from Tasks 6/9 for smoothness near x≈0 and in
transition regions. Runs cases 0 (low AoA) and 3 (high AoA, stagnation zone)
in baseline and composite modes, then produces four diagnostic figures per case:

  Figure 1 (_cp.png):   Cp / Ma distribution comparing baseline vs corrected.
  Figure 2 (_terms.png):Composite intermediate terms (cp_star, cp_common,
                         cp_tsd, bracket before/after improvement) at the LE.
  Figure 3 (_bc.png):   Step-D check: phi_sy_upper vs FXU body slope.
  Figure 4 (_phisx.png):Step-E phi_sx_surface and its numerical derivative.

Key findings documented in doc/improve-progress-2.md §10.
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
from pytsfoil.leading_edge import solve_inner_problem, compute_surface_corrections

try:
    import tsfoil_fortran as tsf
except ImportError:
    tsf = None

GAMMA     = 1.4
_COMMON   = 0.635776   # same constant as composite.py

fname_db   = os.path.join(path_root, 'airfoil_database', 'airfoil_database.json')
path_figs  = os.path.join(path, 'figures')
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


# ── helpers ────────────────────────────────────────────────────────────────────

def _cp_isen(ma, minf, gamma=GAMMA):
    denom = 2.0 + (gamma - 1.0) * ma ** 2
    numer = 2.0 + (gamma - 1.0) * minf ** 2
    return (2.0 / (gamma * minf ** 2)) * ((numer / denom) ** (gamma / (gamma - 1.0)) - 1.0)


def _smooth_step(t):
    """C1 smoothstep 0→1 on t ∈ [0,1]."""
    t = np.clip(t, 0.0, 1.0)
    return t * t * (3.0 - 2.0 * t)


def _bracket_old(cp_tsd, cp_common, s_full, s_min=0.01):
    """Old formula: hard clamp at s_min (effectively only guards x=0 point)."""
    return np.where(s_full >= s_min, cp_tsd - cp_common, 0.0)


def _bracket_new(cp_tsd, cp_common, s_full, s_fade=2.0):
    """New formula: smooth fade-in over s ∈ [0, s_fade]."""
    w = _smooth_step(s_full / s_fade)
    return w * (cp_tsd - cp_common)


# ── run one airfoil/mode ────────────────────────────────────────────────────────

def _run(airfoil, extra_cfg):
    from cst_modeling.section import cst_foil
    x, yu, yl, _, _ = cst_foil(201, airfoil['cst_u'], airfoil['cst_l'],
                                 x=None, t=airfoil['tmax'], tail=0.0)
    coords = np.column_stack((np.concatenate([x[::-1], x[1:]]),
                               np.concatenate([yu[::-1], yl[1:]])))
    cfg = copy.deepcopy(BASE_CFG)
    cfg.update({'ALPHA': airfoil['AoA'], 'EMACH': airfoil['Ma'],
                'REYNLD': airfoil['Re']})
    cfg.update(extra_cfg)
    ts = PyTSFoil(airfoil_coordinates=coords, work_dir=path)
    ts.set_config(**cfg)
    ts.run()
    return ts


# ── correction term diagnostics ────────────────────────────────────────────────

def _correction_diag(ts_baseline, ts_composite, airfoil, inner_tables):
    """Return dict of all intermediate correction quantities."""
    R_c    = ts_baseline.airfoil['R_c']
    h      = ts_baseline.airfoil['h_nose']
    cpfact = ts_baseline._cpfact
    delta  = float(ts_baseline.config['EPS']) if False else float(airfoil['tmax'])
    gamma  = ts_baseline._gamma

    ile    = ts_baseline.mesh['ile']
    ite    = ts_baseline.mesh['ite']
    nfoil  = ts_baseline.mesh['nfoil']
    x_foil = ts_baseline.mesh['xx'][ile:ite + 1]

    uu_b   = ts_baseline.data_summary['uu'][ile:ite + 1]
    ul_b   = ts_baseline.data_summary['ul'][ile:ite + 1]

    s_full   = np.where(x_foil > 0.0, x_foil / R_c, 0.0)
    s_safe   = np.maximum(s_full, 1e-6)
    cp_common = _COMMON / (gamma + 1.0) ** (1.0 / 3.0) * s_safe ** (-1.0 / 3.0)

    cp_tsd_u  = -2.0 * uu_b * cpfact
    cp_tsd_l  = -2.0 * ul_b * cpfact

    # Bracket variants
    bk_old_u  = _bracket_old(cp_tsd_u, cp_common, s_full)
    bk_old_l  = _bracket_old(cp_tsd_l, cp_common, s_full)
    bk_new_u  = _bracket_new(cp_tsd_u, cp_common, s_full)
    bk_new_l  = _bracket_new(cp_tsd_l, cp_common, s_full)

    # Inner table values on x_foil
    s_tab    = inner_tables['s']
    s_clamp  = np.clip(s_full, s_tab[0], s_tab[-1])
    cp_star  = np.interp(s_clamp, s_tab, inner_tables['cp_star'])
    rho_r    = np.clip(np.interp(s_clamp, s_tab, inner_tables['rho_ratio']), 0.0, 2.0)
    phi_ox   = np.clip(np.interp(s_clamp, s_tab, inner_tables['phi_ox']),    0.0, 1.0)

    # Smoothstep blend-out weight w(x) at [s_b0=5, s_b1=10]
    w_blend  = _smooth_step((s_full - 5.0) / 5.0)

    # FXU / FXL from Fortran (after last run → ts_baseline state)
    fxu = fxl = None
    if tsf is not None:
        fxu = np.array(tsf.common_data.fxu[:nfoil], dtype=float)
        fxl = np.array(tsf.common_data.fxl[:nfoil], dtype=float)

    # Step D phi_sy and Step E phi_sx from formula
    delta_val = float(tsf.common_data.delta) if tsf is not None else airfoil['tmax']
    phi_sy, phi_sx = compute_surface_corrections(
        x_foil, h, delta_val, R_c, cpfact, gamma)

    # Cp from ts_composite for comparison
    cpu_c  = ts_composite.data_summary['cpu'][ile:ite + 1]
    cpl_c  = ts_composite.data_summary['cpl'][ile:ite + 1]
    cpu_b  = ts_baseline.data_summary['cpu'][ile:ite + 1]
    cpl_b  = ts_baseline.data_summary['cpl'][ile:ite + 1]

    # Reconstruct composite Cp with OLD vs NEW bracket (lower surface)
    def _rebuild_cp(bk, x_s, cp_s, rho_r_, phi_ox_, cp_t, minf):
        cp_comp = cp_s + rho_r_ * phi_ox_ * bk
        # Blend-out [s_b0=5, s_b1=10]
        t  = np.clip((x_s / R_c - 5.0) / 5.0, 0.0, 1.0)
        w  = t * t * (3.0 - 2.0 * t)
        cp_comp = (1.0 - w) * cp_comp + w * cp_t
        cp_comp = np.where(x_s > inner_tables['s'][-1] * R_c, cp_t, cp_comp)
        cp_comp = np.where(x_s < 0.0, cp_t, cp_comp)
        cp_stag = (2.0 / (gamma * minf ** 2)) * (
            (1.0 + 0.5 * (gamma - 1.0) * minf ** 2) ** (gamma / (gamma - 1.0)) - 1.0)
        return np.minimum(cp_comp, cp_stag)

    cpl_old = _rebuild_cp(bk_old_l, x_foil, cp_star, rho_r, phi_ox,
                          cp_tsd_l, airfoil['Ma'])
    cpl_new = _rebuild_cp(bk_new_l, x_foil, cp_star, rho_r, phi_ox,
                          cp_tsd_l, airfoil['Ma'])

    return {
        'x_foil': x_foil, 's_full': s_full, 'R_c': R_c, 'h': h,
        'cp_common': cp_common,
        'cp_tsd_u': cp_tsd_u, 'cp_tsd_l': cp_tsd_l,
        'bk_old_u': bk_old_u, 'bk_old_l': bk_old_l,
        'bk_new_u': bk_new_u, 'bk_new_l': bk_new_l,
        'cp_star': cp_star, 'rho_r': rho_r, 'phi_ox': phi_ox, 'w_blend': w_blend,
        'fxu': fxu, 'fxl': fxl,
        'phi_sy': np.array(phi_sy, dtype=float),
        'phi_sx': np.array(phi_sx, dtype=float),
        'cpu_b': cpu_b, 'cpl_b': cpl_b,
        'cpu_c': cpu_c, 'cpl_c': cpl_c,
        'cpl_old': cpl_old, 'cpl_new': cpl_new,
        'w_fade_at_mesh': _smooth_step(np.where(x_foil > 0.0, x_foil / R_c, 0.0) / 2.0),
    }


# ── plotting ───────────────────────────────────────────────────────────────────

def _plot_cp(idx, Ma, AoA, ts_b, ts_c, airfoil):
    """Figure 1: Cp / Ma full chord and LE zoom."""
    from scipy.interpolate import interp1d
    ile  = ts_b.mesh['ile']
    ite  = ts_b.mesh['ite']
    xx   = ts_b.mesh['xx']
    cpu_b = ts_b.data_summary['cpu']
    cpl_b = ts_b.data_summary['cpl']
    mau_b = ts_b.data_summary['mau']
    mal_b = ts_b.data_summary['mal']
    cpu_c = ts_c.data_summary['cpu']
    cpl_c = ts_c.data_summary['cpl']
    mau_c = ts_c.data_summary['mau']
    mal_c = ts_c.data_summary['mal']

    fcpu_b = interp1d(xx, cpu_b, kind='linear', fill_value='extrapolate')
    fcpl_b = interp1d(xx, cpl_b, kind='linear', fill_value='extrapolate')
    rmse_b = float(np.sqrt(0.5 * (
        np.mean((fcpu_b(airfoil['xu']) - airfoil['cpu']) ** 2) +
        np.mean((interp1d(xx, cpl_b, fill_value='extrapolate')(airfoil['xl']) - airfoil['cpl']) ** 2))))
    fcpu_c = interp1d(xx, cpu_c, kind='linear', fill_value='extrapolate')
    rmse_c = float(np.sqrt(0.5 * (
        np.mean((fcpu_c(airfoil['xu']) - airfoil['cpu']) ** 2) +
        np.mean((interp1d(xx, cpl_c, fill_value='extrapolate')(airfoil['xl']) - airfoil['cpl']) ** 2))))

    n_ma0_b = int(np.sum(mal_b[ile:ite + 1] == 0.0))
    n_ma0_c = int(np.sum(mal_c[ile:ite + 1] == 0.0))

    fig, axes = plt.subplots(2, 2, figsize=(14, 9))
    fig.suptitle(
        f"Case {idx} | Ma={Ma:.3f}, AoA={AoA:.2f}° "
        f"| nMa0: {n_ma0_b}→{n_ma0_c}  rmCp: {rmse_b:.4f}→{rmse_c:.4f}",
        fontsize=11)

    for ax, xlim, title in zip(axes[0], [(-.05, 1.05), (-.01, 0.12)],
                                 ['Cp full chord', 'Cp LE zoom (x ≤ 12%c)']):
        ax.plot(xx, cpu_b, 'b--', lw=1.2, label='baseline upper')
        ax.plot(xx, cpl_b, 'b--', lw=1.2, label='baseline lower')
        ax.plot(xx, cpu_c, 'r-',  lw=1.5, label='composite upper')
        ax.plot(xx, cpl_c, 'r-',  lw=1.5, label='composite lower')
        ax.plot(airfoil['xu'], airfoil['cpu'], 'g^', ms=3, markevery=5, label='RANS upper')
        ax.plot(airfoil['xl'], airfoil['cpl'], 'gv', ms=3, markevery=5, label='RANS lower')
        ax.invert_yaxis()
        ax.set(title=title, xlabel='x/c', ylabel='Cp', xlim=xlim)
        ax.legend(fontsize=7); ax.grid()

    for ax, xlim, title in zip(axes[1], [(-.05, 1.05), (-.01, 0.12)],
                                 ['Ma full chord', 'Ma LE zoom (x ≤ 12%c)']):
        ax.plot(xx, mau_b, 'b--', lw=1.2, label=f'baseline (nMa0={n_ma0_b})')
        ax.plot(xx, mal_b, 'b--', lw=1.2)
        ax.plot(xx, mau_c, 'r-',  lw=1.5, label=f'composite (nMa0={n_ma0_c})')
        ax.plot(xx, mal_c, 'r-',  lw=1.5)
        ax.axhline(1, color='k', lw=0.6, ls='--')
        ax.set(title=title, xlabel='x/c', ylabel='Ma', xlim=xlim)
        ax.legend(fontsize=7); ax.grid()

    fig.tight_layout()
    fig.savefig(os.path.join(path_figs, f'case_{idx:04d}_cp.png'), dpi=100)
    plt.close(fig)


def _plot_terms(idx, Ma, AoA, diag):
    """Figure 2: composite intermediate terms + old vs new bracket."""
    xf  = diag['x_foil']
    R_c = diag['R_c']
    mask_le = xf <= min(0.15, 20 * R_c)

    fig, axes = plt.subplots(2, 2, figsize=(14, 9))
    fig.suptitle(f"Case {idx} | Ma={Ma:.3f}, AoA={AoA:.2f}° — Composite terms  (R_c={R_c:.4f})",
                 fontsize=11)

    # ── [0,0] cp_tsd vs cp_common at LE ────────────────────────────────────────
    ax = axes[0, 0]
    ax.plot(xf[mask_le], diag['cp_tsd_u'][mask_le], 'b-',  lw=1.5, label='cp_tsd upper')
    ax.plot(xf[mask_le], diag['cp_tsd_l'][mask_le], 'r-',  lw=1.5, label='cp_tsd lower')
    ax.plot(xf[mask_le], diag['cp_common'][mask_le], 'k--', lw=2,   label='cp_common (C·s^{-1/3})')
    ax.axhline(0, color='gray', lw=0.5)
    ax.set(title='cp_tsd vs cp_common at LE  (should overlap for attached flow)',
           xlabel='x/c', ylabel='Cp')
    ax.set_xlim(-0.002, xf[mask_le].max())
    ax.legend(fontsize=7); ax.grid()

    # ── [0,1] bracket: old (hard clamp) vs new (smooth fade) ───────────────────
    ax = axes[0, 1]
    ax.plot(xf[mask_le], diag['bk_old_l'][mask_le], 'r-',  lw=2,   label='bracket old (hard s_min=0.01) — lower')
    ax.plot(xf[mask_le], diag['bk_new_l'][mask_le], 'g-',  lw=2,   label='bracket new (smooth s_fade=2.0) — lower')
    ax.plot(xf[mask_le], diag['bk_old_u'][mask_le], 'b--', lw=1.2, alpha=0.6, label='bracket old — upper')
    ax.plot(xf[mask_le], diag['bk_new_u'][mask_le], 'c--', lw=1.2, alpha=0.6, label='bracket new — upper')
    ax2 = ax.twinx()
    ax2.plot(xf[mask_le], diag['w_fade_at_mesh'][mask_le], 'm:', lw=1.5, label='w_fade (new)')
    ax2.set_ylabel('w_fade  (0 → 1)', color='m')
    ax2.tick_params(axis='y', labelcolor='m')
    ax.axhline(0, color='gray', lw=0.5)
    ax.set(title='Bracket = cp_tsd − cp_common  (old hard-clamp vs new smooth-fade)',
           xlabel='x/c', ylabel='bracket')
    ax.set_xlim(-0.002, xf[mask_le].max())
    ax.legend(fontsize=7, loc='upper right'); ax.grid()

    # ── [1,0] cp_composite lower: old vs new ────────────────────────────────────
    ax = axes[1, 0]
    mask_le2 = xf <= min(0.25, 40 * R_c)
    ax.plot(xf[mask_le2], diag['cpl_b'][mask_le2],   'b--', lw=1.2, label='cp_tsd baseline')
    ax.plot(xf[mask_le2], diag['cp_star'][mask_le2],  'orange', lw=2, label='cp_star (inner M=1)')
    ax.plot(xf[mask_le2], diag['cpl_old'][mask_le2],  'r-',  lw=2,   label='cp_composite OLD (hard clamp)')
    ax.plot(xf[mask_le2], diag['cpl_new'][mask_le2],  'g-',  lw=2,   label='cp_composite NEW (smooth fade)')
    ax.plot(xf[mask_le2], diag['cpl_c'][mask_le2],    'k^',  ms=3, markevery=2, label='pytsfoil composite (should = NEW)')
    ax.axhline(0, color='gray', lw=0.5)
    ax.invert_yaxis()
    ax.set(title='Lower-surface cp_composite  OLD vs NEW  (x ≤ 25%c)',
           xlabel='x/c', ylabel='Cp')
    ax.set_xlim(-0.002, xf[mask_le2].max())
    ax.legend(fontsize=7); ax.grid()

    # ── [1,1] phi_ox, rho_ratio, w_blend (transition functions) ─────────────────
    ax = axes[1, 1]
    mask_all = xf <= min(0.3, 60 * R_c)
    ax.plot(xf[mask_all], diag['phi_ox'][mask_all],   'b-',  lw=1.5, label='φ_ox  (inner vel, 0→1)')
    ax.plot(xf[mask_all], diag['rho_r'][mask_all],    'r-',  lw=1.5, label='ρ*/ρ_inf')
    ax.plot(xf[mask_all], diag['w_blend'][mask_all],  'g-',  lw=1.5, label='blend-out w(x)  [s_b0=5, s_b1=10]')
    ax.axhline(0, color='gray', lw=0.5); ax.axhline(1, color='gray', lw=0.5, ls='--')
    ax2 = ax.twinx()
    ax2.plot(xf[mask_all], diag['w_fade_at_mesh'][mask_all], 'm:', lw=1.5, label='w_fade new')
    ax2.set_ylabel('w_fade', color='m'); ax2.tick_params(axis='y', labelcolor='m')
    ax.set(title='Transition functions vs x',
           xlabel='x/c', ylabel='value  [0, 1]')
    ax.set_xlim(-0.002, xf[mask_all].max())
    ax.legend(fontsize=7); ax.grid()

    fig.tight_layout()
    fig.savefig(os.path.join(path_figs, f'case_{idx:04d}_terms.png'), dpi=100)
    plt.close(fig)


def _plot_bc(idx, Ma, AoA, diag):
    """Figure 3: Step-D check — phi_sy_upper vs FXU body slope."""
    xf    = diag['x_foil']
    R_c   = diag['R_c']
    mask  = xf <= min(0.15, 30 * R_c)

    fig, axes = plt.subplots(1, 2, figsize=(14, 5))
    fig.suptitle(
        f"Case {idx} | Ma={Ma:.3f}, AoA={AoA:.2f}°"
        f" — Step-D BC check  (R_c={R_c:.4f})", fontsize=11)

    ax = axes[0]
    if diag['fxu'] is not None:
        ax.plot(xf[mask], diag['fxu'][mask], 'b-',  lw=2,   label='FXU  (actual upper slope / δ)')
        ax.plot(xf[mask], diag['fxl'][mask], 'r-',  lw=2,   label='FXL  (actual lower slope / δ)')
    ax.plot(xf[mask],  diag['phi_sy'][mask], 'g--', lw=2,   label='φ_s,y upper  (Step D, = +h/(δ√x)·χ)')
    ax.plot(xf[mask], -diag['phi_sy'][mask], 'g--', lw=2)
    ax.axhline(0, color='gray', lw=0.5)
    ax.set(title='FXU/FXL vs Step-D correction φ_s,y  (should overlap near LE)',
           xlabel='x/c', ylabel='slope / δ')
    ax.set_xlim(-0.001, xf[mask].max())
    ax.legend(fontsize=8); ax.grid()

    ax = axes[1]
    if diag['fxu'] is not None:
        residual_u = diag['fxu'][mask] - diag['phi_sy'][mask]
        residual_l = diag['fxl'][mask] + diag['phi_sy'][mask]
        ax.plot(xf[mask], residual_u, 'b-', lw=1.5,
                label='FXU − φ_s,y  = φ_r,y upper BC  (should be bounded)')
        ax.plot(xf[mask], residual_l, 'r-', lw=1.5,
                label='FXL + φ_s,y  = φ_r,y lower BC')
        ax.axhline(0, color='gray', lw=0.5)
        ax.set(title='Residual BC for φ_r  (Step D removes the x^{-1/2} spike)',
               xlabel='x/c', ylabel='residual slope / δ')
        ax.set_xlim(-0.001, xf[mask].max())
        ax.legend(fontsize=8); ax.grid()

    fig.tight_layout()
    fig.savefig(os.path.join(path_figs, f'case_{idx:04d}_bc.png'), dpi=100)
    plt.close(fig)


def _plot_phisx(idx, Ma, AoA, diag):
    """Figure 4: Step-E phi_sx_surface profile and numerical derivative."""
    xf   = diag['x_foil']
    R_c  = diag['R_c']
    phi  = diag['phi_sx']     # Step E correction (x^{-1/3})

    # Numerical derivative d(phi_sx)/dx at interior points
    dx    = np.diff(xf)
    dphi  = np.diff(phi)
    dphidx = np.where(dx > 1e-12, dphi / dx, 0.0)
    xmid   = 0.5 * (xf[:-1] + xf[1:])

    fig, axes = plt.subplots(1, 2, figsize=(14, 5))
    fig.suptitle(
        f"Case {idx} | Ma={Ma:.3f}, AoA={AoA:.2f}°"
        f" — Step-E φ_s,x surface correction  (R_c={R_c:.4f})", fontsize=11)

    ax = axes[0]
    mask_full = xf <= min(0.5, 100 * R_c)
    ax.plot(xf[mask_full], phi[mask_full], 'orange', lw=2,
            label='φ_s,x  = −cp_common·χ / (2·cpfact)  (x^{−1/3})')
    ax.axhline(0, color='gray', lw=0.5)
    ax.axvline(2 * R_c, color='purple', ls=':', lw=1.2, label=f'r=r₁=2  (x={2*R_c:.4f})')
    ax.axvline(8 * R_c, color='brown',  ls=':', lw=1.2, label=f'r=r₂=8  (x={8*R_c:.4f})')
    ax.set(title='φ_s,x surface correction (Step E)',
           xlabel='x/c', ylabel='φ_s,x  (added to ul/uu)')
    ax.set_xlim(-0.002, xf[mask_full].max())
    ax.legend(fontsize=7); ax.grid()

    ax = axes[1]
    # Only show the derivative where phi is non-trivial (xf > 0 and within window)
    mask_d = (xmid > 1e-6) & (xmid <= min(0.3, 80 * R_c))
    ax.plot(xmid[mask_d], dphidx[mask_d], 'orange', lw=1.5,
            label='d(φ_s,x)/dx  (numerical derivative)')
    ax.axhline(0, color='gray', lw=0.5)
    ax.axvline(2 * R_c, color='purple', ls=':', lw=1.2, label=f'window r₁ (x={2*R_c:.4f})')
    ax.axvline(8 * R_c, color='brown',  ls=':', lw=1.2, label=f'window r₂ (x={8*R_c:.4f})')
    ax.set(title='Derivative of φ_s,x  (kinks indicate smoothness issues)',
           xlabel='x/c', ylabel='d(φ_s,x)/dx')
    ax.set_xlim(-0.002, xmid[mask_d].max() if mask_d.any() else 0.1)
    ax.legend(fontsize=7); ax.grid()

    fig.tight_layout()
    fig.savefig(os.path.join(path_figs, f'case_{idx:04d}_phisx.png'), dpi=100)
    plt.close(fig)


# ── main ────────────────────────────────────────────────────────────────────────

def run_case(airfoil):
    from pytsfoil.leading_edge import solve_inner_problem
    from scipy.interpolate import interp1d

    idx = airfoil['entry_index']
    Ma  = airfoil['Ma']
    AoA = airfoil['AoA']
    print(f"  Case {idx} Ma={Ma:.3f} AoA={AoA:.2f}° ...", flush=True)

    ts_b = _run(airfoil, {})
    ts_c = _run(airfoil, {'apply_le_correction': True})

    # Metrics
    xx = ts_b.mesh['xx']
    ile = ts_b.mesh['ile']; ite = ts_b.mesh['ite']
    n_ma0_b = int(np.sum(ts_b.data_summary['mal'][ile:ite + 1] == 0.0))
    n_ma0_c = int(np.sum(ts_c.data_summary['mal'][ile:ite + 1] == 0.0))
    fcpu = interp1d(xx, ts_b.data_summary['cpu'], fill_value='extrapolate')
    fcpl = interp1d(xx, ts_b.data_summary['cpl'], fill_value='extrapolate')
    rmse_b = float(np.sqrt(0.5 * (
        np.mean((fcpu(airfoil['xu']) - airfoil['cpu']) ** 2) +
        np.mean((fcpl(airfoil['xl']) - airfoil['cpl']) ** 2))))
    fcpu_c = interp1d(xx, ts_c.data_summary['cpu'], fill_value='extrapolate')
    fcpl_c = interp1d(xx, ts_c.data_summary['cpl'], fill_value='extrapolate')
    rmse_c = float(np.sqrt(0.5 * (
        np.mean((fcpu_c(airfoil['xu']) - airfoil['cpu']) ** 2) +
        np.mean((fcpl_c(airfoil['xl']) - airfoil['cpl']) ** 2))))

    R_c = ts_b.airfoil['R_c']
    h   = ts_b.airfoil['h_nose']

    inner_tables = solve_inner_problem(
        h=h, c=1.0, gamma=ts_b._gamma,
        cache_dir=os.path.join(path_root, 'pytsfoil', 'leading_edge', 'inner_tables_cache'))

    diag = _correction_diag(ts_b, ts_c, airfoil, inner_tables)

    _plot_cp(idx, Ma, AoA, ts_b, ts_c, airfoil)
    _plot_terms(idx, Ma, AoA, diag)
    _plot_bc(idx, Ma, AoA, diag)
    _plot_phisx(idx, Ma, AoA, diag)

    print(f"    nMa0: {n_ma0_b}→{n_ma0_c}  rmCp: {rmse_b:.4f}→{rmse_c:.4f}"
          f"  R_c={R_c:.4f}  h={h:.4f}")
    return idx, Ma, AoA, n_ma0_b, n_ma0_c, rmse_b, rmse_c


def main():
    print("Loading airfoil database...")
    db      = load_airfoil_database_from_json(fname_db)
    entries = list(db.values())

    # Select representative cases: 0 (low AoA, smooth), 3 (high AoA, stagnation)
    # and optionally a mid-AoA case.
    CASE_INDICES = [0, 1, 3]
    cases = [e for e in entries if e['entry_index'] in CASE_INDICES]
    cases.sort(key=lambda a: a['entry_index'])

    print(f"Running {len(cases)} cases ...")
    print()
    print(f"{'idx':>4}  {'Ma':>5}  {'AoA':>5}  {'nMa0_B':>6}  {'nMa0_C':>6}"
          f"  {'rmCp_B':>7}  {'rmCp_C':>7}  {'improvement':>12}")
    print('-' * 70)

    for airfoil in cases:
        idx, Ma, AoA, n0b, n0c, rm_b, rm_c = run_case(airfoil)
        impr = (rm_b - rm_c) / rm_b * 100.0
        sign = '+' if impr > 0 else ''
        print(f"{idx:>4}  {Ma:>5.3f}  {AoA:>5.2f}  {n0b:>6}  {n0c:>6}"
              f"  {rm_b:>7.4f}  {rm_c:>7.4f}  {sign}{impr:.1f}%")

    print()
    print(f"Figures saved to {path_figs}/")
    print("  case_XXXX_cp.png    — Cp/Ma distribution")
    print("  case_XXXX_terms.png — composite intermediate terms + old vs new bracket")
    print("  case_XXXX_bc.png    — Step-D: phi_sy_upper vs FXU body slope")
    print("  case_XXXX_phisx.png — Step-E: phi_sx_surface + derivative")


if __name__ == '__main__':
    main()
