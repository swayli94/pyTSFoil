"""
Test 6: IBL trailing-edge δ* correction.

Extends test_3 by applying IBL.correction_dstar() after the IBL-coupled TSD
solve.  The correction adjusts δ* near the trailing edge so that each surface's
effective geometry independently slopes at tan(α + d_angle_TE) at x = 1,
matching the RANS observation that TE separation aligns the effective body with
the freestream regardless of the geometric TE wedge angle.

Procedure per case
------------------
1. Run baseline (inviscid) TSD  — same as test_3.
2. Run IBL-coupled TSD (N_OUTER iterations, forced transition at LE)  — same as test_3.
3. Apply TE correction:  correction_dstar(d_angle_TE=D_ANGLE_TE).
4. Plot (3×2 grid, layout mirrors test_3):
   [0,0] Cp          — baseline / IBL-coupled / RANS
   [0,1] Geometry    — airfoil + effective body: IBL raw vs TE-corrected + freestream line
   [1,0] Mach        — baseline / IBL-coupled / RANS
   [1,1] Convergence — CL and Cd_f history
   [2,0] δ*          — IBL raw vs TE-corrected (upper thick, lower thin)
   [2,1] Δδ*         — correction ramp and TE slope angle summary
"""

import os
import sys
import time
import numpy as np
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches
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

LIST_CASES     = [0, 1, 2, 3, 4, 6, 7, 10, 11, 12]   # indices of cases to run (in airfoil_database.json)
N_PROCESS      = 10
N_OUTER        = 10
IBL_RELAX      = 0.5
MAXIT_INNER    = 200
I_OUTER_REPAIR = 3
D_ANGLE_TE     = 0.0
BLEND_START    = 0.9

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
}

C_BASE = '#2ca02c'   # green  – baseline TSD (matches test_3)
C_IBL  = '#1f77b4'   # blue   – IBL-coupled raw (matches test_3)
C_CORR = '#d62728'   # red    – TE-corrected
C_RANS = 'k'         # black  – RANS reference


# ── helpers ───────────────────────────────────────────────────────────────────

def _plot_surface(ax, xx, res, color, key, lw=1.8):
    """Plot a surface quantity, splitting at the transition point (dashed=laminar)."""
    i_tr = res['i_tr']
    if i_tr > 1:
        ax.plot(xx[:i_tr + 1], res[key][:i_tr + 1], color=color, lw=lw, ls='--')
    ax.plot(xx[i_tr:], res[key][i_tr:], color=color, lw=lw, ls='-')


def _te_surface_slope(xx, yy, dstar, sign):
    """Effective surface slope at TE.  sign=+1 for upper (y+δ*), -1 for lower (y-δ*)."""
    return np.gradient(yy + sign * dstar, xx)[-1]


# ── worker ────────────────────────────────────────────────────────────────────

def run_single(airfoil: dict) -> dict:
    """Worker: run baseline + IBL-coupled TSD + TE correction for one airfoil."""
    try:
        t0 = time.time()

        xu    = np.array(airfoil['xu'])
        yu    = np.array(airfoil['yu'])
        xl    = np.array(airfoil['xl'])
        yl    = np.array(airfoil['yl'])
        EMACH  = float(airfoil['Ma'])
        ALPHA  = float(airfoil['AoA'])
        REYNLD = float(airfoil['Re'])

        x_coords = np.concatenate([xu[::-1], xl[1:]])
        y_coords = np.concatenate([yu[::-1], yl[1:]])
        airfoil_coords = np.column_stack([x_coords, y_coords])

        cfg = {**BASE_CFG, 'EMACH': EMACH, 'ALPHA': ALPHA, 'REYNLD': REYNLD}

        # ── Baseline: pure inviscid TSD ───────────────────────────────────────
        t_base0 = time.time()
        ts = PyTSFoil(airfoil_coordinates=airfoil_coords, work_dir=path)
        ts.set_config(**cfg)
        ts.run()
        elapsed_base = time.time() - t_base0

        xx_full = ts.mesh['xx'].copy()
        ile     = ts.mesh['ile']
        ite     = ts.mesh['ite']
        xx_foil = xx_full[ile:ite + 1]

        cpu_base = ts.data_summary['cpu'].copy()
        cpl_base = ts.data_summary['cpl'].copy()
        mau_base = ts.data_summary['mau'].copy()
        mal_base = ts.data_summary['mal'].copy()
        cl_base  = float(ts.data_summary['cl'])
        cd_base  = float(ts.data_summary['cd'])

        # ── IBL-coupled TSD ───────────────────────────────────────────────────
        t_ibl0 = time.time()
        ibl = IBL(Re=REYNLD, M_inf=EMACH)
        history = ts.run_ibl_coupled(
            ibl=ibl,
            n_outer=N_OUTER,
            ibl_relax=IBL_RELAX,
            x_tr_upper=0.0,
            x_tr_lower=0.0,
            maxit_inner=MAXIT_INNER,
            i_outer_repair=I_OUTER_REPAIR,
            use_te_correction = True,
            d_angle_TE = D_ANGLE_TE,
            x_blend_start = BLEND_START,
        )
        elapsed_ibl = time.time() - t_ibl0

        cpu_ibl  = ts.data_summary['cpu'].copy()
        cpl_ibl  = ts.data_summary['cpl'].copy()
        mau_ibl  = ts.data_summary['mau'].copy()
        mal_ibl  = ts.data_summary['mal'].copy()
        cl_ibl   = float(ts.data_summary['cl'])
        cd_ibl   = float(ts.data_summary['cd'])
        cd_f_ibl = float(ts.data_summary['ibl_cd_f'])
        upper    = ts.data_summary['ibl_upper']
        lower    = ts.data_summary['ibl_lower']
        cpstar   = ts.data_summary.get('cpstar')

        dstar_u      = upper['delta_star_raw'].copy()   # pre-TE-correction
        dstar_l      = lower['delta_star_raw'].copy()
        dstar_u_corr = upper['delta_star'].copy()       # post-TE-correction
        dstar_l_corr = lower['delta_star'].copy()

        yu_foil = np.interp(xx_foil, xu, yu)
        yl_foil = np.interp(xx_foil, xl, yl)

        # ── RMSE Cp vs RANS ───────────────────────────────────────────────────
        cpu_rans = np.array(airfoil['cpu'])
        cpl_rans = np.array(airfoil['cpl'])

        f_cpu_base = interp1d(xx_full, cpu_base, kind='linear', fill_value='extrapolate')
        f_cpl_base = interp1d(xx_full, cpl_base, kind='linear', fill_value='extrapolate')
        f_cpu_ibl  = interp1d(xx_full, cpu_ibl,  kind='linear', fill_value='extrapolate')
        f_cpl_ibl  = interp1d(xx_full, cpl_ibl,  kind='linear', fill_value='extrapolate')

        rmse_base = float(np.sqrt(0.5 * (
            np.mean((f_cpu_base(xu) - cpu_rans) ** 2) +
            np.mean((f_cpl_base(xl) - cpl_rans) ** 2)
        )))
        rmse_ibl = float(np.sqrt(0.5 * (
            np.mean((f_cpu_ibl(xu) - cpu_rans) ** 2) +
            np.mean((f_cpl_ibl(xl) - cpl_rans) ** 2)
        )))

        # ── TE slope angles ───────────────────────────────────────────────────
        target_slope = np.tan(np.deg2rad(ALPHA + D_ANGLE_TE))

        m_u_raw  = _te_surface_slope(xx_foil, yu_foil, dstar_u,      +1)
        m_l_raw  = _te_surface_slope(xx_foil, yl_foil, dstar_l,      -1)
        m_u_corr = _te_surface_slope(xx_foil, yu_foil, dstar_u_corr, +1)
        m_l_corr = _te_surface_slope(xx_foil, yl_foil, dstar_l_corr, -1)

        result = {
            'success':     True,
            'entry_index': airfoil['entry_index'],
            'airfoil_id':  airfoil['airfoil_id'],
            'Ma': EMACH, 'AoA': ALPHA, 'Re': REYNLD,
            'elapsed': time.time() - t0,
            'elapsed_base': elapsed_base,
            'elapsed_ibl':  elapsed_ibl,
            'xx_full': xx_full, 'ile': ile, 'ite': ite, 'xx_foil': xx_foil,
            'xu': xu, 'yu': yu, 'xl': xl, 'yl': yl,
            'yu_foil': yu_foil, 'yl_foil': yl_foil,
            'cpu_rans': cpu_rans, 'cpl_rans': cpl_rans,
            'mwu_rans': np.array(airfoil['mwu']),
            'mwl_rans': np.array(airfoil['mwl']),
            'cl_rans': float(airfoil['CL_RANS']),
            'cd_rans': float(airfoil['Cd_RANS']),
            'cpu_base': cpu_base, 'cpl_base': cpl_base,
            'mau_base': mau_base, 'mal_base': mal_base,
            'cl_base': cl_base, 'cd_base': cd_base,
            'cpu_ibl': cpu_ibl, 'cpl_ibl': cpl_ibl,
            'mau_ibl': mau_ibl, 'mal_ibl': mal_ibl,
            'cl_ibl': cl_ibl, 'cd_ibl': cd_ibl, 'cd_f_ibl': cd_f_ibl,
            'upper': upper, 'lower': lower, 'history': history, 'cpstar': cpstar,
            'rmse_base': rmse_base, 'rmse_ibl': rmse_ibl,
            'dstar_u': dstar_u, 'dstar_l': dstar_l,
            'dstar_u_corr': dstar_u_corr, 'dstar_l_corr': dstar_l_corr,
            'm_u_raw':  m_u_raw,  'm_l_raw':  m_l_raw,
            'm_u_corr': m_u_corr, 'm_l_corr': m_l_corr,
            'target_slope': target_slope,
        }

        _plot(result)
        return result

    except Exception as e:
        import traceback
        print(f"Error in case {airfoil.get('entry_index', '?')}: {e}")
        return {
            'success':     False,
            'entry_index': airfoil.get('entry_index', -1),
            'error':       traceback.format_exc(),
        }


# ── plotting ──────────────────────────────────────────────────────────────────

def _plot(r: dict):
    idx = r['entry_index']
    Ma  = r['Ma']
    AoA = r['AoA']
    Re  = r['Re']

    xx_foil  = r['xx_foil']
    xu, xl   = r['xu'], r['xl']
    ile, ite = r['ile'], r['ite']
    xx_full  = r['xx_full']
    upper    = r['upper']
    lower    = r['lower']
    history  = r['history']
    yu_foil  = r['yu_foil']
    yl_foil  = r['yl_foil']
    dstar_u  = r['dstar_u']
    dstar_l  = r['dstar_l']
    dstar_u_corr = r['dstar_u_corr']
    dstar_l_corr = r['dstar_l_corr']

    angle_u_raw  = np.rad2deg(np.arctan(r['m_u_raw']))
    angle_l_raw  = np.rad2deg(np.arctan(r['m_l_raw']))
    angle_u_corr = np.rad2deg(np.arctan(r['m_u_corr']))
    angle_l_corr = np.rad2deg(np.arctan(r['m_l_corr']))
    angle_target = AoA + D_ANGLE_TE

    fig = plt.figure(figsize=(16, 16))
    fig.suptitle(
        f"Case {idx} | airfoil id={r['airfoil_id']}  "
        f"$M_\\infty={Ma:.3f}$,  $\\alpha={AoA:.2f}°$,  $Re={Re:.1e}$\n"
        f"RMSE Cp — baseline: {r['rmse_base']:.4f}  |  IBL-coupled: {r['rmse_ibl']:.4f}  "
        f"|  TE target angle: {angle_target:.1f}°  "
        f"(upper: {angle_u_raw:.2f}°→{angle_u_corr:.2f}°, "
        f"lower: {angle_l_raw:.2f}°→{angle_l_corr:.2f}°)",
        fontsize=11, fontweight='bold',
    )
    gs = fig.add_gridspec(3, 2, hspace=0.38, wspace=0.30,
                          left=0.08, right=0.97, top=0.91, bottom=0.04)

    ax_cp   = fig.add_subplot(gs[0, 0])
    ax_geom = fig.add_subplot(gs[0, 1])
    ax_mach = fig.add_subplot(gs[1, 0])
    ax_conv = fig.add_subplot(gs[1, 1])
    ax_dstr = fig.add_subplot(gs[2, 0])
    ax_corr = fig.add_subplot(gs[2, 1])

    # ── [0,0] Cp distribution ─────────────────────────────────────────────────
    ax = ax_cp
    ax.plot(xu, r['cpu_rans'], color=C_RANS, lw=5.0, ls='-',  alpha=0.5, label='RANS upper')
    ax.plot(xl, r['cpl_rans'], color=C_RANS, lw=5.0, ls='--', alpha=0.5, label='RANS lower')
    ax.plot(xx_foil, r['cpu_base'][ile:ite + 1], color=C_BASE, lw=1.8, ls='-',
            label=f'Baseline upper  (CL={r["cl_base"]:.4f})')
    ax.plot(xx_foil, r['cpl_base'][ile:ite + 1], color=C_BASE, lw=1.8, ls='--',
            label='Baseline lower')
    ax.plot(xx_foil, r['cpu_ibl'][ile:ite + 1],  color=C_IBL,  lw=1.8, ls='-',
            label=f'IBL-coupled upper  (CL={r["cl_ibl"]:.4f})')
    ax.plot(xx_foil, r['cpl_ibl'][ile:ite + 1],  color=C_IBL,  lw=1.8, ls='--',
            label='IBL-coupled lower')
    if r['cpstar'] is not None:
        ax.axhline(r['cpstar'], color='k', lw=0.9, ls=':', alpha=0.7,
                   label=f'Cp* = {r["cpstar"]:.3f}')
    ax.invert_yaxis()
    ax.set_xlim(-0.02, 1.02)
    ax.set_xlabel('$x/c$', fontsize=11)
    ax.set_ylabel('$C_p$', fontsize=11)
    ax.set_title('Pressure coefficient $C_p$', fontsize=11)
    ax.legend(fontsize=8, ncol=3, loc='lower right')
    ax.grid(True, alpha=0.3)

    # ── [0,1] Airfoil geometry + effective body (raw vs corrected) ────────────
    ax = ax_geom
    ax.plot(xx_foil, yu_foil, color='k', lw=1.8, ls='-', label='Airfoil geometry')
    ax.plot(xx_foil, yl_foil, color='k', lw=1.8, ls='-')

    yu_raw  = yu_foil + dstar_u
    yl_raw  = yl_foil - dstar_l
    yu_corr = yu_foil + dstar_u_corr
    yl_corr = yl_foil - dstar_l_corr

    ax.plot(xx_foil, yu_raw,  color=C_IBL,  lw=1.6, ls='--', label='Eff. body IBL raw')
    ax.plot(xx_foil, yl_raw,  color=C_IBL,  lw=1.6, ls='--')
    ax.plot(xx_foil, yu_corr, color=C_CORR, lw=1.8, ls='-',  label='Eff. body TE-corrected')
    ax.plot(xx_foil, yl_corr, color=C_CORR, lw=1.8, ls='-')

    # Freestream direction line through geometric TE midpoint
    y_te   = 0.5 * (yu_foil[-1] + yl_foil[-1])
    x_line = np.array([0.6, 1.05])
    ax.plot(x_line, y_te + (x_line - 1.0) * np.tan(np.deg2rad(AoA)),
            color='royalblue', ls=':', lw=1.4, alpha=0.9,
            label=f'Freestream  ($\\alpha={AoA:.1f}°$)')

    ax.fill_between(xx_foil, yu_raw,  yu_corr, alpha=0.18, color=C_CORR, label='Δδ* region')
    ax.fill_between(xx_foil, yl_corr, yl_raw,  alpha=0.18, color=C_CORR)

    ax.set_xlabel('$x/c$', fontsize=11)
    ax.set_ylabel('$y/c$', fontsize=11)
    ax.set_title('Airfoil geometry and viscous effective body', fontsize=11)
    ax.set_xlim(-0.05, 1.05)
    ax.legend(fontsize=8, loc='upper right')
    ax.grid(True, alpha=0.3)

    # ── [1,0] Mach distribution ───────────────────────────────────────────────
    ax = ax_mach
    ax.plot(xu, r['mwu_rans'], color=C_RANS, lw=5.0, ls='-',  alpha=0.5, label='RANS upper')
    ax.plot(xl, r['mwl_rans'], color=C_RANS, lw=5.0, ls='--', alpha=0.5, label='RANS lower')
    ax.plot(xx_foil, r['mau_base'][ile:ite + 1], color=C_BASE, lw=1.8, ls='-',
            label='Baseline upper')
    ax.plot(xx_foil, r['mal_base'][ile:ite + 1], color=C_BASE, lw=1.8, ls='--',
            label='Baseline lower')
    ax.plot(xx_foil, r['mau_ibl'][ile:ite + 1],  color=C_IBL,  lw=1.8, ls='-',
            label='IBL-coupled upper')
    ax.plot(xx_foil, r['mal_ibl'][ile:ite + 1],  color=C_IBL,  lw=1.8, ls='--',
            label='IBL-coupled lower')
    ax.axhline(1.0, color='k', lw=0.9, ls=':', alpha=0.2, label='Sonic ($M=1$)')
    ax.axhline(Ma, color='k', lw=0.9, ls=':', alpha=0.2, label='$M$ (free-stream)')
    ax.set_xlim(-0.05, 1.05)
    ax.set_ylim(-0.05, None)
    ax.set_xlabel('$x/c$', fontsize=11)
    ax.set_ylabel('Mach number', fontsize=11)
    ax.set_title('Wall Mach number', fontsize=11)
    ax.legend(fontsize=8, ncol=2)
    ax.grid(True, alpha=0.3)

    # ── [1,1] Coupling convergence history ────────────────────────────────────
    ax = ax_conv
    iters  = np.arange(1, len(history) + 1)
    cl_his = np.array([h['cl']   for h in history])
    cf_his = np.array([h['cd_f'] for h in history])
    ax2 = ax.twinx()
    ax.plot(iters, cl_his, 'o-', color=C_IBL, lw=1.8, ms=6, label='CL')
    ax2.plot(iters, cf_his * 1e4, 's--', color='#ff7f0e', lw=1.8, ms=6,
             label='$C_{d,f}$ (×10⁴)')
    ax.axhline(r['cl_rans'], color=C_RANS, lw=1.0, ls=':', label='CL (RANS)')
    ax.set_xlabel('Outer iteration', fontsize=11)
    ax.set_ylabel('$C_L$', fontsize=11)
    ax2.set_ylabel('$C_{d,f} \\times 10^4$', fontsize=11, color='#ff7f0e')
    ax2.tick_params(axis='y', labelcolor='#ff7f0e')
    ax.set_title('Coupling convergence', fontsize=11)
    ax.set_xticks(iters)
    lines1, labs1 = ax.get_legend_handles_labels()
    lines2, labs2 = ax2.get_legend_handles_labels()
    ax.legend(lines1 + lines2, labs1 + labs2, fontsize=8, loc='right')
    ax.grid(True, alpha=0.3)

    # ── [2,0] Displacement thickness: raw vs TE-corrected ────────────────────
    ax = ax_dstr
    C_U = '#1f77b4'
    C_L = '#d62728'

    # Pre-correction δ* (thin lines, lam=dashed / turb=solid)
    _plot_surface(ax, xx_foil, upper, C_U, 'delta_star_raw', lw=1.5)
    _plot_surface(ax, xx_foil, lower, C_L, 'delta_star_raw', lw=1.5)

    # Post-correction δ* (thicker solid lines)
    ax.plot(xx_foil, dstar_u_corr, color=C_U, lw=3.0, ls='-')
    ax.plot(xx_foil, dstar_l_corr, color=C_L, lw=3.0, ls='-')

    ax.axvline(BLEND_START, color='g', lw=2.0, ls=':', alpha=0.8,
               label=f'Blend start $x={BLEND_START}$')
    ax.axvline(upper['x_tr'], color=C_U, lw=1.0, ls=':', alpha=0.5,
               label=f'$x_{{tr}}$ upper={upper["x_tr"]:.3f}')
    ax.axvline(lower['x_tr'], color=C_L, lw=1.0, ls=':', alpha=0.5,
               label=f'$x_{{tr}}$ lower={lower["x_tr"]:.3f}')

    leg = [
        mpatches.Patch(color=C_U, label='Upper'),
        mpatches.Patch(color=C_L, label='Lower'),
        plt.Line2D([0], [0], color='k', lw=3.0, ls='-',  label='TE-corrected $\\delta^*$'),
        plt.Line2D([0], [0], color='k', lw=1.5, ls='-',  label='IBL raw $\\delta^*$ (turb.)'),
        plt.Line2D([0], [0], color='k', lw=1.5, ls='--', label='IBL raw $\\delta^*$ (lam.)'),
    ]
    ax.legend(handles=leg, fontsize=8, ncol=2, loc='upper left')
    ax.set_xlabel('$x/c$', fontsize=11)
    ax.set_ylabel('$\\delta^* / c$', fontsize=11)
    ax.set_title('Displacement thickness $\\delta^*$  (upper thick, lower thin)', fontsize=11)
    ax.set_xlim(-0.02, 1.02)
    ax.grid(True, alpha=0.3)

    # ── [2,1] Skin friction coefficient ──────────────────────────────────────
    ax = ax_corr
    _plot_surface(ax, xx_foil, upper, C_U, 'cf', lw=1.8)
    _plot_surface(ax, xx_foil, lower, C_L, 'cf', lw=1.8)
    ax.axvline(upper['x_tr'], color=C_U, lw=1.0, ls=':', alpha=0.8)
    ax.axvline(lower['x_tr'], color=C_L, lw=1.0, ls=':', alpha=0.8)
    cd_f_u = float(np.trapezoid(upper['cf'], upper['s']))
    cd_f_l = float(np.trapezoid(lower['cf'], lower['s']))
    leg2 = [
        mpatches.Patch(color=C_U, label=f'Upper  $C_{{d,f}}={cd_f_u:.4f}$'),
        mpatches.Patch(color=C_L, label=f'Lower  $C_{{d,f}}={cd_f_l:.4f}$'),
        plt.Line2D([0], [0], color='k', lw=1.8, ls='-',  label='Turbulent'),
        plt.Line2D([0], [0], color='k', lw=1.8, ls='--', label='Laminar'),
    ]
    ax.legend(handles=leg2, fontsize=8, ncol=2, loc='upper right')
    ax.set_xlabel('$x/c$', fontsize=11)
    ax.set_ylabel('$c_f$', fontsize=11)
    ax.set_title(f'Skin friction  ($C_{{d,f}}={r["cd_f_ibl"]:.4f}$ total)', fontsize=11)
    ax.set_xlim(-0.02, 1.02)
    ax.set_ylim(-0.0005, None)
    ax.grid(True, alpha=0.3)

    fig.savefig(os.path.join(path_figs, f'case_{idx:04d}_te_correction.png'),
                dpi=120, bbox_inches='tight')
    plt.close(fig)


# ── summary ───────────────────────────────────────────────────────────────────

def print_summary(all_results, fname):
    with open(fname, 'w') as f:
        f.write('\n' + '=' * 120 + '\n')
        f.write(f'{"idx":>4}  {"Ma":>5}  {"AoA":>5}  {"Re":>8}  '
                f'{"metric":<14}  {"baseline":>10}  {"IBL-coupled":>11}  {"RANS":>10}\n')
        f.write('-' * 120 + '\n')

        for r in all_results:
            if not r['success']:
                f.write(f"  {r['entry_index']:>3}  FAILED: {str(r['error'])[:70]}\n\n")
                continue

            idx = r['entry_index']
            Ma  = r['Ma']
            AoA = r['AoA']
            Re  = r['Re']

            cd_tot_ibl = r['cd_ibl'] + r['cd_f_ibl']

            angle_u_raw  = np.rad2deg(np.arctan(r['m_u_raw']))
            angle_l_raw  = np.rad2deg(np.arctan(r['m_l_raw']))
            angle_u_corr = np.rad2deg(np.arctan(r['m_u_corr']))
            angle_l_corr = np.rad2deg(np.arctan(r['m_l_corr']))
            target       = AoA + D_ANGLE_TE

            rows = [
                ('RMSE_Cp',   f'{r["rmse_base"]:>10.4f}',  f'{r["rmse_ibl"]:>11.4f}',  f'{"N/A":>10}'),
                ('CL',        f'{r["cl_base"]:>10.5f}',    f'{r["cl_ibl"]:>11.5f}',    f'{r["cl_rans"]:>10.5f}'),
                ('CD_wave',   f'{r["cd_base"]:>10.5f}',    f'{r["cd_ibl"]:>11.5f}',    f'{"N/A":>10}'),
                ('CD_fric',   f'{"N/A":>10}',               f'{r["cd_f_ibl"]:>11.5f}',  f'{"N/A":>10}'),
                ('CD_total',  f'{"N/A":>10}',               f'{cd_tot_ibl:>11.5f}',     f'{r["cd_rans"]:>10.5f}'),
                ('TE_ang_u',  f'{"N/A":>10}',
                 f'{angle_u_raw:>+9.3f}°→{angle_u_corr:>+.3f}°', f'{target:>+9.2f}°'),
                ('TE_ang_l',  f'{"N/A":>10}',
                 f'{angle_l_raw:>+9.3f}°→{angle_l_corr:>+.3f}°', f'{target:>+9.2f}°'),
                ('CPU_s',     f'{r["elapsed_base"]:>10.2f}', f'{r["elapsed_ibl"]:>11.2f}', f'{"N/A":>10}'),
            ]

            first = True
            for metric, base_val, ibl_val, rans_val in rows:
                prefix = f" {idx:>3}  {Ma:>5.3f}  {AoA:>5.2f}  {Re:>8.1e}" if first else ' ' * 28
                first = False
                f.write(f"{prefix}  {metric:<14}  {base_val}  {ibl_val}  {rans_val}\n")
            f.write('\n')

        f.write('=' * 120 + '\n')


# ── main ──────────────────────────────────────────────────────────────────────

if __name__ == '__main__':

    db    = load_airfoil_database_from_json(fname_db)
    cases = list(db.values())
    cases = [cases[i] for i in LIST_CASES]

    print(f"Running {len(LIST_CASES)} cases with N_PROCESS={N_PROCESS} ...")
    print(f"  N_OUTER={N_OUTER}  IBL_RELAX={IBL_RELAX}  forced transition at x=0")
    print(f"  TE correction: d_angle_TE={D_ANGLE_TE}°  blend_start={BLEND_START}")

    with mp.Pool(N_PROCESS) as pool:
        all_results = pool.map(run_single, cases)
    
    # all_results = []
    # for case in cases:
    #     all_results.append(run_single(case))

    n_ok   = sum(1 for r in all_results if r['success'])
    n_fail = len(all_results) - n_ok
    print(f"Done: {n_ok}/{len(all_results)} succeeded, {n_fail} failed.")

    fname = os.path.join(path_figs, 'summary.txt')
    print_summary(all_results, fname)
    print(f"Summary written to {fname}")
