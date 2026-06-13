"""
RAE2822 single-case TSD + IBL-coupled calculation.

Procedure
---------
1. Generate RAE2822 geometry from CST coefficients.
2. Run baseline (inviscid) TSD.
3. Run IBL-coupled TSD (forced transition at LE, with TE δ* correction).
4. Plot (3x2 grid):
   [0,0] Cp          — baseline vs IBL-coupled
   [0,1] Geometry    — airfoil + effective body (raw vs TE-corrected)
   [1,0] Mach        — baseline vs IBL-coupled
   [1,1] Convergence — CL and Cd_f history
   [2,0] δ*          — raw vs TE-corrected (upper/lower)
   [2,1] cf          — skin-friction coefficient
"""

import os
import sys
import time
import numpy as np
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches

path      = os.path.abspath(os.path.dirname(__file__))
path_root = os.path.abspath(os.path.join(path, '..', '..'))
sys.path.insert(0, path_root)

from pytsfoil import PyTSFoil, IBL
from cst_modeling.section import cst_foil

# Airfoil geometry

CST_U = np.array([ 0.12829643,  0.12670863,  0.16065898,  0.14942386,  0.15102884,
                   0.22416928,  0.16078175,  0.20998555,  0.18608795,  0.21052324])
CST_L = np.array([-0.12927128, -0.13176061, -0.17044964, -0.07045476, -0.33888064,
                   0.00991923, -0.20070721, -0.03536713, -0.04397496,  0.06436195])

T_MAX  = 0.121   # RAE2822 max thickness ratio
N_FOIL = 201

# Flight conditions

EMACH  = 0.75
ALPHA  = 0.5
REYNLD = 6.5e6

# IBL parameters

N_OUTER        = 10
IBL_RELAX      = 0.5
MAXIT_INNER    = 200
I_OUTER_REPAIR = 3
USE_TE_CORRECTION = True
D_ANGLE_TE     = 0.0
BLEND_START    = 0.9

# Solver configuration

CFG = {
    'EMACH':  EMACH,
    'ALPHA':  ALPHA,
    'REYNLD': REYNLD,
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

C_BASE = '#2ca02c'
C_IBL  = '#1f77b4'
C_CORR = '#d62728'


def _plot_surface(ax, xx, res, color, key, lw=1.8):
    i_tr = res['i_tr']
    if i_tr > 1:
        ax.plot(xx[:i_tr + 1], res[key][:i_tr + 1], color=color, lw=lw, ls='--')
    ax.plot(xx[i_tr:], res[key][i_tr:], color=color, lw=lw, ls='-')


def _te_surface_slope(xx, yy, dstar, sign):
    return np.gradient(yy + sign * dstar, xx)[-1]


def plot_results(r: dict, fname: str) -> None:
    """
    Summary figure for one TSD + IBL-coupled run.

    Parameters
    ----------
    r : dict
        Result dict produced by the main block (keys documented inline).
    fname : str
        Output file path for the saved figure.
    """
    xx_foil = r['xx_foil']
    ile, ite = r['ile'], r['ite']
    upper, lower = r['upper'], r['lower']

    dstar_u      = r['dstar_u']
    dstar_l      = r['dstar_l']
    dstar_u_corr = r['dstar_u_corr']
    dstar_l_corr = r['dstar_l_corr']
    yu_foil      = r['yu_foil']
    yl_foil      = r['yl_foil']

    angle_u_raw  = np.rad2deg(np.arctan(r['m_u_raw']))
    angle_l_raw  = np.rad2deg(np.arctan(r['m_l_raw']))
    angle_u_corr = np.rad2deg(np.arctan(r['m_u_corr']))
    angle_l_corr = np.rad2deg(np.arctan(r['m_l_corr']))

    fig = plt.figure(figsize=(16, 16))
    fig.suptitle(
        f"RAE2822  $M_\\infty={r['emach']:.3f}$,  "
        f"$\\alpha={r['alpha']:.2f}°$,  $Re={r['reynld']:.1e}$\n"
        f"TE slope angle target: {r['alpha'] + D_ANGLE_TE:.1f}°  "
        f"(upper: {angle_u_raw:.2f}°→{angle_u_corr:.2f}°, "
        f"lower: {angle_l_raw:.2f}°→{angle_l_corr:.2f}°)",
        fontsize=12, fontweight='bold',
    )
    gs = fig.add_gridspec(3, 2, hspace=0.38, wspace=0.30,
                          left=0.08, right=0.97, top=0.91, bottom=0.04)

    ax_cp   = fig.add_subplot(gs[0, 0])
    ax_geom = fig.add_subplot(gs[0, 1])
    ax_mach = fig.add_subplot(gs[1, 0])
    ax_conv = fig.add_subplot(gs[1, 1])
    ax_dstr = fig.add_subplot(gs[2, 0])
    ax_cf   = fig.add_subplot(gs[2, 1])

    # [0,0] Cp
    ax = ax_cp
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
    ax.legend(fontsize=8, ncol=2, loc='lower right')
    ax.grid(True, alpha=0.3)

    # [0,1] Geometry + effective body
    ax = ax_geom
    ax.plot(xx_foil, yu_foil, color='k', lw=1.8, label='Airfoil geometry')
    ax.plot(xx_foil, yl_foil, color='k', lw=1.8)

    yu_raw  = yu_foil + dstar_u
    yl_raw  = yl_foil - dstar_l
    yu_corr = yu_foil + dstar_u_corr
    yl_corr = yl_foil - dstar_l_corr

    ax.plot(xx_foil, yu_raw,  color=C_IBL,  lw=1.6, ls='--', label='Eff. body IBL raw')
    ax.plot(xx_foil, yl_raw,  color=C_IBL,  lw=1.6, ls='--')
    ax.plot(xx_foil, yu_corr, color=C_CORR, lw=1.8, ls='-',  label='Eff. body TE-corrected')
    ax.plot(xx_foil, yl_corr, color=C_CORR, lw=1.8, ls='-')

    y_te   = 0.5 * (yu_foil[-1] + yl_foil[-1])
    x_line = np.array([0.6, 1.05])
    ax.plot(x_line, y_te + (x_line - 1.0) * np.tan(np.deg2rad(r['alpha'])),
            color='royalblue', ls=':', lw=1.4, alpha=0.9,
            label=f'Freestream  ($\\alpha={r["alpha"]:.1f}°$)')

    ax.fill_between(xx_foil, yu_raw,  yu_corr, alpha=0.18, color=C_CORR, label='Δδ* region')
    ax.fill_between(xx_foil, yl_corr, yl_raw,  alpha=0.18, color=C_CORR)

    ax.set_xlabel('$x/c$', fontsize=11)
    ax.set_ylabel('$y/c$', fontsize=11)
    ax.set_title('Airfoil geometry and viscous effective body', fontsize=11)
    ax.set_xlim(-0.05, 1.05)
    ax.legend(fontsize=8, loc='upper right')
    ax.grid(True, alpha=0.3)

    # [1,0] Mach
    ax = ax_mach
    ax.plot(xx_foil, r['mau_base'][ile:ite + 1], color=C_BASE, lw=1.8, ls='-',
            label='Baseline upper')
    ax.plot(xx_foil, r['mal_base'][ile:ite + 1], color=C_BASE, lw=1.8, ls='--',
            label='Baseline lower')
    ax.plot(xx_foil, r['mau_ibl'][ile:ite + 1],  color=C_IBL,  lw=1.8, ls='-',
            label='IBL-coupled upper')
    ax.plot(xx_foil, r['mal_ibl'][ile:ite + 1],  color=C_IBL,  lw=1.8, ls='--',
            label='IBL-coupled lower')
    ax.axhline(1.0,            color='k', lw=0.9, ls=':', alpha=0.2, label='Sonic ($M=1$)')
    ax.axhline(r['emach'],     color='k', lw=0.9, ls=':', alpha=0.2, label='$M_\\infty$')
    ax.set_xlim(-0.05, 1.05)
    ax.set_ylim(-0.05, None)
    ax.set_xlabel('$x/c$', fontsize=11)
    ax.set_ylabel('Mach number', fontsize=11)
    ax.set_title('Wall Mach number', fontsize=11)
    ax.legend(fontsize=8, ncol=2)
    ax.grid(True, alpha=0.3)

    # [1,1] Coupling convergence
    ax = ax_conv
    history = r['history']
    iters   = np.arange(1, len(history) + 1)
    cl_his  = np.array([h['cl']   for h in history])
    cf_his  = np.array([h['cd_f'] for h in history])
    ax2 = ax.twinx()
    ax.plot(iters, cl_his, 'o-', color=C_IBL, lw=1.8, ms=6, label='CL')
    ax2.plot(iters, cf_his * 1e4, 's--', color='#ff7f0e', lw=1.8, ms=6,
             label='$C_{d,f}$ (x10⁴)')
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

    # [2,0] Displacement thickness
    C_U = '#1f77b4'
    C_L = '#d62728'
    ax = ax_dstr
    _plot_surface(ax, xx_foil, upper, C_U, 'delta_star_raw', lw=1.5)
    _plot_surface(ax, xx_foil, lower, C_L, 'delta_star_raw', lw=1.5)
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

    # [2,1] Skin friction
    ax = ax_cf
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

    fig.savefig(fname, dpi=150, bbox_inches='tight')
    plt.close(fig)


if __name__ == '__main__':

    #* Build airfoil geometry
    x_cst, yu_cst, yl_cst, tmax, radius_LE = cst_foil(N_FOIL, CST_U, CST_L, t=T_MAX)
    print(f"RAE2822  t/c={tmax:.4f}  r_LE={radius_LE:.4f}")

    xu = x_cst.copy()
    xl = x_cst.copy()
    yu = yu_cst.copy()
    yl = yl_cst.copy()

    airfoil_coords = np.column_stack([
        np.concatenate([xu[::-1], xl[1:]]),
        np.concatenate([yu[::-1], yl[1:]]),
    ])

    #* Baseline: inviscid TSD
    print(f"\nBaseline TSD  (Ma={EMACH}, AoA={ALPHA}°, Re={REYNLD:.1e})")
    t0 = time.time()

    ts = PyTSFoil(airfoil_coordinates=airfoil_coords, work_dir=path)
    ts.set_config(**CFG)
    ts.run()

    elapsed_base = time.time() - t0
    print(f"  CL={ts.data_summary['cl']:.5f}  CD_wave={ts.data_summary['cd']:.5f}"
          f"  t={elapsed_base:.1f}s")

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
    cpstar   = ts.data_summary.get('cpstar')

    #* IBL-coupled TSD
    print(f"\nIBL-coupled TSD  (N_OUTER={N_OUTER}, relax={IBL_RELAX})")
    t0 = time.time()

    ibl = IBL(Re=REYNLD, M_inf=EMACH)
    history = ts.run_ibl_coupled(
        ibl=ibl,
        n_outer=N_OUTER,
        ibl_relax=IBL_RELAX,
        x_tr_upper=0.0,
        x_tr_lower=0.0,
        maxit_inner=MAXIT_INNER,
        i_outer_repair=I_OUTER_REPAIR,
        use_te_correction=USE_TE_CORRECTION,
        d_angle_TE=D_ANGLE_TE,
        x_blend_start=BLEND_START,
    )
    
    #* Post-process results

    elapsed_ibl = time.time() - t0
    cl_ibl   = float(ts.data_summary['cl'])
    cd_ibl   = float(ts.data_summary['cd'])
    cd_f_ibl = float(ts.data_summary['ibl_cd_f'])
    print(f"  CL={cl_ibl:.5f}  CD_wave={cd_ibl:.5f}  CD_fric={cd_f_ibl:.5f}"
          f"  CD_total={cd_ibl + cd_f_ibl:.5f}  t={elapsed_ibl:.1f}s")

    cpu_ibl = ts.data_summary['cpu'].copy()
    cpl_ibl = ts.data_summary['cpl'].copy()
    mau_ibl = ts.data_summary['mau'].copy()
    mal_ibl = ts.data_summary['mal'].copy()
    upper   = ts.data_summary['ibl_upper']
    lower   = ts.data_summary['ibl_lower']

    dstar_u      = upper['delta_star_raw'].copy()
    dstar_l      = lower['delta_star_raw'].copy()
    dstar_u_corr = upper['delta_star'].copy()
    dstar_l_corr = lower['delta_star'].copy()

    yu_foil = np.interp(xx_foil, xu, yu)
    yl_foil = np.interp(xx_foil, xl, yl)

    result = {
        'emach': EMACH, 'alpha': ALPHA, 'reynld': REYNLD,
        'xx_foil': xx_foil, 'ile': ile, 'ite': ite,
        'yu_foil': yu_foil, 'yl_foil': yl_foil,
        'cpu_base': cpu_base, 'cpl_base': cpl_base,
        'mau_base': mau_base, 'mal_base': mal_base,
        'cl_base': cl_base, 'cd_base': cd_base, 'cpstar': cpstar,
        'cpu_ibl': cpu_ibl, 'cpl_ibl': cpl_ibl,
        'mau_ibl': mau_ibl, 'mal_ibl': mal_ibl,
        'cl_ibl': cl_ibl, 'cd_ibl': cd_ibl, 'cd_f_ibl': cd_f_ibl,
        'upper': upper, 'lower': lower, 'history': history,
        'dstar_u': dstar_u, 'dstar_l': dstar_l,
        'dstar_u_corr': dstar_u_corr, 'dstar_l_corr': dstar_l_corr,
        'm_u_raw':  _te_surface_slope(xx_foil, yu_foil, dstar_u,      +1),
        'm_l_raw':  _te_surface_slope(xx_foil, yl_foil, dstar_l,      -1),
        'm_u_corr': _te_surface_slope(xx_foil, yu_foil, dstar_u_corr, +1),
        'm_l_corr': _te_surface_slope(xx_foil, yl_foil, dstar_l_corr, -1),
    }

    plot_results(result, fname='rae2822_ibl.png')

    # Summary
    print('\n' + '=' * 60)
    print(f"{'Metric':<16}  {'Baseline':>12}  {'IBL-coupled':>12}")
    print('-' * 60)
    print(f"{'CL':<16}  {cl_base:>12.5f}  {cl_ibl:>12.5f}")
    print(f"{'CD_wave':<16}  {cd_base:>12.5f}  {cd_ibl:>12.5f}")
    print(f"{'CD_fric':<16}  {'N/A':>12}  {cd_f_ibl:>12.5f}")
    print(f"{'CD_total':<16}  {'N/A':>12}  {cd_ibl + cd_f_ibl:>12.5f}")
    print(f"{'t_wall (s)':<16}  {elapsed_base:>12.2f}  {elapsed_ibl:>12.2f}")
    print('=' * 60)
