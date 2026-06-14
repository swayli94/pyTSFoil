"""
Test 3: Mode B recovery for large local Mach number (Ma_local > 1.2).

Background
----------
When the freestream Mach number or angle-of-attack is high enough that the local
Mach number exceeds ~1.2, the Murman-Cole SOR solver can diverge: the entire upper
surface becomes supersonic with no shock, causing ERROR to rise monotonically until
DVERGE is reached (failure mode B).

Mode B recovery (方向 B)
------------------------
Three cooperating mechanisms are activated automatically when K_MODEB consecutive
iterations of monotonically rising ERROR are detected:

  Mechanism 1 — Sonic-speed penalty (SYOR):
      For columns I = ITE-2 … ITE, add a spring term pulling dP/dx → SONVEL.
      This "pins" a sonic point at the trailing edge, restoring ellipticity and
      providing a downstream constraint for the Murman-Cole scheme.

  Mechanism 2 — Enhanced dissipation (SYOR):
      For columns I = ITE-4 … ITE, amplify EPS by EPS_AMPL.
      Damps the growing oscillations in the supersonic bubble.

  Mechanism 3 — Circulation freeze (SOLVE/RECIRC):
      Set WCIRC = 0.02 while Mode B is active, suppressing CIRCTE oscillations
      that further destabilise the trailing-edge boundary condition.

Test procedure per case
-----------------------
1. Run inviscid TSD with Mode B DISABLED (K_MODEB = 999999) — baseline reference.
   Cases that physically require Ma_local > 1.2 will diverge here.

2. Run inviscid TSD with Mode B ENABLED (default K_MODEB = 5) — Mode B recovery.

3. Plot (3 × 2 grid):
   [0,0] Cp          — no-ModeB / ModeB / RANS
   [0,1] Mach        — no-ModeB / ModeB / RANS
   [1,0] Convergence history (dummy: diverged vs converged)
   [1,1] Cp detail near shock region
   [2,0] Solver status table
   [2,1] CL / CD comparison
"""

import os
import sys
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

from pytsfoil import PyTSFoil
from airfoil_database.utils import load_airfoil_database_from_json

# ── paths ─────────────────────────────────────────────────────────────────────

fname_db  = os.path.join(path_root, 'airfoil_database', 'airfoil_database.json')
path_figs = os.path.join(path, 'figures')
os.makedirs(path_figs, exist_ok=True)

# ── parameters ────────────────────────────────────────────────────────────────

LIST_CASES = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
N_PROCESS  = 10

BASE_CFG = {
    'CVERGE':  1e-5,
    'DVERGE':  10.0,
    'EPS':     0.5,
    'IPRTER':  500,
    'MAXIT':   9999,
    'RIGF':    0.2,
    'SIMDEF':  3,
    'WCIRC':   1.0,
    'WE':      [1.8, 1.9, 1.95],
    'NWDGE':   2,
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
    # Correction of Full-Supersonic (CFS) parameters
    'flag_CFS':   True,
    'BETA_SONIC': 100.0,
    'EPS_AMPL':   500.0,
    'ITER_START_CFS': 100,
}

C_NO_MB = '#d62728'   # red   – no Mode B (may diverge)
C_MB    = '#1f77b4'   # blue  – Mode B active
C_RANS  = 'k'         # black – RANS reference

TE_X_LO, TE_X_HI = 0.98, 1.02  # trailing-edge check window


def _te_supersonic(xx: np.ndarray, mau: np.ndarray) -> bool:
    """Return True if any upper-surface point in x=[0.98, 1.02] has Ma > 1.0."""
    mask = (xx >= TE_X_LO) & (xx <= TE_X_HI)
    if not np.any(mask):
        return False
    return bool(np.any(mau[mask] > 1.0))


# ── worker ────────────────────────────────────────────────────────────────────

def run_single(airfoil: dict) -> dict:
    """Worker: run TSD without/with Mode B for one airfoil."""
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

        # ── Run 1: Mode B DISABLED (K_MODEB = 999999) ─────────────────────────
        cfg_no_mb = {**cfg, 'K_MODEB': 999999}
        t1 = time.time()
        ts_no_mb = PyTSFoil(airfoil_coordinates=airfoil_coords, work_dir=path)
        ts_no_mb.set_config(**cfg_no_mb)
        ts_no_mb.run()
        elapsed_no_mb = time.time() - t1

        xx_full = ts_no_mb.mesh['xx'].copy()
        ile     = ts_no_mb.mesh['ile']
        ite     = ts_no_mb.mesh['ite']
        xx_foil = xx_full[ile:ite + 1]

        cpu_no_mb = ts_no_mb.data_summary['cpu'].copy()
        cpl_no_mb = ts_no_mb.data_summary['cpl'].copy()
        mau_no_mb = ts_no_mb.data_summary['mau'].copy()
        mal_no_mb = ts_no_mb.data_summary['mal'].copy()
        cl_no_mb  = float(ts_no_mb.data_summary['cl'])
        cd_no_mb  = float(ts_no_mb.data_summary['cd'])
        modeb_no_mb    = ts_no_mb.data_summary.get('modeb', False)
        te_super_no_mb = _te_supersonic(xx_full, mau_no_mb)

        # ── Run 2: Mode B ENABLED (default K_MODEB = 5) ───────────────────────
        t2 = time.time()
        ts_mb = PyTSFoil(airfoil_coordinates=airfoil_coords, work_dir=path)
        ts_mb.set_config(**cfg)
        ts_mb.run()
        elapsed_mb = time.time() - t2

        cpu_mb = ts_mb.data_summary['cpu'].copy()
        cpl_mb = ts_mb.data_summary['cpl'].copy()
        mau_mb = ts_mb.data_summary['mau'].copy()
        mal_mb = ts_mb.data_summary['mal'].copy()
        cl_mb  = float(ts_mb.data_summary['cl'])
        cd_mb  = float(ts_mb.data_summary['cd'])
        modeb_mb    = ts_mb.data_summary.get('modeb', False)
        te_super_mb = _te_supersonic(xx_full, mau_mb)

        # ── RMSE Cp vs RANS ───────────────────────────────────────────────────
        cpu_rans = np.array(airfoil['cpu'])
        cpl_rans = np.array(airfoil['cpl'])

        f_cpu_no_mb = interp1d(xx_full, cpu_no_mb, kind='linear', fill_value='extrapolate')
        f_cpl_no_mb = interp1d(xx_full, cpl_no_mb, kind='linear', fill_value='extrapolate')
        f_cpu_mb    = interp1d(xx_full, cpu_mb,    kind='linear', fill_value='extrapolate')
        f_cpl_mb    = interp1d(xx_full, cpl_mb,    kind='linear', fill_value='extrapolate')

        def rmse(fu, fl):
            return float(np.sqrt(0.5 * (
                np.mean((fu(xu) - cpu_rans) ** 2) +
                np.mean((fl(xl) - cpl_rans) ** 2)
            )))

        rmse_no_mb = rmse(f_cpu_no_mb, f_cpl_no_mb)
        rmse_mb    = rmse(f_cpu_mb,    f_cpl_mb)

        result = {
            'success':     True,
            'entry_index': airfoil['entry_index'],
            'airfoil_id':  airfoil['airfoil_id'],
            'Ma': EMACH, 'AoA': ALPHA, 'Re': REYNLD,
            'elapsed': time.time() - t0,
            'elapsed_no_mb': elapsed_no_mb,
            'elapsed_mb':    elapsed_mb,
            'xx_full': xx_full, 'ile': ile, 'ite': ite, 'xx_foil': xx_foil,
            'xu': xu, 'xl': xl,
            'cpu_rans': cpu_rans, 'cpl_rans': cpl_rans,
            'mwu_rans': np.array(airfoil['mwu']),
            'mwl_rans': np.array(airfoil['mwl']),
            'cl_rans': float(airfoil['CL_RANS']),
            'cd_rans': float(airfoil['Cd_RANS']),
            'cpu_no_mb': cpu_no_mb, 'cpl_no_mb': cpl_no_mb,
            'mau_no_mb': mau_no_mb, 'mal_no_mb': mal_no_mb,
            'cl_no_mb': cl_no_mb, 'cd_no_mb': cd_no_mb,
            'modeb_no_mb': modeb_no_mb, 'te_super_no_mb': te_super_no_mb,
            'cpu_mb': cpu_mb, 'cpl_mb': cpl_mb,
            'mau_mb': mau_mb, 'mal_mb': mal_mb,
            'cl_mb': cl_mb, 'cd_mb': cd_mb,
            'modeb_mb': modeb_mb, 'te_super_mb': te_super_mb,
            'rmse_no_mb': rmse_no_mb,
            'rmse_mb':    rmse_mb,
            'cpstar': ts_mb.data_summary.get('cpstar'),
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

    xx_foil = r['xx_foil']
    ile, ite = r['ile'], r['ite']
    xx_full  = r['xx_full']
    xu, xl   = r['xu'], r['xl']

    status_no_mb = 'TE_SUPER' if r['te_super_no_mb'] else ('ModeB!' if r['modeb_no_mb'] else 'OK')
    status_mb    = 'TE_SUPER' if r['te_super_mb']    else ('ModeB'  if r['modeb_mb']    else 'OK')

    fig = plt.figure(figsize=(16, 12))
    fig.suptitle(
        f"Case {idx} | airfoil id={r['airfoil_id']}  "
        f"$M_\\infty={Ma:.3f}$,  $\\alpha={AoA:.2f}°$\n"
        f"No-ModeB: {status_no_mb}  RMSE={r['rmse_no_mb']:.4f}  CL={r['cl_no_mb']:.4f}  |  "
        f"ModeB: {status_mb}  RMSE={r['rmse_mb']:.4f}  CL={r['cl_mb']:.4f}  |  "
        f"RANS: CL={r['cl_rans']:.4f}",
        fontsize=11, fontweight='bold',
    )
    gs = fig.add_gridspec(2, 2, hspace=0.38, wspace=0.30,
                          left=0.08, right=0.97, top=0.89, bottom=0.06)

    ax_cp   = fig.add_subplot(gs[0, 0])
    ax_mach = fig.add_subplot(gs[0, 1])
    ax_cl   = fig.add_subplot(gs[1, 0])
    ax_cd   = fig.add_subplot(gs[1, 1])

    # ── [0,0] Cp distribution ─────────────────────────────────────────────────
    ax = ax_cp
    ax.plot(xu, r['cpu_rans'], color=C_RANS, lw=5.0, ls='-',  alpha=0.5, label='RANS upper')
    ax.plot(xl, r['cpl_rans'], color=C_RANS, lw=5.0, ls='--', alpha=0.5, label='RANS lower')
    ax.plot(xx_foil, r['cpu_no_mb'][ile:ite+1], color=C_NO_MB, lw=1.8, ls='-',
            label=f'No ModeB upper  ({status_no_mb})')
    ax.plot(xx_foil, r['cpl_no_mb'][ile:ite+1], color=C_NO_MB, lw=1.8, ls='--',
            label='No ModeB lower')
    ax.plot(xx_foil, r['cpu_mb'][ile:ite+1],    color=C_MB,    lw=1.8, ls='-',
            label=f'ModeB upper  ({status_mb})')
    ax.plot(xx_foil, r['cpl_mb'][ile:ite+1],    color=C_MB,    lw=1.8, ls='--',
            label='ModeB lower')
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

    # ── [0,1] Mach distribution ───────────────────────────────────────────────
    ax = ax_mach
    ax.plot(xu, r['mwu_rans'], color=C_RANS, lw=5.0, ls='-',  alpha=0.5, label='RANS upper')
    ax.plot(xl, r['mwl_rans'], color=C_RANS, lw=5.0, ls='--', alpha=0.5, label='RANS lower')
    ax.plot(xx_foil, r['mau_no_mb'][ile:ite+1], color=C_NO_MB, lw=1.8, ls='-',
            label=f'No ModeB upper  ({status_no_mb})')
    ax.plot(xx_foil, r['mal_no_mb'][ile:ite+1], color=C_NO_MB, lw=1.8, ls='--',
            label='No ModeB lower')
    ax.plot(xx_foil, r['mau_mb'][ile:ite+1],    color=C_MB,    lw=1.8, ls='-',
            label=f'ModeB upper  ({status_mb})')
    ax.plot(xx_foil, r['mal_mb'][ile:ite+1],    color=C_MB,    lw=1.8, ls='--',
            label='ModeB lower')
    ax.axhline(1.0, color='k', lw=0.9, ls=':', alpha=0.2, label='Sonic ($M=1$)')
    ax.axhline(Ma, color='k', lw=0.9, ls=':', alpha=0.2, label='$M_\\infty$')
    ax.set_xlim(-0.02, 1.02)
    ax.set_ylim(-0.05, None)
    ax.set_xlabel('$x/c$', fontsize=11)
    ax.set_ylabel('Mach number', fontsize=11)
    ax.set_title('Wall Mach number', fontsize=11)
    ax.legend(fontsize=8, ncol=2)
    ax.grid(True, alpha=0.3)

    # ── [1,0] CL comparison bar chart ─────────────────────────────────────────
    ax = ax_cl
    bars = ax.bar(['No ModeB', 'ModeB', 'RANS'],
                  [r['cl_no_mb'], r['cl_mb'], r['cl_rans']],
                  color=[C_NO_MB, C_MB, C_RANS], alpha=0.8, width=0.5)
    ax.bar_label(bars, fmt='%.4f', padding=3, fontsize=9)
    ax.set_ylabel('$C_L$', fontsize=11)
    ax.set_title('Lift coefficient comparison', fontsize=11)
    ax.grid(True, alpha=0.3, axis='y')

    # ── [1,1] RMSE Cp comparison ──────────────────────────────────────────────
    ax = ax_cd
    bars2 = ax.bar(['No ModeB', 'ModeB'],
                   [r['rmse_no_mb'], r['rmse_mb']],
                   color=[C_NO_MB, C_MB], alpha=0.8, width=0.5)
    ax.bar_label(bars2, fmt='%.4f', padding=3, fontsize=9)
    ax.set_ylabel('RMSE $C_p$ vs RANS', fontsize=11)
    ax.set_title('Cp accuracy vs RANS', fontsize=11)
    ax.grid(True, alpha=0.3, axis='y')

    fig.savefig(os.path.join(path_figs, f'case_{idx:04d}_modeb.png'),
                dpi=120, bbox_inches='tight')
    plt.close(fig)


# ── summary ───────────────────────────────────────────────────────────────────

def print_summary(all_results, fname):
    with open(fname, 'w') as f:
        f.write('\n' + '=' * 130 + '\n')
        f.write(f'{"idx":>4}  {"Ma":>5}  {"AoA":>5}  {"Re":>8}  '
                f'{"metric":<14}  {"No ModeB":>12}  {"ModeB":>12}  {"RANS":>10}\n')
        f.write('-' * 130 + '\n')

        for r in all_results:
            if not r['success']:
                f.write(f"  {r['entry_index']:>3}  FAILED: {str(r['error'])[:70]}\n\n")
                continue

            idx = r['entry_index']
            Ma  = r['Ma']
            AoA = r['AoA']
            Re  = r['Re']

            status_no_mb = 'TE_SUPER' if r['te_super_no_mb'] else ('ModeB' if r['modeb_no_mb'] else 'OK')
            status_mb    = 'TE_SUPER' if r['te_super_mb']    else ('ModeB' if r['modeb_mb']    else 'OK')

            rows = [
                ('Status',    f'{status_no_mb:>12}',          f'{status_mb:>12}',        f'{"N/A":>10}'),
                ('RMSE_Cp',   f'{r["rmse_no_mb"]:>12.4f}',   f'{r["rmse_mb"]:>12.4f}',  f'{"N/A":>10}'),
                ('CL',        f'{r["cl_no_mb"]:>12.5f}',     f'{r["cl_mb"]:>12.5f}',    f'{r["cl_rans"]:>10.5f}'),
                ('CD_wave',   f'{r["cd_no_mb"]:>12.5f}',     f'{r["cd_mb"]:>12.5f}',    f'{"N/A":>10}'),
                ('CD_RANS',   f'{"N/A":>12}',                 f'{"N/A":>12}',             f'{r["cd_rans"]:>10.5f}'),
                ('CPU_s',     f'{r["elapsed_no_mb"]:>12.2f}', f'{r["elapsed_mb"]:>12.2f}', f'{"N/A":>10}'),
            ]

            first = True
            for metric, v_no_mb, v_mb, v_rans in rows:
                prefix = f" {idx:>3}  {Ma:>5.3f}  {AoA:>5.2f}  {Re:>8.1e}" if first else ' ' * 28
                first = False
                f.write(f"{prefix}  {metric:<14}  {v_no_mb}  {v_mb}  {v_rans}\n")
            f.write('\n')

        f.write('=' * 130 + '\n')


# ── main ──────────────────────────────────────────────────────────────────────

if __name__ == '__main__':

    db    = load_airfoil_database_from_json(fname_db)
    cases = list(db.values())
    cases = [cases[i] for i in LIST_CASES]

    print(f"Running {len(LIST_CASES)} cases with N_PROCESS={N_PROCESS} ...")
    print(f"  Testing Mode B recovery vs no-ModeB baseline")
    print(f"  Base K_MODEB=5, BETA_SONIC=500, EPS_AMPL=200")

    with mp.Pool(N_PROCESS) as pool:
        all_results = pool.map(run_single, cases)

    n_ok   = sum(1 for r in all_results if r['success'])
    n_fail = len(all_results) - n_ok
    print(f"Done: {n_ok}/{len(all_results)} succeeded, {n_fail} failed.")

    # Count Mode B triggers and TE-supersonic cases
    n_modeb        = sum(1 for r in all_results if r.get('success') and r.get('modeb_mb'))
    n_te_super_no_mb = sum(1 for r in all_results if r.get('success') and r.get('te_super_no_mb'))
    n_te_super_mb    = sum(1 for r in all_results if r.get('success') and r.get('te_super_mb'))
    print(f"  Mode B triggered: {n_modeb}/{n_ok} cases")
    print(f"  TE supersonic (x=[0.98,1.02]) without Mode B: {n_te_super_no_mb}/{n_ok} cases")
    print(f"  TE supersonic (x=[0.98,1.02]) with Mode B:    {n_te_super_mb}/{n_ok} cases")

    fname = os.path.join(path_figs, 'summary.txt')
    print_summary(all_results, fname)
    print(f"Summary written to {fname}")
