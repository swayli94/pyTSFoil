"""
Test 9: Full singularity subtraction (Steps A+C+D+E) verification.

Compares four modes per case:
  baseline  -- no LE correction
  composite -- Task 6 composite only
  sing_sub  -- full singularity subtraction (A+C+D+E), no composite
  full      -- singularity subtraction + composite

Metrics:
  nMa0     -- Ma=0 count on lower surface within airfoil
  rmse_cp  -- Cp RMSE vs RANS on full airfoil
  dCL%     -- CL relative change vs baseline
  self     -- Cp/Ma self-consistency RMSE (should be ~0 after composite)

Usage:
  python run_test.py
"""

import os
import sys
import copy
import time
import numpy as np
import matplotlib.pyplot as plt

path      = os.path.abspath(os.path.dirname(__file__))
path_root = os.path.abspath(os.path.join(path, '..'))
sys.path.insert(0, path_root)

from pytsfoil import PyTSFoil
from airfoil_database.utils import load_airfoil_database_from_json
from pytsfoil.leading_edge import compute_surface_corrections

try:
    import tsfoil_fortran as _tsf
except ImportError:
    _tsf = None

fname_db  = os.path.join(path_root, 'airfoil_database', 'airfoil_database.json')
path_figs = os.path.join(path, 'figures')
os.makedirs(path_figs, exist_ok=True)

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

N_CASES = 10
GAMMA   = 1.4


def _cp_isentropic(ma, minf, gamma=GAMMA):
    denom = 2.0 + (gamma - 1.0) * ma ** 2
    numer = 2.0 + (gamma - 1.0) * minf ** 2
    return (2.0 / (gamma * minf ** 2)) * ((numer / denom) ** (gamma / (gamma - 1.0)) - 1.0)


def run_single(airfoil: dict) -> dict:
    try:
        from cst_modeling.section import cst_foil
        from scipy.interpolate import interp1d

        t0 = time.time()
        cst_u = airfoil['cst_u']
        cst_l = airfoil['cst_l']
        tmax  = airfoil['tmax']
        x, yu, yl, _, _ = cst_foil(201, cst_u, cst_l, x=None, t=tmax, tail=0.0)
        coords = np.column_stack((np.concatenate([x[::-1], x[1:]]),
                                   np.concatenate([yu[::-1], yl[1:]])))

        modes = {
            'baseline':  {'apply_singularity_subtraction': False, 'apply_le_correction': False},
            'composite': {'apply_singularity_subtraction': False, 'apply_le_correction': True},
            'sing_sub':  {'apply_singularity_subtraction': True,  'apply_le_correction': False},
            'full':      {'apply_singularity_subtraction': True,  'apply_le_correction': True},
        }
        results = {}
        _ts_cache = {}  # save ts objects to extract diagnostics later

        for mode_name, extra in modes.items():
            cfg = copy.deepcopy(BASE_CFG)
            cfg.update({'ALPHA': airfoil['AoA'], 'EMACH': airfoil['Ma'], 'REYNLD': airfoil['Re']})
            cfg.update(extra)

            ts = PyTSFoil(airfoil_coordinates=coords, work_dir=path)
            ts.set_config(**cfg)
            ts.run()
            _ts_cache[mode_name] = ts

            xx  = ts.mesh['xx']
            cpu = ts.data_summary['cpu']
            cpl = ts.data_summary['cpl']
            mau = ts.data_summary['mau']
            mal = ts.data_summary['mal']
            uu  = ts.data_summary['uu']
            ul  = ts.data_summary['ul']
            vu  = ts.data_summary['vu']
            vl  = ts.data_summary['vl']
            cl  = ts.data_summary.get('cl_le', ts.data_summary['cl'])
            cm  = ts.data_summary.get('cm_le', ts.data_summary['cm'])
            ile = ts.mesh['ile']
            ite = ts.mesh['ite']

            n_ma0 = int(np.sum(mal[ile:ite + 1] == 0.0))

            fcpu = interp1d(xx, cpu, kind='linear', fill_value='extrapolate')
            fcpl = interp1d(xx, cpl, kind='linear', fill_value='extrapolate')
            rmse_cp = float(np.sqrt(0.5 * (
                np.mean((fcpu(airfoil['xu']) - airfoil['cpu'])**2) +
                np.mean((fcpl(airfoil['xl']) - airfoil['cpl'])**2))))

            cp_from_ma = _cp_isentropic(mal[ile:ite + 1], airfoil['Ma'])
            rmse_self  = float(np.sqrt(np.mean((cp_from_ma - cpl[ile:ite + 1])**2)))

            results[mode_name] = {
                'xx': xx, 'cpu': cpu, 'cpl': cpl, 'mau': mau, 'mal': mal,
                'uu': uu, 'ul': ul, 'vu': vu, 'vl': vl,
                'cl': cl, 'cm': cm,
                'n_ma0': n_ma0, 'rmse_cp': rmse_cp, 'rmse_self': rmse_self,
            }

        # ── Collect correction-term diagnostics ───────────────────────────────
        ts_b  = _ts_cache['baseline']
        ts_s  = _ts_cache['sing_sub']
        ile   = ts_b.mesh['ile']
        ite   = ts_b.mesh['ite']
        nfoil = ts_b.mesh['nfoil']
        x_foil = ts_b.mesh['xx_airfoil'][:nfoil]

        # Body slope BC (fxu/fxl) from Fortran state after last run
        fxu = fxl = None
        if _tsf is not None:
            fxu = np.array(_tsf.common_data.fxu[:nfoil], dtype=float)
            fxl = np.array(_tsf.common_data.fxl[:nfoil], dtype=float)

        # Step D (disabled) and Step E corrections via compute_surface_corrections
        R_c    = ts_b.airfoil.get('R_c')
        h      = ts_b.airfoil.get('h_nose')
        cpfact = ts_b._cpfact
        gamma  = ts_b._gamma
        delta  = float(tmax)  # thickness ratio = max thickness / chord
        phi_sy_upper = phi_sx_surface = None
        if R_c is not None and h is not None and R_c > 0:
            phi_sy_upper, phi_sx_surface = compute_surface_corrections(
                x_foil, h, delta, R_c, cpfact, gamma)

        # Step E correction actually stored on ts_s (sing_sub mode)
        phi_sx_applied = (ts_s._phi_sx_surface.copy()
                          if ts_s._phi_sx_surface is not None else None)

        diagnostics = {
            'x_foil':         x_foil,
            'ile':            ile,
            'ite':            ite,
            'fxu':            fxu,
            'fxl':            fxl,
            'phi_sy_upper':   phi_sy_upper,   # Step D (disabled) — x^{-1/2}
            'phi_sx_surface': phi_sx_applied, # Step E applied    — x^{-1/3}
            'phi_sx_formula': phi_sx_surface, # Step E recomputed from formula
        }

        _plot(airfoil, results)
        _plot_corrections(airfoil, results, diagnostics)
        return {
            'success': True,
            'entry_index': airfoil['entry_index'],
            'Ma': airfoil['Ma'], 'AoA': airfoil['AoA'],
            'elapsed': time.time() - t0,
            'results': results,
        }
    except Exception:
        import traceback
        return {'success': False, 'entry_index': airfoil['entry_index'],
                'error': traceback.format_exc()}


def _plot(airfoil, results):
    idx   = airfoil['entry_index']
    Ma    = airfoil['Ma']
    AoA   = airfoil['AoA']
    delta = airfoil['tmax']
    u_crit = -Ma / (2.4 * delta ** (2.0 / 3.0))

    fig, axes = plt.subplots(2, 2, figsize=(14, 10))
    fig.suptitle(f"Case {idx} | Ma={Ma:.2f}, AoA={AoA:.2f} deg, delta={delta:.3f}", fontsize=11)

    colours = {'baseline': 'b', 'composite': 'g', 'sing_sub': 'orange', 'full': 'r'}
    styles  = {'baseline': '--', 'composite': ':', 'sing_sub': '-.', 'full': '-'}
    labels  = {'baseline': 'Baseline', 'composite': 'Composite(T6)',
               'sing_sub': 'SingSub(A+C+D+E)', 'full': 'Full(A+C+D+E+Comp)'}
    mode_order = ('baseline', 'composite', 'sing_sub', 'full')
    xx = results['baseline']['xx']

    ax = axes[0, 0]
    for m in mode_order:
        r = results[m]
        ax.plot(xx, r['cpu'], color=colours[m], ls=styles[m], label=labels[m])
        ax.plot(xx, r['cpl'], color=colours[m], ls=styles[m])
    ax.plot(airfoil['xu'], airfoil['cpu'], 'k^', markevery=5, ms=4, label='RANS upper')
    ax.plot(airfoil['xl'], airfoil['cpl'], 'kv', markevery=5, ms=4, label='RANS lower')
    ax.invert_yaxis()
    ax.set(title='Cp (upper+lower)', xlabel='x/c', ylabel='Cp')
    ax.set_xlim(-0.05, 1.05); ax.legend(fontsize=7); ax.grid()

    ax = axes[0, 1]
    for m in mode_order:
        r = results[m]
        n0 = r['n_ma0']
        ax.plot(xx, r['mau'], color=colours[m], ls=styles[m], label=f'{labels[m]} (n0={n0})')
        ax.plot(xx, r['mal'], color=colours[m], ls=styles[m])
    ax.axhline(1, color='k', lw=0.5, ls='--')
    ax.set(title='Mach (upper+lower)', xlabel='x/c', ylabel='Mach')
    ax.set_xlim(-0.05, 1.05); ax.legend(fontsize=7); ax.grid()

    ax = axes[1, 0]
    for m in mode_order:
        r = results[m]
        ax.plot(xx, r['uu'], color=colours[m], ls=styles[m], label=labels[m])
        ax.plot(xx, r['ul'], color=colours[m], ls=styles[m])
    ax.axhline(u_crit, color='red', ls='--', lw=1.5, label=f'U_crit={u_crit:.3f}')
    ax.set(title='U = dP/dX (both surfaces)', xlabel='x/c', ylabel='U')
    ax.set_xlim(-0.05, 1.05); ax.legend(fontsize=7); ax.grid()

    LE_XLIM = 0.15
    ax = axes[1, 1]
    mal_b   = results['baseline']['mal']
    xx_af   = xx[(xx >= 0) & (xx <= 1)]
    mal_af  = mal_b[(xx >= 0) & (xx <= 1)]
    zero_idx = np.where(mal_af == 0.0)[0]
    x_exit  = float(xx_af[zero_idx[-1]]) if len(zero_idx) > 0 else None
    mask_le = (xx >= -0.01) & (xx <= LE_XLIM)
    for m in mode_order:
        r = results[m]
        ax.plot(xx[mask_le], r['vu'][mask_le], color=colours[m], ls=styles[m], label=labels[m])
        ax.plot(xx[mask_le], r['vl'][mask_le], color=colours[m], ls=styles[m])
    if x_exit is not None:
        ax.axvline(x_exit, color='red', ls=':', lw=1.5, label=f'x_exit(base)={x_exit:.4f}')
    ax.axhline(0, color='gray', lw=0.7, ls='-', alpha=0.4)
    ax.set(title=f'V = dP/dY (upper+lower, x <= {LE_XLIM:.0%})', xlabel='x/c', ylabel='V')
    ax.set_xlim(-0.01, LE_XLIM); ax.legend(fontsize=7); ax.grid()

    fig.tight_layout()
    fig.savefig(os.path.join(path_figs, f'case_{idx:04d}.png'), dpi=100, bbox_inches='tight')
    plt.close(fig)


def _plot_corrections(airfoil, results, diag):
    """Figure 2: visualise each correction term for debugging.

    Subplots:
      [0,0] Body BC: FXU/FXL (body slope dy/dx / delta) + Step D phi_sy (disabled)
      [0,1] Step E phi_sx_surface (velocity restoration x^{-1/3})
            + residual ul_sing_sub - ul_baseline (should ~0 if converged)
      [1,0] Composite Cp correction  Delta_cp = cp_composite - cp_baseline
            (upper & lower; also for full mode)
      [1,1] phi_r,x vs phi_1,x decomposition at LE
            ul_baseline = phi_1,x
            ul_sing_sub - phi_sx = phi_r,x (bounded)
            phi_sx = phi_s,x (singular part restored by Step E)
    """
    idx = airfoil['entry_index']
    Ma  = airfoil['Ma']
    AoA = airfoil['AoA']

    x_foil = diag['x_foil']
    ile    = diag['ile']
    ite    = diag['ite']
    fxu    = diag['fxu']
    fxl    = diag['fxl']
    phi_sy = diag['phi_sy_upper']    # Step D (disabled), x^{-1/2}
    phi_sx = diag['phi_sx_surface']  # Step E (applied),  x^{-1/3}

    xx    = results['baseline']['xx']
    xx_af = xx[ile:ite + 1]          # x-coords of airfoil region in full mesh

    uu_b = results['baseline']['uu'][ile:ite + 1]
    ul_b = results['baseline']['ul'][ile:ite + 1]
    uu_s = results['sing_sub']['uu'][ile:ite + 1]
    ul_s = results['sing_sub']['ul'][ile:ite + 1]
    cpu_b = results['baseline']['cpu']
    cpl_b = results['baseline']['cpl']
    cpu_c = results['composite']['cpu']
    cpl_c = results['composite']['cpl']
    cpu_f = results['full']['cpu']
    cpl_f = results['full']['cpl']

    LE_XLIM = 0.20

    fig, axes = plt.subplots(2, 2, figsize=(14, 9))
    fig.suptitle(
        f"Case {idx} | Ma={Ma:.3f}, AoA={AoA:.2f}° — Correction terms",
        fontsize=11)

    # ── [0,0] Body slope BC + Step D ─────────────────────────────────────────
    ax = axes[0, 0]
    mask_foil = x_foil <= LE_XLIM
    if fxu is not None:
        ax.plot(x_foil[mask_foil], fxu[mask_foil],
                'b-',  lw=1.5, label='FXU (upper slope / δ)')
        ax.plot(x_foil[mask_foil], fxl[mask_foil],
                'r-',  lw=1.5, label='FXL (lower slope / δ)')
    if phi_sy is not None:
        ax.plot(x_foil[mask_foil],  phi_sy[mask_foil],
                'g--', lw=1.5, label='Step D: +φ_s,y (disabled, inner h/δ√x)')
        ax.plot(x_foil[mask_foil], -phi_sy[mask_foil],
                'g--', lw=1.5)
    ax.axhline(0, color='gray', lw=0.5)
    ax.set(title='Body BC: FXU/FXL  (= dy/dx / δ,  x^{-½} singularity)',
           xlabel='x/c', ylabel='slope / δ')
    ax.set_xlim(-0.005, LE_XLIM)
    ax.legend(fontsize=7); ax.grid()

    # ── [0,1] Step E phi_sx + convergence residual ───────────────────────────
    ax = axes[0, 1]
    mask_foil_full = x_foil <= 0.60
    if phi_sx is not None:
        ax.plot(x_foil[mask_foil_full], phi_sx[mask_foil_full],
                'orange', lw=2, label='Step E: φ_s,x  (= –cp_common/2cpfact,  x^{-⅓})')
        ax.plot(x_foil[mask_foil_full], diag['phi_sx_formula'][mask_foil_full],
                'brown', lw=1, ls=':', label='Step E formula (recomputed)')

    # Residual: how much ul changed sing_sub vs baseline (should ≈ 0)
    # Align x_foil with xx_af (both should cover same airfoil x-range)
    res_uu = uu_s - uu_b  # upper residual Δ(U_sing_sub - U_baseline)
    res_ul = ul_s - ul_b  # lower residual
    ax.plot(xx_af[xx_af <= 0.60], res_uu[xx_af <= 0.60],
            'b--', lw=1, alpha=0.8, label='ΔUU = uu_sing − uu_base (residual ≈ 0)')
    ax.plot(xx_af[xx_af <= 0.60], res_ul[xx_af <= 0.60],
            'r--', lw=1, alpha=0.8, label='ΔUL = ul_sing − ul_base')
    ax.axhline(0, color='gray', lw=0.5)
    ax.set(title='Step E correction φ_s,x  and convergence residual',
           xlabel='x/c', ylabel='velocity perturbation')
    ax.set_xlim(-0.005, 0.60)
    ax.legend(fontsize=7); ax.grid()

    # ── [1,0] Composite Δcp ───────────────────────────────────────────────────
    ax = axes[1, 0]
    dcp_u_c = cpu_c - cpu_b
    dcp_l_c = cpl_c - cpl_b
    dcp_u_f = cpu_f - cpu_b
    dcp_l_f = cpl_f - cpl_b
    mask_xx = (xx >= -0.005) & (xx <= 0.35)
    ax.plot(xx[mask_xx], dcp_u_c[mask_xx], 'b-',  lw=1.5, label='ΔCp_upper  composite')
    ax.plot(xx[mask_xx], dcp_l_c[mask_xx], 'r-',  lw=1.5, label='ΔCp_lower  composite')
    ax.plot(xx[mask_xx], dcp_u_f[mask_xx], 'b--', lw=1.0, label='ΔCp_upper  full')
    ax.plot(xx[mask_xx], dcp_l_f[mask_xx], 'r--', lw=1.0, label='ΔCp_lower  full')
    ax.axhline(0, color='gray', lw=0.5)
    ax.set(title='Composite correction ΔCp = Cp_mode − Cp_baseline',
           xlabel='x/c', ylabel='ΔCp')
    ax.set_xlim(-0.005, 0.35)
    ax.legend(fontsize=7); ax.grid()

    # ── [1,1] Decomposition φ_1,x = φ_r,x + φ_s,x  (LE zoom) ───────────────
    ax = axes[1, 1]
    mask_le = xx_af <= LE_XLIM
    ax.plot(xx_af[mask_le], ul_b[mask_le],
            'b-', lw=2, label='ul_baseline = φ_1,x  (TSD)')
    ax.plot(xx_af[mask_le], uu_b[mask_le],
            'b--', lw=1, alpha=0.6, label='uu_baseline (upper)')
    if phi_sx is not None:
        # phi_r,x = ul_sing_sub_raw = ul_sing_sub_after_E - phi_sx
        n = min(len(ul_s), len(phi_sx))
        phi_r_lower = ul_s[:n][mask_le[:n]] - phi_sx[:n][mask_le[:n]]
        phi_r_upper = uu_s[:n][mask_le[:n]] - phi_sx[:n][mask_le[:n]]
        ax.plot(xx_af[:n][mask_le[:n]], phi_r_lower,
                'orange', lw=2, ls='-', label='φ_r,x (lower) = ul_sing − φ_s,x  (bounded)')
        ax.plot(xx_af[:n][mask_le[:n]], phi_r_upper,
                'orange', lw=1, ls='--', alpha=0.6, label='φ_r,x (upper)')
        ax.plot(x_foil[mask_foil], phi_sx[mask_foil],
                'g-', lw=1.5, label='φ_s,x = Step E correction  (singular)')
    ax.axhline(0, color='gray', lw=0.5)
    ax.set(title='Decomposition: φ_1,x = φ_r,x (bounded) + φ_s,x (singular)',
           xlabel='x/c', ylabel='x-velocity perturbation U')
    ax.set_xlim(-0.005, LE_XLIM)
    ax.legend(fontsize=7); ax.grid()

    fig.tight_layout()
    fig.savefig(os.path.join(path_figs, f'case_{idx:04d}_corrections.png'),
                dpi=100, bbox_inches='tight')
    plt.close(fig)


def print_summary(all_results):
    print()
    hdr  = f"{'Idx':>4}  {'Ma':>5}  {'AoA':>5}  "
    hdr += f"{'nMa0_B':>6} {'nMa0_C':>6} {'nMa0_S':>6} {'nMa0_F':>6}  "
    hdr += f"{'rmCp_B':>7} {'rmCp_C':>7} {'rmCp_S':>7} {'rmCp_F':>7}  "
    hdr += f"{'dCL_C%':>7} {'dCL_S%':>7} {'dCL_F%':>7}  rmSelf_S rmSelf_F"
    print(hdr)
    print('-' * len(hdr))

    for r in all_results:
        if not r['success']:
            print(f"{r['entry_index']:>4}  FAILED: {r['error'][:80]}")
            continue
        b = r['results']['baseline']
        c = r['results']['composite']
        s = r['results']['sing_sub']
        f = r['results']['full']
        cl_b = max(abs(b['cl']), 1e-6)
        print(f"{r['entry_index']:>4}  {r['Ma']:>5.3f}  {r['AoA']:>5.2f}  "
              f"{b['n_ma0']:>6} {c['n_ma0']:>6} {s['n_ma0']:>6} {f['n_ma0']:>6}  "
              f"{b['rmse_cp']:>7.4f} {c['rmse_cp']:>7.4f} {s['rmse_cp']:>7.4f} {f['rmse_cp']:>7.4f}  "
              f"{100*(c['cl']-b['cl'])/cl_b:>+7.2f} "
              f"{100*(s['cl']-b['cl'])/cl_b:>+7.2f} "
              f"{100*(f['cl']-b['cl'])/cl_b:>+7.2f}  "
              f"{s['rmse_self']:>7.4f} {f['rmse_self']:>7.4f}")


def main():
    print("Loading airfoil database...")
    db = load_airfoil_database_from_json(fname_db)
    entries = list(db.values())[:N_CASES]

    all_results = []
    for i, airfoil in enumerate(entries):
        print(f"  Case {i+1}/{N_CASES}: {airfoil['airfoil_id']} "
              f"Ma={airfoil['Ma']:.3f} AoA={airfoil['AoA']:.2f}  ", end='', flush=True)
        result = run_single(airfoil)
        all_results.append(result)
        if result['success']:
            b = result['results']['baseline']
            s = result['results']['sing_sub']
            f = result['results']['full']
            print(f"nMa0: {b['n_ma0']}->{s['n_ma0']}->{f['n_ma0']}  "
                  f"rmCp: {b['rmse_cp']:.4f}->{f['rmse_cp']:.4f}  "
                  f"t={result['elapsed']:.1f}s")
        else:
            print(f"FAILED")

    print_summary(all_results)
    n_ok = sum(r['success'] for r in all_results)
    print(f"\n{n_ok}/{len(all_results)} cases succeeded. Figures -> {path_figs}/")
    return n_ok == len(all_results)


if __name__ == '__main__':
    ok = main()
    sys.exit(0 if ok else 1)
