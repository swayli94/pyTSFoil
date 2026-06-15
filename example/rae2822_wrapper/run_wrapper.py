"""
RAE2822 transonic analysis using the run_airfoil_analysis wrapper.

This is the recommended starting point for new users.  All solver
parameters have been pre-tuned; you only need to supply the airfoil
geometry and flight conditions.

Procedure
---------
1. Load RAE2822 geometry.
2. Minimal viscous run (IBL-coupled TSD, default settings).
3. Override selected parameters to demonstrate configs usage.
4. Plot: Cp and wall Mach — baseline (inviscid) vs IBL-coupled.
"""

import os
import sys
import numpy as np
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt

path      = os.path.abspath(os.path.dirname(__file__))
path_root = os.path.abspath(os.path.join(path, '..', '..'))
sys.path.insert(0, path_root)

from pytsfoil import run_airfoil_analysis

# ── Airfoil geometry ──────────────────────────────────────────────────────────
# Data layout: TE (upper) → LE → TE (lower), counter-clockwise
dat_file = os.path.join(path, 'rae2822.dat')
coords = np.loadtxt(dat_file, skiprows=1)   # shape (N, 2)

# ── Run 1: minimal viscous analysis (all defaults) ────────────────────────────
print("=" * 60)
print("Run 1: default viscous IBL-coupled analysis")
print("=" * 60)

r = run_airfoil_analysis(
    coords,
    Mach=0.75,
    AoA_degrees=0.5,
    Re=6.5e6,
    # flag_IBL=True   (default: viscous IBL coupling enabled)
    # flag_TEC=True   (default: trailing-edge δ* correction enabled)
    # flag_CFS=True   (default: full-supersonic correction enabled)
)

# Baseline (pure inviscid TSD) is always included for comparison
b = r['baseline']
print(f"\nBaseline (inviscid TSD):")
print(f"  CL={b['cl']:.5f}  CD_wave={b['cd_wave']:.5f}")

print(f"\nIBL-coupled (viscous):")
print(f"  CL={r['cl']:.5f}  CD_wave={r['cd_wave']:.5f}  "
      f"CD_f={r['cd_f']:.5f}  CD_total={r['cd_total']:.5f}")

upper = r['ibl_upper']
lower = r['ibl_lower']
print(f"\nTransition:  x_tr_upper={upper['x_tr']:.3f}  "
      f"x_tr_lower={lower['x_tr']:.3f}")

# ── Run 2: override selected parameters via configs ───────────────────────────
print("\n" + "=" * 60)
print("Run 2: configs overrides (denser mesh + forced transition)")
print("=" * 60)

r2 = run_airfoil_analysis(
    coords,
    Mach=0.75,
    AoA_degrees=0.5,
    Re=6.5e6,
    configs={
        # TSD solver keys
        'n_point_x':       300,   # denser mesh in x
        'n_point_airfoil': 150,
        'flag_print_info': False, # suppress per-iteration console output
        # IBL coupling keys
        'n_outer':     15,        # more outer iterations
        'x_tr_upper':  0.05,      # forced transition at 5% chord (upper)
        'x_tr_lower':  0.10,      # forced transition at 10% chord (lower)
    },
)

print(f"\nFine-mesh + forced transition:")
print(f"  CL={r2['cl']:.5f}  CD_wave={r2['cd_wave']:.5f}  "
      f"CD_f={r2['cd_f']:.5f}  CD_total={r2['cd_total']:.5f}")

# ── Run 3: inviscid only ──────────────────────────────────────────────────────
print("\n" + "=" * 60)
print("Run 3: inviscid TSD only (flag_IBL=False)")
print("=" * 60)

r3 = run_airfoil_analysis(
    coords,
    Mach=0.75,
    AoA_degrees=0.5,
    Re=6.5e6,
    flag_IBL=False,
    configs={'flag_print_info': False},
)

print(f"\nInviscid:  CL={r3['cl']:.5f}  CD_wave={r3['cd_wave']:.5f}")

# ── Plot ──────────────────────────────────────────────────────────────────────
ile, ite = r['ile'], r['ite']
xx  = r['xx_foil']

fig, axes = plt.subplots(1, 2, figsize=(14, 5))
fig.suptitle(
    f"RAE2822   Ma=0.75   AoA=0.5°   Re=6.5e6\n"
    f"Baseline: CL={b['cl']:.4f}  CD_wave={b['cd_wave']:.5f}   "
    f"IBL: CL={r['cl']:.4f}  CD_wave={r['cd_wave']:.5f}  "
    f"CD_f={r['cd_f']:.5f}  CD_total={r['cd_total']:.5f}",
    fontsize=10, fontweight='bold',
)

C_BASE = '#2ca02c'
C_IBL  = '#1f77b4'

# Cp
ax = axes[0]
ax.plot(xx, b['cpu'][ile:ite+1],  color=C_BASE, lw=1.8, ls='-',
        label=f"Baseline upper  (CL={b['cl']:.4f})")
ax.plot(xx, b['cpl'][ile:ite+1],  color=C_BASE, lw=1.8, ls='--',
        label='Baseline lower')
ax.plot(xx, r['cpu'][ile:ite+1],  color=C_IBL,  lw=1.8, ls='-',
        label=f"IBL-coupled upper  (CL={r['cl']:.4f})")
ax.plot(xx, r['cpl'][ile:ite+1],  color=C_IBL,  lw=1.8, ls='--',
        label='IBL-coupled lower')
ax.axhline(r['cpstar'], color='k', lw=0.9, ls=':', alpha=0.7,
           label=f"Cp* = {r['cpstar']:.3f}")
ax.invert_yaxis()
ax.set_xlim(-0.02, 1.02)
ax.set_xlabel('x/c', fontsize=11)
ax.set_ylabel('$C_p$', fontsize=11)
ax.set_title('Pressure coefficient $C_p$', fontsize=11)
ax.legend(fontsize=8, ncol=2, loc='lower right')
ax.grid(True, alpha=0.3)

# Mach
ax = axes[1]
ax.plot(xx, b['mau'][ile:ite+1],  color=C_BASE, lw=1.8, ls='-',  label='Baseline upper')
ax.plot(xx, b['mal'][ile:ite+1],  color=C_BASE, lw=1.8, ls='--', label='Baseline lower')
ax.plot(xx, r['mau'][ile:ite+1],  color=C_IBL,  lw=1.8, ls='-',  label='IBL-coupled upper')
ax.plot(xx, r['mal'][ile:ite+1],  color=C_IBL,  lw=1.8, ls='--', label='IBL-coupled lower')
ax.axhline(1.0,       color='k', lw=0.9, ls=':', alpha=0.3, label='Sonic (M=1)')
ax.axhline(0.75,      color='k', lw=0.9, ls=':', alpha=0.3, label='$M_\\infty$=0.75')
ax.axvline(upper['x_tr'], color=C_IBL, lw=1.0, ls=':', alpha=0.7,
           label=f"$x_{{tr}}$ upper={upper['x_tr']:.3f}")
ax.axvline(lower['x_tr'], color='#d62728', lw=1.0, ls=':', alpha=0.7,
           label=f"$x_{{tr}}$ lower={lower['x_tr']:.3f}")
ax.set_xlim(-0.02, 1.02)
ax.set_ylim(-0.05, None)
ax.set_xlabel('x/c', fontsize=11)
ax.set_ylabel('Mach number', fontsize=11)
ax.set_title('Wall Mach number', fontsize=11)
ax.legend(fontsize=8, ncol=2)
ax.grid(True, alpha=0.3)

plt.tight_layout()
out_png = os.path.join(path, 'wrapper_results.png')
plt.savefig(out_png, dpi=150, bbox_inches='tight')
plt.close(fig)
print(f"\nPlot saved to: {out_png}")
