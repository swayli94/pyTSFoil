'''
Wrapper for the easy-to-use interface of the pytsfoil package.
'''

import os
import shutil
import tempfile
from typing import Dict, Any
import numpy as np


# Keys routed to run_ibl_coupled() rather than set_config()
_IBL_PARAM_KEYS = frozenset({
    'n_outer', 'x_tr_upper', 'x_tr_lower', 'coupling_relax_final',
    'mach_smooth_sigma', 'slope_smooth_sigma', 'delta_star_max',
    'slope_correction_max', 'maxit_inner', 'i_outer_repair',
    'te_relax', 'x_blend_start',
})

# Keys handled outside set_config()
_DIR_KEYS = frozenset({'work_dir', 'output_dir'})


def run_airfoil_analysis(airfoil_coordinates: np.ndarray,
            Mach: float, AoA_degrees: float, Re: float,
            flag_IBL: bool = True,
            flag_TEC: bool = True,
            flag_CFS: bool = True,
            configs: Dict[str, Any] = {}
            ) -> Dict[str, Any]:
    '''
    Run a transonic airfoil analysis with recommended default settings.

    Two analysis modes are available:

    - **Inviscid** (`flag_IBL=False`): Transonic Small Disturbance (TSD)
      equation solved on a structured mesh.  Returns wave drag, CL, CM, and
      surface Cp/Mach distributions.

    - **Viscous** (`flag_IBL=True`, default): TSD coupled with an Integral
      Boundary Layer (IBL) method.  Adds friction drag, displacement thickness,
      and transition location on top of the inviscid result.

    All solver parameters have been pre-tuned for typical transonic cases
    (0.6 ≤ Mach ≤ 0.9, -4° ≤ AoA ≤ 4°).  Use `configs` to override any
    individual parameter without touching the rest.

    Terms
    -----
    - TSD: Transonic Small Disturbance solver
    - IBL: Integral Boundary Layer method (Thwaites → Michel → Head)
    - TEC: Trailing Edge δ* Correction (blends IBL blow-up near TE)
    - CFS: Correction of Full-Supersonic flow (stabilises converge for
      shocks, recommended when IBL is active)

    Parameters
    ----------
    airfoil_coordinates : np.ndarray, shape (N, 2)
        X and Y coordinates of the airfoil.  Data must start from the
        trailing edge on the upper surface and proceed counter-clockwise
        (upper TE → LE → lower TE).
    Mach : float
        Free-stream Mach number (0.5-0.85 recommended).
    AoA_degrees : float
        Angle of attack in degrees (-9 to 9).
    Re : float
        Chord-based Reynolds number (e.g. 6.5e6).
    flag_IBL : bool, optional
        Enable viscous TSD-IBL coupling (default True).
    flag_TEC : bool, optional
        Enable trailing-edge δ* correction inside IBL (default True).
        Only used when ``flag_IBL=True``.
    flag_CFS : bool, optional
        Enable Correction of Full-Supersonic flow (default True).
        Improves convergence for cases with strong shocks.
    configs : dict, optional
        Override any default parameter.  Three categories are accepted:

        **TSD solver keys** (passed to ``PyTSFoil.set_config``):
          ``MAXIT``, ``CVERGE``, ``EPS``, ``RIGF``, ``NWDGE``,
          ``n_point_x``, ``n_point_y``, ``n_point_airfoil``,
          ``flag_output``, ``flag_output_summary``, ``flag_output_shock``,
          ``flag_output_field``, ``flag_print_info``, …

        **IBL coupling keys** (passed to ``run_ibl_coupled``):
          ``n_outer``, ``x_tr_upper``, ``x_tr_lower``,
          ``coupling_relax_final``, ``maxit_inner``, ``i_outer_repair``,
          ``te_relax``, ``x_blend_start``, ``mach_smooth_sigma``,
          ``slope_smooth_sigma``, ``delta_star_max``, ``slope_correction_max``

        **Directory keys**:
          ``work_dir``   - working directory for Fortran solver I/O
          ``output_dir`` - directory for Python output files (cpxs.dat, etc.)

    Returns
    -------
    results : dict
        ============  ======================================================
        Key           Description
        ============  ======================================================
        ``cl``        Lift coefficient
        ``cm``        Pitching moment coefficient (about quarter-chord)
        ``cd_wave``   Wave drag coefficient (momentum integral method)
        ``cd_f``      Friction drag coefficient (IBL); 0.0 if inviscid
        ``cd_total``  Total drag = cd_wave + cd_f
        ``xx``        x-coordinates of the full computational mesh
        ``xx_foil``   x-coordinates over the airfoil chord [0, 1]
        ``ile``       Leading-edge index in ``xx``
        ``ite``       Trailing-edge index in ``xx`` (inclusive)
        ``cpu``       Upper-surface Cp at each point in ``xx``
        ``cpl``       Lower-surface Cp at each point in ``xx``
        ``mau``       Upper-surface Mach number at each point in ``xx``
        ``mal``       Lower-surface Mach number at each point in ``xx``
        ``cpstar``    Critical pressure coefficient (sonic condition)
        ``history``   List of per-iteration dicts from IBL outer loop
                      (empty list when ``flag_IBL=False``)
        ``ibl_upper`` Final IBL result dict for upper surface
                      (``None`` when ``flag_IBL=False``)
        ``ibl_lower`` Final IBL result dict for lower surface
                      (``None`` when ``flag_IBL=False``)
        ``solver``    Underlying :class:`PyTSFoil` instance (advanced use)
        ============  ======================================================

    Examples
    --------
    Minimal viscous run::

        import numpy as np
        from pytsfoil import run_airfoil_analysis

        coords = np.loadtxt('rae2822.dat', skiprows=1)
        r = run_airfoil_analysis(coords, Mach=0.75, AoA_degrees=0.5, Re=6.5e6)
        print(f"CL={r['cl']:.4f}  CD_total={r['cd_total']:.5f}")

    Inviscid run with denser mesh::

        r = run_airfoil_analysis(
            coords, Mach=0.75, AoA_degrees=0.5, Re=6.5e6,
            flag_IBL=False,
            configs={'n_point_x': 300, 'n_point_airfoil': 150},
        )

    Viscous run with forced transition and file output::

        r = run_airfoil_analysis(
            coords, Mach=0.75, AoA_degrees=0.5, Re=6.5e6,
            configs={
                'x_tr_upper': 0.1,
                'x_tr_lower': 0.2,
                'work_dir':   '/tmp/my_run',
                'flag_output_shock': True,
            },
        )
    '''
    from pytsfoil.pytsfoil import PyTSFoil
    from pytsfoil.ibl import IBL

    # --- Partition configs ---
    work_dir   = configs.get('work_dir',   None)
    output_dir = configs.get('output_dir', None)

    tsd_overrides: Dict[str, Any] = {}
    ibl_overrides: Dict[str, Any] = {}
    for key, val in configs.items():
        if key in _DIR_KEYS:
            continue
        elif key in _IBL_PARAM_KEYS:
            ibl_overrides[key] = val
        else:
            tsd_overrides[key] = val

    # --- TSD recommended defaults ---
    tsd_cfg: Dict[str, Any] = {
        'EMACH':  Mach,
        'ALPHA':  AoA_degrees,
        'REYNLD': Re,
        'MAXIT':  9999,
        'CVERGE': 1e-5,
        'DVERGE': 10.0,
        'EPS':    0.5,
        'RIGF':   0.2 if flag_IBL else 0.0,
        # NWDGE is set to 2 when using IBL (providing more damping near shocks)
        'NWDGE':  2 if flag_IBL else 0,
        'WCIRC':  1.0,
        'SIMDEF': 3,
        'WE':     [1.8, 1.9, 1.95],
        'n_point_x':       200,
        'n_point_y':        80,
        'n_point_airfoil': 100,
        'flag_CFS':            flag_CFS,
        'flag_output':         False,
        'flag_output_summary': False,
        'flag_output_shock':   False,
        'flag_output_field':   False,
        'flag_print_info':     True,
    }
    tsd_cfg.update(tsd_overrides)

    # --- IBL coupling recommended defaults ---
    ibl_cfg: Dict[str, Any] = {
        'n_outer':              10,
        'x_tr_upper':           None,   # Michel criterion (auto-detect)
        'x_tr_lower':           None,
        'coupling_relax_final': 0.1,
        'mach_smooth_sigma':    2.0,
        'slope_smooth_sigma':   3.0,
        'delta_star_max':       0.05,
        'slope_correction_max': 0.1,
        'maxit_inner':          200,
        'i_outer_repair':       3,
        'te_relax':             0.5,
        'x_blend_start':        0.9,
    }
    ibl_cfg.update(ibl_overrides)

    # --- Work directory setup ---
    _cwd_saved = os.getcwd()
    _tmpdir: str | None = None
    if work_dir is None:
        _tmpdir = tempfile.mkdtemp(prefix='pytsfoil_')
        work_dir = _tmpdir

    # --- Run analysis ---
    try:
        ts = PyTSFoil(
            airfoil_coordinates=airfoil_coordinates,
            work_dir=work_dir,
            output_dir=output_dir or work_dir,
        )
        ts.set_config(**tsd_cfg)

        # Always run a full inviscid TSD baseline first (same pattern as the
        # example scripts).  This gives a clean reference solution and lets
        # run_ibl_coupled start from a known state.
        ts.run()

        baseline: Dict[str, Any] = {
            'cl':      float(ts.data_summary['cl']),
            'cm':      float(ts.data_summary['cm']),
            'cd_wave': float(ts.data_summary.get('cd', 0.0)),
            'cpu':     ts.data_summary['cpu'].copy(),
            'cpl':     ts.data_summary['cpl'].copy(),
            'mau':     ts.data_summary['mau'].copy(),
            'mal':     ts.data_summary['mal'].copy(),
            'cpstar':  float(ts.data_summary.get('cpstar', 0.0)),
        }

        if tsd_cfg.get('flag_print_info', True):
            print(f"\n[pyTSFoil] Inviscid baseline  "
                  f"Ma={Mach:.3f}  AoA={AoA_degrees:.2f}°  Re={Re:.2e}")
            print(f"  CL={baseline['cl']:.5f}  "
                  f"CM={baseline['cm']:.5f}  "
                  f"CD_wave={baseline['cd_wave']:.5f}")

        if flag_IBL:
            ibl = IBL(Re=Re, M_inf=Mach)
            history = ts.run_ibl_coupled(
                ibl=ibl,
                use_te_correction=flag_TEC,
                **ibl_cfg,
            )
        else:
            history = []

    finally:
        os.chdir(_cwd_saved)
        # Remove temp dir only if we created it and no output files were requested
        if _tmpdir is not None:
            any_output = any(tsd_cfg.get(k, False) for k in (
                'flag_output', 'flag_output_summary',
                'flag_output_shock', 'flag_output_field'))
            if not any_output:
                shutil.rmtree(_tmpdir, ignore_errors=True)

    # --- Collect results ---
    xx  = ts.mesh['xx']
    ile = ts.mesh['ile']
    ite = ts.mesh['ite']

    cd_wave = float(ts.data_summary.get('cd', 0.0))
    cd_f    = float(ts.data_summary.get('ibl_cd_f', 0.0))

    if tsd_cfg.get('flag_print_info', True):
        mode = 'IBL-coupled' if flag_IBL else 'Inviscid'
        print(f"\n[pyTSFoil] {mode} result  "
              f"Ma={Mach:.3f}  AoA={AoA_degrees:.2f}°  Re={Re:.2e}")
        print(f"  CL={ts.data_summary['cl']:.5f}  "
              f"CM={ts.data_summary['cm']:.5f}  "
              f"CD_wave={cd_wave:.5f}  "
              f"CD_f={cd_f:.5f}  "
              f"CD_total={cd_wave + cd_f:.5f}")

    results: Dict[str, Any] = {
        # Final results (IBL-coupled when flag_IBL=True, inviscid otherwise)
        'cl':       float(ts.data_summary['cl']),
        'cm':       float(ts.data_summary['cm']),
        'cd_wave':  cd_wave,
        'cd_f':     cd_f,
        'cd_total': cd_wave + cd_f,
        'xx':       xx,
        'xx_foil':  xx[ile : ite + 1],
        'ile':      ile,
        'ite':      ite,
        'cpu':      ts.data_summary['cpu'],
        'cpl':      ts.data_summary['cpl'],
        'mau':      ts.data_summary['mau'],
        'mal':      ts.data_summary['mal'],
        'cpstar':   float(ts.data_summary.get('cpstar', 0.0)),
        'history':  history,
        'ibl_upper': ts.data_summary.get('ibl_upper') if flag_IBL else None,
        'ibl_lower': ts.data_summary.get('ibl_lower') if flag_IBL else None,
        # Pure inviscid TSD baseline (always present for comparison)
        'baseline': baseline,
        'solver':   ts,
    }

    return results
