"""
Post-processing for 3-D TSD solution — Stage 1.

Functions
---------
surface_cp_3d_station  — isentropic Cp on the upper/lower surface at one k
spanwise_cl            — integrate Cp → cl(z), then integrate cl(z) → CL
"""

import numpy as np


def surface_cp_3d_station(P_k, d, pytsfoil_obj):
    """
    Isentropic Cp on the upper/lower airfoil surface for spanwise station k.

    P_k  : float64[nj, ni], the k-th spanwise slice of the 3-D potential.
    d    : coefficient dict from extract_arrays (stage_0/solver2d_py.py).

    Returns (cpu, cpl, xx_foil) — same as stage_0 surface_cp_from_P.
    """
    ile    = d['ile']
    ite    = d['ite']
    jup    = d['jup']
    jlow   = d['jlow']
    XDIFF  = d['XDIFF']
    CJUP   = d['CJUP'];   CJUP1  = d['CJUP1']
    CJLOW  = d['CJLOW'];  CJLOW1 = d['CJLOW1']

    emach  = float(pytsfoil_obj.config['EMACH'])
    delta  = float(pytsfoil_obj.airfoil['t_max'])
    ak     = d['AK']
    gam1   = 2.4
    delrt2 = delta ** (2.0 / 3.0)
    simdef = pytsfoil_obj.config['SIMDEF']

    def phi_x(j, i):
        return 0.5 * (XDIFF[i + 1] * (P_k[j, i + 1] - P_k[j, i])
                      + XDIFF[i]   * (P_k[j, i]     - P_k[j, i - 1]))

    def emach1(u):
        ak_loc = ak - gam1 * u
        if simdef == 1:
            arg = 1.0 - delrt2 * ak_loc
        elif simdef == 2:
            arg = 1.0 - delrt2 * ak_loc * emach ** (4.0 / 3.0)
        else:
            arg = 1.0 - delrt2 * ak_loc * emach
        return np.sqrt(max(arg, 0.0))

    n_foil = ite - ile + 1
    cpu = np.zeros(n_foil)
    cpl = np.zeros(n_foil)

    for idx, i in enumerate(range(ile, ite + 1)):
        uu = (CJUP  * phi_x(jup,      i)
              - CJUP1 * phi_x(jup + 1, i))
        ul = (CJLOW  * phi_x(jlow,     i)
              - CJLOW1 * phi_x(jlow - 1, i))
        cpu[idx] = pytsfoil_obj._cp_isentropic(emach1(uu), emach)
        cpl[idx] = pytsfoil_obj._cp_isentropic(emach1(ul), emach)

    xx_foil = d['X'][ile: ite + 1]
    return cpu, cpl, xx_foil


def spanwise_cl(P, d, pytsfoil_obj, eta, k_tip):
    """
    Spanwise lift distribution and integrated CL for the 3-D solution.

    Integrates (cpl - cpu) over the chord at each wing station k ≤ k_tip,
    then integrates cl(z) over the half-span to get CL.

    Returns
    -------
    cl_z  : float64[k_tip+1], section lift coefficient at each spanwise station
    CL    : float, integrated lift coefficient (normalised by wing area S = b*c)
    """
    nk = P.shape[0]
    cl_z = np.zeros(nk)

    for k in range(k_tip + 1):
        cpu, cpl, xx = surface_cp_3d_station(P[k], d, pytsfoil_obj)
        delta_cp = cpl - cpu
        cl_z[k]  = np.trapezoid(delta_cp, xx)

    # Integrate cl(z) over half-span: CL = (2/b) ∫₀^{b/2} cl(z) dz
    half_span = eta[k_tip]
    if half_span > 1e-10:
        CL = 2.0 / (2.0 * half_span) * np.trapezoid(cl_z[:k_tip + 1],
                                                      eta[:k_tip + 1])
    else:
        CL = cl_z[0]

    return cl_z, CL
