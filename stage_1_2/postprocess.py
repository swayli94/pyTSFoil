"""
Post-processing for 3-D TSD solution — Stage 1.

Both ends are symmetric planes: all nk stations are wing stations.
"""

import numpy as np


def surface_cp_3d_station(P_k, d, pytsfoil_obj):
    """
    Isentropic Cp on the upper/lower airfoil surface for spanwise station k.

    P_k  : float64[nj, ni], the k-th spanwise slice of the 3-D potential.
    d    : coefficient dict from extract_arrays (stage_0/solver2d_py.py).
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


def spanwise_cl(P, d, pytsfoil_obj, eta):
    """
    Spanwise lift distribution and mean CL for all nk stations.

    All stations are wing stations (both ends symmetric, no wingtip gap).
    CL is the spanwise average of cl(z):  CL = trapz(cl, z) / span.

    Returns
    -------
    cl_z : float64[nk]
    CL   : float
    """
    nk   = P.shape[0]
    cl_z = np.zeros(nk)

    for k in range(nk):
        cpu, cpl, xx = surface_cp_3d_station(P[k], d, pytsfoil_obj)
        cl_z[k] = np.trapezoid(cpl - cpu, xx)

    span = eta[-1]
    if span > 1e-10:
        CL = np.trapezoid(cl_z, eta) / span
    else:
        CL = cl_z[0]

    return cl_z, CL
