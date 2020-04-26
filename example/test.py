
import time

import matplotlib.pyplot as plt

from pyTSFoil.TSfoil import TSfoil


if __name__ == "__main__":

    t0 = time.perf_counter()

    #TODO: CST airfoil for Xfoil
    cst_u = [ 0.139800,  0.083450,  0.200000,  0.076980,  0.256960,  0.155120,  0.192000]
    cst_l = [-0.100000,  0.000000, -0.240000, -0.160000, -0.111660,  0.000000,  0.219750]
    t = 0.105
    Minf = 0.76
    AoA  = 0.70  # CL = 0.75
    Re   = 5e6
    
    tsfoil = TSfoil()
    tsfoil.foil_byCST(cst_u, cst_l, t)

    AoA  = 0.60
    tsfoil.flight_condition(Minf, AoA, Re=Re, wc=20.0)
    tsfoil.run(show=False)
    tsfoil.get_result()

    plt.plot(tsfoil.x, tsfoil.mwu, 'g')
    plt.plot(tsfoil.x, tsfoil.mwl, 'g')
    print(tsfoil.CL, tsfoil.Cd, tsfoil.Cdw, tsfoil.Cm)

    AoA  = 0.65
    tsfoil.flight_condition(Minf, AoA, Re=Re, wc=20.0)
    tsfoil.run(show=False)
    tsfoil.get_result()

    plt.plot(tsfoil.x, tsfoil.mwu, 'g--')
    plt.plot(tsfoil.x, tsfoil.mwl, 'g--')
    print(tsfoil.CL, tsfoil.Cd, tsfoil.Cdw, tsfoil.Cm)

    AoA  = 0.70
    tsfoil.flight_condition(Minf, AoA, Re=Re, wc=20.0)
    tsfoil.run(show=False)
    tsfoil.get_result()

    plt.plot(tsfoil.x, tsfoil.mwu, 'b')
    plt.plot(tsfoil.x, tsfoil.mwl, 'b')
    print(tsfoil.CL, tsfoil.Cd, tsfoil.Cdw, tsfoil.Cm)

    t1 = time.perf_counter()
    print('Time = %.3f s'%(t1-t0))

    plt.show()
