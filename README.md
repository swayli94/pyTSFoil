# pyTSFoil

A python interface of TSFOIL2.

TSFOIL2 is an inviscid transonic small-disturbance (TSD) solver for ﬂow past lifting airfoils. The code was chosen for its rapid solution time, ease of use, and its open source architecture. It solves the transonically-scaled perturbation potential, and similarity variables that lead to the computation of the pressure coeﬃcient distribution, Cp, along the airfoil surface, that can then be integrated to yield lift and drag coeﬃcients.

Murman, E.M., Bailey, F.R., and Johnson, M.L., "TSFOIL - A Computer Code for TwoDimensional Transonic Calculations, Including Wind-Tunnel Wall Effects and Wave Drag Evaluation," NASA SP-347, March 1975.

<http://www.dept.aoe.vt.edu/~mason/Mason_f/MRsoft.html#TSFOIL2>

## # TSFoil

Generate foil and setup input/output files for TSFOIL2.

1. run(path)

    Run TSFOIL2 in given path. If path is not provided, run in the local directory.

2. foil_byCST(cst_u, cst_l, t)

    Generate airfoil for TSFOIL by given CST coefficients.

        cst-u, cst-l:   list of upper/lower CST coefficients of the airfoil.
        t:              airfoil maximum thickness or None

3. set_foil(xu, yu, xl, yl, t)

    Set airfoil coordinates.

4. flight_condition(Minf, AoA, path=None, **kwargs)

    Generate input file by given foil and flight conditions.
    Must set foil coordinates in advance.

        Minf:   free stream Mach number (can not be 1.0)
        AoA:    deg
        path:   working directory. None means local directory.

    kwargs:

        maxiter: maximum iteration. (default 1500)
        simdef: similarity definition. (default 3)
                1   Cole
                2   Spreiter
                3   Krupp
        wedge:  control for viscous wedge inclusion
                0   no wedge (default)
                1   Murman bump, then set Re and wc
                2   Yoshihara wedge
        Re:     Reynolds number based on chord for wedge = 1 (default, 4.0e6)
                If provided Re, automatically set wedge to 1
        wc:     wedge constant for wedge = 1 (default, 4.0)
        Amesh:  analytical mesh calculation (bool, default True)
                T   X and Y mesh values are computed, ni, nj should also be supplied.
                    ni is an odd number (4*n+1), nj is an even number.
                F   X and Y points are the default values.
        ni, nj: number of X and Y mesh points (less than 100)

5. get_result

    Extract result from foil-cp.out.

## # tsfoil-modify.f

A modified version of TSFOIL2 for better collaboration with python.

Compile with gfortran:

    Windows: gfortran -w -o tsfoil2.exe tsfoil2-modify.f
    Linux:   gfortran -w -o tsfoil2 tsfoil2-modify.f
