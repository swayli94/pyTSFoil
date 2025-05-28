# Functions and Subroutines in tsfoil2-modify.f

## Main Program

- `tsfoil2` - Main program for TSFOIL. Computes transonic flow past a two-dimensional lifting airfoil using transonic small disturbance theory.

## Subroutines

1. `ANGLE` (Line 299) - Computes the angle theta at mesh points.

2. `AYMESH` (Line 361) - Handles mesh generation.

3. `BCEND` (Line 691) - Modifies the DIAG and RHS vectors.

4. `BODY` (Line 807) - Computes airfoil geometry and prints it.

5. `CUTOUT` - Removes mesh points from the input.

6. `DIFCOE` - Calculates finite difference coefficients.

7. `DLAOUT` (Line 5491) - Outputs CP data in form usable by Antani's integration program.

8. `ECHINP` - Provides a listing of all data.

9. `FARFLD` - Sets far field boundary conditions.

10. `GUESSP` - Initializes P array.

11. `PRINT1` - Prints out body pressure.

12. `RECIRC` - Updates PJUMP values.

13. `REDUB` - Resets DUB variable.

14. `REFINE` - Adds mesh points.

15. `READIN` - Reads all input and checks it.

16. `RESET` - Resets values.

17. `SCALE` - Scales various quantities used in calculations.

18. `SETBC` (Line 4691) - Sets the limits on range of I and J for solution of the difference equations. Adjusts the airfoil slope boundary condition.

19. `SIMP` (Line 4754) - Integrates using Simpson's rule. Called by BODY.

20. `SOLVE` (Line 4815) - Controls the main iteration loop.

21. `SPLN1` (Line 5006) - Continuous derivative interpolation subroutines. CURFIT computes coefficients of cubics.

22. `SYOR` (Line 5098) - Computes new P at all mesh points.

23. `TRAP` (Line 5292) - Integrates Y DX by trapezoidal rule.

24. `VROOTS` (Line 5309) - Computes constants BETA0, BETA1, BETA2, PSI0, PSI1, PSI2 used in formula for vortex in slotted wind tunnel with subsonic freestream.

25. `VWEDGE` (Line 5376) - Computes Murman or Yoshihara viscous wedge and modifies slope conditions to account for jump in displacement thickness due to shock/boundary layer interaction.

## Functions

1. `ARF(X)` (Line 333) - Approximation from Handbook of Math Functions.

2. `CDCOLE` - Calculates drag coefficient.

3. `EMACH1(U)` - Calculates local Mach number.

4. `FINDSK` - Locates shock on airfoil surface.

5. `LIFT` - Calculates lift coefficient.

6. `PITCH` - Calculates pitching moment.

7. `PX` - Evaluates partial derivative of P with respect to x.

8. `WANGLE` (Line 5471) - Calculates wedge angle for viscous wedge calculation.

## Entry Points

1. `SPLN1X` (Line 5072) - Entry point for interpolation in SPLN1.

## Code Structure

The code is organized into a main program and multiple subroutines and functions that handle different aspects of the transonic flow calculation:

1. Input/output handling
2. Mesh generation and refinement
3. Boundary condition implementation
4. Flow field computation using successive line over-relaxation
5. Viscous interaction modeling
6. Calculation of aerodynamic coefficients

The program uses several COMMON blocks to share data between subroutines. The main computation is done in SOLVE, which controls the main iteration loop, with SYOR doing the actual relaxation calculations at each step.

The program implements the transonic small disturbance theory using fully conservative finite difference equations and successive line over-relaxation to solve for the flow field around an airfoil in transonic flow conditions.
