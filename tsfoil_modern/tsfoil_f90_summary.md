# Summary of `tsfoil.f90` Routines

This document lists all major subroutines and functions in **tsfoil.f90** with a brief description of their role. It will serve as a guideline when refactoring into modern Fortran modules.

---

## Functions

- **ARF(X)**
  Rational approximation for the error function `erf(X)`.

- **EMACH1(U)**
  Computes local Mach number (or similarity parameter) from velocity increment `U`.

- **DRAG(CDFACT)**
  Integrates pressure drag by momentum integral around the body.

- **LIFT(CLFACT)**
  Computes lift coefficient from potential jump at trailing edge.

- **PITCH(CMFACT)**
  Computes pitching moment about quarter-chord.

- **PX(I,J)**
  Finite-difference approximation for ∂P/∂x at grid point `(I,J)`.

- **PY(I,J)**
  Finite-difference approximation for ∂P/∂y at grid point `(I,J)`.

- **SIMP(R,X,Y,N,IER)**
  Numerical integration using Simpson’s rule (and special edge cases).

## Subroutines

- **ANGLE**
  Calculates angle potential `THETA` at each mesh point for far-field BC.

- **AYMESH**
  Builds analytical mesh (`XIN`,`YIN`) if `AMESH = .TRUE.`.

- **BCEND**
  Applies upper and lower boundary conditions (Dirichlet/Neumann) on each i-line.

- **BODY**
  Reads or generates airfoil geometry (NACA, spline, Jameson format, etc.) and computes thickness, camber.

- **CDCOLE**
  Computes overall drag coefficient by assembling contour integrals (body, shocks, far-field).

- **CKMESH**
  Ensures odd/even mesh counts around the slit and tail; adjusts mesh to allow refinement.

- **CPPLOT**
  Prepares a printer plot of pressure coefficient vs. x on the body.

- **CUTOUT**
  Coarsens mesh (halves points) for initial relaxation pass.

- **DIFCOE**
  Computes finite-difference coefficients for derivatives in x and y (P, Pxx, Pyy).

- **DROOTS**
  Iteratively solves for slotted-wall angle roots (used in `FARFLD`).

- **ECHINP**
  Echoes raw input cards for logging.

- **EXTRAP**
  Extrapolates potential to infinity for far-field (subsonic) conditions.

- **FARFLD**
  Sets up far-field boundary values for potential (doublet + vortex) on outer mesh boundaries.

- **FINDSK**
  Finds bow and detachment shocks on a given j-line by scanning Mach > 1 → < 1.

- **FIXPLT**
  Constructs arrays for printer-plotting Cp on body and dividing streamlines.

- **GUESSP**
  Initializes the solution array `P` to zero, saved values, or from restart.

- **INPERR(I)**
  Prints a fatal input error message and stops execution.

- **ISLIT(X)**
  Locates indices `ILE`, `ITE` where X crosses airfoil from leading to trailing edge.

- **JSLIT(Y)**
  Locates indices `JLOW`, `JUP` around the slit (Y=0) for upper/lower surfaces.

- **M1LINE**
  Finds coordinates where the flow becomes sonic (Mach = 1) between grid points.

- **NEWISK**
  Adjusts shock‐wave index location when marching along `FINDSK` paths.

- **PRBODY**
  Prints body geometry summary (thickness, camber, volume) in physical or similarity units.

- **PRINT**
  Main output driver: prints global parameters, calls specialized print routines (`PRINT1`, `PRTMC`, etc.).

- **PRINT1**
  Prints Cp and Mach number along the body (dividing streamline) and builds Cp plot.

- **PRTFLD**
  Prints pressure coefficient, flow angle, and Mach number in the field on selected j-lines.

- **PRTMC**
  Prints a character map of flow type at each grid point: elliptic, hyperbolic, parabolic, shock.

- **PRTSK**
  Prints wave drag contributions and pressure loss profiles along each shock wave.

- **PRTWAL**
  Prints Cp and flow angles on tunnel walls (Y=±H) for wall BC cases.

- **READIN**
  Reads all namelist parameters and arrays, sets defaults, checks inputs, calls mesh routines.

- **RECIRC**
  Updates circulation-jump boundary at trailing edge and along the dividing streamline.

- **REDUB**
  Updates doublet strength `DUB` (for lifting bodies and nonlinear correction).

- **REFINE**
  Refines (doubles) the mesh and interpolates the solution `P` onto the finer grid.

- **RESET**
  Updates far-field boundary values after a Kutta slice or mesh refinement.

- **SAVEP**
  Stores current solution in restart arrays and writes to unit 3 if saving is requested.

- **SCALE**
  Converts physical variables to transonic similarity variables (Cole, Spreiter, Krupp, or user).

- **SETBC(IJUMP)**
  Defines solution limits (`IMIN`, `IMAX`, `JBOT`, `JTOP`) and applies body slope BC arrays.

- **SPLN1**
  Sets up cubic-spline coefficients (`A`,`B`) for interpolation and extrapolation of airfoil ordinates.

- **SYOR**
  Performs one SOR sweep (or line‐by‐line) computing residuals and updating `P`.

- **SOLVE**
  Main iteration loop: SOR solver, residual tracking, circulations, viscous-wedge, convergence checks.

- **VWEDGE**  **(Implicit)**
  Computes viscous wedge corrections (Murman/Yoshihara) along shocks if enabled.

---

**Next steps for modernization**

- Replace COMMON blocks with Fortran 2003+ modules and derived types in **common_data**.
- Mesh routines (`AYMESH`, `CKMESH`, `CUTOUT`, `REFINE`) in **mesh_module**.
- Geometry routines (`BODY`, `PRBODY`) in **airfoil_module**.
- Finite-difference solver routines (`DIFCOE`, `SETBC`, `BCEND`) in **solver_module**.
- SOR iteration routines (`SYOR`, `SOLVE`, `RECIRC`, `REDUB`, `RESET`) in **numerical_solvers**.
- Input/output routines (`READIN`, `SCALE`, `ECHINP`, `PRINT`, `PRINT1`, `PRTFLD`, printer plots) in **io_module**.
- Refactor utility routines (`SPLN1`, `SIMP`, `ARF`) into **spline_module** and **math_module**.
- Adopt allocatable arrays instead of fixed-size arrays where possible.
- Use named constants and `intent(inout)`/`intent(in)` attributes for procedure arguments.
- Leverage `select case` for BC/type dispatch instead of computed `GO TO` tables.

---

*End of summary.*
