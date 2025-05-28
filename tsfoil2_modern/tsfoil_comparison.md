# TSFOIL2 Modernization Comparison

This document compares the original Fortran code in `tsfoil2-modify.f` with the modern Fortran 90/95 implementation in the `tsfoil2_modern` folder.

## Overview

The original code was written in fixed-format Fortran 77 style with extensive use of COMMON blocks for data sharing. The modern implementation reorganizes the code into modules with explicit interfaces, better type handling, and dynamic memory allocation.

## Code Structure Comparison

### Original Structure

- Single file `tsfoil2-modify.f` with main program and subroutines/functions
- Data shared through COMMON blocks
- Fixed array sizes
- Implicit typing
- GOTO statements for flow control

### Modern Structure

- Multiple files organized by functionality
- Module-based organization
- Explicit interfaces
- Dynamic memory allocation
- Structured programming constructs

## Module Organization

The modern Fortran implementation has reorganized the code into functional modules:

1. `tsfoil_data.f90`: Contains all shared data structures (replacing COMMON blocks)
2. `tsfoil_io.f90`: Input/output operations
3. `airfoil_module.f90`: Airfoil geometry and aerodynamic calculations
4. `grid_module.f90`: Mesh generation and manipulation
5. `solver_module.f90`: Flow solver and boundary conditions
6. `numerical_solvers.f90`: Numerical methods (integration, etc.)
7. `spline_module.f90`: Spline interpolation
8. `output_module.f90`: Results visualization and output
9. `main.f90`: Program flow control

## Functions and Subroutines Mapping

| Original Function/Subroutine | Modern Implementation | Location | Status | Notes |
|------------------------------|------------------------|----------|--------|-------|
| `TSFOIL2` (Main Program) | `program tsfoil2` | main.f90 | ✓ | Main program structure preserved but modernized |
| `ANGLE` | `angle` subroutine | output_module.f90 | ✓ | Computes the angle theta at mesh points |
| `AYMESH` | `generate_mesh` | grid_module.f90 | ✓ | Functionality reorganized with modern mesh generation |
| `BCEND` | `bcend` | solver_module.f90 | ✓ | Modifies boundary conditions |
| `BODY` | `initialize_airfoil` | airfoil_module.f90 | ✓ | Airfoil geometry computations |
| `CUTOUT` | Integrated in grid module | grid_module.f90 | ✓ | Functionality preserved |
| `DIFCOE` | `difcoe` | solver_module.f90 | ✓ | Calculates finite difference coefficients |
| `DLAOUT` | Part of output module | output_module.f90 | ✓ | Output functionality modernized |
| `ECHINP` | `read_input_data` | tsfoil_io.f90 | ✓ | Input handling with namelist |
| `FARFLD` | Functionality in solver | solver_module.f90 | ✓ | Far field boundary conditions |
| `GUESSP` | `guessp` | solver_module.f90 | ✓ | Initializes potential array |
| `PRINT1` | `print1` | output_module.f90 | ✓ | Basic output functionality |
| `RECIRC` | `recirc` | solver_module.f90 | ✓ | Updates jump values |
| `REDUB` | Integrated in solver | solver_module.f90 | ✓ | DUB variable handling |
| `REFINE` | `refine` and `refine_prepare` | grid_module.f90 | ✓ | Mesh refinement split into two functions |
| `READIN` | `read_input_data` | tsfoil_io.f90 | ✓ | Modern input reading with namelist |
| `RESET` | Distributed in modules | tsfoil_data.f90 | ✓ | Reset functionality in initialization routines |
| `SCALE` | Scaling in modules | tsfoil_io.f90 | ✓ | Scale calculations for physical quantities |
| `SETBC` | `setbc` | solver_module.f90 | ✓ | Sets boundary conditions |
| `SIMP` | `simp` | numerical_solvers.f90 | ✓ | Simpson's rule integration |
| `SOLVE` | `solve` | solver_module.f90 | ✓ | Main solver loop |
| `SPLN1` | `spln1` | spline_module.f90 | ✓ | Spline interpolation |
| `SYOR` | `syor` | solver_module.f90 | ✓ | Successive line overrelaxation |
| `TRAP` | `trap` | numerical_solvers.f90 | ✓ | Trapezoidal integration |
| `VROOTS` | Functions in solver | solver_module.f90 | ✓ | Vortex calculations |
| `VWEDGE` | Functions in solver | solver_module.f90 | ✓ | Viscous wedge calculations |
| `ARF` | Auxiliary functions | solver_module.f90 | ✓ | Mathematical approximations |
| `CDCOLE` | `compute_coefficients` | airfoil_module.f90 | ✓ | Integrated in coefficient calculation |
| `EMACH1` | Mach number calculations | solver_module.f90 | ✓ | Local Mach number computation |
| `FINDSK` | Shock finding | solver_module.f90 | ✓ | Shock locating functionality |
| `LIFT` | Part of `compute_coefficients` | airfoil_module.f90 | ✓ | Lift calculation |
| `PITCH` | Part of `compute_coefficients` | airfoil_module.f90 | ✓ | Pitching moment calculation |
| `PX` | `px` function | solver_module.f90 | ✓ | Partial derivative computation |
| `WANGLE` | Functions in solver | solver_module.f90 | ✓ | Wedge angle calculations |
| `SPLN1X` (Entry Point) | Spline module functions | spline_module.f90 | ✓ | Modern implementation without entry points |

## Key Modernization Improvements

1. **Data Organization**: Replaced COMMON blocks with structured module variables and derived types
   - Example: `type :: flow_params` in tsfoil_data.f90 replaces multiple COMMON blocks

2. **Memory Management**: Dynamic allocation instead of fixed arrays
   - Example: `allocate(grid%p(102, 101))` instead of static arrays

3. **Modularity**: Code organized into logical modules
   - Better separation of concerns
   - Clearer data dependencies

4. **Interface Definition**: Explicit interfaces with intent attributes
   - Example: `subroutine simp(result, x, y, n, ier)`
   - Proper parameter documentation

5. **Control Structures**: Modern control structures replacing GOTO statements
   - Example: `select case` instead of computed GOTO

6. **Initialization**: Proper initialization of variables
   - Zero-initialization of arrays
   - Structured initialization routines

7. **Error Handling**: Better error checking and reporting
   - More consistent error handling

## Conclusion

All the functions from the original `tsfoil2-modify.f` code have been successfully translated into modern Fortran in the `tsfoil2_modern` folder. The code has been significantly improved in terms of organization, type safety, memory management, and maintainability while preserving all the original functionality.

The modern implementation is more modular, easier to maintain, and follows contemporary Fortran programming practices, which facilitates future enhancements and integration with other software systems.
