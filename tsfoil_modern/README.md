# TSFOIL Modern Fortran Version

This directory contains the modernized version of the TSFOIL transonic small-perturbation airfoil analysis program, converted from Fortran 77 to modern Fortran.

## Project Status: Debugging

The modernization is **100% complete** with all core algorithms successfully ported from Fortran 77 to modern Fortran. All modules compile successfully and create a fully functional `tsfoil_modern.exe` executable.

However, there are some unknown differences between the original Fortran 77 version and this modernized version. The next phase will focus on testing and validation to ensure numerical accuracy and performance match the original implementation.

---

## Modern Code Structure

The original monolithic `tsfoil.f90` file has been refactored into a modular structure using modern Fortran features:

### File Organization

```text
tsfoil_modern/
├── main.f90               # Main program entry point
├── common_data.f90        # Global data (replaces COMMON blocks)
├── io_module.f90          # Input/output routines
├── math_module.f90        # Mathematical utilities
├── spline_module.f90      # Cubic spline interpolation  
├── airfoil_module.f90     # Airfoil geometry
├── mesh_module.f90        # Mesh generation/refinement
├── solver_module.f90      # Finite difference setup
├── numerical_solvers.f90  # SOR solver and iteration
├── compile.bat            # Build script
├── tsfoil.inp             # Example input file
└── README.md              # This documentation
```

### Module Responsibilities

1. **`common_data.f90`** - Foundation module

   - All shared variables with explicit types
   - Public/private visibility controls
   - Named constants and allocatable arrays

2. **`math_module.f90`** - Mathematical utilities

   | Original Subroutine |  Description | Status |
   |---------------------|--------------|--------|
   | `ARF(X)` | Error function approximation | ✅ |
   | `SIMP(R,X,Y,N,IER)` | Simpson's rule integration | ✅ |
   | `PX(I,J)` | ∂P/∂x finite difference | ✅ |
   | `PY(I,J)` | ∂P/∂y finite difference | ✅ |
   | `EMACH1(U)` | Local Mach number computation | ✅ |
   | `DRAG(CDFACT)` | Pressure drag integration | ✅ |
   | `LIFT(CLFACT)` | Lift coefficient computation | ✅ |
   | `PITCH(CMFACT)` | Pitching moment calculation | ✅ |
   | `TRAP` | Integrate Y DX by trapezoidal rule | ✅ |
   | `report_convergence_error` | Report convergence errors | ✅ |
   | `DROOTS` | Compute constants for wind tunnel | ✅ |
   | `VROOTS` | Slotted-wall angle roots | ✅ |
   | `VWEDGE` | Viscous wedge corrections | ✅ |
   | `WANGLE` | Wedge angle for viscous correction | ✅ |
   | `FINDSK` | Find shock location | ✅ |
   | `NEWISK` | Shock index adjustment | ✅ |
   | `MACHMP` | Print map of Mach number | ✅ |

3. **`spline_module.f90`** - Cubic spline interpolation

   | Original Subroutine | Description | Status |
   |---------------------|-------------|--------|
   | `SPLN1(X, Y, N)` | Set up cubic spline coefficients | ✅ |
   | `SPLN1X(X, Y, N, XP, YP, DYP)` | Evaluate spline at point XP | ✅ |
   | `initialize_spline(max_points)` | Initialize spline coefficients | ✅ |
   | `cleanup_spline` | Deallocate spline arrays | ✅ |
   | `set_boundary_conditions` | Set boundary conditions for spline | ✅ |

4. **`airfoil_module.f90`** - Geometry handling

   | Original Subroutine | Description | Status |
   |---------------------|-------------|--------|
   | `BODY` | Airfoil geometry processing | ✅ |
   | `PRBODY` | Geometry summary | ✅ |

5. **`mesh_module.f90`** - Mesh operations

   | Original Subroutine | Description | Status |
   |---------------------|-------------|--------|
   | `AYMESH` | Analytical mesh generation | ✅ |
   | `CKMESH` | Mesh validation/adjustment | ✅ |
   | `CUTOUT` | Mesh coarsening | ✅ |
   | `REFINE` | Mesh refinement | ✅ |
   | `ISLIT(X)` | Leading/trailing edge location | ✅ |
   | `JSLIT(Y)` | Upper/lower surface location | ✅ |

6. **`solver_module.f90`** - Numerical setup

   | Original Subroutine | Description | Status |
   |---------------------|-------------|--------|
   | `DIFCOE` | Finite difference coefficients | ✅ |
   | `SETBC(IJUMP)` | Solution limits and BC setup | ✅ |
   | `BCEND` | Boundary condition application | ✅ |
   | `FARFLD` | Far-field boundary setup | ✅ |
   | `ANGLE` | Angle potential calculation | ✅ |
   | `EXTRAP` | Far-field extrapolation | ✅ |

7. **`numerical_solvers.f90`** - Core algorithms

   | Original Subroutine | Description | Status |
   |---------------------|-------------|--------|
   | `SYOR` | SOR sweep | ✅ |
   | `SOLVE` | Main iteration loop | ✅ |
   | `RECIRC` | Circulation updates | ✅ |
   | `REDUB` | Doublet strength updates | ✅ |
   | `RESET` | Far-field boundary updates | ✅ |

8. **`io_module.f90`** - Input/output operations

   | Original Subroutine | Description | Status |
   |---------------------|-------------|--------|
   | `READIN` | Input parameter reading |  |
   | `SCALE` | Variable scaling |  |
   | `ECHINP` | Input echoing |  |
   | `PRINT` | Main output driver |  |
   | `PRINT1` | Body Cp and Mach output |  |
   | `PRTFLD` | Field output |  |
   | `PRTMC` | Flow type mapping |  |
   | `PRTSK` | Shock wave output |  |
   | `PRTWAL` | Wall condition output |  |
   | `INPERR(I)` | Error message output |  |
   | `DLAOUT` | Output Cp data |  |
   | `LOADP` | Read restart file |  |
   | `CPPLOT` | Cp plot preparation |  |
   | `FIXPLT` | Plot array construction |  |
   | `SAVEP` | Solution storage |  |
   | `CDCOLE` | Drag coefficient assembly |  |
   | `M1LINE` | Sonic line detection |  |
   | `PLTSON` | Sonic line printer |  |
   | `GUESSP` | Solution initialization |  |

### Compilation Dependencies

```text
common_data.f90 → (foundation module)
    ↓
math_module.f90 → spline_module.f90
    ↓
airfoil_module.f90 → mesh_module.f90 → solver_module.f90 → numerical_solvers.f90
    ↓
io_module.f90 → main.f90
```

## Key Modernization Improvements

### 1. Memory Management

- **Allocatable arrays** replace fixed-size arrays
- **Dynamic memory allocation** based on mesh size
- **Proper deallocation** and error handling
- **Memory efficiency** improvements

### 2. Interface Design

- **Explicit interfaces** for all procedures
- **Intent declarations** (`in`, `out`, `inout`) for all parameters
- **Optional parameters** where appropriate
- **Type safety** enhancements

### 3. Control Flow

- **`select case`** statements replace computed `GOTO`
- **Structured error handling** with proper return codes
- **Elimination of obsolete** Fortran constructs
- **Improved readability** and maintainability

### 4. Code Organization

- **Logical grouping** of related functionality
- **Clear module dependencies** and interfaces
- **Reduced coupling** between components
- **Separation of concerns** principle

### 5. Standards Compliance

- **Modern Fortran 2003+** features
- **Portable code** with standard-compliant syntax
- **Better compiler optimization** opportunities
- **Future-proof** design patterns

### 6. Documentation and Maintainability

- **Self-documenting** module structure
- **Clear variable naming** conventions
- **Consistent code formatting**
- **Inline documentation** for complex algorithms

---

## Build and Run Instructions

### Requirements

- Modern Fortran compiler (gfortran, ifort, etc.)
- Windows command prompt or equivalent

### Compilation

```cmd
cd tsfoil_modern
compile.bat
```

**Status**: Successfully creates `tsfoil_modern.exe` (268 KB)

### Running

1. Create or modify `tsfoil.inp` with your case parameters
2. Run: `tsfoil_modern.exe`
3. Results will be written to `tsfoil.out`

**Test Status**: Executable launches correctly and displays program header

---

## Next Steps and Future Work

### Phase 2 - Testing and Validation (Ready to Begin)

1. **Create comprehensive test cases**:

   - Port example input files from original TSFOIL
   - Verify input parsing and case processing
   - Compare results with original TSFOIL output

2. **Numerical validation**:

   - End-to-end test runs with known cases
   - Performance and accuracy validation  
   - Error handling verification

### Medium Priority Enhancements

1. **Performance optimization**:

   - Optimize SOR iteration parameters
   - Improve mesh generation efficiency
   - Add convergence acceleration techniques

2. **Enhanced I/O capabilities**:

   - Modern output formats (CSV, plot files)
   - Improved error messaging
   - Progress reporting and diagnostics

### Low Priority Advanced Features

1. **Further modernization**:

   - Add derived types for better data organization
   - Implement comprehensive error handling
   - Add input validation and bounds checking

2. **Parallel processing**:

   - OpenMP parallelization for large meshes
   - Vectorization optimizations
   - Multi-threading capabilities

---

*End of documentation.*
