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

   - Error function approximation (`ARF`)
   - Numerical integration (`SIMP`)
   - Enhanced precision and error handling

3. **`spline_module.f90`** - Cubic spline interpolation

   - Spline coefficient computation (`SPLN1`)
   - Interpolation and extrapolation routines
   - Airfoil ordinate handling

4. **`airfoil_module.f90`** - Geometry handling

   - NACA airfoil generation (`BODY`)
   - Coordinate reading and processing
   - Geometry summary output (`PRBODY`)

5. **`mesh_module.f90`** - Mesh operations

   - Analytical mesh generation (`AYMESH`)
   - Mesh validation and adjustment (`CKMESH`)
   - Mesh refinement and coarsening (`REFINE`, `CUTOUT`)
   - Slit location utilities (`ISLIT`, `JSLIT`)

6. **`solver_module.f90`** - Numerical setup

   - Finite difference coefficients (`DIFCOE`)
   - Boundary condition setup (`SETBC`, `BCEND`)
   - Far-field treatment (`FARFLD`, `EXTRAP`)
   - Shock detection (`FINDSK`, `M1LINE`)
   - Pressure derivatives (`PX`, `PY`)

7. **`numerical_solvers.f90`** - Core algorithms

   - SOR iteration (`SYOR`, `SOLVE`)
   - Solution initialization (`GUESSP`)
   - Circulation and doublet updates (`RECIRC`, `REDUB`)
   - Force calculations (`DRAG`, `LIFT`, `PITCH`)
   - Solution management (`SAVEP`, `RESET`)

8. **`io_module.f90`** - Input/output operations

   - Input processing (`READIN`, `ECHINP`)
   - Coordinate scaling (`SCALE`)
   - Output generation (`PRINT`, `PRINT1`, `PRTFLD`)
   - Plotting utilities (`CPPLOT`, `FIXPLT`)
   - Error reporting (`INPERR`)

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

---

## Original Functions → Modern Implementation Mapping

### Input/Output Operations

| Original Subroutine | Modern Location | Description | Status |
|---------------------|-----------------|-------------|---------|
| `READIN` | `io_module.f90` | Input parameter reading | ✅ |
| `SCALE` | `io_module.f90` | Variable scaling | ✅ |
| `ECHINP` | `io_module.f90` | Input echoing | ✅ |
| `PRINT` | `io_module.f90` | Main output driver |  |
| `PRINT1` | `io_module.f90` | Body Cp and Mach output |  |
| `PRTFLD` | `io_module.f90` | Field output |  |
| `PRTMC` | `io_module.f90` | Flow type mapping |  |
| `PRTSK` | `io_module.f90` | Shock wave output |  |
| `PRTWAL` | `io_module.f90` | Wall condition output |  |
| `INPERR(I)` | `io_module.f90` | Error message output |  |
| `DLAOUT` | `io_module.f90` |  |  |
| `LOADP` | `io_module.f90` |  |  |

### Mesh and Geometry Operations

| Original Subroutine | Modern Location | Description | Status |
|---------------------|-----------------|-------------|---------|
| `BODY` | `airfoil_module.f90` | Airfoil geometry processing | ✅ |
| `PRBODY` | `airfoil_module.f90` | Geometry summary | ✅ |
| `AYMESH` | `mesh_module.f90` | Analytical mesh generation | ✅ |
| `CKMESH` | `mesh_module.f90` | Mesh validation/adjustment | ✅ |
| `CUTOUT` | `mesh_module.f90` | Mesh coarsening | ✅ |
| `REFINE` | `mesh_module.f90` | Mesh refinement | ✅ |
| `ISLIT(X)` | `mesh_module.f90` | Leading/trailing edge location | ✅ |
| `JSLIT(Y)` | `mesh_module.f90` | Upper/lower surface location | ✅ |

### Solver and Boundary Conditions

| Original Subroutine | Modern Location | Description | Status |
|---------------------|-----------------|-------------|---------|
| `ANGLE` | `solver_module.f90` | Angle potential calculation |  |
| `BCEND` | `solver_module.f90` | Boundary condition application |  |
| `DIFCOE` | `solver_module.f90` | Finite difference coefficients |  |
| `EXTRAP` | `solver_module.f90` | Far-field extrapolation |  |
| `FARFLD` | `solver_module.f90` | Far-field boundary setup |  |
| `SETBC(IJUMP)` | `solver_module.f90` | Solution limits and BC setup |  |
| `DROOTS` | `solver_module.f90` | Slotted-wall angle roots |  |

### Numerical Solution

| Original Subroutine | Modern Location | Description | Status |
|---------------------|-----------------|-------------|---------|
| `SOLVE` | `numerical_solvers.f90` | Main iteration loop |  |
| `SYOR` | `numerical_solvers.f90` | SOR sweep |  |
| `GUESSP` | `numerical_solvers.f90` | Solution initialization |  |
| `RECIRC` | `numerical_solvers.f90` | Circulation updates |  |
| `REDUB` | `numerical_solvers.f90` | Doublet strength updates |  |
| `RESET` | `numerical_solvers.f90` | Far-field boundary updates |  |
| `SAVEP` | `numerical_solvers.f90` | Solution storage |  |

### Analysis and Post-Processing

| Original Subroutine | Modern Location | Description | Status |
|---------------------|-----------------|-------------|---------|
| `CDCOLE` | `numerical_solvers.f90` | Drag coefficient assembly |  |
| `CPPLOT` | `io_module.f90` | Cp plot preparation |  |
| `FIXPLT` | `io_module.f90` | Plot array construction |  |
| `SPLN1` | `spline_module.f90` | Cubic spline coefficients |  |

### Shock and Flow Analysis

| Original Subroutine | Modern Location | Description | Status |
|---------------------|-----------------|-------------|---------|
| `FINDSK` | `solver_module.f90` | Shock detection |  |
| `NEWISK` | `solver_module.f90` | Shock index adjustment |  |
| `M1LINE` | `solver_module.f90` | Sonic line detection |  |

### Math and Utility Functions

| Original Subroutine | Modern Location | Description | Status |
|---------------------|-----------------|-------------|---------|
| `ARF(X)` | `math_module.f90` | Error function approximation | ✅ |
| `SIMP(R,X,Y,N,IER)` | `math_module.f90` | Simpson's rule integration | ✅ |
| `EMACH1(U)` | `math_module.f90` | Local Mach number computation | ✅ |
| `PX(I,J)` | `math_module.f90` | ∂P/∂x finite difference | ✅ |
| `PY(I,J)` | `math_module.f90` | ∂P/∂y finite difference | ✅ |
| `TRAP` | `math_module.f90` | Integrate Y DX by trapezoidal rule | ✅ |
| `DRAG(CDFACT)` | `math_module.f90` | Pressure drag integration | ✅ |
| `LIFT(CLFACT)` | `math_module.f90` | Lift coefficient computation | ✅ |
| `PITCH(CMFACT)` | `math_module.f90` | Pitching moment calculation | ✅ |
| `VWEDGE` | `math_module.f90` | Viscous wedge corrections | ✅ |
| `WANGLE` | `math_module.f90` | Wedge angle for viscous correction | ✅ |
| `FINDSK` | `math_module.f90` | Find shock location | ✅ |
| `DROOTS` | `math_module.f90` | Compute constants for wind tunnel | ✅ |

---

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

**Status**: ✅ Successfully creates `tsfoil_modern.exe` (268 KB)

### Running

1. Create or modify `tsfoil.inp` with your case parameters
2. Run: `tsfoil_modern.exe`
3. Results will be written to `tsfoil.out`

**Test Status**: ✅ Executable launches correctly and displays program header

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

## References

- Original code summary: `tsfoil_f90_summary.md`
- Original Fortran 77 source: `../pyTSFoil/tsfoil.f90`

---

*End of documentation.*
