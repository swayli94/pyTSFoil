# Floating-Point Exception Handling in TSFOIL

## Overview

This document describes the floating-point exception handling implemented in the TSFOIL modern Fortran code to address the IEEE floating-point warnings that were previously only showing as notes.

## Problem

The original TSFOIL program was showing warnings like:
```
Note: The following floating-point exceptions are signalling: IEEE_UNDERFLOW_FLAG IEEE_DENORMAL
```

These warnings indicated numerical issues but didn't halt the program, making it difficult to debug potential numerical instabilities.

## Solution

We've implemented comprehensive floating-point exception handling at multiple levels:

### 1. Compiler-Level Exception Trapping

Modified the compilation scripts to include floating-point exception trapping flags:

- **Normal mode**: `-ffpe-trap=invalid,zero,overflow` (traps critical exceptions)
- **Strict mode**: `-ffpe-trap=invalid,zero,overflow,underflow,denormal` (traps ALL exceptions)
- **Debug mode**: Includes core dump generation for detailed debugging

### 2. Runtime Exception Handling

Added IEEE exception handling in the main program (`main.f90`):

```fortran
use, intrinsic :: ieee_exceptions
use, intrinsic :: ieee_arithmetic

! Enable halting on all IEEE exceptions
call ieee_set_halting_mode(ieee_all, .true.)
```

### 3. Iteration-Level Exception Checking

Added exception checking in the numerical solver (`numerical_solvers.f90`) to catch exceptions during iterations:

```fortran
subroutine check_iteration_fp_exceptions(iter_num)
  ! Checks for IEEE_INVALID, IEEE_OVERFLOW, IEEE_DIVIDE_BY_ZERO
  ! Halts execution with specific error codes and messages
end subroutine
```

## Compilation Options

Use the new compilation script `compile_with_fpe.bat` with different modes:

```batch
compile_with_fpe.bat            # Default mode (basic FPE trapping)
compile_with_fpe.bat normal     # Normal mode (critical exceptions)
compile_with_fpe.bat strict     # Strict mode (ALL exceptions including underflow)
compile_with_fpe.bat debug      # Debug mode (no optimization, maximum checking)
```

## Exception Types and Handling

| Exception Type | Description | Action |
|----------------|-------------|--------|
| IEEE_INVALID   | NaN generation (0/0, sqrt(-1), etc.) | HALT with exit code 2 |
| IEEE_OVERFLOW  | Values exceed representable range | HALT with exit code 3 |
| IEEE_DIVIDE_BY_ZERO | Division by zero | HALT with exit code 4 |
| IEEE_UNDERFLOW | Values too small (strict mode only) | HALT with exit code (varies) |
| IEEE_DENORMAL  | Denormalized numbers (strict mode only) | HALT with exit code (varies) |
| IEEE_INEXACT   | Rounding occurred (informational only) | Continue with warning |

## Benefits

1. **Early Detection**: Numerical problems are caught immediately when they occur
2. **Precise Location**: Backtraces show exactly where exceptions happen
3. **Deterministic Behavior**: Program behavior is consistent and predictable
4. **Debugging Aid**: Stack traces help identify the source of numerical issues
5. **Configurable Strictness**: Different compilation modes for different needs

## Usage Recommendations

- **Development**: Use `strict` mode to catch all numerical issues
- **Production**: Use `normal` mode to catch only critical issues
- **Debugging**: Use `debug` mode with core dumps for detailed analysis

## Example Output

When an exception occurs, you'll see:
```
FATAL: IEEE_INVALID exception (NaN) detected at iteration 1
This indicates invalid mathematical operations (e.g., 0/0, sqrt(-1))
```

Or with compiler trapping:
```
Program received signal SIGFPE: Floating-point exception - erroneous arithmetic operation.
Backtrace for this error:
#0  0xc270daa4
#1  0xc26774b3
...
```

This provides immediate feedback about numerical issues that would previously only generate warnings.
