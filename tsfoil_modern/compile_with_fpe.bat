@echo off
REM Compilation script for TSFOIL modern Fortran code with floating-point exception handling
REM This version enables aggressive floating-point exception trapping

echo Compiling TSFOIL modern Fortran code with floating-point exception handling...

REM Clean up any previous compilation files
del *.mod *.o *.exe 2>nul

REM Set compiler flags with different levels of FPE checking
if "%1"=="strict" (
    REM Strictest mode - trap all exceptions including underflow and denormal
    set FFLAGS=-O2 -Wall -fcheck=all -ffpe-trap=invalid,zero,overflow,underflow,denormal -g -fbacktrace
    echo Using STRICT mode: trapping ALL floating-point exceptions
) else if "%1"=="normal" (
    REM Normal mode - trap only critical exceptions
    set FFLAGS=-O2 -Wall -fcheck=all -ffpe-trap=invalid,zero,overflow -g -fbacktrace
    echo Using NORMAL mode: trapping critical floating-point exceptions
) else if "%1"=="debug" (
    REM Debug mode - no optimization, maximum checking
    set FFLAGS=-O0 -Wall -fcheck=all -ffpe-trap=invalid,zero,overflow,underflow,denormal -g -fbacktrace -fdump-core
    echo Using DEBUG mode: no optimization, maximum exception checking
) else (
    REM Default mode - original behavior with basic FPE trapping
    set FFLAGS=-O2 -Wall -fcheck=all -ffpe-trap=invalid,zero,overflow
    echo Using DEFAULT mode: basic floating-point exception trapping
    echo Available modes: normal, strict, debug
)

echo Compiler flags: %FFLAGS%
echo.

REM Compile modules in dependency order
echo Compiling common_data module...
gfortran %FFLAGS% -c common_data.f90
if errorlevel 1 goto error

echo Compiling spline_module...
gfortran %FFLAGS% -c spline_module.f90
if errorlevel 1 goto error

echo Compiling math_module...
gfortran %FFLAGS% -c math_module.f90
if errorlevel 1 goto error

echo Compiling solver_module...
gfortran %FFLAGS% -c solver_module.f90
if errorlevel 1 goto error

echo Compiling mesh_module...
gfortran %FFLAGS% -c mesh_module.f90
if errorlevel 1 goto error

echo Compiling airfoil_module...
gfortran %FFLAGS% -c airfoil_module.f90
if errorlevel 1 goto error

echo Compiling io_module...
gfortran %FFLAGS% -c io_module.f90
if errorlevel 1 goto error

echo Compiling numerical_solvers...
gfortran %FFLAGS% -c numerical_solvers.f90
if errorlevel 1 goto error

REM Compile main program and link
echo Compiling main program and linking...
gfortran %FFLAGS% -o tsfoil_modern.exe main.f90 common_data.o spline_module.o solver_module.o math_module.o mesh_module.o airfoil_module.o io_module.o numerical_solvers.o
if errorlevel 1 goto error

echo.
echo Compilation successful!
echo Executable: tsfoil_modern.exe
echo.
echo Usage examples:
echo   tsfoil_modern.exe           (runs with FPE checking enabled)
echo.
echo Compilation modes available:
echo   compile_with_fpe.bat        (default - basic FPE trapping)
echo   compile_with_fpe.bat normal (trap critical exceptions)
echo   compile_with_fpe.bat strict (trap ALL exceptions including underflow)
echo   compile_with_fpe.bat debug  (debug mode with core dumps)
goto end

:error
echo.
echo ERROR: Compilation failed!
echo Check the error messages above.
exit /b 1

:end
