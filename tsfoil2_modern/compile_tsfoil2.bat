@echo off
REM Compilation script for TSFOIL2 modern Fortran code
REM This script compiles the modules in dependency order

echo Compiling TSFOIL2 modern Fortran code...

REM Clean up any previous compilation files
del *.mod *.o *.exe 2>nul

REM Set compiler flags
set FFLAGS=-O2 -Wall -fcheck=all

REM First compile independent modules
echo Compiling tsfoil_data module...
gfortran %FFLAGS% -c tsfoil_data.f90

echo Compiling spline_module...
gfortran %FFLAGS% -c spline_module.f90

echo Compiling numerical_solvers module...
gfortran %FFLAGS% -c numerical_solvers.f90

REM Then compile dependent modules
echo Compiling grid_module...
gfortran %FFLAGS% -c grid_module.f90

echo Compiling tsfoil_io module...
gfortran %FFLAGS% -c tsfoil_io.f90

echo Compiling airfoil_module...
gfortran %FFLAGS% -c airfoil_module.f90

echo Compiling solver_module...
gfortran %FFLAGS% -c solver_module.f90

echo Compiling output_module...
gfortran %FFLAGS% -c output_module.f90

REM Finally compile the main program and link all modules
echo Compiling main program and linking...
gfortran %FFLAGS% -o tsfoil2_modern.exe main.f90 tsfoil_data.o spline_module.o numerical_solvers.o grid_module.o tsfoil_io.o airfoil_module.o solver_module.o output_module.o

echo Compilation complete!
if exist tsfoil2_modern.exe (
    echo Successfully created tsfoil2_modern.exe
) else (
    echo Error: Compilation failed
)
