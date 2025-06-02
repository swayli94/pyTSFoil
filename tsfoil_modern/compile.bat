@echo off
REM Compilation script for TSFOIL modern Fortran code
REM Compiles modules in dependency order

echo Compiling TSFOIL modern Fortran code...

REM Clean up any previous compilation files
del *.mod *.o *.exe 2>nul

REM Set compiler flags
set FFLAGS=-O2 -Wall -fcheck=all

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

echo Compilation successful!
if exist tsfoil_modern.exe (
    echo Successfully created tsfoil_modern.exe
) else (
    echo Error: Executable not found
    goto error
)

echo.
echo To run the program, create a tsfoil.inp input file and run:
echo tsfoil_modern.exe
goto end

:error
echo.
echo Compilation failed! Check error messages above.
exit /b 1

:end
