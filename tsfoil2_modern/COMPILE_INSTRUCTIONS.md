# How to Compile TSFOIL2 Modern Fortran Code

This document explains how to compile the modernized TSFOIL2 Fortran code using gfortran.

## Requirements

- GNU Fortran compiler (gfortran) installed on your system
- Basic command line knowledge

## Compilation Options

There are three ways to compile the code:

### 1. Using the Batch Script (Windows)

1. Open a Command Prompt window
2. Navigate to the tsfoil2_modern directory:
   ```
   cd c:\codes\pyTSFoil\tsfoil2_modern
   ```
3. Run the batch file:
   ```
   compile_tsfoil2.bat
   ```

### 2. Using the Shell Script (Linux/macOS)

1. Open a terminal window
2. Navigate to the tsfoil2_modern directory:
   ```
   cd /path/to/pyTSFoil/tsfoil2_modern
   ```
3. Make the script executable:
   ```
   chmod +x compile_tsfoil2.sh
   ```
4. Run the shell script:
   ```
   ./compile_tsfoil2.sh
   ```

### 3. Using the Makefile (All platforms with make utility)

1. Open a terminal or Command Prompt window
2. Navigate to the tsfoil2_modern directory
3. Run the make command:
   ```
   make
   ```
4. To clean compiled objects and start fresh:
   ```
   make clean
   ```

## Manual Compilation

If you prefer to compile manually, you need to follow the module dependency order:

1. First compile base modules that don't depend on others:
   ```
   gfortran -c tsfoil_data.f90
   gfortran -c spline_module.f90
   gfortran -c numerical_solvers.f90
   ```

2. Then compile modules that depend on the base modules:
   ```
   gfortran -c grid_module.f90
   gfortran -c tsfoil_io.f90
   gfortran -c airfoil_module.f90
   gfortran -c solver_module.f90
   gfortran -c output_module.f90
   ```

3. Finally compile and link the main program:
   ```
   gfortran -o tsfoil2_modern main.f90 tsfoil_data.o spline_module.o numerical_solvers.o grid_module.o tsfoil_io.o airfoil_module.o solver_module.o output_module.o
   ```

## Compiler Flags

The compilation scripts use these gfortran flags:
- `-O2`: Level 2 optimization for better performance
- `-Wall`: Enable all common compiler warnings
- `-fcheck=all`: Enable runtime checks for array bounds, etc.

You may adjust these flags based on your needs:
- For debugging: Add `-g`
- For more aggressive optimization: Use `-O3` instead of `-O2`
- For static linking: Add `-static`

## Running the Program

After successful compilation, run the program:

- On Windows:
  ```
  tsfoil2_modern.exe
  ```

- On Linux/macOS:
  ```
  ./tsfoil2_modern
  ```

Note: The program expects input files in the same format as the original TSFOIL2 program.
