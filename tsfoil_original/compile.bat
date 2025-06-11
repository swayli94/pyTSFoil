@echo off
setlocal enabledelayedexpansion
REM Compilation script for TSFOIL.F90 with build type selection
REM This script compiles the main tsfoil.f90 program with either debug or release settings

echo TSFOIL Compilation Script
echo ========================
echo.

REM Check command line argument or prompt user
if "%1"=="debug" (
    set BUILD_TYPE=debug
) else if "%1"=="release" (
    set BUILD_TYPE=release
) else if "%1"=="d" (
    set BUILD_TYPE=debug
) else if "%1"=="r" (
    set BUILD_TYPE=release
) else (
    echo Choose build type:
    echo   1. Debug   ^(with GDB symbols, runtime checks, no optimization^)
    echo   2. Release ^(optimized, no debug symbols^)
    echo.
    set /p choice="Enter 1 or 2 (or 'd'/'r'): "
    
    if "!choice!"=="1" set BUILD_TYPE=debug
    if "!choice!"=="2" set BUILD_TYPE=release
    if "!choice!"=="d" set BUILD_TYPE=debug
    if "!choice!"=="r" set BUILD_TYPE=release
    if "!choice!"=="debug" set BUILD_TYPE=debug
    if "!choice!"=="release" set BUILD_TYPE=release
)

REM Clean up any previous compilation files
del *.mod *.o *.exe 2>nul

REM Set compiler flags based on build type
if "%BUILD_TYPE%"=="debug" (
    echo Building DEBUG version...
    REM Debug flags:
    REM -g: Generate debug information for GDB
    REM -O0: No optimization for better debugging
    REM -fbacktrace: Generate backtrace on runtime errors
    REM -fcheck=all: Enable all runtime checks
    REM -Wall: Enable all warnings
    REM -Wextra: Enable extra warnings
    set FFLAGS=-g -O0 -fbacktrace -fcheck=all -Wall -Wextra
    set EXECUTABLE=tsfoil.exe
) else (
    echo Building RELEASE version...
    REM Release flags:
    REM -O2: Optimization level 2
    REM -DNDEBUG: Define NDEBUG macro (disables assert statements)
    REM -Wall: Enable warnings
    set FFLAGS=-O2 -DNDEBUG -Wall
    set EXECUTABLE=tsfoil.exe
)

echo Compiler flags: %FFLAGS%
echo Output file: %EXECUTABLE%
echo.

REM Compile the main TSFOIL program
echo Compiling tsfoil.f90...
gfortran %FFLAGS% -o %EXECUTABLE% tsfoil.f90

REM Check if compilation was successful
if %ERRORLEVEL% EQU 0 (
    echo.
    echo Compilation successful!
    echo Executable created: %EXECUTABLE%
    echo.
    if "%BUILD_TYPE%"=="debug" (
        echo Debug build complete! Features enabled:
        echo   - GDB debug symbols included
        echo   - Runtime array bounds checking
        echo   - Floating-point exception detection
        echo   - Stack backtrace on errors
        echo   - No optimization for easier debugging
        echo.
        echo To debug with GDB:
        echo   gdb %EXECUTABLE%
        echo.
        echo To run with error checking:
        echo   %EXECUTABLE%
    ) else (
        echo Release build complete! Features:
        echo   - Optimized for performance
        echo   - No debug symbols ^(smaller executable^)
        echo   - Runtime checks disabled
        echo.
        echo To run:
        echo   %EXECUTABLE%
    )
) else (
    echo.
    echo Compilation failed with error code %ERRORLEVEL%
    echo Please check the error messages above.
    echo.
    echo Build type was: %BUILD_TYPE%
    echo Flags used: %FFLAGS%
)

echo.
echo Usage examples:
echo   compile.bat debug    ^(or compile.bat d^)
echo   compile.bat release  ^(or compile.bat r^)
echo   compile.bat          ^(interactive mode^)


