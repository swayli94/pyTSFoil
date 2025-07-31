#!/usr/bin/env python3
"""
F2PY compilation script for TSFoil_modern
Compiles Fortran modules into a Python extension module
"""

import os
import sys
import subprocess
import shutil
import glob
from pathlib import Path


MODULE_NAME = "tsfoil_fortran"
FLAG_GENERATE_SIGNATURE = True # Generate *.pyf (The generated file is not 100% correct)


def run_command(cmd, cwd=None):
    """Run a shell command and check for errors"""
    print(f"Running: {cmd}")
    if cwd:
        print(f"  in directory: {cwd}")
    
    result = subprocess.run(cmd, shell=True, cwd=cwd, capture_output=True, text=True)
    
    if result.returncode != 0:
        print(f"ERROR: Command failed with return code {result.returncode}")
        print(f"STDOUT: {result.stdout}")
        print(f"STDERR: {result.stderr}")
        sys.exit(1)
    else:
        print("SUCCESS")
        if result.stdout:
            print(f"STDOUT: {result.stdout}")
    
    return result

def clean_up_files():
    '''
    Clean up previous compilation files
    '''
    print("Cleaning up previous build files...")
    
    cleanup_patterns = ["*.mod", "*.o", "*.so"]
    
    if FLAG_GENERATE_SIGNATURE:
        cleanup_patterns.append("*.pyf")
    
    for pattern in cleanup_patterns:
        files_to_remove = glob.glob(pattern)
        for file_path in files_to_remove:
            try:
                os.remove(file_path)
                print(f"Removed: {file_path}")
            except OSError as e:
                print(f"Warning: Could not remove {file_path}: {e}")

def check_files_exist(files):
    '''
    Check if all files exist
    '''
    for f in files:
        if not Path(f).exists():
            print(f"ERROR: {f} not found!")
            sys.exit(1)
    print("All Fortran source files found.")

def generate_signature_file(files):
    '''
    Generate a signature file using f2py
    '''
    print("\nGenerating f2py signature file...")
    files_str = " ".join(files)
    
    # Generate .pyf file
    cmd = f"f2py -m {MODULE_NAME} -h {MODULE_NAME}.pyf {files_str} --overwrite-signature"
    run_command(cmd)
    
    # Check if signature file was created
    if not Path(MODULE_NAME + ".pyf").exists():
        print("ERROR: Failed to generate signature file!")
        sys.exit(1)

    # Fix the .pyf file
    with open(MODULE_NAME + ".pyf", 'r') as f:
        content = f.read()
    
    # Replace nmesh with nmp_plus2
    content = content.replace('dimension(nmesh)', 'dimension(nmp_plus2)')
    content = content.replace('gam1=5.0', 'gam1=2.4')

    with open(MODULE_NAME + ".pyf", 'w') as f:
        f.write(content)
    
    print("Signature file generated successfully.")
    
    return Path(MODULE_NAME + ".pyf")

def compile_module(files) -> Path:
    '''
    Compile the module using f2py
    '''
    print("\nCompiling Fortran code with f2py...")
    
    files_str = " ".join(files)
    
    # Set compiler flags (similar to compile.sh but for f2py)
    fflags = "-O2 -Wall -g -fbacktrace"
    
    cmd = f"f2py --fcompiler=gfortran --f90flags='{fflags}' -c {MODULE_NAME}.pyf {files_str}"
    result = run_command(cmd)
    
    # Check if the shared library was created
    so_files = list(Path(".").glob(MODULE_NAME + "*.so"))
    if not so_files:
        print("ERROR: No shared library (.so) file was created!")
        sys.exit(1)
    
    so_file = so_files[0]
    print(f"SUCCESS: Shared library created: {so_file}")
    
    return so_file


if __name__ == "__main__":
    
    # Get the directory where this script is located
    script_dir = Path(__file__).parent
    
    # Change to source directory (relative to script location)
    src_dir = script_dir.parent / "src"
    os.chdir(src_dir)
    clean_up_files()
    
    # Define Fortran source files in dependency order
    fortran_files = [
        "common_data.f90",
        "solver_data.f90", 
        "math_module.f90",
        "solver_base.f90",
        "solver_functions.f90",
        "main_iteration.f90",
        "io_module.f90"
    ]
    
    check_files_exist(fortran_files)
    
    # Create signature file using f2py
    pyf_file = None
    if FLAG_GENERATE_SIGNATURE:
        pyf_file = generate_signature_file(fortran_files)
    
    # Compile the module using f2py
    so_file = compile_module(fortran_files)
    
    # Move the .so file to pyTSFoil directory for easier access
    pytsfoil_dir = script_dir
    target_so = pytsfoil_dir / so_file.name
    shutil.move(str(so_file), str(target_so))
    print(f"Moved {so_file.name} to pyTSFoil directory")
    
    # Move the .pyf file to pyTSFoil directory if it was generated
    if pyf_file and pyf_file.exists():
        target_pyf = pytsfoil_dir / pyf_file.name
        shutil.move(str(pyf_file), str(target_pyf))
        print(f"Moved {pyf_file.name} to pyTSFoil directory")
    
    print("\n" + "="*60)
    print("F2PY COMPILATION COMPLETED SUCCESSFULLY!")
    print("="*60)
    print(f"Python module: {target_so.name}")
    print("\nTo test the module, run:")
    print("  python3 -c 'import tsfoil_fortran; print(dir(tsfoil_fortran))'")
    print("\nTo use in Python:")
    print("  import tsfoil_fortran")
    print("  # Access modules like: tsfoil_fortran.common_data")
    print("  # Access parameters like: tsfoil_fortran.common_data.n_mesh_points")
    print("  # Call functions like: tsfoil_fortran.common_data.initialize_common()")
    print("\nSuccessfully compiled modules:")
    for f in fortran_files:
        module_name = f.replace('.f90', '')
        print(f"  - {module_name}")
