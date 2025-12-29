from pathlib import Path

# Version info - must be first to avoid import issues during build
__version__ = (Path(__file__).parent / "VERSION").read_text().strip()

import os
import sys
import subprocess
import shutil
from importlib.resources import files

# Enable access to the tsfoil_fortran module
repo_path = os.path.dirname(os.path.abspath(__file__))
if repo_path not in sys.path:
    sys.path.append(repo_path)

def _check_and_compile_fortran():
    """
    Check if Fortran module exists, if not, try to compile it automatically
    """
    try:
        import tsfoil_fortran
        return True
    except ImportError:
        print("Fortran module not found. Attempting automatic compilation...")
        
        # Check if we have the compilation tools
        if not shutil.which('gfortran') or not shutil.which('f2py'):
            print("Warning: gfortran or f2py not found. Please install them or compile manually:")
            print("  sudo apt install gfortran")  # for Ubuntu/Debian
            print("  pip install numpy")  # f2py comes with numpy
            return False
        
        # Try to compile
        compile_script = Path(__file__).parent / "compile_f2py.py"
        if compile_script.exists():
            try:
                print("Running automatic Fortran compilation...")
                result = subprocess.run([
                    sys.executable, str(compile_script)
                ], check=True, capture_output=True, text=True)
                print("✓ Fortran compilation completed successfully!")
                
                # Try importing again
                import tsfoil_fortran
                return True
            except subprocess.CalledProcessError as e:
                print(f"✗ Automatic compilation failed: {e}")
                if e.stderr:
                    print(f"Error: {e.stderr}")
                return False
            except ImportError:
                print("✗ Compilation completed but module still not importable")
                return False
        else:
            print(f"✗ Compilation script not found: {compile_script}")
            return False

# Only try to compile Fortran module if we're not in a build environment
# This prevents issues during package building
if not os.environ.get('PEP517_BUILD_BACKEND'):
    # Try to compile Fortran module if needed
    _fortran_available = _check_and_compile_fortran()
else:
    _fortran_available = False

if _fortran_available:
    # Import refactored modules
    from .utils import clustcos
    from .pytsfoil import PyTSFoil
else:
    # Provide a stub class that shows helpful error message
    class PyTSFoil:
        def __init__(self, *args, **kwargs):
            raise ImportError(
                "Fortran module 'tsfoil_fortran' is not available. "
                "Please compile it manually:\n"
                f"  cd {Path(__file__).parent.parent}\n"
                f"  python {Path(__file__).parent / 'compile_f2py.py'}"
            )
    
__version__ = (files(__package__) / "VERSION").read_text().strip()

# Main interface is PyTSFoil, do not disclose other classes
__all__ = [
    'PyTSFoil', '__version__',
    # Utility functions
    'clustcos'
]
