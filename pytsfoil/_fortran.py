"""
Centralized Fortran module import

This module handles the import of the tsfoil_fortran module and provides
a single point of access for all other modules. This avoids duplicating
the import logic and error handling across multiple files.
"""

import sys

try:
    import tsfoil_fortran as tsf
except ImportError as e:
    print("ERROR: Could not import tsfoil_fortran module!")
    print(f"Import error: {e}")
    print()
    print("Make sure you have compiled the Fortran modules with f2py:")
    print("  python3 pyTSFoil/compile_f2py.py")
    sys.exit(1)

__all__ = ['tsf']

