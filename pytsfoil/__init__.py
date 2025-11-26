import os
import sys
from importlib.resources import files

# Enable access to the tsfoil_fortran module
repo_path = os.path.dirname(os.path.abspath(__file__))
if repo_path not in sys.path:
    sys.path.append(repo_path)

from .pytsfoil import PyTSFoil

__version__ = (files(__package__) / "VERSION").read_text().strip()

__all__ = ['PyTSFoil', '__version__']
