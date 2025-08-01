import os
import sys

# Enable access to the tsfoil_fortran module
repo_path = os.path.dirname(os.path.abspath(__file__))
if repo_path not in sys.path:
    sys.path.append(repo_path)

