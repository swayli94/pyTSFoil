# Fortran source code structure

## Background and goal

### File structure

The original Fortran source code is stored in `pytsfoil/original_src`, these codes are already compatible with Python, and can be called from Python using `f2py`. However, the original Fortran source code is not very readable, and it contains some unimportant features that are not necessary for our project. Therefore, we need to modify the Fortran source code to make it more readable and remove unimportant features.

The current Fortran source code is stored in `pytsfoil/src`. It is modified from the original Fortran source code.

### Goal

This branch is to modify the Fortran source code to:

- remove unimportant features and make it more readable;
- replace some I/O Fortran functions with Python functions;
- add additional correction functions to improve the accuracy of the results.
