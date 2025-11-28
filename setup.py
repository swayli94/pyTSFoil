from setuptools import setup

# Minimal setup.py - metadata is now in pyproject.toml
# Fortran compilation happens at runtime during first import
setup(
    zip_safe=False,  # Ensure the package is not compressed, so the .so file can be loaded normally
)

