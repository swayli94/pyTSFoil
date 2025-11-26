from setuptools import setup, find_packages
from setuptools.command.build_ext import build_ext
from setuptools.command.build_py import build_py
from setuptools.command.install import install
from pathlib import Path
import subprocess
import sys
import os

NAME = "pytsfoil"
VERSION = (Path(__file__).parent / NAME / "VERSION").read_text().strip()

class CustomBuildExt(build_ext):
    '''
    Custom command to compile Fortran extension modules
    '''
    
    def run(self):
        '''
        Run the custom compilation process
        '''
        self.compile_fortran_modules()
        super().run()
    
    def compile_fortran_modules(self):
        '''
        Compile Fortran modules
        '''
        print("Compiling Fortran modules...")
        
        # Get the path to the compilation script
        script_path = Path(__file__).parent / NAME / "compile_f2py.py"
        
        if not script_path.exists():
            print(f"Warning: Compilation script {script_path} does not exist")
            return
        
        # Run the compilation script
        try:
            result = subprocess.run([
                sys.executable, str(script_path)
            ], check=True, capture_output=True, text=True)
            print("Fortran modules compiled successfully!")
            if result.stdout:
                print(result.stdout)
        except subprocess.CalledProcessError as e:
            print(f"Compilation failed: {e}")
            if e.stdout:
                print(f"Standard output: {e.stdout}")
            if e.stderr:
                print(f"Error output: {e.stderr}")
            # Do not stop installation, just warn
            print("Warning: Fortran modules compilation failed, but installation will continue")

class CustomBuildPy(build_py):
    '''
    Custom command to build Python modules
    '''
    def run(self):
        '''
        Compile Fortran modules first, then build Python modules
        '''
        self.compile_fortran_modules()
        super().run()
    
    def compile_fortran_modules(self):
        '''
        Compile Fortran modules
        '''
        print("Compiling Fortran modules...")
        
        # Get the path to the compilation script
        script_path = Path(__file__).parent / NAME / "compile_f2py.py"
        
        if not script_path.exists():
            print(f"Warning: Compilation script {script_path} does not exist")
            return
        
        # Run the compilation script
        try:
            result = subprocess.run([
                sys.executable, str(script_path)
            ], check=True, capture_output=True, text=True)
            print("Fortran modules compiled successfully!")
            if result.stdout:
                print(result.stdout)
        except subprocess.CalledProcessError as e:
            print(f"Compilation failed: {e}")
            if e.stdout:
                print(f"Standard output: {e.stdout}")
            if e.stderr:
                print(f"Error output: {e.stderr}")
            # Do not stop installation, just warn
            print("Warning: Fortran modules compilation failed, but installation will continue")

class CustomInstall(install):
    '''
    Custom installation command
    '''
    
    def run(self):
        '''
        Compile Fortran modules first, then install
        '''
        self.compile_fortran_modules()
        super().run()
    
    def compile_fortran_modules(self):
        '''
        Compile Fortran modules
        '''
        print("Compiling Fortran modules...")
        
        # Get the path to the compilation script
        script_path = Path(__file__).parent / NAME / "compile_f2py.py"
        
        if not script_path.exists():
            print(f"Warning: Compilation script {script_path} does not exist")
            return
        
        # Run the compilation script
        try:
            result = subprocess.run([
                sys.executable, str(script_path)
            ], check=True, capture_output=True, text=True)
            print("Fortran modules compiled successfully!")
            if result.stdout:
                print(result.stdout)
        except subprocess.CalledProcessError as e:
            print(f"Compilation failed: {e}")
            if e.stdout:
                print(f"Standard output: {e.stdout}")
            if e.stderr:
                print(f"Error output: {e.stderr}")
            # Do not stop installation, just warn
            print("Warning: Fortran modules compilation failed, but installation will continue")

setup(name=NAME,
      version=VERSION,
      description='This is a python interface of TSFOIL2',
      keywords=['CFD', 'TSFOIL2', 'transonic small disturbance theory'],
      download_url='https://github.com/swayli94/pyTSFoil',
      license='MIT',
      author='Runze LI',
      author_email='swayli94@gmail.com',
      packages=find_packages(),
      package_data={"":['*.f','*.exe','*.so','*.f90','*.pyf']},
      install_requires=['numpy','scipy','matplotlib','meson'],
      setup_requires=['numpy'],  # f2py needs numpy
      classifiers=[
            'Programming Language :: Python :: 3'
      ],
      cmdclass={
          'build_ext': CustomBuildExt,
          'build_py': CustomBuildPy,
          'install': CustomInstall,
      },
      zip_safe=False,  # Ensure the package is not compressed, so the .so file can be loaded normally
)

