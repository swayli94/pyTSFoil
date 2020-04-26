from setuptools import setup, find_packages
from pyTSFoil import __version__, __name__

with open('Readme.md') as f:
      long_description = f.read()

setup(name=__name__,
      version=__version__,
      description='This is a python interface of TSFOIL2 (transonic small disturbance theory for foils)',
      long_description=long_description,
      keywords=['CFD', 'TSFOIL2', 'transonic small disturbance theory'],
      download_url='https://github.com/swayli94/cfdpost/',
      license='MIT',
      author='Runze LI',
      author_email='swayli94@gmail.com',
      install_requires=['numpy', 'cst-modeling3d'],
      classifiers=[
            'Programming Language :: Python :: 3'
      ]
)

