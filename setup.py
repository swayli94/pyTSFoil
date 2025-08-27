from setuptools import setup, find_packages
from pathlib import Path

NAME = "pyTSFoil"
VERSION = (Path(__file__).parent / NAME / "VERSION").read_text().strip()
setup(name=NAME,
      version=VERSION,
      description='This is a python interface of TSFOIL2',
      keywords=['CFD', 'TSFOIL2', 'transonic small disturbance theory'],
      download_url='https://github.com/swayli94/pyTSFoil',
      license='MIT',
      author='Runze LI',
      author_email='swayli94@gmail.com',
      packages=find_packages(),
      package_data={"":['*.f','*.exe']},
      install_requires=['numpy','scipy','matplotlib'],
      classifiers=[
            'Programming Language :: Python :: 3'
      ]
)

