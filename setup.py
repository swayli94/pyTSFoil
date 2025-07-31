from setuptools import setup, find_packages
from pyTSFoil import __version__, __name__

setup(name=__name__,
      version=__version__,
      description='This is a python interface of TSFOIL2',
      keywords=['CFD', 'TSFOIL2', 'transonic small disturbance theory'],
      download_url='https://github.com/swayli94/pyTSFoil',
      license='MIT',
      author='Runze LI',
      author_email='swayli94@gmail.com',
      packages=find_packages(),
      package_data={"":['*.f','*.exe']},
      install_requires=['numpy'],
      classifiers=[
            'Programming Language :: Python :: 3'
      ]
)

