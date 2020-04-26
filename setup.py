from setuptools import setup, find_packages
from pyTSFoil import __version__, __name__

with open('README.md') as f:
      long_description = f.read()

setup(name=__name__,
      version=__version__,
      description='This is a python interface of TSFOIL2',
#     long_description=long_description,        # somehow pypi does not recognize this long_description
      keywords=['CFD', 'TSFOIL2', 'transonic small disturbance theory'],
      download_url='https://github.com/swayli94/pyTSFoil',
      license='MIT',
      author='Runze LI',
      author_email='swayli94@gmail.com',
      packages=find_packages(),
      package_data={"":['*.f','*.exe']},
      install_requires=['numpy', 'cst-modeling3d'],
      classifiers=[
            'Programming Language :: Python :: 3'
      ]
)

