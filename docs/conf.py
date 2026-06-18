# Configuration file for the Sphinx documentation builder.
# https://www.sphinx-doc.org/en/master/usage/configuration.html

import os
import sys

# Make the package importable without installing it
sys.path.insert(0, os.path.abspath('..'))

# -- Project information -------------------------------------------------------

project = 'pyTSFoil'
copyright = '2024, Runze LI'
author = 'Runze LI'
release = '0.3.5'
version = '0.3'

# -- General configuration -----------------------------------------------------

extensions = [
    'sphinx.ext.autodoc',
    'sphinx.ext.autosummary',
    'sphinx.ext.napoleon',      # NumPy / Google docstring support
    'numpydoc',
    'sphinx.ext.intersphinx',   # cross-links to NumPy, SciPy, etc.
    'sphinx.ext.mathjax',       # LaTeX math in HTML
    # sphinx.ext.viewcode is intentionally omitted:
    # it imports modules outside the autodoc mock context and triggers
    # sys.exit(1) in pytsfoil/pytsfoil.py when tsfoil_fortran is absent.
]

templates_path = ['_templates']
exclude_patterns = ['_build', 'Thumbs.db', '.DS_Store']

# -- Autodoc -------------------------------------------------------------------

# The Fortran extension module may not be compiled in the build environment.
# Mock it so that autodoc can import pytsfoil without errors.
autodoc_mock_imports = ['tsfoil_fortran']

autodoc_default_options = {
    'members': True,
    'undoc-members': False,
    'show-inheritance': True,
    'member-order': 'bysource',
    'special-members': '__init__',
}

autosummary_generate = True

# -- numpydoc ------------------------------------------------------------------

numpydoc_show_class_members = False
numpydoc_class_members_toctree = False

# -- Intersphinx ---------------------------------------------------------------

intersphinx_mapping = {
    'python':     ('https://docs.python.org/3', None),
    'numpy':      ('https://numpy.org/doc/stable', None),
    'scipy':      ('https://docs.scipy.org/doc/scipy', None),
    'matplotlib': ('https://matplotlib.org/stable', None),
}

# -- HTML output ---------------------------------------------------------------

html_theme = 'furo'

html_theme_options = {
    'sidebar_hide_name': False,
    'navigation_with_keys': True,
    'source_repository': 'https://github.com/swayli94/pyTSFoil',
    'source_branch': 'main',
    'source_directory': 'docs/',
}

html_title = f'pyTSFoil {release}'
html_static_path = ['_static']

html_css_files = ['custom.css']
