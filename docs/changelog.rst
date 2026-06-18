Changelog
=========

v0.3.6 (current)
-----------------

* Minor stability improvements to the divergence check logic.
* Updated default ``coupling_relax_final`` in :func:`~pytsfoil.run_airfoil_analysis`.

v0.3.5
-------

* Recommended stable release for TSD + IBL coupling.
* Divergence Check Correction (DCC) added (``use_divergence_check``).
* Improved TE δ\* repair and blending.

v0.3.3
-------

* Initial public release of the IBL coupling framework.
* Trailing-edge correction (TEC) and Correction of Full-Supersonic (CFS) added.

v0.3.0
-------

* Refactored IBL solver into a separate :class:`~pytsfoil.IBL` class.
* :func:`~pytsfoil.run_airfoil_analysis` wrapper introduced.

v0.2.8
-------

* Fully functional inviscid TSD solver (recommended over earlier v0.2.x releases).

v0.2.4
-------

* Basic TSD solver interface, first PyPI release.

v0.1.*
-------

* Initial release with basic TSD solver interface (not fully functional).
