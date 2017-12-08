Known issues: https://github.com/PredictiveEcology/reproducible/issues


version 0.1.4
=============

* Cache arg `sideEffect` can now be a path
* New `Require` function that attempts to create a lighter weight package reproducibility chain. This function is usable in a reproducible workflow: it includes both installing and loading of packages, it maintains version numbers, and uses smart caching for speed. In tests, it can evaluate whether 20 packages and their dependencies (~130 packages) are installed and loaded in ~0.1 seconds. This is much slower than running require on those 20 packages, but this may be fast enough for many purposes.
* remove dplyr from Imports

version 0.1.3
=============

* fix R CMD check errors on Solaris that were not previously resolved

version 0.1.2
=============

* fix R CMD check errors on Solaris
* fix bug in `digestRaster` affecting in-memory rasters
* move `rgdal` to Suggests

version 0.1.1
=============

* cleanup examples and *do* run them (per CRAN)
* add tests to ensure all exported (non-dot) functions have examples

version 0.1.0
=============

* A new package, which takes all caching utilities out of the `SpaDES` package.
