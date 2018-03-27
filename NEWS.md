Known issues: https://github.com/PredictiveEcology/reproducible/issues

version 0.1.4
=============

* memoise::memoise is now used on loadFromLocalRepo, meaning that the 3rd time a Cache function is run on the same arguments (and the 2nd time in a session), the returned Cache will be from a RAM object via memoise. To stop this behaviour and use only disk-based Caching, set `options(reproducible.useMemoise = FALSE)` 
* Cache assign -- %<% can be used instead of normal assign, equivalent to lhss <- Cache(rhs)
* new option: reproducible.verbose, set to FALSE by default, but if set to true may help understand caching behaviour, especially for complex highly nested code
* all options now described in ?reproducible
* All Cache arguments other than FUN and ... will now propagate to internal, nested Cache calls, if they are not specified explicitly in each of the inner Cache calls. 
* Cached pipe operator %C% -- use to begin a pipe sequence, e.g., `Cache() %C% ...`
* Cache arg `sideEffect` can now be a path
* Cache arg `digestPathContent` default changed from FALSE (was for speed) to TRUE (for content accuracy)
* New function, `searchFull`, which shows the full search path, known alternatively as "scope", or "binding environments". It is where R will search for a function when requested by a user.
* Uses memoise::memoise for several functions (`loadFromLocalRepo`, `pkgDep`, `package_dependencies`, `available.packages`) for speed -- will impact memory at the expense of speed
* New `Require` function 

    - attempts to create a lighter weight package reproducibility chain. This function is usable in a reproducible workflow: it includes both installing and loading of packages, it can maintain version numbers, and uses smart caching for speed. In tests, it can evaluate whether 20 packages and their dependencies (~130 packages) are installed and loaded quickly (i.e., if all TRUE,  ~0.1 seconds). This is much slower than running `require` on those 20 packages, but `require` does not check for dependencies and deal with them if missing: it just errors. This speed should be fast enough for many purposes.
    - can accept uncommented name, if length 1.
    
* remove `dplyr` from Imports
* Add `RCurl` to Imports
* change name of `digestRaster` to `.digestRaster`

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
