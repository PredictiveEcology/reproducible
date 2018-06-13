Known issues: https://github.com/PredictiveEcology/reproducible/issues

version 0.2.0
=============

## New features

* new functions: 

    - `prepInputs` to aid in data downloading and preparation problems, solved in a reproducible, Cache-aware way.
    - `postProcess` which is a wrapper for sequences of several other new functions (`cropInputs`, `fixErrors`, `projectInputs`, `maskInputs`, `writeOutputs`, and `determineFilename`) 
    - `downloadFile` can handle Google Drive and ftp/http(s) files
    - `zipCache` and `mergeCache`
    - `compareNA` does comparisons with NA as a possible value e.g., `compareNA(c(1,NA), c(2, NA))` returns `FALSE, TRUE`

* Cache -- new features:

    - new arguments `showSimilar`, `verbose` which can help with debugging
    - new argument `useCache` which allows turning caching on and off at a high level (e.g., options("useCache"))
    - new argument `cacheId` which allows user to hard code a result from a Cache
    - deprecated arguments: `digestPathContent` --> `quick`, `compareRasterFileLength` --> `length`
    - Cache arguments now propagate inward to nested `Cache` function calls, unless explicitly set on the inner functions
    - more precise messages provided upon each use
    - many more `userTags` added automatically to cache entries so much more powerful searching via `showCache(userTags="something")`

* `clearCache` and `showCache` now give messages and require user intervention if request to `clearCache` would be large quantities of data deleted
* `memoise::memoise` now used on 3rd run through an identical `Cache` call, dramatically speeding up in most cases
* new options: `reproducible.cachePath`, `reproducible.quick`, `reproducible.useMemoise`, `reproducible.useCache`, `reproducible.useragent`, `reproducible.verbose`
* `asPath` has a new argument indicating how deep should the path be considered when included in caching (only relevant when `quick = TRUE`)
* New vignette on using Cache
* Cache is `parallel`-safe, meaning there are `tryCatch` around every attempt at writing to SQLite database so it can be used safely on multi-threaded machines
* bug fixes, unit tests, more `imports` for packages e.g., `stats`
* updates for R 3.6.0 compact storage of sequence vectors
* experimental pipes (`%>%`, `%C%`) and assign `%<%`
* several performance enhancements

version 0.1.4
=============

* `mergeCache`: a new function to merge two different Cache repositories
* `memoise::memoise` is now used on `loadFromLocalRepo`, meaning that the 3rd time `Cache()` is run on the same arguments (and the 2nd time in a session), the returned Cache will be from a RAM object via memoise. To stop this behaviour and use only disk-based Caching, set `options(reproducible.useMemoise = FALSE)` .
* Cache assign -- `%<%` can be used instead of normal assign, equivalent to `lhs <- Cache(rhs)`.
* new option: reproducible.verbose, set to FALSE by default, but if set to true may help understand caching behaviour, especially for complex highly nested code.
* all options now described in `?reproducible`.
* All Cache arguments other than FUN and ... will now propagate to internal, nested Cache calls, if they are not specified explicitly in each of the inner Cache calls. 
* Cached pipe operator `%C%` -- use to begin a pipe sequence, e.g., `Cache() %C% ...`
* Cache arg `sideEffect` can now be a path
* Cache arg `digestPathContent` default changed from FALSE (was for speed) to TRUE (for content accuracy)
* New function, `searchFull`, which shows the full search path, known alternatively as "scope", or "binding environments". It is where R will search for a function when requested by a user.
* Uses `memoise::memoise` for several functions (`loadFromLocalRepo`, `pkgDep`, `package_dependencies`, `available.packages`) for speed -- will impact memory at the expense of speed.
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
