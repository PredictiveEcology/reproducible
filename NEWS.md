Known issues: https://github.com/PredictiveEcology/reproducible/issues

version 0.2.7
=============

## New features

* `CHECKSUMS.txt` should now be ordered consistently across operating systems (note: `base::order` will not succeed in doing this --> now using `.orderDotsUnderscoreFirst`)
* `cloudSyncCache` has a new argument: `cacheIds`. Now user can control entries by `cacheId`, so  can delete/upload individual objects by `cacheId`
* Experimental support within the `postProcess` family for `sf` class objects

## bug fixes
* mostly minor
* `cloudCache` bugfixes for more cases

version 0.2.6
=============

## Dependency changes

* remove `tibble` from Imports as it's no longer being used

## New features

* remove `%>%` pipe that was long ago deprecated. User should use `%C%` if they want a pipe that is Cache-aware. See examples.
* Full rewrite of all `options` descriptions now in `reproducible`, see `?reproducibleOptions`
* now `cacheRepo` and `options("reproducible.cachePath")` can take a vector of paths. Similar to how .libPaths() works for libraries, `Cache` will search first in the first entry in the `cacheRepo`, then the second etc. until it finds an entry. It will only write to the first entry.
* new value for the option: `options("reproducible.useCache" = "devMode")`. The point of this mode is to facilitate using the Cache when functions and datasets are continually in flux, and old Cache entries are likely stale very often. In `devMode`, the cache mechanism will work as normal if the Cache call is the first time for a function OR if it successfully finds a copy in the cache based on the normal Cache mechanism. It *differs* from the normal Cache if the Cache call does *not* find a copy in the `cacheRepo`, but it does find an entry that matches based on `userTags`. In this case, it will delete the old entry in the `cacheRepo` (identified based on matching `userTags`), then continue with normal `Cache`. For this to work correctly, `userTags` must be unique for each function call. This should be used with caution as it is still experimental.
* change to how hashes are calculated. This will cause existing caches to not work correctly. To allow a user to keep old behaviour (during a transition period), the "old" algorigthm can be used, with `options("reproducible.useNewDigestAlgorithm" = FALSE)`. There is a message of this change on package load.
* add experimental `cloud*` functions, especially `cloudCache` which allows sharing of Cache among collaborators. Currently only works with `googledrive`
* updated `assessDataType` to consolidate `assessDataTypeGDAL` and `assessDataType` into single function (#71, @ianmseddy)
* `cc`: new function -- a shortcut for some commonly used options for `clearCache()`
* added experimental capacity for `prepInputs` to handle `.rar` archives, on systems with correct binaries to deal with them (#86, @tati-micheletti)
* remove `fastdigest::fastdigest` as it is not return the identical hash across operating systems

## Bug fixes

* `prepInputs` on GIS objects that don't use `raster::raster` to load object were skipping `postProcess`. Fixed.
* under some circumstances, the `prepInputs` would cause virtually all entries in `CHECKSUMS.txt` to be deleted. 2 cases where this happened were identified and corrected. 
* `data.table` class objects would give an error sometimes due to use of `attr(DT)`. Internally, attributes are now added with `data.table::setattr` to deal with this.
* calling `gdalwarp` from `prostProcess` now correctly matches extent (#73, @tati-micheletti)
* files from url that have unknown extension are now guessed with by `preProcess` (#92, @tati-micheletti)

version 0.2.5
=============

## Dependency changes

* Added `remotes` to Imports and removed `devtools`

## New features

* New value possible for `options(reproducible.useCache = 'overwrite')`, which
  allows use of `Cache` in cases where the function call has an entry in the `cacheRepo`,
  will purge it and add the output of the current call instead.
* New option `reproducible.inputPaths` (default `NULL`) and `reproducible.inputPathsRecursive`
  (default `FALSE`), which will be used in `prepInputs` as possible directory sources
  (searched recursively or not) for files being downloaded/extracted/prepared.
  This allows the using of local copies of files in (an)other location(s) instead
  of downloading them. If local location does not have the required files,
  it will proceed to download so there is little cost in setting this option.
  If files do exist on local system, the function will attempt to use a hardlink before making a copy.
* `dlGoogle()` now sets `options(httr_oob_default = TRUE)` if using Rstudio Server.
* Files in `CHECKSUMS` now sorted alphabetically.
* `Checksums` can now have a `CHECKSUMS.txt` file located in a different place than the `destinationPath`
* Attempt to select raster resampling method based on raster type if no method supplied (#63, @ianmseddy)
* `projectInputs` 
* new function `assessDataTypeGDAL`, used in `postProcess`, to identify smallest `datatype` for large Raster* objects passed to GDAL system call

    - when masking and reprojecting large `Raster` objects, enact `gdalwarp` system call if `raster::canProcessInMemory(x,4) = FALSE` for faster and memory-safe processing 
    - better handling of various data types in `Raster` objects, including factor rasters

## Bug fixes

* Work around internally inside `extractFromArchive` for large (>2GB) zip files.
  In the `R` help manual, `unzip` fails for zip files >2GB.
  This uses a system call if the zip file is too large and fails using `base::unzip`.
* Work around for `raster::getData` issues.
* Speed up of `Cache()` when deeply nested, due to `grep(sys.calls(), ...)` that would take long and hang.
* Bugfix for `preProcess(url = NULL)` (#65, @tati-micheletti)
* Improved memory performance of `clearCache` (#67), especially for large `Raster` objects that are stored as binary `R` files (i.e., `.rda`)
* Other minor bugfixes

## Other changes

* Deal with new `raster` package changes in development version of `raster` package
* Added checks for float point number issues in raster resolutions produced by `raster::projectRaster`
* `.robustDigest` now does not include `Cache`-added attributes
* Additional tests for `preProcess()` (#68, @tati-micheletti)
* Many new unit tests written, which caught several minor bugs

version 0.2.3
=============

* fix and skip downloading test on CRAN

version 0.2.2
=============

## Dependency changes

* Add `future` to Suggests.

## New features

* new option on non-Windows OSs to use `future` for `Cache` saving to SQLite database, via `options("reproducible.futurePlan")`, if the `future` package is installed. This is `FALSE` by default.
* If a `do.call` function is Cached, previously, it would be labelled in the database as `do.call`. Now it attempts to extract the actual function being called by the `do.call`. Messaging is similarly changed.
* new option `reproducible.ask`, logical, indicating whether `clearCache` should ask for deletions when in an interactive session
* `prepInputs`, `preProcess` and `downloadFile` now have `dlFun`, to pass a custom function for downloading (e.g., "raster::getData")
* `prepInputs` will automatically use `readRDS` if the file is a `.rds`.
* `prepInputs` will return a `list` if `fun = "base::load"`, with a message; can still pass an `envir` to obtain standard behaviour of `base::load`.
* `clearCache` - new argument `ask`.
* new function `assessDataType`, used in `postProcess`, to identify smallest `datatype` for Raster* objects, if user does not pass an explicit `datatype` in `prepInputs` or `postProcess` (#39, @CeresBarros).

## Bug fixes

* fix problems with tests introduced by recent `git2r` update (@stewid, #36).
* `.prepareRasterBackedFile` -- now will postpend an incremented numeric to a cached copy of a file-backed Raster object, if it already exists. This mirrors the behaviour of the `.rda` file. Previously, if two Cache events returned the same file name backing a Raster object, even if the content was different, it would allow the same file name. If either cached object was deleted, therefore, it would cause the other one to break as its file-backing would be missing.
* options were wrongly pointing to `spades.XXX` and should have been `reproducible.XXX`.
* `copyFile` did not perform correctly under all cases; now better handling of these cases, often sending to `file.copy` (slower, but more reliable)
* `extractFromArchive` needed a new `Checksum` function call under some circumstances
* several other minor bug fixes.
* `extractFromArchive` -- when dealing with nested zips, not all args were passed in recursively (#37, @CeresBarros)
* `prepInputs` -- arguments that were same as `Cache` were not being correctly passed internally to `Cache`, and if wrapped in Cache, it was not passed into prepInputs. Fixed.
* `.prepareFileBackedRaster` was failing in some cases (specifically if it was inside a `do.call`)  (#40, @CeresBarros).
* `Cache` was failing under some cases of `Cache(do.call, ...)`. Fixed.
* `Cache` -- when arguments to Cache were the same as the arguments in `FUN`, Cache would "take" them. Now, they are correctly passed to the `FUN`.
* `preProcess` -- writing to checksums may have produced a warning if `CHECKSUMS.txt` was not present. Now it does not.
* numerous other minor bugfixes

## Other changes

- most tests now use a standardized approach to attaching libraries, creating objects, paths, enabling easier, error resistant test building

version 0.2.1
=============

## New features

* new functions: 

    - `convertPaths` and `convertRasterPaths` to assist with renaming moved files.
    
* `prepInputs` -- new features

    - `alsoExtract` now has more options (`NULL`, `NA`, `"similar"`) and defaults to extracting all files in an archive (`NULL`).
    - skips `postProcess` altogether if no `studyArea` or `rasterToMatch`. Previously, this would invoke Cache even if there was nothing to `postProcess`.
    
## Bug fixes

* `copyFile` correctly handles directory names containing spaces.
* `makeMemoisable` fixed to handle additional edge cases.
* other minor bug fixes.

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

* `checksums` now returns a data.table with the same columns whether `write = TRUE` or `write = FALSE`. 
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
