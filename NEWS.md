# reproducible 2.0.11

## Changes
* default for `gdalMask` has changed default for "touches". Now has equivalent for `terra::mask(..., touches = TRUE)`, using `"-wo CUTLINE_ALL_TOUCHED=TRUE"`
* `gdalProject` now uses 2 threads, setting `"-wo NUM_THREADS=2"`; can be changed by user with `options("reproducible.gdalwarpThreads" = X)`; see `?reproducibleOptions`
* `showSimilar` (e.g., `options(reproducible.showSimilar = 1)`) now preferentially shows the most recent item in cache if there are several with equivalent matching.


# reproducible 2.0.10

## Bug fixes
* critical bugfixes for file-backed SpatRaster objects

# reproducible 2.0.9

## Enhancements
* new function `isUpdated()` to determine whether a cached object has been updated;
* `makeRelative()` is now exported for use downstream (e.g., `SpaDES.core`);
* new functions `getRelative()` and `normPathRel()` for improved symlink handling (#362);
* messaging is improved for `Cache` with the function named instead of just `cacheId`
* messaging for `prepInputs`: minor changes
* more edge cases for `Checksums` dealt with, so fewer unneeded downloads
* `wrapSpatRaster` (`wrap` for file-backed `spatRaster` objects) fixes for more edge cases
* `postProcessTo` can now use `sf::gdal_utils` for the case of `from` is a gridded object and `to` is a polygon vector. This appears to be between 2x and 10x faster in tests.
* `postProcessTo` does a pre-crop (with buffer) to make the `projectTo` faster. When both `from` and `to` are vector objects, this pre-crop appears to create slivers in some cases. This step is now skipped for these cases.
* `Cache` can now deal with unnamed functions, e.g., `Cache((function(x) x)(1))`. It will be refered to as "headless".
* `terra` would fail if internet was unavailable, even when internet is not necessary, due to needing to retrieve projection information. Many cases where this happens will now divert to use `sf`.
* `Cache` can now skip calculating `objSize`, which can take a non-trivial amount of time for large, complicated objects; see `reproducibleOptions()`

## Bug fixes
* Filenames for some classes returned ""; now returns NULL so character vectors are only pointers to files
* Cache on a terra object that writes file to disk, when `quick` argument is specified was failing, always creating the same object; fixed with #PR368
* `useDBI` was incorrectly used if a user had set the option prior to package loading. Now works as expected.
* several other minor
* `preProcess` deals better with more cases of nested paths in archives.
* more edge cases corrected for `inputPaths`

# reproducible 2.0.8

## Enhancements
* minor formatting changes
* sometimes a cache entry gets corrupted. Previously, a message was supplied on how to fix; now this is just tried directly instead of just suggesting a user do it.

## Bug fixes
* only use character strings when comparing `getRVersion() <= "XXX"` 
* fixes for `assessDataType` for categorical (factor) `Raster` and `SpatRaster`

# reproducible 2.0.7

## Enhancements
* Address change in `round` with `R > 4.3.1`; now a primitive, that does method dispatch. Failure was identified with unit tests, by Luke Tierney who was making the change in `base::round`.

## Bug fixes
* several identified and fixed (PRs by Ceres Barros, notably, PRs #341, #342, #343). These fix missing argument in a `.unwrap` call, and missing check in `preProcess`, when `targetFilePath` was `NULL`.
* minor documentation updates

# reproducible 2.0.5

## Enhancements
* Updates of `Copy` & new `.wrap`, `.unwrap` generics and methods to *wrap* classes that don't save well to disk as is. This uses the name similar to `terra::wrap`, but with slight differences internally to allow for `SpatRaster` objects who are file-backed and must have their files moved when they are unwrapped.
* `loadFiles` updated for more cases
* convert to using `withr` throughout testing for cleaning up
* more methods for `Filename` added, including for `Path` class
* `Cache(..., useCloud = TRUE)` had many cases that were not working; known cases are now working. Also, now file from file-backed cases are now placed inside the `cacheOutputs` folder rather than inside a separate folder (used to be "rasters")


## Bugfixes
* several small for edge cases


## Dependency changes
* none

# reproducible 2.0.4

## Enhancements
- `reproducible.useFuture` now defaults to `"multisession"`
- updated tests to deal with `data.table` development branch (#314)
- removed all use of `data.table::setattr` to deal with "modified compiler constants" issue that was detected during CRAN checks
- Improvements with testing using GitHub Actions

## Bugfixes
- `preProcess` failed when `googledrive` url filename could be found, but `destinationPath` was not `"."`
- `normPath` had different behaviour on *nix-alikes and Windows. Now it is the same.
- `SpatRaster` objects if saved to a specific, non relative (to `getwd()`) path would not be recovered correctly (#316)
- Several other Issues that addressed edge cases for `prepInputs` and family.

# reproducible 2.0.2

## Enhancements
- new optional backend for `Cache` via `options(reproducible.useDBI = FALSE)` is single data files with the same `basename` as the cached object, i.e., with the same `cacheId` in the file name. This is a replacement for `RSQLite` and will likely become the default in the next release. This approach makes cloud caching easier as all metadata are available in small binary files for each cached object. This is simpler, faster and creates far fewer package dependencies (now 11 recursive; before 27 recursive). If a user has `DBI` and `RSQLite` installed, then the backend will default to use these currently, i.e., the previous behaviour. The user can change the backend without loss of Cache data. 
- moved `raster` and `sp` to `Suggests`; no more internal functions use these. User can still work with `Raster` and `sp` class objects as before.
- `preProcess` can now handle Google docs files, if `type = ...` is passed.
- `postProcess` now uses `terra` and `sf` internally (with #253) throughout the family of `postProcess` functions. The previous `*Input` and `*Output` functions now redirect to the new `*To*` functions. These are faster, more stable, and cover vastly more cases than the previous `*Inputs` family. The old backends no longer work as before.
- minor functions to assist with transition from `raster` to `terra`: `maxFn`, `minFn`, `rasterRead`
- `.dealWithClass` and `.dealWithClassOnRecovery` are now exported generics, with several methods here, notably, list, environment, default
- other miscellaneous changes to deal with `raster` to `terra` transition (e.g. `studyAreaName` can deal with `SpatVector`)
- `prepInputs` now deals with archives that have sub-folder structure are now dealt with correctly in all examples and tests esp. #181. 
- `prepInputs` can now deal with `.gdb` files. Though, it is limited to `sf` out of the box, so e.g., Raster layers inside `gdb` files are not supported (yet?). User can pass `fun = NA` to not try to load it, but at least have the `.gdb` file locally on disk.
- `hardLinkOrCopy` now uses `linkOrCopy(symlink = FALSE)`; more cases dealt with especially nested directory structures that do not exist in the `to`.
- many GitHub issues closed after transition to using `terra` and `sf`. 
- `preProcess` had multiple changes. The following now work: archives with subfolders, archives with subfolders with identical basenames (different dirnames), gdb files, other files where `targetFile` is a directory.
- ~40 issues were closed with current release.
- code coverage now approaching 85%
- substantial changes to `preProcess` for minor efficiency gains, edge cases, code cleaning
- new function `CacheGeo` that weaves together `prepInputs` and `Cache` to create a geo-spatial caching. See help and examples.
- `maskTo` now allows `touches` arg for `terra::mask`
- `Spatial` class is also "fixed" in `fixErrorsIn`
- `prepInputs` and `preProcess` now capture `dlFun`, so user can pass unquoted `dlFun`
- `Copy` method for `SpatRaster`, with and without file-backing
- `Cache(..., useCloud = TRUE)` reworked so appears to be more robust than previously.
- `maskTo` now works even if `to` is larger than `from`
- `netCDF` works with `prepInputs`; thanks to user nbsmokee with PR #300.

## Dependency changes
- no spatial packages are automatically installed any more; to work with `prepInputs` and family, the user will have to install `terra` and `sf` at a minimum.
- `terra`, `sf` are in `Suggests`
- removed entirely: `fasterize`, `fpCompare`, `magrittr`
- moved to `Suggests`: `raster`, `sp`, `rlang`
- A normal (minimal) install of `reproducible` no longer installs `DBI`, nor does it use `RSQLite`. All cache repositories database files will be in binary individual files in the `cacheOutputs` file. If a user has `DBI` and a `SQLite` engine, then the previous behaviour will be used. 

## Defunct 
- `reproducible.useNewDigestAlgorithm` is not longer an option as the old algorithms do not work reliably.

## Defunct and removed
- removed `assessDataTypeGDAL()`, `clearStubArtifacts()`, 
- removed non-exported `digestRasterLayer2()`; `evalArgsOnly()`; `.getSourceURL()`; `.getTargetCRS()`; `.checkSums()`, `.groupedMessage()`; `.checkForAuxililaryFiles()`
- `option("reproducible.polygonShortcut")` removed

## Non exported function changes
- `.basename` renamed to `basename2`


## Bugfixes
- `Cache` was incorrectly dealing with `environment` and `environment-like` objects. Since some objects, e.g., `Spat*` objects in `terra`, must be wrapped prior to saving, environments must be scanned for these classes of objects prior to saving. This previously only occurred for `list` objects;
- When working with revdep `SpaDES.core`, there were some cases where the `Cache` was failing as it could not find the module name;
- during transition from `postProcess` (using `raster` and `sp`) to `postProcessTo`, some cases are falling through the cracks; these have being addressed.


# reproducible 1.2.16

## Dependency changes
- none

## Enhancements
- `Cache` now captures the first argument passed to it without evaluating it, so `Cache(rnorm(1))` now works as expected.
- As a result of previous, `Cache` now works with base pipe |> (with R >= 4.1). 
- Due to some internal changes in the way arguments are evaluated and digested, there may be some cache entries that will be rerun. However, in simple cases of `FUN` passed to `Cache`, there should be no problems with previous cache databases being successfully recovered. 
- Added more unit tests
- Reworked `Cache` internals so that digesting is more accurate, as the correct methods for functions are more accurately found, objects within functions are more precisely evaluated.
- Improved documentation:
  - Examples were reworked, replaced, improved;
  - All user-facing exported functions and methods now have complete documentation;
  - Added `()` in DESCRIPTION for functions;
  - Added `\value` in `.Rd` files for exported methods (structure, the class, the output meaning);
  - Remove commented code in examples.

## Bug fixes
- `postProcess` now also checks resolution when assessing whether to project 
- `prepInputs` has an internal `Cache` call for loading the object into memory; this was incorrectly evaluating all files if there were more than one file downloaded and extracted. This resulted in cases, e.g. shapefiles, being considered identical if they had the identical geometries, even if their data were different. This is fixed now as it uses the digest of all files extracted.

## Deprecated and defunct
- remove defunct argument `digestPathContent` from `Cache`
- `options("reproducible.useGDAL")` is now deprecated; the package is moving towards `terra`. 

# reproducible 1.2.11

## Dependency changes
- none

## Enhancements
- none

## Bug fixes
- fix tests for `postProcessTo` to deal with changes in GDAL/PROJ/GEOS (#253; @rsbivand)
- fixed issue with masking

# reproducible 1.2.10

## Dependency changes
- Drop support for R 3.6 (#230)
- remove `gdalUtilities`, `gdalUtils`, and `rgeos` from `Suggests`
- Added minimum versions of `raster` and `terra`, because previous versions were causing collisions.

## Enhancements
- all direct calls to GDAL are removed: only `terra` and `sf` are used throughout
- `prepInputs` can now take `fun` as a quoted expression on `x`, the object loaded by `dlFun` in `preProcess`
- `preProcess` arg `dlFun` can now be a quoted expression
- changes to the internals and outputs of `objSize`; now is primarily a wrapper around `lobstr::obj_size`, but has an option to get more detail for lists and environments.
- `.robustDigest` now deals explicitly with numerics, which digest differently on different OSs. Namely, they get rounded prior to digesting. Through trial and error, it was found that setting `options("reproducible.digestDigits" = 7)` was sufficient for all known cases. Rounding to deeper than 7 decimal places was insufficient. There are also new methods for `language`, `integer`, `data.frame` (which does each column one at a time to address the numeric issue)
- New version of `postProcess` called `postProcessTo`. This will eventually replace `postProcess` as it is much faster in all cases and simpler code base thanks to the fantastic work of Robert Hijmans (`terra`) and all the upstream work that `terra` relies on
- Minor message updates, especially for "adding to memoised copy...". The three dots made it seem like it was taking a long time. When in reality, it is instantaneous and is the last thing that happens in the `Cache` call. If there is a delay after this message, then it is the code following the `Cache` call that is (silently) slow.
- `retry` can now return a named list for the `exprBetween`, which allows for more than one object to be modified between retries.
 
## Bug fixes
- `.robustDigest` was removing Cache attributes from objects under many conditions, when it should have left them there. It is unclear what the issues were, as this would likely not have impacted `Cache`. Now these attributes are left on.
- `data.table` objects appear to not be recovered correctly from disk (e.g., from Cache repository. We have added `data.table::copy` when recovering from Cache repository
- `clearCache` and `cc` did not correctly remove file-backed raster files (when not clearing whole CacheRepo); this may have resulted in a proliferation of files, each a filename with an underscore and a new higher number. This fix should eliminate this problem.
- deal with development versions of GDAL in `getGDALVersion()` (#239)
- fix issue with `maskInputs()` when not passing `rasterToMatch`.
- fix issue with `isna.SpatialFix` when using `postProcess.quosure`


# reproducible 1.2.8

## Dependency changes
- `lwgeom` now a suggested package

## Enhancements
- `terra` class objects can now be correctly saved and recovered by `Cache`
- `fixErrors` can now distinguish `testValidity = NA` meaning don't fix errors and `testValidity = FALSE` run buffering which fixes many errors, but don't test whether there are any invalid polygons first (maybe slow), or `testValidity = TRUE` meaning test for validity, then if some are invalid, then run buffer.
- Change default option to `reproducible.useNewDigestAlgorithm = 2` which will have user visible changes. To keep old behaviour, set `options(reproducible.useNewDigestAlgorithm = 1)`
- minor changes to messaging when `options(reproducible.showSimilar)` is set. It is now more compact e.g., 3 lines instead of 5.
- added `sf` methods to `studyAreaName`

## Bug fixes
- A small, but very impactful bug that created false positive `Cache` returns; i.e., a 2nd time through a Cache would return a cached copy, when some of the arguments were different. It occurred for when the differences were in unnamed arguments only.

# reproducible 1.2.7

`reproducible` will be slowly changing the defaults for vector GIS datasets from the `sp` package to the `sf` package. 
There is a large user-visible change that will come (in the next release), which will cause `prepInputs` to read `.shp` files with `sf::st_read` instead of `raster::shapefile`, as it is much faster. To change now, set  `options("reproducible.shapefileRead" = "sf::st_read")`

## Enhancements
- default `fun` in `prepInputs` for shapefiles (`.shp`) is now `sf::st_read` if the system has `sf` installed. This can be overridden with `options("reproducible.shapefileRead" = "raster::shapefile")`, and this is indicated with a message at the moment this is occurring, as it will cause different behaviour.
- `quick` argument in `Cache` can now be a character vector, allowing individual character arguments to be digested as character vectors and others to be digested as files located at the specified path as represented by the character vector.
- `objSize` previously included objects in `namespaces`, `baseenv` and `emptyenv`, so it was generally too large. Now uses the same criteria as `pryr::object_size`
- improvements with messaging when `unzip` missing (thanks to @CeresBarros #202)
- while unzipping, will also search for `7z.exe` on Windows if the object is larger than 2GB, if can't find `unzip`.
- `fun` argument in `prepInputs` and family can now be a quoted expression.
- `archive` argument in `prepInputs` can now be `NA` which means to treat the file downloaded not as an archive, even if it has a `.zip` file extension
- many minor improvements to functioning of esp. `prepInputs`
- speed improvements during `postProcess` especially for very large objects (>5GB tested). Previously, it was running many `fixErrors` calls; now only calls `fixErrors` on fail of the proximate call (e.g., st_crop or whatever)
- `retry` now has a new argument `exprBetween` to allow for doing something after the fail (for example, if an operation fails, e.g., `st_crop`, then run `fixErrors`, then return back to `st_crop` for the retry)
- `Cache` now has MUCH better nested levels detection, with messaging... and control of how deep the Caching goes seems good, via useCache = 2 will only Cache 2 levels in...
- `archive` argument in `prepInputs` family can now be NA ... meaning do not try to unzip even if it is a `.zip` file or other standard archive extension
- `gdb.zip` files (e.g., a file with a .zip extension, but that should not be opened with an unzip-type program) can now be opened with `prepInputs(url = "whateverUrl", archive = NA, fun = "sf::st_read")`
- `fun` argument in `prepInputs` can now be a quoted function call.
- `preProcess` now does a better job with large archives that can't be correctly handled with the default `zip` and `unzip` with R, by trying `system2` calls to possible `7z.exe` or other options on Linux-alikes.

## Bug fixes
- `Copy` generic no longer has `fileBackedDir` argument. It is now passed through with the `...`. This was creating a bug with some cases where `fileBackedDir` was not being correctly executed.
- `fixErrors()` now better handles `sf` polygons with mixed geometries that include points.
- inadvertent deleting of file-backed rasters in multi-filed stacks during `Cache`
- `writeOutputs.Raster` attempted to change `datatype` of `Raster` class objects using the setReplacement `dataType<-`, without subsequently writing to disk via `writeRaster`. This created bad values in the `Raster*` object. This now performs a `writeRaster` if there is a `datatype` passed to `writeOutputs` e.g., through `prepInputs` or `postProcess`.
- `updateSlotFilename` has many more tests.
- `prepInputs(..., fun = NA)` now is the correct specification for "do not load object into R". This essentially replicates `preProcess` with same arguments.
- several minor bugfixes
- `Copy` did not correctly copy `RasterStack`s when some of the `RasterLayer` objects were in memory, some on disk; `raster::fromDisk` returned `FALSE` in those cases, so `Copy` didn't occur on the file-backed layer files. Using `Filenames` instead to determine if there are any files that need copying.

# reproducible 1.2.6

## Enhancements
- Optional (and may be default soon) -- An update to the internal digesting for file-backed Rasters that should be substantially faster, and smaller disk footprint. Set using `options("reproducible.useNewDigestAlgorithm" = 2)`
- changed default of `options("reproducible.polygonShortcut" = FALSE)` as there were still too many edge cases that were not covered.

## Bug fix
- fixed an error with *rcnst* on CRAN
- `RasterStack` objects with a single file (thus acting like a `RasterBrick`) are now handled correctly by `Cache` and `prepInputs` families, especially with new `options("reproducible.useNewDigestAlgorithm" = 2)`, though in tests, it worked with default also
- `RSQLite` now uses a RNG during `dbAppend`; this affected 2 tests (#185).

# reproducible 1.2.4

## Bug fix
- typo in date

# reproducible 1.2.3

## Bug fix
- minor url fix

# reproducible 1.2.2

## New features
- removed several uses of `rgeos`
- moved `paddedFloatToChar` to reproducible from SpaDES.core.
- increased code coverage
- Pull in legacy `%>%` code from `magrittr` to allow the cached alternative, `%C%`. With new `magrittr` pipe now in compiled source code, more of the legacy code is required here.


## Bug fixes
- several minor

# reproducible 1.2.1

## New features
- harmonized message colours that are use adjustable via options: `reproducible.messageColourPrepInputs` for all `prepInputs` functions;  `reproducible.messageColourCache` for all `Cache` functions; and `reproducible.messageColourQuestion` for questions that require user input. Defaults are `cyan`, `blue` and `green` respectively. These are user-visible colour changes.
- improved messaging for `Cache` cases where a `file.link` is used instead of saving.
- with improved messaging, now `options(reproducible.verbose = 0)` will turn off almost all messaging.
- `postProcess` and family now have `filename2 = NULL` as the default, so not saved to disk. This is a change.
- `verbose` is now an argument throughout, whose default is `getOption(reproducible.verbose)`, which is set by default to `1`. Thus, individual function calls can be more or less verbose, or the whole session via option. 

## Bug fixes
- `RasterStack` objects were not correctly saved to disk under some conditions in `postProcess` - fixed
- several minor

# reproducible 1.2.0

## New features
- `postProcess` now uses a simpler single call to `gdalwarp`, if available, for `RasterLayer` class to accomplish `cropInputs`, `projectInputs`, `maskInputs`, and `writeOutputs` all at once. This should be faster, simpler and, perhaps, more stable. It will only be invoked if the `RasterLayer` is too large to fit into RAM. To force it to be used the user must set `useGDAL = "force"` in `prepInputs` or `postProcess` or globally with `options("reproducible.useGDAL" = "force")`
- `postProcess` when using the new `gdalwarp`, has better persistence of colour table, and NA values as these are kept with better reliability
- concurrent `Cache` now works as expected (e.g., with parallel processing, it will avoid collisions) with SQLite thanks to suggestion here: <https://stackoverflow.com/a/44445010>
- updated digesting of `Raster` class objects to account for more of the metadata (including the colortable). This will change the digest value of all `Raster` layers, causing re-run of `Cache`
- removed `Require`, `pkgDep`, `trimVersionNumber`, `normPath`, `checkPath` that were moved to `Require` package. For backwards compatibility, these are imported and reexported
- address permanently or temporarily new changes in GDAL>3 and PROJ>6 in the spatial packages.
- new function `file.move` used to rename/copy files across disks (a situation where `file.rename` would fail)
- all `DBI` type functions now have default `cachePath` of `getOption("reproducible.cachePath")`
- `Cache(prepInputs, ...` on a file-backed `Raster*` class object now gives the non-Cache repository folder as the `filename(returnRaster)`. Previously, the return object would contain the cache repository as the folder for the file-backed `Raster*` 

## Dependency changes
- net reduction in number of packages that are imported from by 14. Removed completely: `backports`, `memoise`, `quickPlot`, `R.utils`, `remotes`, `tools`, and `versions`; moved to Suggests: `fastdigest`, `gdalUtils`, `googledrive`, `httr`, `qs`, `rgdal`, `sf`, `testthat`; added: `Require`. Now there are 12 non-base packages listed in Imports. This is down from 31 prior to Ver 1.0.0.

## bug fixes
- fix over-wide tables in PDF manual (#144)
- use `file.link` not `file.symlink` for `saveToCache`. This would have resulted in C Stack overflow errors due to missing original file in the `file.symlink`
- use system call to `unzip` when extracting large (>= 4GB) files (#145, @tati-micheletti)
- several minor including `projectInputs` when converting to longlat projections, `setMinMax` for `gdalwarp` results
- `Filenames` now consistently returns a character vector (#149)
- improvements to file-backed Raster caching to accommodate a few more edge cases

# reproducible 1.1.1

## New features
- none

## Dependency changes
- none

## bug fixes
- fix CRAN test failure when `file.link` does not succeed.

# reproducible 1.1.0

## New features
- begin to accommodate changes in GDAL/PROJ and associated updates to other spatial packages.
  More updates are expected as other spatial packages (namely `raster`) are updated.
- can now change `options('reproducible.cacheSaveFormat')` on the fly; cache will look for the file by `cacheId` and write it using `options('reproducible.cacheSaveFormat')`.
  If it is in another format, Cache will load it and resave it with the new format. Experimental still.
- new `Copy` methods for `refClass` objects, `SQLite` and moved `environment` method into `ANY` as it would be dispatched for unknown classes that inherit from `environment`, of which there are many and this should be intercepted
- `Require` can now handle minimum version numbers, e.g., `Require("bit (>=1.1-15.2)")`; this can be worked into downstream tools. Still experimental.
- Cache will do `file.link` or `file.symlink` if an existing Cache entry with identical output exists and it is large (currently `1e6` bytes); this will save disk space. 
- Cache database now has tags for elapsed time of "digest", "original call", and "subsequent recovery from file", `elapsedTimeDigest`, `elapsedTimeFirstRun`, and `elapsedTimeLoad`, respectively.
- Better management of temporary files in package and tests, e.g., during downloading (`preProcess`). Includes 2 new functions, `tempdir2` and `tempfile2` for use with `reproducible` package
- New option: `reproducible.tempPath`, which is used for the new control of temporary files. Defaults to `file.path(tempdir(), "reproducible")`. This feature was requested to help manage large amounts of temporary objects that were not being easily and automatically cleaned
- Copying or moving of Cache directories now works automatically if using default `drv` and `conn`; user may need to manually call `movedCache` if cache is not responding correctly.
  File-backed Rasters are automatically updated with new paths.
- Cache now treats file-backed Rasters as though they had a relative path instead of their absolute path.
  This means that Cache directories can be copied from one location to another and the file-backed `Raster*` will have their filenames updated on the fly during a Cache recovery.
  User doesn't need to do anything.
- `postProcess` now will perform simple tests and skip `cropInputs` and `projectInputs` with a message if it can, rather than using `Cache` to "skip". This should speed up `postProcess` in many cases.
- messaging with `Cache` has change. Now, `cacheId` is shown in all cases, making it easier to identify specific items in the cache.
- Automatically cleanup temporary (intermediate) raster files (with #110).

## Dependency changes
- none

## bug fixes
- `Copy` only creates a temporary directory for filebacked rasters; previously any `Copy` command was creating a temporary directory, regardless of whether it was needed
- `cropInputs.spatialObjects` had a bug when object was a large non-Raster class.
- `cropInputs` may have failed due to "self intersection" error when x was a `SpatialPolygons*` object; now catches error, runs `fixErrors` and retries `crop`.
  Great reprex by @tati-micheletti. Fixed in commit `89e652ef111af7de91a17a613c66312c1b848847 `.
- `Filenames` bugfix related to `RasterBrick`
- `prepInputs` does a better job of keeping all temporary files in a temporary folder; and cleans up after itself better.
- `prepInputs` now will not show message that it is loading object into R if `fun = NULL` (#135).

# reproducible 1.0.0

## New features

- This version is not backwards-compatible out of the box. To maintain backwards compatibility, set: `options("reproducible.useDBI" = FALSE)`
- A new backend was introduced that uses `DBI` package directly, without `archivist`. This has much improved speed. 
- New option: `options("reproducible.cacheSaveFormat")`. This can be either `rds` (default) or `qs`. All cached objects will be saved with this format. Previously it was `rda`. 
- Cache objects can now be saved with with `qs::qsave`. In many cases, this has much improved speed and file sizes compared to `rds`; however, testing across a wide range of conditions will occur before it becomes the default.
- Changed default behaviour for memoising `...` because `Cache` is now much faster, the default is to turn memoising off, via `options("reproducible.useMemoise" = FALSE)`.
  In cases of large objects, memoising should still be faster, so user can still activate it, setting the option to `TRUE`.
- Much better SQLite database handling for concurrent write attempts.
  Tested with dozens of write attempts per second by 3 cores with abundant locked database occurrences.
- `postProcess` arg `useGDAL` can now take `"force"` as the default behaviour is to not use GDAL if the problem can fit into RAM and `sf` or `raster` tools will be faster than `GDAL` tools
- `useCloud` argument in `Cache` and family has slightly modified functionality (see ?Cache new section `useCloud`) and now has more tests including edge cases, such as `useCloud = TRUE, useCache = 'overwrite'`.
  The cloud version now will also follow the `"overwrite"` command.

## Dependency changes

- deprecating `archivist`; moved to Suggests.
- removed imports for `bitops`, `dplyr`, `fasterize`, `flock`, `git2r`, `lubridate`, `RcppArmadillo`, `RCurl` and `tidyselect`. Some of these went to Suggests.

## bug fixes

- `postProcess` calls that use GDAL made more robust (including #93).
- Several minor, edge cases were detected and fixed.

# reproducible 0.2.11

## Dependency changes

- remove `dplyr` as a direct dependency. It is still an indirect dependency through `DiagrammeR`

## New features

- new option: `reproducible.showSimilarDepth` allows for a deeper assessment of nested lists for differences between the nearest cached object and the present object. This greater depth may allow more fine tuned understanding of why an object is not correctly caching
- for downloading large files from GoogleDrive (currently only implemented), if user has set `options("reproducible.futurePlan")` to something other than `FALSE`, then it will show download progress if the file is "large".

## bug fixes

- Several minor, edge cases were detected and fixed.

# reproducible 0.2.10

## Dependency changes

- made compatible with `googledrive` v 1.0.0 (#119)

## New features

- `pkgDep2`, a new convenience function to get the dependencies of the "first order" dependencies.
- `useCache`, used in many functions (incl `Cache`, `postProcess`) can now be numeric, a qualitative indicator of "how deep" nested `Cache` calls should set `useCache = TRUE` -- implemented as 1 or 2 in `postProcess` currently. See `?Cache`

## bug fixes

- `pkgDep` was becoming unreliable for unknown reasons. It has been reimplemented, much faster, without memoising. The speed gains should be immediately noticeable (6 second to 0.1 second for `pkgDep("reproducible")`)
- improved `retry` to use exponential backoff when attempting to access online resources (#121)

# reproducible 0.2.9

## New features

- Cache has 2 new arguments, `useCloud` and `cloudFolderID`. This is a new approach to cloud caching.
  It has been tested with file backed `RasterLayer`, `RasterStack` and `RasterBrick` and all normal R objects.
  It will not work for any other class of disk-backed files, e.g., `ff` or `bigmatrix`, nor is it likely to work for R6 class objects.
- Slowly deprecating cloudCache and family of functions in favour of a new approach using arguments to `Cache`, i.e., `useCache` and `cloudFolderID`
- `downloadData` from Google Drive now protects against HTTP2 error by capturing error and retrying.
  This is a curl issue for interrupted connections.

## Bug fixes

- fixes for `rcnst` errors on R-devel, tested using `devtools::check(env_vars = list("R_COMPILE_PKGS"=1, "R_JIT_STRATEGY"=4, "R_CHECK_CONSTANTS"=5))`
- other minor improvements, including fixes for #115

# reproducible 0.2.8

## New features

- new functions for accessing specific items from the `cacheRepo`: `getArtifact`, `getCacheId`, `getUserTags`
- `retry`, a new function, wraps `try` with an explicit attempt to retry the same code upon error.
  Useful for flaky functions, such as `googldrive::drive_download` which sometimes fails due to `curl` HTTP2 error.
- removed all `Rcpp` functionality as the functions were no longer faster than their R base alternatives.

## Bug fixes

- `prepInputs` was not correctly passing `useCache` 
- `cropInputs` was reprojecting extent of y as a time saving approach, but this was incorrect if `studyArea` is a `SpatialPolygon` that is not close to filling the extent. It now reprojects `studyArea` directly which will be slower, but correct. (#93)
- other minor improvements

# reproducible 0.2.7

## New features

- `CHECKSUMS.txt` should now be ordered consistently across operating systems (note: `base::order` will not succeed in doing this --> now using `.orderDotsUnderscoreFirst`)
- `cloudSyncCache` has a new argument: `cacheIds`. Now user can control entries by `cacheId`, so  can delete/upload individual objects by `cacheId`
- Experimental support within the `postProcess` family for `sf` class objects

## bug fixes

- mostly minor
- `cloudCache` bugfixes for more cases

# reproducible 0.2.6

## Dependency changes

- remove `tibble` from Imports as it's no longer being used

## New features

- remove `%>%` pipe that was long ago deprecated. User should use `%C%` if they want a pipe that is Cache-aware. See examples.
- Full rewrite of all `options` descriptions now in `reproducible`, see `?reproducibleOptions`
- now `cacheRepo` and `options("reproducible.cachePath")` can take a vector of paths. Similar to how .libPaths() works for libraries, `Cache` will search first in the first entry in the `cacheRepo`, then the second etc. until it finds an entry. It will only write to the first entry.
- new value for the option: `options("reproducible.useCache" = "devMode")`. The point of this mode is to facilitate using the Cache when functions and datasets are continually in flux, and old Cache entries are likely stale very often. In `devMode`, the cache mechanism will work as normal if the Cache call is the first time for a function OR if it successfully finds a copy in the cache based on the normal Cache mechanism. It *differs* from the normal Cache if the Cache call does *not* find a copy in the `cacheRepo`, but it does find an entry that matches based on `userTags`. In this case, it will delete the old entry in the `cacheRepo` (identified based on matching `userTags`), then continue with normal `Cache`. For this to work correctly, `userTags` must be unique for each function call. This should be used with caution as it is still experimental.
- change to how hashes are calculated. This will cause existing caches to not work correctly. To allow a user to keep old behaviour (during a transition period), the "old" algorithm can be used, with `options("reproducible.useNewDigestAlgorithm" = FALSE)`. There is a message of this change on package load.
- add experimental `cloud*` functions, especially `cloudCache` which allows sharing of Cache among collaborators. Currently only works with `googledrive`
- updated `assessDataType` to consolidate `assessDataTypeGDAL` and `assessDataType` into single function (#71, @ianmseddy)
- `cc`: new function -- a shortcut for some commonly used options for `clearCache()`
- added experimental capacity for `prepInputs` to handle `.rar` archives, on systems with correct binaries to deal with them (#86, @tati-micheletti)
- remove `fastdigest::fastdigest` as it is not return the identical hash across operating systems

## Bug fixes

- `prepInputs` on GIS objects that don't use `raster::raster` to load object were skipping `postProcess`. Fixed.
- under some circumstances, the `prepInputs` would cause virtually all entries in `CHECKSUMS.txt` to be deleted. 2 cases where this happened were identified and corrected. 
- `data.table` class objects would give an error sometimes due to use of `attr(DT)`. Internally, attributes are now added with `data.table::setattr` to deal with this.
- calling `gdalwarp` from `prostProcess` now correctly matches extent (#73, @tati-micheletti)
- files from url that have unknown extension are now guessed with by `preProcess` (#92, @tati-micheletti)

# reproducible 0.2.5

## Dependency changes

- Added `remotes` to Imports and removed `devtools`

## New features

- New value possible for `options(reproducible.useCache = 'overwrite')`, which
  allows use of `Cache` in cases where the function call has an entry in the `cacheRepo`,
  will purge it and add the output of the current call instead.
- New option `reproducible.inputPaths` (default `NULL`) and `reproducible.inputPathsRecursive`
  (default `FALSE`), which will be used in `prepInputs` as possible directory sources
  (searched recursively or not) for files being downloaded/extracted/prepared.
  This allows the using of local copies of files in (an)other location(s) instead
  of downloading them. If local location does not have the required files,
  it will proceed to download so there is little cost in setting this option.
  If files do exist on local system, the function will attempt to use a hardlink before making a copy.
- `dlGoogle()` now sets `options(httr_oob_default = TRUE)` if using Rstudio Server.
- Files in `CHECKSUMS` now sorted alphabetically.
- `Checksums` can now have a `CHECKSUMS.txt` file located in a different place than the `destinationPath`
- Attempt to select raster resampling method based on raster type if no method supplied (#63, @ianmseddy)
- `projectInputs` 
- new function `assessDataTypeGDAL`, used in `postProcess`, to identify smallest `datatype` for large Raster* objects passed to GDAL system call

    - when masking and reprojecting large `Raster` objects, enact `gdalwarp` system call if `raster::canProcessInMemory(x,4) = FALSE` for faster and memory-safe processing 
    - better handling of various data types in `Raster` objects, including factor rasters

## Bug fixes

- Work around internally inside `extractFromArchive` for large (>2GB) zip files.
  In the `R` help manual, `unzip` fails for zip files >2GB.
  This uses a system call if the zip file is too large and fails using `base::unzip`.
- Work around for `raster::getData` issues.
- Speed up of `Cache()` when deeply nested, due to `grep(sys.calls(), ...)` that would take long and hang.
- Bugfix for `preProcess(url = NULL)` (#65, @tati-micheletti)
- Improved memory performance of `clearCache` (#67), especially for large `Raster` objects that are stored as binary `R` files (i.e., `.rda`)
- Other minor bugfixes

## Other changes

- Deal with new `raster` package changes in development version of `raster` package
- Added checks for float point number issues in raster resolutions produced by `raster::projectRaster`
- `.robustDigest` now does not include `Cache`-added attributes
- Additional tests for `preProcess()` (#68, @tati-micheletti)
- Many new unit tests written, which caught several minor bugs

# reproducible 0.2.3

- fix and skip downloading test on CRAN

# reproducible 0.2.2

## Dependency changes

- Add `future` to Suggests.

## New features

- new option on non-Windows OSs to use `future` for `Cache` saving to SQLite database, via `options("reproducible.futurePlan")`, if the `future` package is installed. This is `FALSE` by default.
- If a `do.call` function is Cached, previously, it would be labelled in the database as `do.call`. Now it attempts to extract the actual function being called by the `do.call`. Messaging is similarly changed.
- new option `reproducible.ask`, logical, indicating whether `clearCache` should ask for deletions when in an interactive session
- `prepInputs`, `preProcess` and `downloadFile` now have `dlFun`, to pass a custom function for downloading (e.g., "raster::getData")
- `prepInputs` will automatically use `readRDS` if the file is a `.rds`.
- `prepInputs` will return a `list` if `fun = "base::load"`, with a message; can still pass an `envir` to obtain standard behaviour of `base::load`.
- `clearCache` - new argument `ask`.
- new function `assessDataType`, used in `postProcess`, to identify smallest `datatype` for Raster* objects, if user does not pass an explicit `datatype` in `prepInputs` or `postProcess` (#39, @CeresBarros).

## Bug fixes

- fix problems with tests introduced by recent `git2r` update (@stewid, #36).
- `.prepareRasterBackedFile` -- now will postpend an incremented numeric to a cached copy of a file-backed Raster object, if it already exists. This mirrors the behaviour of the `.rda` file. Previously, if two Cache events returned the same file name backing a Raster object, even if the content was different, it would allow the same file name. If either cached object was deleted, therefore, it would cause the other one to break as its file-backing would be missing.
- options were wrongly pointing to `spades.XXX` and should have been `reproducible.XXX`.
- `copyFile` did not perform correctly under all cases; now better handling of these cases, often sending to `file.copy` (slower, but more reliable)
- `extractFromArchive` needed a new `Checksum` function call under some circumstances
- several other minor bug fixes.
- `extractFromArchive` -- when dealing with nested zips, not all args were passed in recursively (#37, @CeresBarros)
- `prepInputs` -- arguments that were same as `Cache` were not being correctly passed internally to `Cache`, and if wrapped in Cache, it was not passed into prepInputs. Fixed.
- `.prepareFileBackedRaster` was failing in some cases (specifically if it was inside a `do.call`)  (#40, @CeresBarros).
- `Cache` was failing under some cases of `Cache(do.call, ...)`. Fixed.
- `Cache` -- when arguments to Cache were the same as the arguments in `FUN`, Cache would "take" them. Now, they are correctly passed to the `FUN`.
- `preProcess` -- writing to checksums may have produced a warning if `CHECKSUMS.txt` was not present. Now it does not.
- numerous other minor bugfixes

## Other changes

- most tests now use a standardized approach to attaching libraries, creating objects, paths, enabling easier, error resistant test building

# reproducible 0.2.1

## New features

- new functions: 

    - `convertPaths` and `convertRasterPaths` to assist with renaming moved files.
    
- `prepInputs` -- new features

    - `alsoExtract` now has more options (`NULL`, `NA`, `"similar"`) and defaults to extracting all files in an archive (`NULL`).
    - skips `postProcess` altogether if no `studyArea` or `rasterToMatch`. Previously, this would invoke Cache even if there was nothing to `postProcess`.
    
## Bug fixes

- `copyFile` correctly handles directory names containing spaces.
- `makeMemoisable` fixed to handle additional edge cases.
- other minor bug fixes.

# reproducible 0.2.0

## New features

- new functions: 

    - `prepInputs` to aid in data downloading and preparation problems, solved in a reproducible, Cache-aware way.
    - `postProcess` which is a wrapper for sequences of several other new functions (`cropInputs`, `fixErrors`, `projectInputs`, `maskInputs`, `writeOutputs`, and `determineFilename`) 
    - `downloadFile` can handle Google Drive and ftp/http(s) files
    - `zipCache` and `mergeCache`
    - `compareNA` does comparisons with NA as a possible value e.g., `compareNA(c(1,NA), c(2, NA))` returns `FALSE, TRUE`

- Cache -- new features:

    - new arguments `showSimilar`, `verbose` which can help with debugging
    - new argument `useCache` which allows turning caching on and off at a high level (e.g., options("useCache"))
    - new argument `cacheId` which allows user to hard code a result from a Cache
    - deprecated arguments: `digestPathContent` --> `quick`, `compareRasterFileLength` --> `length`
    - Cache arguments now propagate inward to nested `Cache` function calls, unless explicitly set on the inner functions
    - more precise messages provided upon each use
    - many more `userTags` added automatically to cache entries so much more powerful searching via `showCache(userTags="something")`

- `checksums` now returns a data.table with the same columns whether `write = TRUE` or `write = FALSE`. 
- `clearCache` and `showCache` now give messages and require user intervention if request to `clearCache` would be large quantities of data deleted
- `memoise::memoise` now used on 3rd run through an identical `Cache` call, dramatically speeding up in most cases
- new options: `reproducible.cachePath`, `reproducible.quick`, `reproducible.useMemoise`, `reproducible.useCache`, `reproducible.useragent`, `reproducible.verbose`
- `asPath` has a new argument indicating how deep should the path be considered when included in caching (only relevant when `quick = TRUE`)
- New vignette on using Cache
- Cache is `parallel`-safe, meaning there are `tryCatch` around every attempt at writing to SQLite database so it can be used safely on multi-threaded machines
- bug fixes, unit tests, more `imports` for packages e.g., `stats`
- updates for R 3.6.0 compact storage of sequence vectors
- experimental pipes (`%>%`, `%C%`) and assign `%<%`
- several performance enhancements

# reproducible 0.1.4

- `mergeCache`: a new function to merge two different Cache repositories
- `memoise::memoise` is now used on `loadFromLocalRepo`, meaning that the 3rd time `Cache()` is run on the same arguments (and the 2nd time in a session), the returned Cache will be from a RAM object via memoise. To stop this behaviour and use only disk-based Caching, set `options(reproducible.useMemoise = FALSE)` .
- Cache assign -- `%<%` can be used instead of normal assign, equivalent to `lhs <- Cache(rhs)`.
- new option: reproducible.verbose, set to FALSE by default, but if set to true may help understand caching behaviour, especially for complex highly nested code.
- all options now described in `?reproducible`.
- All Cache arguments other than FUN and ... will now propagate to internal, nested Cache calls, if they are not specified explicitly in each of the inner Cache calls. 
- Cached pipe operator `%C%` -- use to begin a pipe sequence, e.g., `Cache() %C% ...`
- Cache arg `sideEffect` can now be a path
- Cache arg `digestPathContent` default changed from FALSE (was for speed) to TRUE (for content accuracy)
- New function, `searchFull`, which shows the full search path, known alternatively as "scope", or "binding environments". It is where R will search for a function when requested by a user.
- Uses `memoise::memoise` for several functions (`loadFromLocalRepo`, `pkgDep`, `package_dependencies`, `available.packages`) for speed -- will impact memory at the expense of speed.
- New `Require` function 

    - attempts to create a lighter weight package reproducibility chain. This function is usable in a reproducible workflow: it includes both installing and loading of packages, it can maintain version numbers, and uses smart caching for speed. In tests, it can evaluate whether 20 packages and their dependencies (~130 packages) are installed and loaded quickly (i.e., if all TRUE,  ~0.1 seconds). This is much slower than running `require` on those 20 packages, but `require` does not check for dependencies and deal with them if missing: it just errors. This speed should be fast enough for many purposes.
    - can accept uncommented name, if length 1.
    
- remove `dplyr` from Imports
- Add `RCurl` to Imports
- change name of `digestRaster` to `.digestRaster`

# reproducible 0.1.3

- fix R CMD check errors on Solaris that were not previously resolved

# reproducible 0.1.2

- fix R CMD check errors on Solaris
- fix bug in `digestRaster` affecting in-memory rasters
- move `rgdal` to Suggests

# reproducible 0.1.1

- cleanup examples and *do* run them (per CRAN)
- add tests to ensure all exported (non-dot) functions have examples

# reproducible 0.1.0

- A new package, which takes all caching utilities out of the `SpaDES` package.
