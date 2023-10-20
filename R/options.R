#' `reproducible` options
#'
#' These provide top-level, powerful settings for a comprehensive
#' reproducible workflow. To see defaults, run `reproducibleOptions()`.
#' See Details below.
#'
#' @export
#' @return
#' This function returns a list of all the options that the `reproducible` package
#' sets and uses. See below for details of each.
#'
#' @details
#'
#' Below are options that can be set with `options("reproducible.xxx" = newValue)`,
#' where `xxx` is one of the values below, and `newValue` is a new value to
#' give the option. Sometimes these options can be placed in the user's `.Rprofile`
#' file so they persist between sessions.
#'
#' The following options are likely of interest to most users:
#' \describe{
#'   \item{`ask`}{
#'     Default: `TRUE`. Used in [clearCache()] and [keepCache()].
#'   }
#'   \item{`cachePath`}{
#'     Default: `.reproducibleTempCacheDir`. Used in [Cache()] and many others.
#'     The default path for repositories if not passed as an argument.
#'   }
#'   \item{`cacheSaveFormat`}{
#'     Default: `"rds"`. What save format to use; currently, `"qs"` or `"rds"`.
#'   }
#'   \item{`cacheSpeed`}{
#'     Default `"slow"`. One of `"slow"` or `"fast"` (1 or 2).
#'     `"slow"` uses `digest::digest` internally, which is transferable across operating
#'     systems, but much slower than `fastdigest::fastdigest`.
#'     So, if all caching is happening on a single machine, `"fast"` would be a good setting.
#'   }
#'   \item{`conn`}{
#'     Default: `NULL`. Sets a specific connection to a database, e.g.,
#'     `dbConnect(drv = RSQLite::SQLite())` or `dbConnect(drv = RPostgres::Postgres()`.
#'     For remote database servers, setting one connection may be far faster than using
#'     `drv` which must make a new connection every time.
#'   }
#'   \item{`destinationPath`}{
#'     Default: `NULL`. Used in [prepInputs()] and [preProcess()].
#'     Can be set globally here.
#'   }
#'   \item{`drv`}{
#'     Default: `RSQLite::SQLite()`. Sets the default driver for the backend database system.
#'     Only tested with `RSQLite::SQLite()` and `RPostgres::Postgres()`.
#'   }
#'   \item{`futurePlan`}{
#'     Default: `FALSE`. On Linux OSes, `Cache` and `cloudCache` have some
#'     functionality that uses the `future` package.
#'     Default is to not use these, as they are experimental.
#'     They may, however, be very effective in speeding up some things, specifically,
#'     uploading cached elements via `googledrive` in `cloudCache`.
#'   }
#'   \item{`inputPaths`}{
#'     Default: `NULL`. Used in [prepInputs()] and [preProcess()].
#'     If set to a path, this will cause these functions to save their downloaded and preprocessed
#'     file to this location, with a hardlink (via `file.link`) to the file created in the
#'     `destinationPath`.
#'     This can be used so that individual projects that use common data sets can maintain
#'     modularity (by placing downloaded objects in their `destinationPath`, but also minimize
#'     re-downloading the same (perhaps large) file over and over for each project.
#'     Because the files are hardlinks, there is no extra space taken up by the apparently
#'     duplicated files.
#'   }
#'   \item{`inputPathsRecursive`}{
#'     Default: `FALSE`. Used in [prepInputs()] and [preProcess()].
#'     Should the `reproducible.inputPaths` be searched recursively for existence of a file?
#'   }
#'   \item{`memoisePersist`}{
#'     Default: `FALSE`. Used in [Cache()].
#'     Should the memoised copy of the Cache objects persist even if `reproducible` reloads
#'     e.g., via `devtools::load_all`? This is mostly useful for developers of
#'     `reproducible`. If `TRUE`, a object named `paste0(".reproducibleMemoise_", cachePath)`
#'     will be placed in the `.GlobalEnv`, i.e., one for each `cachePath`.
#'   }
#'   \item{`nThreads`}{
#'     Default: `1`. The number of threads to use for reading/writing cache files.
#'   }
#'   \item{`overwrite`}{
#'     Default: `FALSE`. Used in [prepInputs()], [preProcess()],
#'     [downloadFile()], and [postProcess()].
#'   }
#'   \item{`quick`}{
#'     Default: `FALSE`. Used in [Cache()]. This will cause `Cache` to use
#'     `file.size(file)` instead of the `digest::digest(file)`.
#'     Less robust to changes, but faster. *NOTE: this will only affect objects on disk*.
#'   }
#'   \item{`rasterRead`}{
#'     Used during `prepInputs` when reading `.tif`, `.grd`, and `.asc` files.
#'     Default: `terra::rast`. Can be `raster::raster` for backwards compatibility.
#'     Can be set using environment variable `R_REPRODUCIBLE_RASTER_READ`.
#'   }
#'   \item{`shapefileRead`}{
#'     Default `NULL`. Used during `prepInputs` when reading a `.shp` file.
#'     If `NULL`, it will use `sf::st_read` if `sf` package is available; otherwise,
#'     it will use `raster::shapefile`
#'   }
#'   \item{`showSimilar`}{
#'     Default `FALSE`. Passed to `Cache`.
#'   }
#'   \item{`useCache`}{
#'     Default: `TRUE`. Used in [Cache()]. If `FALSE`, then the entire
#'     `Cache` machinery is skipped and the functions are run as if there was no Cache occurring.
#'     Can also take 2 other values: `'overwrite'` and `'devMode'`.
#'     `'overwrite'` will cause no recovery of objects from the cache repository, only new
#'     ones will be created. If the hash is identical to a previous one, then this will overwrite
#'     the previous one.
#'     `'devMode'` will function as normally `Cache` except it will use the
#'     `userTags` to determine if a previous function has been run. If the `userTags`
#'     are identical, but the digest value is different, the old value will be deleted from the
#'     cache repository and this new value will be added.
#'     This addresses a common situation during the development stage: functions are changing
#'     frequently, so any entry in the cache repository will be stale following changes to
#'     functions, i.e., they will likely never be relevant again.
#'     This will therefore keep the cache repository clean of stale objects.
#'     If there is ambiguity in the `userTags`, i.e., they do not uniquely identify a single
#'     entry in the `cachePath`, then this option will default back to the non-dev-mode
#'     behaviour to avoid deleting objects.
#'     This, therefore, is most useful if the user is using unique values for `userTags`.
#'   }
#'   \item{`useCloud`}{
#'     Default `FALSE`. Passed to `Cache`.
#'   }
#'   \item{`useDBI`}{
#'     Default: `TRUE` if \pkg{DBI} is available.
#'     Default value can be overridden by setting environment variable `R_REPRODUCIBLE_USE_DBI`.
#'     As of version 0.3, the backend is now \pkg{DBI} instead of \pkg{archivist}.
#'   }
#'   \item{`useGDAL`}{
#'     Default `TRUE`. Passed to `useGDAL`. Deprecated.
#'   }
#'   \item{`useMemoise`}{
#'     Default: `FALSE`. Used in [Cache()]. If `TRUE`, recovery of cached
#'     elements from the `cachePath` will use `memoise::memoise`.
#'     This means that the 2nd time running a function will be much faster than the first
#'     in a session (which either will create a new cache entry to disk or read a cached
#'     entry from disk).
#'     *NOTE: memoised values are removed when the R session is restarted*.
#'     **This option will use more RAM** and so may need to be turned off if RAM is limiting.
#'     `clearCache` of any sort will cause all memoising to be 'forgotten' (`memoise::forget`).
#'   }
#'   \item{`useNewDigestAlgorithm`}{
#'     Default: `1`. Option 1 is the version that has existed for sometime.
#'     There is now an option `2` which is substantially faster.
#'     It will, however, create Caches that are not compatible with previous ones.
#'     Options `1` and `2` are not compatible with the earlier `0`.
#'     `1` and `2` will make `Cache` less sensitive to minor but irrelevant changes
#'     (like changing the order of arguments) and will work successfully across operating systems
#'     (especially relevant for the new `cloudCache` function.
#'   }
#'   \item{`useTerra`}{
#'     Default: `FALSE`. The GIS operations in postProcess, by default use primarily
#'     the Raster package. The newer terra package does similar operations, but usually
#'     faster. A user can now set this option to `TRUE` and `prepInputs`
#'     and several components of `postProcess` will use `terra` internally.
#'   }
#'   \item{`verbose`}{
#'     Default: `FALSE`. If set to `TRUE` then every `Cache` call will show a
#'     summary of the objects being cached, their `object.size` and the time it took to digest
#'     them and also the time it took to run the call and save the call to the cache repository or
#'     load the cached copy from the repository.
#'     This may help diagnosing some problems that may occur.
#'   }
#' }
#'
#' @section Advanced:
#' The following options are likely not needed by a user.
#' \describe{
#'   \item{`cloudChecksumsFilename`}{
#'     Default: `file.path(dirname(.reproducibleTempCacheDir()), "checksums.rds")`.
#'     Used as an experimental argument in [Cache()]
#'   }
#'   \item{`length`}{
#'     Default: `Inf`. Used in [Cache()], specifically to the internal
#'     calls to [CacheDigest()]. This is passed to `digest::digest`.
#'     Mostly this would be changed from default `Inf` if the digesting is taking too long.
#'     Use this with caution, as some objects will have *many* `NA` values in their first
#'     *many* elements
#'   }
#'   \item{`useragent`}{
#'     Default: `"https://github.com/PredictiveEcology/reproducible"`.
#'     User agent for downloads using this package.
#'   }
#' }
reproducibleOptions <- function() {
  list( # nolint
    reproducible.ask = TRUE,
    reproducible.cachePath = file.path(tempdir(), "reproducible", "cache"),
    reproducible.cacheSaveFormat = "rds",
    reproducible.cacheSpeed = "slow",
    reproducible.conn = NULL,
    reproducible.destinationPath = NULL,
    reproducible.drv = NULL, # RSQLite::SQLite(),
    reproducible.futurePlan = FALSE, # future::plan("multisession"), #memoise
    reproducible.inputPath = file.path(tempdir(), "reproducible", "input"),
    reproducible.inputPaths = NULL,
    reproducible.inputPathsRecursive = FALSE,
    reproducible.length = Inf,
    reproducible.memoisePersist = FALSE,
    reproducible.messageColourPrepInputs = "cyan",
    reproducible.messageColourCache = "blue",
    reproducible.messageColourQuestion = "green",
    reproducible.nThreads = 1,
    reproducible.overwrite = FALSE,
    reproducible.quick = FALSE,
    reproducible.rasterRead = getEnv("R_REPRODUCIBLE_RASTER_READ",
      default = "terra::rast",
      allowed = c("terra::rast", "raster::raster")
    ),
    reproducible.shapefileRead = "sf::st_read",
    reproducible.showSimilar = FALSE,
    reproducible.showSimilarDepth = 3,
    reproducible.tempPath = file.path(tempdir(), "reproducible"),
    reproducible.useCache = TRUE, # override Cache function
    reproducible.useCloud = FALSE, #
    reproducible.useDBI = {getEnv("R_REPRODUCIBLE_USE_DBI",
      default = useDBI(getOption("reproducible.useDBI", NULL),  # a user may have set it before this runs; keep setting
                       verbose = interactive() - (useDBI() + 1)), # `FALSE` is useMultipleDBFiles now
      allowed = c("true", "false")
    ) |> as.logical()},
    reproducible.useMemoise = FALSE, # memoise
    reproducible.useragent = "https://github.com/PredictiveEcology/reproducible",
    reproducible.verbose = 1
  )
}

getEnv <- function(envvar, default = NULL, allowed = NULL) {
  if (nzchar(Sys.getenv(envvar))) {
    val <- Sys.getenv(envvar)

    if (!val %in% allowed) {
      val <- default
    }
  } else {
    val <- default
  }

  return(val)
}
