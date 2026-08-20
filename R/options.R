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
#'   \item{`cacheChaining`}{
#'     Default: `FALSE`. Used in [Cache()] in the `.cacheChaining` argument.
#'   }
#'   \item{`cachePath`}{
#'     Default: `NULL`. Used in [Cache()] and many others. The option is no
#'     longer pre-set when the package is loaded; instead it is resolved
#'     lazily on first use by an entry point ([Cache()], [clearCache()],
#'     [showCache()], [keepCache()], ...). If still unset at that point,
#'     it is set to `.reproducibleTempCacheDir()` for the rest of the
#'     session. This lets project-setup layers (e.g.
#'     `SpaDES.project::setupProject()`) detect "unset" cleanly and avoids
#'     committing every R session to a session-tempdir path that would not
#'     persist across sessions. Set this early (e.g. in your project setup
#'     script) to use a persistent cache.
#'   }
#'   \item{`cacheSaveFormat`}{
#'     Default: `"rds"`. What save format to use; currently, `"qs"` (which will use
#'     `qs2` package as of `reproducible` version ">= 2.1.3"), `"qs2"`, or `"rds"`.
#'   }
#'   \item{`cacheSpeed`}{
#'     Default `"slow"`. One of `"slow"` or `"fast"` (1 or 2).
#'     `"slow"` uses `digest::digest` internally, which is transferable across operating
#'     systems, but much slower than `digest::digest(algo = "spooky)`.
#'     So, if all caching is happening on a single machine, `"fast"` would be a good setting.
#'   }
#'   \item{`checkRemoteHash`}{
#'     Default: `FALSE`. Used in [preProcess()] / [prepInputs()]. Controls whether
#'     `pp_remote_hash_check` re-contacts the remote source (e.g. Google Drive,
#'     HTTP HEAD) when a `.hash` sidecar from a previous successful match
#'     already exists in `destinationPath`. With the default (`FALSE`), the
#'     sidecar is trusted and the remote check is skipped — typically saving
#'     1–2 s per file when the cluster cache is warm. Set to `TRUE` to force a
#'     remote round-trip on every call (the pre-3.0.0.9050 behaviour); use
#'     this if the upstream file may change and you need to detect that.
#'     Removing the `<file>_*.hash` sidecar also forces a re-check.
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
#'   \item{`downloadProgressInterval`}{
#'     Default: `2`, in seconds. Minimum interval between download-progress lines
#'     in non-interactive / non-dynamic sessions (logged runs, CI, a SpaDES
#'     `simInit`). In those sessions `httr2`'s native cli progress bar emits
#'     nothing, so `preProcess()` instead streams the body and reports progress
#'     via [messagePreProcess()] (which the calling app can timestamp). In a
#'     dynamic terminal the native in-place bar is used and this has no effect.
#'   }
#'   \item{`drv`}{
#'     Default: `RSQLite::SQLite()`. Sets the default driver for the backend database system.
#'     Only tested with `RSQLite::SQLite()` and `RPostgres::Postgres()`.
#'   }
#'   \item{dryRun}{
#'     Default: `FALSE`.
#'   }
#'   \item{`fileBackedAnchors`}{
#'     Default: `NULL`. A named list of "anchor" directories (e.g. the result of
#'     SpaDES `paths(sim)`: `cachePath`, `inputPath`, `outputPath`, `modulePath`,
#'     ...) used to make *file-backed* objects (such as a `terra` `SpatRaster`)
#'     portable across machines and users. A file-backed object embeds an
#'     *absolute* path to its backing file; when stored relative to a named anchor
#'     here, `Cache` records the anchor name plus the path relative to it, and on
#'     load rebuilds the file under the *receiver's* anchor of the same name. Each
#'     entry may hold one or more directories, and the most specific (longest)
#'     matching anchor wins. The same named list must be set on both the machine
#'     that writes the cache entry and the one that reads it (e.g. a shared cloud
#'     cache). When the file lives under no anchor, or the anchor name is not set
#'     on the receiver, the object is restored *self-contained under the
#'     receiver's `cachePath`* rather than the producing machine's absolute path.
#'     `cachePath` and the current working directory are always available as
#'     fallback anchors.
#'   }
#'   \item{`futurePlan`}{
#'     Default: `FALSE`. On Linux OSes, `Cache` and `cloudCache` have some
#'     functionality that uses the `future` package.
#'     Default is to not use these, as they are experimental.
#'     They may, however, be very effective in speeding up some things, specifically,
#'     uploading cached elements via `googledrive` in `cloudCache`.
#'   }
#'   \item{`gdalwarp`}{
#'     **Deprecated — do not use.** Default: `FALSE`. This option previously
#'     switched `postProcessTo` to use `sf::gdal_utils("warp")` for a specific
#'     combination of raster/vector inputs. It is no longer needed: current
#'     versions of `terra` handle this case well and produce equivalent results
#'     without the GDAL detour. The option is retained only for backwards
#'     compatibility and will be removed in a future release.
#'   }
#'   \item{`gdalwarpThreads`}{
#'     **Deprecated — do not use** (see `gdalwarp` above). Default: `2`.
#'     Previously set `-wo NUM_THREADS=` for `gdalProject`.
#'   }
#'   \item{`gdriveNoAuth`}{
#'     Default: `FALSE`. Used in [prepInputs()] and [preProcess()].
#'     When `TRUE`, Google Drive files are accessed without a `googledrive` token
#'     (i.e. no `drive_auth()`): both the *metadata* read in `assessGoogle()` and
#'     the download itself are done anonymously (the metadata read by
#'     deauthorizing `googledrive` so it uses an API key). This only works for
#'     files shared as "Anyone with the link". The same no-auth path is taken
#'     automatically — regardless of this option — when no Drive token is loaded
#'     (the common "cloud reader" case) or when the supplied `url` is the public
#'     web-download form, e.g.
#'     `https://drive.google.com/uc?export=download&id=<ID>`. A loaded token (a
#'     "cloud writer" who needs auth to write a shared cloud cache) is left
#'     intact.
#'   }
#'   \item{`destinationPathShared`}{
#'     Default: `NULL`. Used in [prepInputs()] and [preProcess()].
#'     If set to a path, this will cause these functions to save their downloaded and preprocessed
#'     file to this location, with a hardlink (via `file.link`) to the file created in the
#'     `destinationPath`.
#'     This can be used so that individual projects that use common data sets can maintain
#'     modularity (by placing downloaded objects in their `destinationPath`, but also minimize
#'     re-downloading the same (perhaps large) file over and over for each project.
#'     Because the files are hardlinks, there is no extra space taken up by the apparently
#'     duplicated files.
#'
#'     **Note:** the previous name for this option was `reproducible.inputPaths`; the old
#'     name is still accepted and will continue to work, but `reproducible.destinationPathShared` is
#'     preferred going forward (it matches the [prepInputs()] naming family).
#'   }
#'   \item{`destinationPathSharedRecursive`}{
#'     Default: `FALSE`. Used in [prepInputs()] and [preProcess()].
#'     Should `reproducible.destinationPathShared` be searched recursively for existence of a file?
#'
#'     **Note:** the previous name for this option was `reproducible.inputPathsRecursive`;
#'     the old name is still accepted but `reproducible.destinationPathSharedRecursive` is preferred.
#'   }
#'   \item{`inputPaths`}{
#'     **Deprecated** — use `reproducible.destinationPathShared` instead.
#'     Retained for backwards compatibility; if set and `reproducible.destinationPathShared` is `NULL`,
#'     the value of `reproducible.inputPaths` is used automatically.
#'   }
#'   \item{`inputPathsRecursive`}{
#'     **Deprecated** — use `reproducible.destinationPathSharedRecursive` instead.
#'     Retained for backwards compatibility.
#'   }
#'   \item{`leaveOnDisk`}{
#'     Default: `TRUE`. Used in [postProcess()].
#'     When there is a `SpatRaster` object, should `postProcess` force any file-backed object,
#'     to use the file-based, memory-safe tools within `terra` (by temporarily setting
#'     `terraOption(memfrac = 0)`. Alternatively, if this is set to `FALSE`,
#'     then `postProcess` will let `terra` decide on its own based on its internal
#'     cues (largely based on `memfrac`, `maxmem` `terraOptions`). This will be ignored,
#'     however, if the user has set the `terraOptions` away from its default of `0.5`. The default
#'     increases predictability of whether the returned object is on disk or in memory.
#'   }
#'   \item{`terraMemmax`}{
#'     Default: `2` (gigabytes). Used in [postProcessTo()].
#'     Caps `terra`'s per-raster memory budget for the duration of a `postProcessTo()`
#'     call by temporarily setting `terraOptions(memmax = ...)`, restored via
#'     `on.exit()`. Small values force `terra` to process in chunks, which on
#'     high-RAM machines is substantially faster than letting it pull whole
#'     rasters into RAM (in benchmarks on a 1TB-RAM machine, `memmax = 4` was
#'     ~45% faster and used ~3x less peak RSS than the unbounded default; the
#'     2GB default is conservative for shared nodes). Set to `NULL` to disable
#'     and let `terra` choose. **Respects user-set values**: if the caller has
#'     already set `terraOptions(memmax = ...)` to a positive finite value,
#'     `postProcessTo()` leaves it alone -- the option only applies when
#'     `terra`'s `memmax` is at its default ("ignored": `NA`, `NULL`, or `<= 0`;
#'     terra's out-of-the-box default is `-1`). See also `memfrac` in
#'     [terra::terraOptions()]; a `memfrac` ceiling of `0.1` is sensible on
#'     shared machines.
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
#'   \item{`objSize`}{
#'     Default: `TRUE`. Logical. If `TRUE`, then object sizes will be included in
#'     the cache database. Simplying calculating object size of large objects can
#'     be time consuming, so setting this to `FALSE` will make caching up to 10%
#'     faster, depending on the objects.
#'   }
#'   \item{`overwrite`}{
#'     Default: `FALSE`. Used in [prepInputs()], [preProcess()],
#'     [downloadFile()], and [postProcess()].
#'   }
#'   \item{`parallel.streams`}{
#'     Default: `48L`. The number of contiguous byte-range **parts** a single
#'     large HTTPS file is split into. **This has no effect unless you have opted
#'     in by setting `reproducible.urlRemap`** (see below): if no remap is set,
#'     downloads are always single-stream, regardless of this value. Once opted
#'     in, the parallel path is used **only for a URL that the `urlRemap` hook
#'     actually redirected** to a (Range-capable) mirror — *not* for arbitrary
#'     range-capable origin servers reached without a redirect, since those may
#'     cap concurrent connections per IP (in which case parallel streams all
#'     stall and the download is slower than a single stream). For an eligible,
#'     redirected URL the file is split into this many parts — but only when the
#'     server advertises `Accept-Ranges: bytes` and the file is larger than
#'     `reproducible.parallel.threshold`; otherwise, and on any failure, it
#'     falls back transparently to a single stream. (A redirected mirror that
#'     *does* turn out to cap concurrency is detected on the first attempt — see
#'     `reproducible.parallel.minConcurrentFrac` — and also falls back.)
#'     Splitting into many small
#'     parts keeps retries cheap (a dropped connection costs only one
#'     part re-fetch). The number that download *at once* is separately capped by
#'     `reproducible.parallel.maxConnections`. Especially useful on networks that
#'     shape bandwidth per-connection. Set to `1L` to force single-stream
#'     downloads even when a remap is set. Requires the \pkg{curl} and
#'     \pkg{httr2} packages. The assembled file is byte-identical to a
#'     single-stream download, so checksums are unaffected.
#'   }
#'   \item{`parallel.maxConnections`}{
#'     Default: `NULL`, meaning `parallelly::availableCores() - 1` (or
#'     `parallel::detectCores() - 1` if the Suggested \pkg{parallelly} package is
#'     not installed). The maximum
#'     number of ranged parts that download **simultaneously**; the rest queue
#'     until a connection frees up. This bounds the burst of concurrent TLS
#'     handshakes, which some stacks (notably Windows) refuse when all
#'     `reproducible.parallel.streams` are opened at once — the symptom being
#'     most parts failing immediately at connection time. Set a positive integer
#'     to override the default ceiling.
#'   }
#'   \item{`parallel.connecttimeout`}{
#'     Default: `30L`, in seconds. The per-connection establishment timeout for
#'     each ranged stream. This is distinct from `reproducible.timeout` (the
#'     overall download timeout, which may be hours): a short, dedicated cap so a
#'     stalled handshake fails its own part quickly and is retried rather than
#'     hanging.
#'   }
#'   \item{`parallel.minConcurrentFrac`}{
#'     Default: `0.25`. A guard against a redirected mirror that advertises Range
#'     support but actually caps concurrent connections per IP. If the *first*
#'     concurrent attempt completes fewer than this fraction of the parts (most
#'     having stalled at 0 bytes), the parallel path gives up immediately and
#'     falls back to a single stream, instead of grinding through every retry.
#'     Set to `0` to disable the early fallback.
#'   }
#'   \item{`parallel.threshold`}{
#'     Default: `10 * 1024^2` (10 MiB), in bytes. Files at or below this size are
#'     always downloaded single-stream; only files larger than this use the
#'     parallel ranged path. Like `reproducible.parallel.streams`, this has no
#'     effect unless you have opted in via `reproducible.urlRemap`.
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
#'   \item{`showCachePreWarm`}{
#'     Default `TRUE` (override with environment variable
#'     `R_REPRODUCIBLE_SHOWCACHE_PREWARM`). When `TRUE`, a `Cache(showSimilar = TRUE)`
#'     call spawns a one-time background process (a fork; not on Windows) that
#'     pre-scans the flat-file cache so the subsequent `showCache()`/`showSimilar`
#'     lookup returns quickly for large caches. This is skipped automatically under
#'     a DBI backend (`useDBI(TRUE)`), which is answered from an index. **Advanced
#'     option:** set to `FALSE` to disable the automatic pre-warm entirely -- useful
#'     in memory-constrained runners that touch many distinct `cachePath`s in one
#'     session (e.g. `covr::package_coverage()`), where the per-path forks can
#'     accumulate. Explicit `prepopulateCacheAsync()` calls are unaffected.
#'   }
#'   \item{`preDigestDump`}{
#'     Default: `NULL` (off). A diagnostic for "why is my `cacheId` different on
#'     this machine than that one?" (e.g. a cloud cache that will not share across
#'     OSs). Unlike `showSimilar` (closest prior call only) or `dryRun`/`verbose`,
#'     this dumps the **full** element-by-element `name = hash` list that produced
#'     the `cacheId`, for *every* `Cache()` call (including ones built inside other
#'     packages, e.g. SpaDES.core events). Set to `TRUE` to print each call's
#'     sorted list via `messageCache()`, or to a **directory path** to write one
#'     `preDigest_<functionName>[_<n>].txt` file per call. Point it at a fresh,
#'     empty directory on each machine, run, then `diff` the two directories: the
#'     differing `name = hash` line is exactly what is splitting the `cacheId`.
#'   }
#'   \item{`preDigestDumpPattern`}{
#'     Default: `NULL`. Optional regular expression matched against a call's
#'     `.functionName`; when set, only matching `Cache()` calls are dumped by
#'     `reproducible.preDigestDump` (e.g. `"init|inputObjects"`).
#'   }
#'   \item{`testCharacterAsFile`}{
#'     Default `FALSE`. The behaviour of `.robustDigest` on `character` vectors prior to
#'     `reproducible == 2.1.2` was that the function would test for whether they were
#'     filenames by using `file.exists`. If it was a filename, then it would digest
#'     the file content. In cases of a character vector or a data.frame of "filenames",
#'     this could cause long hanging of the R system as it tries to digest the file
#'     contents of potentially many files. This behaviour is not transparent to a user.
#'     Now the default is to **not** digest the file content of a `character` vector
#'     even if they are filenames. To force file content digesting, then convert to
#'     either `asPath` or `fs::as_fs_path`. Or set this option to `TRUE` and the previous
#'     behaviour will return, where it tries to guess whether a character vector
#'     is filenames or not, and if it is, then digest the file content.
#'   }
#'   \item{`connecttimeout`}{
#'     Default: `30L`, in seconds. The per-connection establishment (TLS
#'     handshake) timeout for the single-stream download in `preProcess`. This is
#'     distinct from `reproducible.timeout` (the overall download budget, which
#'     may be hours): a short, dedicated cap so a stalled or flaky connect fails
#'     quickly and is retried rather than hanging for the full timeout. Mirrors
#'     `reproducible.parallel.connecttimeout` for the parallel ranged path.
#'   }
#'   \item{`timeout`}{
#'     Default `12000`. Used in `preProcess` when downloading occurs. If a user has `R.utils`
#'     package installed, `R.utils::withTimeout(  , timeout = getOption("reproducible.timeout"))`
#'     will be wrapped around the download so that it will timeout (and error) after this many
#'     seconds.
#'   }
#'   \item{`urlLog`}{
#'     Default: `NULL`. Controls whether `prepInputs()` / `preProcess()` keep a
#'     record of the files and web addresses (URLs) they download. `NULL` (the
#'     default) records each download as a permanent tag on the matching cache
#'     entry, which you can look up later with
#'     `showCache(userTags = "reproducible.url")`; it keeps no in-session list.
#'     `TRUE` additionally keeps an in-memory list for the current session, which
#'     you can read with [prepInputsLog()] and empty with [clearUrlLog()].
#'     `FALSE` turns the recording off completely. Advanced: you may instead
#'     supply an environment (records are appended to `env$records`, which you
#'     own and manage) or a function (called once with each record).
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
#'   \item{`urlRemap`}{
#'     Default: `NULL` (feature off). **This is the opt-in switch for the faster
#'     download path.** It may be set to any of: a function `function(url,
#'     filename)`; a manifest `data.frame` with `filename` and `url` columns; or a
#'     length-one character path/URL to a CSV with those columns. For the
#'     `data.frame`/CSV forms, `reproducible` builds the remap function internally
#'     (once, then cached) — so a novice can simply write
#'     `options(reproducible.urlRemap = read.csv("manifest.csv"))` without calling
#'     [makeUrlRemap()] themselves. However supplied, it is
#'     consulted in the download path once the target `filename` has been
#'     resolved (for Google Drive URLs, after the `drive_get()` lookup). The
#'     function may return an alternative URL to download from instead, e.g. a
#'     public mirror that supports HTTP Range requests (which then triggers the
#'     parallel ranged download governed by `reproducible.parallel.streams` and
#'     `reproducible.parallel.threshold`); returning `NULL` or the original URL
#'     keeps the behaviour unchanged. A function that errors is ignored (with a
#'     warning) so a broken remap cannot break a download. With the default
#'     `NULL`, no remapping occurs and downloads behave exactly as before.
#'   }
#'   \item{`reproducible.useCacheV3`}{
#'     Default: `TRUE`. If this is set to `FALSE`, it will use the old `Cache` source
#'     code. This will only be available for a short period before it is deleted
#'     from the package. See also `reproducible.digestVersion`. It is not guaranteed to
#'     be identical to using a previous version of `reproducible (<3.0)`.
#'   }
#'   \item{`useCloud`}{
#'     Default `FALSE`. Passed to `Cache`.
#'   }
#'   \item{`useDBI`}{
#'     Default: `FALSE`, i.e., the file-backed cache metadata backend, which writes
#'     one small metadata file per `cacheId` alongside the cached object in
#'     `cacheOutputs/`. This needs no database packages, works on network
#'     filesystems (e.g., NFS, CIFS), where `SQLite` file locking is unreliable, and
#'     makes cloud caching straightforward because each entry's metadata is a
#'     self-contained, uploadable file.
#'     If `TRUE`, cache metadata are instead kept in a \pkg{DBI} database --
#'     `SQLite` (`cache.db`) by default, or any \pkg{DBI} backend supplied via
#'     `reproducible.drv`/`reproducible.conn`. This answers `showCache()` from the
#'     database rather than by scanning the cache directory, which is faster on
#'     large cache repositories. It requires both \pkg{DBI} and \pkg{RSQLite};
#'     if either is missing, the option silently reverts to `FALSE` with a message.
#'     Switching this option on an existing cache repository is supported: the
#'     metadata are converted to the other backend on first use, without loss.
#'     Default value can be overridden by setting environment variable
#'     `R_REPRODUCIBLE_USE_DBI` to `"true"` or `"false"`.
#'     As of version 0.3, the database backend is \pkg{DBI} instead of \pkg{archivist}.
#'   }
#'   \item{`useGdown`}{
#'     Default: `FALSE`. If a user provides a Google Drive url to `preProcess`/`prepInputs`,
#'     `reproducible` will use the `googledrive` package. This works reliably in most cases.
#'     However, for large files on unstable internet connections, it will stall and
#'     stop the download with no error. If a user is finding this behaviour, they can
#'     install the `gdown` package, making sure it is available on the PATH. This call
#'     to `gdown` will only work for files that do not need authentication. If authentication
#'     is needed, `dlGoogle` will fall back to `googledrive::drive_download`, even
#'     if this option is `TRUE`, with a message.
#'     .
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
#'   \item{`digestVersion`}{
#'     Default: `NULL` (which resolves to `4`). A single integer that selects the
#'     digest (i.e. `cacheId`) algorithm used by [Cache()]. This is the
#'     going-forward control; it supersedes the booleans `digestV3` and
#'     `digestV4` below (which are still honoured when `digestVersion` is unset).
#'     Higher numbers are newer; each builds on the previous one. (Note: this is
#'     the *digest-algorithm* version, **not** the version of the `reproducible`
#'     **package** — although the lower numbers echo the package generation that
#'     introduced them. References below to e.g. "the `reproducible` package
#'     v3.x" mean the package release, not this option.) The versions:
#'     \describe{
#'       \item{`2`}{*Some of* the hash assembly used by older `reproducible`
#'         **package** versions (before package v3.0.0), for partial backwards
#'         compatibility with caches they created. It cannot be made exact for
#'         all classes, particularly file-backed objects. (Equivalent to the old
#'         `digestV3 = FALSE`.)}
#'       \item{`3`}{The hash assembly introduced in the `reproducible` **package**
#'         v3.x (the default through package v3.1.1): includes the names of list
#'         elements and several other tweaks. `sf`/`SpatVector` use the original
#'         (pre-platform-stable) geometry digest, which could differ across
#'         operating systems. Set `digestVersion = 3` to reproduce the `cacheId`s
#'         of the `reproducible` package v3.1.1 and earlier, and to avoid the
#'         one-time cache invalidation described under `4`.}
#'       \item{`4`}{**Default.** As `3`, but `sf` and `SpatVector` objects are
#'         digested with a platform-stable algorithm: the geometry is the numeric
#'         vertex matrix with coordinates rounded to a fixed precision (plus the
#'         geometry type), and attributes are kept in feature order with columns
#'         sorted by a locale-independent method. The same vector data then
#'         produces the same `cacheId` on Windows, macOS and Linux (digest
#'         version `3` could differ across operating systems, preventing
#'         shared/cloud caching of these objects), and an `sf` object and its
#'         `SpatVector` equivalent digest identically. Requires the \pkg{terra}
#'         package. **NOTE:** because `4` is now the default, the `cacheId` of
#'         every `sf`/`SpatVector` object differs from that produced by the
#'         `reproducible` package v3.1.1 and earlier, so cached results that
#'         involved such objects are *recomputed once* under the new algorithm.
#'         Set `digestVersion = 3` to avoid this.}
#'     }
#'   }
#'   \item{`digestV3`}{
#'     **Superseded by `digestVersion`** (still honoured when `digestVersion` is
#'     unset). Default: `TRUE`. `TRUE` leaves the version at its default; `FALSE`
#'     is equivalent to `digestVersion = 2` (see above).
#'   }
#'   \item{`digestV4`}{
#'     **Superseded by `digestVersion`** (still honoured when `digestVersion` is
#'     unset). Default: `FALSE`. `TRUE` is equivalent to `digestVersion = 4`,
#'     which is now also the default (see above).
#'   }
#'
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
    reproducible.cacheChaining = FALSE,
    reproducible.checkRemoteHash = FALSE,
    reproducible.cachePath = NULL,
    reproducible.cacheSaveFormat = .rdsFormat,
    reproducible.cacheSpeed = "slow",
    reproducible.connecttimeout = 30L,              # seconds; single-stream connect/handshake cap (NOT the overall download timeout)
    reproducible.conn = NULL,
    reproducible.destinationPath = NULL,
    reproducible.downloadProgressInterval = 2,      # seconds between streamed-download progress lines (non-dynamic sessions)
    reproducible.drv = NULL, # RSQLite::SQLite(),
    reproducible.dryRun = FALSE,
    reproducible.fileBackedAnchors = NULL, # named list of semantic project paths (e.g. SpaDES paths(sim)); used to store/restore file-backed object paths *relative* to a portable anchor
    reproducible.futurePlan = FALSE, # future::plan("multisession"), #memoise
    reproducible.gdalwarpThreads = 2L,
    reproducible.gdriveNoAuth = FALSE,
    reproducible.inputPath = file.path(tempdir(), "reproducible", "input"),
    reproducible.destinationPathShared = NULL,
    reproducible.destinationPathSharedRecursive = FALSE,
    reproducible.inputPaths = NULL,           # deprecated alias for reproducible.destinationPathShared
    reproducible.inputPathsRecursive = FALSE, # deprecated alias for reproducible.destinationPathSharedRecursive
    reproducible.leaveOnDisk = TRUE,
    reproducible.length = Inf,
    reproducible.terraMemmax = 2, # GB; chunked path is faster on high-RAM machines

    reproducible.memoisePersist = FALSE,
    reproducible.messageColourPrepInputs = "cyan",
    reproducible.messageColourCache = "blue",
    reproducible.messageColourQuestion = "green",
    reproducible.messageColourFunction = "red",
    reproducible.nThreads = 1,
    reproducible.objSize = TRUE,
    reproducible.overwrite = FALSE,
    reproducible.preDigestDump = NULL,              # NULL/FALSE off; TRUE -> message each call's preDigest; or a dir path -> one file per call
    reproducible.preDigestDumpPattern = NULL,       # optional regex on .functionName to limit which calls are dumped
    reproducible.parallel.connecttimeout = 30L,     # seconds; per-connection establishment timeout for ranged streams
    reproducible.parallel.maxConnections = NULL,    # max simultaneous connections; NULL => parallelly::availableCores() - 1
    reproducible.parallel.minConcurrentFrac = 0.25, # fall back to single stream if 1st attempt completes < this frac of parts
    reproducible.parallel.streams = 48L,            # number of ranged parts; used only for urlRemap-redirected URLs
    reproducible.parallel.threshold = 10 * 1024^2,  # bytes; only files larger use the parallel path
    reproducible.quick = FALSE,
    reproducible.rasterRead = getEnv("R_REPRODUCIBLE_RASTER_READ",
      default = "terra::rast",
      allowed = c("terra::rast", "raster::raster")
    ),
    reproducible.shapefileRead = "sf::st_read",
    reproducible.showCachePreWarm = as.logical(getEnv(
      "R_REPRODUCIBLE_SHOWCACHE_PREWARM",
      # Mirror useDBI: reflect a value already set (e.g. FALSE by tests/covr) so
      # reproducibleOptions() stays identical to options() (see test-misc.R).
      default = getOption("reproducible.showCachePreWarm", TRUE),
      allowed = c("true", "false")
    )),
    reproducible.showSimilar = FALSE,
    reproducible.showSimilarDepth = 3,
    reproducible.tempPath = file.path(tempdir(), "reproducible"),
    reproducible.testCharacterAsFile = FALSE,
    reproducible.timeout = 12000,
    reproducible.useCOG = TRUE,
    reproducible.useCache = TRUE, # override Cache function
    reproducible.useCacheV3 = TRUE, # override Cache function
    reproducible.urlLog = NULL,
    reproducible.urlRemap = NULL,
    reproducible.useCloud = FALSE, #
    reproducible.useDBI = {
      getEnv("R_REPRODUCIBLE_USE_DBI",
      default = {
        useDBI(getOption("reproducible.useDBI", NULL),  # a user may have set it before this runs; keep setting
                       verbose = interactive() - (useDBI() + 1), default = FALSE)
        }, # `FALSE` is useMultipleDBFiles now
      allowed = c("true", "false")
    ) |> as.logical()},
    reproducible.useGdown = FALSE,
    reproducible.useMemoise = FALSE, # memoise
    reproducible.useragent = "https://github.com/PredictiveEcology/reproducible",
    reproducible.verbose = 1,
    reproducible.digestVersion = NULL, # NULL => 4; the going-forward digest-algorithm selector
    reproducible.digestV3 = TRUE,      # superseded by digestVersion (still honoured when it is unset)
    reproducible.digestV4 = FALSE      # superseded by digestVersion (still honoured when it is unset)
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

