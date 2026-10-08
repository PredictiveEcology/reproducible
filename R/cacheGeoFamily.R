## The CacheGeo family: a ledger of spatial rows (a polygon plus any columns, list-columns included)
## kept in one .rds file, optionally shared through a remote. See ?CacheGeoLedger.

.cacheGeoMatches <- c("intersects", "covers", "within")
.cacheGeoModes <- c("upsert", "append")
.cacheGeoRemoteTypes <- c("auto", "drive", "disk", "url")
.cacheGeoMaxTries <- 3L # a write that finds the remote changed under it merges again this many times

## Per-session state: the ledger files read so far (keyed on path, valid while the file's md5 is
## the same) and whether the old-argument message has been given.
.cacheGeoSession <- new.env(parent = emptyenv())
.cacheGeoSession$reads <- new.env(parent = emptyenv())
.cacheGeoSession$legacyNoted <- FALSE

#' A ledger of spatial rows, shared through a folder or Google Drive
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' A *ledger* is one `.rds` file of spatial rows: a data.frame with a geometry column (and
#' any other columns, including list-columns such as fitted parameters or models). The
#' functions here describe a ledger, read the rows that overlap an area, add rows, and run a
#' computation only for an area the ledger does not yet cover.
#'
#' * `CacheGeoLedger()` describes a ledger. It does no input or output.
#' * `CacheGeoRead()` returns the rows that match an `area`, as an `sf` object in the ledger's
#'   CRS (0 rows if none match). It never writes, never creates a folder and never uploads. If
#'   the ledger has a `remote`, the file is fetched only when its md5 differs from the local
#'   copy's. Reading a file on Google Drive that is shared "Anyone with the link" needs no
#'   Google login.
#' * `CacheGeoWrite()` adds rows. `mode = "upsert"` replaces the rows that have the same key
#'   (or, with no `key`, the same geometry); `mode = "append"` always adds. It reads the
#'   remote again just before writing, writes, pushes only if the content changed, and checks
#'   the push. If another writer changed the remote in between, it merges again, up to 3
#'   times. On a shared-disk folder the write is also held under a file lock. Writing to Google
#'   Drive needs a Google login (see `googledrive::drive_auth()`).
#' * `CacheGeo()` reads; if the `area` is not covered by the ledger it calls `compute(area)`,
#'   writes the result and returns the rows. An error in `compute` is an error in `CacheGeo()`.
#'
#' @section Remotes:
#' `remote` can be a folder on a shared disk (e.g. `"/mnt/fast/ledgers"`), a Google Drive
#' folder (a `drive.google.com` URL or a folder id; the folder is created only by
#' `CacheGeoWrite()`), or a read-only URL (the ledger is `remote/file`; `CacheGeoWrite()`
#' stops). With no `remote` the ledger is the local file only.
#'
#' To share a ledger through Google Drive, create a folder, share it "Anyone with the link"
#' as a viewer, and give the folder's URL as `remote` to everyone. Only the people who write
#' need a Google login.
#'
#' @section Old arguments of `CacheGeo()`:
#' `targetFile`, `domain`, `FUN` (an unevaluated call that may use `domain` and the named
#' objects in `...`), `cloudFolderID`, `useCloud`, `action`, `bufferOK`, `purge`, `useCache`
#' and `overwrite` are still accepted, with one message per session. `action = "nothing"`
#' neither computes nor writes, `"update"` always calls `FUN` and replaces the rows with the
#' same `polygonID` (or the same geometry), `"append"` adds. `bufferOK = TRUE` becomes a
#' `tolerance` of 2.5% of the ledger's extent. `purge`, `useCache` and `overwrite` are ignored.
#' A `targetFile` that is not `.rds` is written as `.rds`.
#'
#' @param file The ledger's file name, e.g. `"fireSenseParams.rds"`.
#' @param remote `NULL` (a local ledger), a folder on a shared disk, a Google Drive folder
#'   (URL, id, or a `dribble`), or a read-only URL.
#' @param destinationPath The local folder that holds the ledger file (or its local copy).
#' @param key The name of the column that identifies a row, e.g. `"polygonID"`. `NULL` for a
#'   ledger without one.
#' @param remoteType How to treat `remote`. `"auto"` decides from `remote`: a Google Drive URL
#'   or id is `"drive"`, another `http(s)` URL is `"url"`, anything else is `"disk"`. A
#'   folder *name* on Google Drive has to be given as `"drive"`.
#' @param ledger A `CacheGeoLedger` object.
#' @param area The area of interest: an `sf`, `sfc` or `SpatVector` object, in any CRS (it is
#'   transformed to the ledger's). `NULL` asks for the whole ledger and has to be given
#'   explicitly.
#' @param match Which rows to return for the `area`. `"intersects"` (the default): every row
#'   whose polygon overlaps the area. `"covers"`: the overlapping rows, but only when together
#'   they cover the area (otherwise none). `"within"`: the rows whose polygon lies inside the
#'   area.
#' @param tolerance A distance, in the ledger's map units (degrees for a longitude/latitude CRS). A row whose overlap with the area
#'   is gone after shrinking it inward by `tolerance` is a sliver and is not returned; each
#'   one is named in the message. For `"covers"`, gaps that narrow are ignored. For `"within"`,
#'   the area is widened by `tolerance`.
#' @param rows The rows to add: an `sf` or `SpatVector` object, or a data.frame with a
#'   geometry column. They are transformed to the ledger's CRS.
#' @param mode `"upsert"` (default) or `"append"`; see above.
#' @param compute A function of one argument, the `area`, that returns the rows for it (an
#'   `sf` object or a data.frame with a geometry column).
#' @param ... In `CacheGeo()`, the named objects that the old `FUN` call uses. Unnamed
#'   arguments are `ledger`, `area`, `compute`, `match`, `tolerance` and `verbose`, in that
#'   order. `...` comes first so that an object named e.g. `le` is not partially matched to
#'   `ledger`; the arguments after it must be named in full.
#' @param targetFile,domain,FUN,cloudFolderID,useCloud,action,bufferOK,purge,useCache,overwrite
#'   The old arguments of `CacheGeo()`; see above.
#' @param verbose `0` is silent, `1` (default) gives one short message per call, `2` also
#'   prints the rows returned.
#'
#' @return `CacheGeoLedger()` a `CacheGeoLedger` object. `CacheGeoRead()` and `CacheGeo()` an
#'   `sf` object in the ledger's CRS, possibly with 0 rows. `CacheGeoWrite()` the whole ledger
#'   as an `sf` object, invisibly.
#'
#' @export
#' @examplesIf requireNamespace("sf", quietly = TRUE) && requireNamespace("terra", quietly = TRUE)
#' square <- function(id, x0, x1) {
#'   box <- sf::st_polygon(list(rbind(c(x0, 0), c(x1, 0), c(x1, 10), c(x0, 10), c(x0, 0))))
#'   out <- sf::st_sf(polygonID = id, geometry = sf::st_sfc(box, crs = 32618))
#'   out$params <- I(list(list(fit = id)))
#'   out
#' }
#' led <- CacheGeoLedger("params.rds", destinationPath = tempdir2("cacheGeoEx"), key = "polygonID")
#'
#' # write: one row per key; writing the same key again replaces the row
#' CacheGeoWrite(led, rbind(square("A", 0, 10), square("B", 10, 20)))
#' CacheGeoWrite(led, square("A", 0, 10))
#'
#' # read: the rows that overlap an area; always an sf object, 0 rows if none
#' area <- sf::st_as_sf(sf::st_sfc(sf::st_point(c(12, 5)), crs = 32618) |> sf::st_buffer(2))
#' CacheGeoRead(led, area)$polygonID
#' CacheGeoRead(led, area, match = "covers")$polygonID
#'
#' # read-or-compute: compute() runs only for an area the ledger does not cover
#' area2 <- sf::st_as_sf(sf::st_sfc(sf::st_point(c(25, 5)), crs = 32618) |> sf::st_buffer(2))
#' rows <- CacheGeo(led, area2, compute = function(area) square("C", 20, 30))
#' rows$polygonID
CacheGeoLedger <- function(file, remote = NULL,
                           destinationPath = getOption("reproducible.destinationPath", "."),
                           key = NULL, remoteType = .cacheGeoRemoteTypes) {
  remoteType <- match.arg(remoteType)
  if (isAbsolutePath(file)) {
    destinationPath <- dirname(file)
    file <- basename(file)
  }
  if (is.null(remote)) {
    remoteType <- "none"
  } else {
    if (inherits(remote, "dribble")) remote <- remote$id
    remote <- as.character(remote)
    if (identical(remoteType, "auto")) {
      remoteType <- if (isGoogleDriveURL(remote) || isGoogleID(remote)) "drive" else
        if (grepl("^https?://", remote)) "url" else "disk"
    }
  }
  structure(list(file = file, remote = remote, remoteType = remoteType,
                 destinationPath = destinationPath, key = key,
                 path = file.path(destinationPath, file)),
            class = "CacheGeoLedger")
}

#' @export
print.CacheGeoLedger <- function(x, ...) {
  cat("<CacheGeoLedger> ", x$path, "\n  remote: ",
      if (is.null(x$remote)) "none (local file only)" else paste0(x$remote, " (", x$remoteType, ")"),
      "\n  key: ", if (is.null(x$key)) "none (rows with the same geometry are replaced)" else x$key,
      "\n", sep = "")
  invisible(x)
}

#' @export
#' @rdname CacheGeoLedger
CacheGeoRead <- function(ledger, area, match = .cacheGeoMatches, tolerance = 0,
                         verbose = getOption("reproducible.verbose", 1)) {
  if (missing(area))
    stop("`area` is required; use `area = NULL` to read the whole ledger")
  match <- match.arg(match)
  .geoRead(ledger, area, match, tolerance, verbose)$rows
}

#' @export
#' @rdname CacheGeoLedger
CacheGeoWrite <- function(ledger, rows, mode = .cacheGeoModes,
                          verbose = getOption("reproducible.verbose", 1)) {
  mode <- match.arg(mode)
  if (identical(ledger$remoteType, "url"))
    stop("The ledger's remote (", ledger$remote, ") is a read-only URL; CacheGeoWrite() cannot write ",
         "to it. Use a Google Drive folder or a shared-disk folder as `remote`.")
  rows <- .geoToSf(rows)
  dir.create(ledger$destinationPath, recursive = TRUE, showWarnings = FALSE)
  .geoWithRemote(ledger, login = TRUE, verbose = verbose, expr =
    .geoWithLock(ledger, .geoWriteAttempts(ledger, rows, mode, verbose))
  )
}

#' @export
#' @rdname CacheGeoLedger
CacheGeo <- function(..., ledger, area, compute, match = .cacheGeoMatches, tolerance = 0,
                     verbose = getOption("reproducible.verbose", 1),
                     targetFile = NULL, domain, FUN,
                     destinationPath = getOption("reproducible.destinationPath", "."),
                     useCloud = getOption("reproducible.useCloud", FALSE), cloudFolderID = NULL,
                     purge = FALSE, useCache = getOption("reproducible.useCache"),
                     overwrite = getOption("reproducible.overwrite"),
                     action = c("nothing", "update", "replace", "append"), bufferOK = FALSE) {
  dots <- list(...)
  named <- if (is.null(names(dots))) rep(FALSE, length(dots)) else nzchar(names(dots))
  if (!all(named)) {
    slots <- c("ledger", "area", "compute", "match", "tolerance", "verbose")
    slots <- slots[c(missing(ledger), missing(area), missing(compute), missing(match),
                     missing(tolerance), missing(verbose))]
    if (sum(!named) > length(slots)) stop("Too many unnamed arguments")
    for (i in seq_len(sum(!named))) assign(slots[i], dots[!named][[i]])
    dots <- dots[named]
  }
  legacy <- missing(ledger) || is.character(ledger)
  refit <- FALSE
  if (legacy) {
    if (!missing(ledger)) targetFile <- ledger
    if (is.null(targetFile)) stop("Either targetFile must be supplied")
    action <- match.arg(action)
    .geoLegacyNote()
    ledger <- .geoLegacyLedger(targetFile, destinationPath, useCloud, cloudFolderID)
    match <- if (missing(match)) "covers" else match.arg(match)
    if (missing(area)) {
      area <- if (missing(domain)) {
        message("Spatial domain is missing; returning entire spatial domain")
        NULL
      } else domain
    }
    if (isTRUE(bufferOK)) tolerance <- .geoBufferOK
    if (!missing(FUN)) {
      compute <- .geoLegacyCompute(substitute(FUN), dots, parent.frame())
      refit <- identical(action, "update") # a refit replaces the row of a covered area
    }
  } else {
    match <- match.arg(match)
    if (missing(area)) stop("`area` is required; use `area = NULL` to read the whole ledger")
    if (length(dots)) stop("Unused arguments: ", paste(names(dots), collapse = ", "))
    action <- "update"
  }

  sel <- .geoRead(ledger, area, match, tolerance, verbose, wantCovered = TRUE)
  if (isTRUE(sel$covered)) {
    message(.message$cacheGeoDomainContained)
    if (!refit) return(sel$rows)
  } else if (!sel$exists) {
    message(.message$cacheGeoNoRemoteExists)
  } else {
    message(.message$cacheGeoDomainNotContained)
  }
  if (missing(compute)) {
    message("FUN is missing; no evaluation possible")
    return(sel$rows)
  }
  if (identical(action, "nothing")) {
    message("The spatial domain is new, and should be added, but\n")
    message("action was 'nothing'; nothing done")
    return(sel$rows)
  }

  newRows <- .geoToSf(compute(area))
  if (legacy && is.null(ledger$key) && "polygonID" %in% names(newRows)) ledger$key <- "polygonID"
  merged <- CacheGeoWrite(ledger, newRows, mode = if (identical(action, "append")) "append" else "upsert",
                          verbose = verbose)
  .geoSelect(merged, area, match, tolerance, verbose, key = ledger$key)$rows
}

## ---- the old CacheGeo() arguments -------------------------------------------------------------

.geoLegacyNote <- function() {
  if (!.cacheGeoSession$legacyNoted) {
    .cacheGeoSession$legacyNoted <- TRUE
    message("CacheGeo(): the arguments targetFile, domain, FUN, cloudFolderID, useCloud, action, ",
            "bufferOK, purge, useCache and overwrite are kept but replaced by CacheGeoLedger(), ",
            "CacheGeoRead(), CacheGeoWrite() and CacheGeo(ledger, area, compute); see ?CacheGeoLedger")
  }
}

.geoIsRds <- function(file) identical(tolower(fs::path_ext(file)), "rds")

.geoLegacyLedger <- function(targetFile, destinationPath, useCloud, cloudFolderID) {
  led <- CacheGeoLedger(targetFile, destinationPath = destinationPath)
  if (!.geoIsRds(led$file)) {
    rdsFile <- paste0(tools::file_path_sans_ext(led$file), ".rds")
    led <- CacheGeoLedger(rdsFile, destinationPath = led$destinationPath)
    warning("Dropping the '.", fs::path_ext(targetFile), "' format, which cannot hold list-columns. ",
            .message$BecauseOfLossOfColumn(led$path))
  }
  remote <- if (!is.null(cloudFolderID)) cloudFolderID else
    if (isTRUE(useCloud)) getOption("reproducible.cloudFolderID") else NULL
  if (!is.null(remote))
    led <- CacheGeoLedger(led$file, remote = remote, destinationPath = led$destinationPath,
                          remoteType = "drive")
  led
}

## `FUN` was an unevaluated call that could use `domain` and the objects in `...`
.geoLegacyCompute <- function(call, objects, callerEnv) {
  env <- list2env(objects, parent = callerEnv)
  function(area) {
    env$domain <- area
    eval(call, envir = env)
  }
}

## The old `bufferOK = TRUE` buffered by 2.5% of the extent of the ledger
.geoBufferOK <- function(rows) {
  sides <- vapply(list(c("xmin", "xmax"), c("ymin", "ymax")), function(mm)
    abs(diff(sf::st_bbox(rows)[mm])), numeric(1))
  mean(sides) * 0.025
}

## ---- reading -----------------------------------------------------------------------------------

## Fetch the remote if it changed, read the file, select the rows. `wantCovered`: also say whether
## the area is covered (the rule of `match = "covers"`), whatever `match` is.
.geoRead <- function(ledger, area, match, tolerance, verbose, wantCovered = FALSE) {
  sync <- .geoWithRemote(ledger, login = FALSE, verbose = verbose, expr = .geoSync(ledger, verbose))
  all <- if (sync$exists) .geoLoad(ledger$path, sync$md5) else .geoEmpty()
  sel <- .geoSelect(all, area, match, tolerance, verbose, key = ledger$key, wantCovered = wantCovered)
  messagePreProcess("CacheGeoRead: ", ledger$file, ": ", NROW(sel$rows), " of ", NROW(all),
                    " row(s) matched (", sync$state, ")", verbose = verbose)
  if (verbose >= 2) print(sel$rows)
  c(sel, list(exists = sync$exists))
}

.geoEmpty <- function() sf::st_sf(geometry = sf::st_sfc())

.geoToSf <- function(x) {
  if (!inherits(x, "sf")) x <- sf::st_as_sf(x)
  checkNameHasGeom(x)
}

## An old file may keep its CRS in a `crs` list-column rather than on the geometry
.geoReadFile <- function(path) {
  x <- if (.geoIsRds(path)) readRDS(path) else sf::st_read(path, quiet = TRUE)
  x <- .geoToSf(x)
  if (is.na(sf::st_crs(x)) && "crs" %in% names(x))
    suppressWarnings(sf::st_crs(x) <- x[["crs"]][[1]])
  x <- x[!sf::st_is_empty(x), ]
  rownames(x) <- NULL
  x
}

## The file is read again only when its md5 is not the one last read.
.geoLoad <- function(path, md5) {
  seen <- get0(path, envir = .cacheGeoSession$reads, inherits = FALSE)
  if (!is.null(seen) && identical(seen$md5, md5)) return(seen$rows)
  rows <- .geoReadFile(path)
  assign(path, list(md5 = md5, rows = rows), envir = .cacheGeoSession$reads)
  rows
}

## `area` as one geometry in the ledger's CRS. An area with no CRS is taken to be in the ledger's.
.geoAreaGeometry <- function(area, crs) {
  if (!inherits(area, c("sf", "sfc"))) area <- sf::st_as_sf(area)
  if (is.na(sf::st_crs(area))) {
    sf::st_crs(area) <- crs
  } else if (!is.na(crs) && sf::st_crs(area) != crs) {
    area <- sf::st_transform(area, crs)
  }
  sf::st_union(sf::st_geometry(area))
}

## Is this geometry, shrunk inward by `tolerance`, empty? With `tolerance = 0` a line or a point
## (two polygons that only touch) counts as empty: it has no area.
.geoShrunkEmpty <- function(geom, tolerance) {
  if (tolerance > 0) return(sf::st_is_empty(sf::st_buffer(geom, -tolerance)))
  sf::st_is_empty(geom) | sf::st_dimension(geom) %in% c(0L, 1L)
}

## Rows that overlap `areaGeom`, less the slivers; the slivers are named in a message
.geoOverlaps <- function(rows, areaGeom, tolerance, key, verbose) {
  hit <- lengths(sf::st_intersects(rows, areaGeom)) > 0
  geom <- sf::st_geometry(rows)
  sliver <- vapply(which(hit), function(i)
    all(.geoShrunkEmpty(sf::st_intersection(geom[i], areaGeom), tolerance)), logical(1))
  dropped <- which(hit)[sliver]
  hit[dropped] <- FALSE
  if (length(dropped) && tolerance > 0) {
    ids <- if (!is.null(key) && key %in% names(rows)) rows[[key]][dropped] else dropped
    messagePreProcess("CacheGeoRead: tolerance ", format(tolerance), " dropped sliver polygon(s): ",
                      paste(ids, collapse = ", "), verbose = verbose)
  }
  hit
}

.geoSelect <- function(all, area, match, tolerance, verbose, key = NULL, wantCovered = FALSE) {
  if (is.null(area)) return(list(rows = all, covered = TRUE))
  if (!NROW(all)) return(list(rows = all, covered = FALSE))
  if (is.function(tolerance)) tolerance <- tolerance(all)
  .geoPlanar({
    areaGeom <- .geoAreaGeometry(area, sf::st_crs(all))
    if (identical(match, "within")) {
      widened <- if (tolerance > 0) sf::st_buffer(areaGeom, tolerance) else areaGeom
      list(rows = all[lengths(sf::st_within(sf::st_geometry(all), widened)) > 0, ], covered = NA)
    } else {
      rows <- all[.geoOverlaps(all, areaGeom, tolerance, key, verbose), ]
      covered <- if (identical(match, "covers") || wantCovered) .geoCovered(rows, areaGeom, tolerance) else NA
      if (identical(match, "covers") && !covered) rows <- rows[integer(0), ]
      list(rows = rows, covered = covered)
    }
  })
}

## Is `areaGeom` covered by the union of `rows`, apart from gaps narrower than `tolerance`?
.geoCovered <- function(rows, areaGeom, tolerance) {
  if (!NROW(rows)) return(FALSE)
  geomOnly <- sf::st_sf(geometry = sf::st_geometry(rows))
  extractPolygonIfWithin(domain = sf::st_sf(geometry = areaGeom), existingObjSF = geomOnly,
                         bufferOK = FALSE, existingObj = geomOnly, verbose = FALSE,
                         tolerance = tolerance)$domainExisted
}

## ---- remotes -----------------------------------------------------------------------------------

## Google Drive reads need no login for a file shared "Anyone with the link"; googledrive is left
## anonymous then. A write needs one.
.geoWithRemote <- function(ledger, login, verbose, expr) {
  if (identical(ledger$remoteType, "drive")) {
    .requireNamespace("googledrive", stopOnFALSE = TRUE, messageStart = "to use google drive files")
    mode <- .gdrivePrepareAuth(ledger$remote, verbose = verbose - 1)
    on.exit(.gdriveRestoreAuth())
    if (isTRUE(login) && !identical(mode, "token"))
      stop("CacheGeoWrite() to Google Drive needs a Google login: run googledrive::drive_auth() first. ",
           "(Reading a file shared 'Anyone with the link' does not.)")
  }
  force(expr)
}

## One writer at a time on a shared-disk folder
.geoWithLock <- function(ledger, expr) {
  if (!identical(ledger$remoteType, "disk")) return(expr)
  dir.create(ledger$remote, recursive = TRUE, showWarnings = FALSE)
  withLockFile(file.path(ledger$remote, paste0(ledger$file, suffixLockFile())), expr)
}

## The Drive folder as a dribble. NULL if it does not exist and `create` is FALSE.
.geoDriveFolder <- function(ledger, create) {
  folder <- suppressWarnings(checkAndMakeCloudFolderID(ledger$remote, create = FALSE, verbose = 0))
  if (!is(folder, "dribble") && isTRUE(create))
    folder <- suppressWarnings(checkAndMakeCloudFolderID(ledger$remote, create = TRUE, verbose = 0))
  if (is(folder, "dribble")) folder else NULL
}

## The ledger file on a shared-disk remote
.geoRemoteFile <- function(ledger) file.path(ledger$remote, ledger$file)

.geoFileUrl <- function(ledger) {
  if (identical(basename(ledger$remote), ledger$file)) ledger$remote else
    paste0(sub("/$", "", ledger$remote), "/", ledger$file)
}

## Does the ledger file exist on the remote, what is its md5 and where can it be fetched from?
## `md5` is NULL when the remote cannot say (a URL; prepInputs() then checks it with its hash sidecars).
.geoRemoteState <- function(ledger) {
  none <- list(exists = FALSE, md5 = NULL, url = NULL)
  switch(ledger$remoteType,
    none = none,
    disk = {
      p <- .geoRemoteFile(ledger)
      if (file.exists(p)) list(exists = TRUE, md5 = digest::digest(file = p), url = p) else none
    },
    url = list(exists = TRUE, md5 = NULL, url = .geoFileUrl(ledger)),
    drive = {
      folder <- .geoDriveFolder(ledger, create = FALSE)
      if (is.null(folder)) return(none)
      files <- driveLs(folder, verbose = 0)
      files <- files[files$name %in% ledger$file, ]
      if (!NROW(files)) return(none)
      resources <- files$drive_resource
      modified <- vapply(resources, `[[`, character(1), "modifiedTime") # ISO times sort as text
      latest <- resources[[order(modified, decreasing = TRUE)[1]]]
      list(exists = TRUE, md5 = latest$md5Checksum, url = latest$webViewLink)
    })
}

## Make the local file the remote one. Returns where the local file stands afterwards.
.geoSync <- function(ledger, verbose) {
  localMd5 <- function() if (file.exists(ledger$path)) digest::digest(file = ledger$path)
  md5 <- localMd5()
  remote <- .geoRemoteState(ledger)
  state <- if (is.null(ledger$remote)) "local file" else "no file on the remote yet"
  if (remote$exists) {
    if (!is.null(md5) && identical(md5, remote$md5)) {
      state <- "up to date"
    } else {
      if (identical(ledger$remoteType, "disk")) {
        .geoReplaceFile(remote$url, ledger$path)
      } else {
        dir.create(ledger$destinationPath, recursive = TRUE, showWarnings = FALSE)
        ## Drive: the md5 differs, so fetch. A URL has no md5 to compare: prepInputs() decides from its hash sidecar.
        prepInputs(url = remote$url, targetFile = ledger$file, destinationPath = ledger$destinationPath,
                   fun = NA, overwrite = !identical(ledger$remoteType, "url"), useCache = FALSE,
                   verbose = verbose - 2)
      }
      state <- if (identical(md5, localMd5())) "up to date" else "downloaded"
      md5 <- localMd5()
    }
  }
  list(exists = !is.null(md5), md5 = md5, remoteMd5 = remote$md5, state = state)
}

## Copy `from` over `to` so that `to` is always the old file or the new one, never partly written
.geoReplaceFile <- function(from, to) {
  dir.create(dirname(to), recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile(pattern = paste0(".", basename(to), "."), tmpdir = dirname(to))
  if (!file.copy(from, tmp)) stop("Could not copy ", from, " to ", tmp)
  if (!file.rename(tmp, to)) {
    unlink(tmp)
    stop("Could not replace ", to)
  }
  invisible(to)
}

.geoPush <- function(ledger) {
  switch(ledger$remoteType,
    disk = .geoReplaceFile(ledger$path, .geoRemoteFile(ledger)),
    drive = {
      folder <- .geoDriveFolder(ledger, create = TRUE)
      retry(quote(googledrive::drive_put(media = ledger$path, path = googledrive::as_id(folder$id),
                                         name = ledger$file)))
    })
  invisible(NULL)
}

## ---- writing -----------------------------------------------------------------------------------

## Sync, merge, save, push, check the push; merge again if another writer got in between.
.geoWriteAttempts <- function(ledger, rows, mode, verbose) {
  for (attempt in seq_len(.cacheGeoMaxTries)) {
    sync <- .geoSync(ledger, verbose)
    current <- if (sync$exists) .geoLoad(ledger$path, sync$md5) else .geoEmpty()
    merged <- .geoMerge(current, rows, ledger$key, mode)
    localMd5 <- .geoSave(merged, ledger$path, sync$md5)
    if (is.null(ledger$remote) || identical(localMd5, sync$remoteMd5)) {
      messagePreProcess("CacheGeoWrite: ", ledger$file, ": ", NROW(merged), " row(s)",
                        if (identical(localMd5, sync$md5)) ", unchanged" else ", written", verbose = verbose)
      return(invisible(merged))
    }
    .geoPush(ledger)
    if (identical(.geoRemoteState(ledger)$md5, localMd5)) {
      messagePreProcess("CacheGeoWrite: ", ledger$file, ": ", NROW(merged), " row(s), pushed to ",
                        ledger$remote, verbose = verbose)
      return(invisible(merged))
    }
    messagePreProcess("CacheGeoWrite: ", ledger$file, " changed on the remote during the write; ",
                      "merging again (", attempt, " of ", .cacheGeoMaxTries, ")", verbose = verbose)
  }
  stop("CacheGeoWrite: ", ledger$file, " kept changing on ", ledger$remote, "; gave up after ",
       .cacheGeoMaxTries, " attempts")
}

## Which rows of `current` do `rows` replace? Same key; with no key, same geometry.
.geoReplaced <- function(current, rows, key) {
  if (!NROW(current)) return(logical(0))
  if (is.null(key)) return(rowSums(sf::st_equals(current, rows, sparse = FALSE)) > 0)
  if (!key %in% names(rows)) stop("The new rows have no `", key, "` column (the ledger's key)")
  if (!key %in% names(current)) return(rep(FALSE, NROW(current)))
  current[[key]] %in% rows[[key]]
}

## Polygons and multipolygons are kept as multipolygons, each side before they are joined, so a row
## that is not changed is saved as it was.
.geoMultiPolygon <- function(geom) {
  if (all(sf::st_geometry_type(geom) %in% c("POLYGON", "MULTIPOLYGON")))
    sf::st_cast(geom, "MULTIPOLYGON") else geom
}

.geoMerge <- function(current, rows, key, mode) {
  if (!NROW(current)) current <- rows[integer(0), ] # nothing yet: the ledger takes the new rows' CRS
  crs <- sf::st_crs(current)
  if (!is.na(crs) && !is.na(sf::st_crs(rows)) && sf::st_crs(rows) != crs)
    rows <- sf::st_transform(rows, crs)
  keep <- if (identical(mode, "upsert")) !.geoReplaced(current, rows, key) else rep(TRUE, NROW(current))
  current <- current[keep, ]
  ## rbindlist() takes the data.frames as they are. Not as.data.table() or copy(): they stop on a
  ## list-column that holds an ALTREP object read back from an rds (an xgboost model). setDF(), not
  ## as.data.frame(), which copy()s: a copied xgboost model is a new one, so the rows returned
  ## would no longer be identical to those `compute` returned.
  attrs <- data.table::setDF(data.table::rbindlist(
    list(sf::st_drop_geometry(current), sf::st_drop_geometry(rows)), fill = TRUE, use.names = TRUE))
  geom <- c(.geoMultiPolygon(sf::st_geometry(current)), .geoMultiPolygon(sf::st_geometry(rows)))
  merged <- sf::st_sf(attrs, geometry = geom)
  if (!is.null(key) && key %in% names(merged)) merged <- merged[order(merged[[key]]), ]
  rownames(merged) <- NULL
  merged
}

## Save to a temporary name and rename over the file, unless the content is what is already there.
## Returns the md5 of the file.
.geoSave <- function(rows, path, currentMd5) {
  tmp <- tempfile(pattern = paste0(".", basename(path), "."), tmpdir = dirname(path))
  saveRDS(as.data.frame(rows), tmp)
  md5 <- digest::digest(file = tmp)
  if (identical(md5, currentMd5)) unlink(tmp) else file.rename(tmp, path)
  md5
}
