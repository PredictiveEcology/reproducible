#' Fast spatial subsetting of Cloud Optimized GeoTiff (COG) files
#'
#' An alternative fast-path inside [prepInputs()] for remote Cloud Optimized GeoTiffs.
#' When a URL points to a COG and the user has specified a spatial subsetting argument
#' (`to`, `cropTo`, or `maskTo`), this function reads only the spatial window of interest
#' via GDAL's `/vsicurl/` virtual filesystem — no full-file download is needed.
#' The returned `SpatRaster` is passed back to `prepInputs` where the normal
#' `postProcess` step (mask, reproject, write) completes the pipeline.
#'
#' This function is called automatically from inside [prepInputs()] when
#' `getOption("reproducible.useCOG")` is `TRUE` (the default). It can also be
#' called directly.
#'
#' @param url Character. An HTTP(S) URL pointing to a GeoTiff file.
#' @param verbose Numeric or Logical. Verbosity level.
#' @param ... Passed through; expected to contain at least one of `to`, `cropTo`,
#'   or `maskTo` (a spatial object defining the area of interest).
#'
#' @return A `SpatRaster` windowed to the bounding box of the `to`/`cropTo`/`maskTo`
#'   object (in the COG's own CRS), or the character string `"NULL"` if any
#'   pre-condition fails (not HTTP, no spatial arg, network error, empty window, etc.).
#'
#' @seealso [prepInputs()], [prepInputsWithTiles()]
#' @export
prepInputsCOG <- function(url,
                          verbose = getOption("reproducible.verbose", 1),
                          ...) {

  # ---- Pre-conditions: fast exits before any network access ----------------
  if (is.null(url) || !grepl("^https?://", url))
    return("NULL")

  # Strip query/fragment, then require a GeoTiff-style extension. This avoids
  # firing `/vsicurl/` reads against archives (.zip, .tar, .gz, ...) or other
  # non-raster URLs, which would emit a GDAL warning before failing.
  url_path <- sub("[?#].*$", "", url)
  if (!grepl("\\.(tif|tiff|cog|gtiff)$", url_path, ignore.case = TRUE))
    return("NULL")

  dots   <- list(...)
  to_obj <- if (!is.null(dots$to)) dots$to else if (!is.null(dots$cropTo)) dots$cropTo else dots$maskTo
  if (is.null(to_obj))
    return("NULL")

  if (!.requireNamespace("terra", stopOnFALSE = FALSE))
    return("NULL")

  if (!internetExists())
    return("NULL")

  vsicurl_path <- paste0("/vsicurl/", url)

  # ---- Open metadata-only remote connection --------------------------------
  r_meta <- tryCatch(
    terra::rast(vsicurl_path),
    error = function(e) NULL
  )
  if (is.null(r_meta))
    return("NULL")

  # ---- Reproject to_obj extent into COG's CRS for the window read ----------
  # Both `terra::crs()` and `postProcessTo()` can fail on a system where PROJ
  # is misconfigured (e.g. missing proj.db on macOS), in ways that range from
  # an empty CRS string to an unhandled C-level error. Wrap both, and bail
  # out to the regular `prepInputs` download path on any failure.
  cog_crs <- tryCatch(terra::crs(r_meta), error = function(e) NA_character_)
  if (is.na(cog_crs) || !nzchar(cog_crs))
    return("NULL")
  to_reproj <- tryCatch(
    postProcessTo(to_obj, to = cog_crs, verbose = verbose - 2),
    error = function(e) NULL
  )
  if (is.null(to_reproj))
    return("NULL")
  to_ext <- tryCatch(terra::ext(to_reproj), error = function(e) NULL)
  if (is.null(to_ext))
    return("NULL")

  # ---- Window-crop: GDAL fetches only tile blocks intersecting to_ext ------
  r_windowed <- tryCatch({
    terra::window(r_meta) <- to_ext
    terra::crop(r_meta, to_ext)
  }, error = function(e) NULL)

  if (is.null(r_windowed) || terra::ncell(r_windowed) == 0L)
    return("NULL")

  messagePreProcess(
    "prepInputsCOG: windowed remote read complete (tiled GeoTiff); full postProcess will follow",
    verbose = verbose
  )

  r_windowed
}
