#' @param x A simList or a directory containing a valid archivist repository
#' @param after A time (POSIX, character understandable by data.table).
#'                  Objects cached after this time will be shown or deleted.
#' @param before A time (POSIX, character understandable by data.table).
#'                   Objects cached before this time will be shown or deleted.
#' @param ... Other arguments. Currently unused.
#'
#' If neither \code{after} or \code{before} are provided, nor \code{userTags},
#'  then all objects will be removed.
#' If both \code{after} and \code{before} are specified, then all objects between \code{after} and
#' \code{before} will be deleted.
#' If \code{userTags} is used, this will override \code{after} or \code{before}.
#'
#' @return Will clear all (or that match \code{userTags}, or between \code{after} or \code{before})
#' objects from the repository located at \code{cachePath} of the sim object,
#' if \code{sim} is provided, or located in \code{cacheRepo}. Also returns a data.table invisibly
#' of the removed items.
#'
#' @export
#' @importFrom archivist rmFromLocalRepo searchInLocalRepo
#' @param userTags Character vector. If used, this will be used in place of the \code{after} and
#'                 \code{before}. Specifying one or more \code{userTag} here will
#'                 clear all objects that
#'                 match those tags. Matching is via regular expresssion, meaning
#'                 partial matches
#'                 will work unless strict beginning (^) and end ($) of string
#'                 characters are used. Matching
#'                 will be against any of the 3 columns returned by \code{showCache()},
#'                 i.e., artifact, tagValue or tagName. Also, length \code{userTags} > 1,
#'                 then matching is by `and`. For `or` matching, use | in a single character
#'                 string. See examples.
#'
#' @docType methods
#' @rdname viewCache
#' @examples
#' \dontrun{
#' clearCache(mySim)
#'
#' mySim <- simInit(
#'   times = list(start = 0.0, end = 1.0, timeunit = "year"),
#'   params = list(
#'     .globals = list(stackName = "landscape", burnStats = "nPixelsBurned"),
#'     # Turn off interactive plotting
#'     fireSpread = list(.plotInitialTime = NA),
#'     caribouMovement = list(.plotInitialTime = NA),
#'     randomLandscapes = list(.plotInitialTime = NA, .useCache = "init")
#'   ),
#'   modules = list("randomLandscapes", "fireSpread", "caribouMovement"),
#'   paths = list(modulePath = system.file("sampleModules", package = "SpaDES"),
#'                outputPath = tmpdir,
#'                cachePath = tmpdir),
#'   # Save final state of landscape and caribou
#'   outputs = data.frame(objectName = c("landscape", "caribou"),
#'                        stringsAsFactors = FALSE)
#' )
#'
#' ## the caching is inside randomLandscape module
#' sims <- spades(Copy(mySim), notOlderThan = Sys.time())
#' showCache(mySim)
#'
#' ranNums <- Cache(runif, 4, cacheRepo=cachePath(mySim), userTags = "objectName:a")
#'
#' showCache(mySim, userTags = c("objectName"))
#' showCache(mySim, userTags = c("^a$")) # regular expression ... "a" exactly
#' showCache(mySim, userTags = c("eventTime")) # show only cached objects made during spades call
#'
#' clearCache(mySim, userTags = c("eventTime")) # remove only cached objects made during spades call
#' showCache(mySim) # only those made during spades call they are gone
#'
#'# example using the "accessed" tag
#'  clearCache(mySim)
#'  sims <- spades(Copy(mySim), notOlderThan = Sys.time())
#'  ranNums <- Cache(runif, 4, cacheRepo=cachePath(mySim), userTags = "objectName:a")
#'  # access it again, but "later"
#'  Sys.sleep(1)
#'  sims <- spades(Copy(mySim)) # i.e., this is a "read" operation, does not create a new artifact
#'
#'  wholeCache <- showCache(mySim)
#'  # keep only items accessed "recently"
#'  onlyRecentlyAccessed <- showCache(mySim,
#'                                    userTags = max(wholeCache[tagKey=="accessed"]$tagValue))
#'  # inverse join with 2 data.tables ... using: a[!b]
#'      # i.e., return all of wholeCache that was not recently accessed
#'  toRemove <- unique(wholeCache[!onlyRecentlyAccessed], by="artifact")$artifact
#'  clearCache(mySim, toRemove) # remove ones not recently accessed
#'  showCache(mySim) # still has more recently accessed,
#'                   #  based on time passed to onlyRentlyAccessed
#'
#'  # keepCache examples
#'  # keep only those cached items from the last 24 hours
#'  keepCache(mySim, after = Sys.time() - dday(1))
#'
#'  # Keep all Cache items that happened within a spades call
#'  keepCache(mySim, userTags = "spades")
#'  # Remove all Cache items that happened within a spades call
#'  clearCache(mySim, userTags = "spades")
#'
#'  # Default userTags is "and" matching. For "or" matching use |
#'  ranNums <- Cache(runif, 4, cacheRepo=cachePath(mySim), userTags = "objectName:a")
#'  ranNums <- Cache(rnorm, 4, cacheRepo=cachePath(mySim), userTags = "objectName:a")
#'  showCache(mySim) # shows spades, runif and rnorm objects
#'  showCache(mySim, userTags = c("spades","rnorm")) # shows nothing because object
#'                                                   #  has both spades and rnorm
#'  showCache(mySim, userTags = "spades|rnorm") # "or" search
#'  keepCache(mySim, userTags = "spades|rnorm") # keep all with spades or rnorm
#'  showCache(mySim) # shows spades, runif and rnorm objects
#'
#' }
setGeneric("clearCache", function(x, userTags = character(), after, before, ...) {
  standardGeneric("clearCache")
})

#' @export
#' @rdname viewCache
setMethod(
  "clearCache",
  definition = function(x, userTags, after, before, ...) {
    if (missing(x)) {
      message("x not specified; using ", getOption("spades.cachePath") )
      x <- getOption("spades.cachePath")
    }
    if (missing(after)) after <- "1970-01-01"
    if (missing(before)) before <- Sys.Date() + 1
    if (is(x, "simList")) x <- x@paths$cachePath

    args <- append(list(x = x, after = after, before = before, userTags = userTags),
                   list(...))

    objsDT <- do.call(showCache, args = args)

    if (NROW(objsDT)) {
      rastersInRepo <- objsDT[grep(tagValue, pattern = "Raster")]
      if (all(!is.na(rastersInRepo$artifact))) {
        rasters <- lapply(rastersInRepo$artifact, function(ras) {
          loadFromLocalRepo(ras, repoDir = x, value = TRUE)
        })
        filesToRemove <- unlist(lapply(rasters, function(x) filename(x)))
        filesToRemove <- gsub(filesToRemove, pattern = ".{1}$", replacement = "*")
        unlink(filesToRemove)
      }

      rmFromLocalRepo(unique(objsDT$artifact), x, many = TRUE)
    }
    return(invisible(objsDT))
})

#' \code{showCache}, \code{clearCache} and \code{keepCache}
#'
#' These are wrappers around \code{archivist} package
#' functions, specific to simList objects.
#' They allow the user a bit of control over what is being cached.
#'
#' \code{keepCache} can be used to pare down the cache to only keep
#' a set of archives based on userTags or times.
#'
#' \code{clearCache} is the opposite, where items can be removed by
#' userTag or times
#'
#' @inheritParams clearCache
#'
#' @seealso \code{\link[archivist]{splitTagsLocal}}.
#' @export
#' @importFrom archivist splitTagsLocal
#' @importFrom data.table data.table set setkeyv
#' @docType methods
#' @rdname viewCache
#' @examples
#' \dontrun{
#' showCache(mySim)
#' }
setGeneric("showCache", function(x, userTags = character(), after, before, ...) {
  standardGeneric("showCache")
})

#' @export
#' @rdname viewCache
setMethod(
  "showCache",
  definition = function(x, userTags, after, before, ...) {
    if (missing(x)) {
      message("x not specified; using ", getOption("spades.cachePath") )
      x <- getOption("spades.cachePath")
    }
    if (missing(after)) after <- "1970-01-01"
    if (missing(before)) before <- Sys.Date() + 1
    if (is(x, "simList")) x <- x@paths$cachePath

    objsDT <- showLocalRepo(x) %>% data.table()
    #objsDT <- objsDT[createdDate <= before & createdDate >= after]
    setkeyv(objsDT, "md5hash")
    if (NROW(objsDT) > 0) {
      objsDT <- data.table(splitTagsLocal(x), key = "artifact")
      objsDT3 <- objsDT[tagKey == "accessed"][(tagValue <= before) & (tagValue >= after)][!duplicated(artifact)]
      objsDT <- objsDT[artifact %in% objsDT3$artifact]
      #objsDT3 <- objsDT3[(createdDate <= before & createdDate >= after) ]
      if (length(userTags) > 0) {
        for (ut in userTags) {
          objsDT2 <- objsDT[
            grepl(tagValue, pattern = ut)   |
              grepl(tagKey, pattern = ut) |
              grepl(artifact, pattern = ut)]
          setkeyv(objsDT2, "artifact")
          objsDT <- objsDT[unique(objsDT2, by = "artifact")[, artifact]] # merge each userTags
        }

        # grepPattern <- paste(userTags,collapse="|")
        # # https://stackoverflow.com/questions/7597559/grep-using-a-character-vector-with-multiple-patterns
        # objsDT3 <- unique(objsDT[grepl(artifact, pattern=grepPattern)], by="artifact")
        # objsDT4 <- unique(objsDT[grepl(tagKey, pattern=grepPattern)], by="tagKey")
        # objsDT5 <- objsDT[grepl(tagValue, pattern=grepPattern)]
        #                   # grepl(tagKey, pattern=grepPattern) |
        #                   #   grepl(tagValue, pattern=grepPattern) ]
        #
        # objsDT3 <- objsDT3[unique(artifact),list(allMatched=.N==length(userTags)),by="artifact"][allMatched==TRUE]
        # if(NROW(objsDT3)) {
        #   setkeyv(objsDT, "artifact")
        #   setkeyv(objsDT3, "artifact")
        #   objsDT <- objsDT[objsDT3[!duplicated(objsDT3$artifact)]]
        #   #set(objsDT, , "i.createdDate", NULL)
        #   #set(objsDT, , "i.tagKey", NULL)
        #   #set(objsDT, , "i.tagValue", NULL)
        # } else {
        #   objsDT <- objsDT3
        # }
      }
      # objsDT <- objsDT2[objsDT]
      # if (length(userTags) > 0) {
      #   for (ut in userTags) {
      #     objsDT2 <- objsDT[
      #       grepl(tagValue, pattern = ut)   |
      #         grepl(tagKey, pattern = ut) |
      #         grepl(artifact, pattern = ut)]
      #     setkeyv(objsDT2, "artifact")
      #     objsDT <- objsDT[unique(objsDT2, by = "artifact")[, artifact]] # merge each userTags
      #   }
      # }
    }
    objsDT
})

#' @docType methods
#' @rdname viewCache
setGeneric("keepCache", function(x, userTags = character(), after, before, ...) {
  standardGeneric("keepCache")
})

#' @export
#' @rdname viewCache
setMethod(
  "keepCache",
  definition = function(x, userTags, after, before, ...) {
    if (missing(x)) {
      message("x not specified; using ", getOption("spades.cachePath") )
      x <- getOption("spades.cachePath")
    }
    if (missing(after)) after <- "1970-01-01"
    if (missing(before)) before <- Sys.Date() + 1
    if (is(x, "simList")) x <- x@paths$cachePath

    args <- append(list(x = x, after = after, before = before, userTags = userTags),
                   list(...))

    objsDTAll <- showCache(x)
    objsDT <- do.call(showCache, args = args)
    keep <- unique(objsDT$artifact)
    eliminate <- unique(objsDTAll$artifact[!(objsDTAll$artifact %in% keep)])

    if (length(eliminate)) {
      eliminate <- paste(eliminate, collapse = "|")
      clearCache(x, eliminate)
    }

    return(objsDT)
})
