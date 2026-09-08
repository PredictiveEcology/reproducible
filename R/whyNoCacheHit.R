## Compare one call's element digests against each candidate entry, one frame
## per candidate. Shared by the single-entry and whole-run modes.
.compareAgainst <- function(thisDigest, all, candidates) {
  out <- lapply(candidates, function(cand) {
    candRows <- all[cacheId %in% cand]
    otherDigest <- .preDigestOfRows(candRows[tagKey %in% "preDigest"])
    if (!length(otherDigest)) return(NULL)
    shared <- intersect(names(thisDigest), names(otherDigest))
    differs <- shared[thisDigest[shared] != otherDigest[shared]]
    onlyThis <- setdiff(names(thisDigest), names(otherDigest))
    onlyOther <- setdiff(names(otherDigest), names(thisDigest))
    created <- candRows[tagKey %in% "createdDate" | tagKey %in% "accessed"][["tagValue"]]
    element <- c(differs, onlyThis, onlyOther)
    ## A candidate that matches on every element is the interesting answer "this
    ## should have been reused"; it contributes no rows, so build the frame at
    ## the right length rather than recycling the scalars against nothing.
    data.frame(
      candidate = rep(cand, length(element)),
      created = rep(if (length(created)) created[[1L]] else NA_character_, length(element)),
      element = element,
      status = c(rep("differs", length(differs)),
                 rep("only in this call", length(onlyThis)),
                 rep("only in the other call", length(onlyOther))),
      thisCall = c(unname(thisDigest[differs]), unname(thisDigest[onlyThis]),
                   rep(NA_character_, length(onlyOther))),
      otherCall = c(unname(otherDigest[differs]), rep(NA_character_, length(onlyThis)),
                    unname(otherDigest[onlyOther])),
      nDiff = rep(length(element), length(element)),
      nShared = rep(length(shared), length(element)),
      stringsAsFactors = FALSE
    )
  })
  out[lengths(out) > 0]
}

## Every entry written since `since`, with the function each belongs to.
.entriesSince <- function(cachePath, since, before = NULL) {
  recent <- data.table::as.data.table(
    showCache(cachePath, after = since, before = before, verbose = -2))
  if (!NROW(recent)) return(recent)
  recent[!startsWith(cacheId, "preDigest_")]
}

utils::globalVariables(c("tagKey", "tagValue", "cacheId", "createdDate"))

## Cache stores, for every call, one "preDigest" tag per digested element:
## "<path>:<hash>", where the path walks into nested structures with dots
## (e.g. "sim.params.canClimateData..studyAreaName"). Two calls of the same
## function can therefore be compared element by element, after the fact, from
## the repository alone. Split on the LAST colon: paths contain colons.
.preDigestOfRows <- function(rows) {
  v <- rows[["tagValue"]]
  if (!length(v)) return(character(0))
  stats::setNames(sub("^.*:", "", v), sub(":[^:]*$", "", v))
}

#' Why did this call not reuse the cache, and what changed?
#'
#' @description
#' `Cache()` reuses a previous result only when every digested input matches. When
#' it does not, the question is always which input changed -- and the answer is
#' recorded: each cache entry carries the hash of every element it digested,
#' addressed by its path into the call's arguments. This compares one entry
#' against the other entries for the same function and reports exactly the
#' elements that differ.
#'
#' Use it after the fact, on the entry the surprising run wrote. It reads the
#' repository only, so it needs neither the objects nor a re-run, and it works on
#' entries written before you suspected anything was wrong.
#'
#' It is the post-hoc counterpart of `options(reproducible.showSimilar = TRUE)`,
#' which reports at call time and stops descending at
#' `reproducible.showSimilarDepth`. This descends as far as the recorded paths
#' go, so a difference nested inside a parameter list is named in full rather
#' than reported as "possible, unknown, differences in a nested list".
#'
#' @param cacheId The `cacheId` of the run that missed, or the object returned by
#'   `Cache(..., dryRun = TRUE)` -- which answers the question *before* the
#'   expensive call runs, since a dry run digests the arguments without
#'   evaluating or saving anything. Defaults to the most recently created entry
#'   in `cachePath`, i.e. "whatever my latest run wrote".
#' @param other Optional `cacheId`(s) to compare against. By default, every other
#'   entry for the same function, ranked so the closest comes first.
#' @param cachePath The cache repository. Defaults to
#'   `getOption("reproducible.cachePath")`.
#' @param since A time (or a `difftime`, meaning "ago"). Instead of explaining one
#'   entry, explain a whole run: every entry written after `since` is compared
#'   against the closest earlier call of the same function. This is the form to
#'   use when the `Cache()` calls belong to modules rather than to you, as in a
#'   `SpaDES` pipeline -- ask why the run recomputed, not why one call did.
#' @param before With `since`, the far end of the window, so a run can be
#'   isolated from whatever else was writing to a shared cache at the time.
#' @param n Report at most this many candidate entries (single-entry mode).
#' @param verbose Numeric or logical; controls messaging.
#'
#' @return A `data.frame` of class `whyNoCacheHit`, one row per differing
#'   element, with columns `candidate` (the entry compared against), `created`,
#'   `element` (the path), `status` (`"differs"`, `"only in this call"`,
#'   `"only in the other call"`), `thisCall` and `otherCall` (the hashes).
#'   Candidates that match on every element yield zero rows for that candidate,
#'   which means the two calls digested identically. Printing it gives the
#'   one-line answer.
#'
#' @section Cost on a real repository:
#' Explaining a whole run reads the repository once (about a minute for ~37,000
#' entries). `showCache()` memoises that read per `cachePath` for the session, so
#' later calls are quick; [prepopulateCacheAsync()] warms it in a background fork
#' if you would rather not wait for the first one.
#'
#' @section How fine the answer is:
#' Each entry records its elements to the depth given by
#' `reproducible.showSimilarDepth` (default 3) at the time it was written. A
#' difference deeper than that is reported at the deepest recorded path, so raise
#' the option if an answer stops short of the element you need.
#'
#' @seealso [showCache()], [clearCache()]. `Cache(dryRun = TRUE)` asks the same
#'   question before running anything; `options(reproducible.showSimilar = TRUE)`
#'   reports at call time.
#' @export
#' @examples
#' cachePath <- file.path(tempdir(), "whyNoCacheHitExample")
#' f <- function(x, settings) paste(x, length(settings))
#' Cache(f, x = 1, settings = list(a = 1, b = list(name = "12.4")), cachePath = cachePath)
#' Cache(f, x = 1, settings = list(a = 1, b = list(name = 12.4)), cachePath = cachePath)
#' ## Names the element that changed: settings.b.name
#' whyNoCacheHit(cachePath = cachePath)
whyNoCacheHit <- function(cacheId = NULL, other = NULL,
                          cachePath = getOption("reproducible.cachePath"),
                          since = NULL, before = NULL, n = 3,
                          verbose = getOption("reproducible.verbose")) {
  ## Cache(dryRun = TRUE) hands back the prospective digest; use it as "this
  ## call", so nothing has to be run or written to get an answer.
  dryRun <- NULL
  if (inherits(cacheId, "cacheDryRun")) {
    dryRun <- cacheId
    if (missing(cachePath) || is.null(cachePath)) cachePath <- dryRun$cachePath
    cacheId <- dryRun$cacheId
  }
  if (is.null(cachePath) || !CacheIsACache(cachePath))
    stop("`cachePath` is not a cache repository: ", paste(cachePath, collapse = ", "), call. = FALSE)

  ## Whole-run mode. In a pipeline the Cache() calls belong to the modules, not
  ## to the user, so the useful question is not about one entry but about the
  ## run: which of the entries it wrote had a close previous call, and what
  ## differed. Candidates exclude everything this run wrote, or two misses from
  ## the same run would explain each other.
  if (!is.null(since)) {
    if (!is.null(cacheId))
      stop("Give either `cacheId` (one entry) or `since` (a whole run), not both.", call. = FALSE)
    if (inherits(since, "difftime")) since <- Sys.time() - since
    recent <- .entriesSince(cachePath, since, before)
    newIds <- unique(recent[["cacheId"]])
    if (!length(newIds)) {
      messageCache("Nothing was written to the cache after ", format(since),
                   ": that run reused everything.", verbose = verbose)
      return(.emptyWhyNoCacheHit(NA_character_, NULL, 0L))
    }
    messageCache(length(newIds), " entries were written after ", format(since),
                 "; looking for the closest previous call to each.", verbose = verbose)
    fnOf <- recent[tagKey %in% "function"]
    perFn <- split(fnOf[["cacheId"]], fnOf[["tagValue"]])
    ## One read of the whole repository, then split in R. A run touches many
    ## functions (140 in a real SpaDES pipeline), and a per-function query costs
    ## the same full scan each time: 59 s once beats an hour.
    allRows <- data.table::as.data.table(showCache(cachePath, verbose = -2))
    allRows <- allRows[!startsWith(cacheId, "preDigest_")]
    fnRows <- allRows[tagKey %in% "function"]
    idsByFn <- split(fnRows[["cacheId"]], fnRows[["tagValue"]])
    out <- lapply(names(perFn), function(fnName) {
      allFn <- allRows[cacheId %in% idsByFn[[fnName]]]
      candidatesFn <- setdiff(unique(allFn[["cacheId"]]), newIds)  # only pre-run entries
      if (!length(candidatesFn)) return(NULL)
      do.call(rbind, lapply(unique(perFn[[fnName]]), function(id) {
        thisDigest <- .preDigestOfRows(allFn[cacheId %in% id & tagKey %in% "preDigest"])
        if (!length(thisDigest)) return(NULL)
        cmp <- .compareAgainst(thisDigest, allFn, candidatesFn)
        if (!length(cmp)) return(NULL)
        best <- cmp[[which.min(vapply(cmp, NROW, numeric(1)))]]
        if (!NROW(best)) return(NULL)
        cbind(entry = id, fn = fnName, best, stringsAsFactors = FALSE)
      }))
    })
    out <- do.call(rbind, out[lengths(out) > 0])
    if (is.null(out)) {
      messageCache("None of those entries had an earlier call of the same function to compare ",
                   "against: that run was doing genuinely new work.", verbose = verbose)
      return(.emptyWhyNoCacheHit(NA_character_, NULL, length(newIds)))
    }
    attr(out, "cacheId") <- NA_character_
    attr(out, "fn") <- unique(out$fn)
    attr(out, "nCandidates") <- length(newIds)
    attr(out, "since") <- since
    attr(out, "nEntries") <- length(unique(out$entry))
    attr(out, "nWritten") <- length(newIds)
    class(out) <- c("whyNoCacheHit", "data.frame")
    return(out)
  }
  ## A whole-repository read is heavy on a real cache (tens of thousands of
  ## entries), so only take one when the caller has not said which entry to
  ## explain -- and then only to find the newest one.
  if (is.null(cacheId)) {
    all <- data.table::as.data.table(showCache(cachePath, verbose = -2))
    if (!NROW(all)) stop("There is nothing in the cache at ", cachePath, call. = FALSE)
    ## preDigest artifacts (written under reproducible.savePreDigest) are a
    ## parallel copy of the digested objects, not calls in their own right.
    all <- all[!startsWith(cacheId, "preDigest_")]
    created <- all[tagKey %in% "createdDate" | tagKey %in% "accessed"]
    cacheId <- if (NROW(created)) created[order(-tagValue)][1L][["cacheId"]] else all[["cacheId"]][1L]
    messageCache("Using the most recent entry: ", cacheId, verbose = verbose)
  }
  ## `cacheId` is also a column of these tables, and in `i` the column wins, so
  ## hold the argument in a name that cannot be shadowed.
  wantedId <- cacheId
  thisRows <- if (is.null(dryRun))
    data.table::as.data.table(showCache(cachePath, cacheId = wantedId, verbose = -2))
  else data.table::data.table(cacheId = wantedId, tagKey = "function", tagValue = dryRun$functionName)
  if (!NROW(thisRows)) stop("No cache entry with cacheId '", cacheId, "' in ", cachePath, call. = FALSE)
  thisDigest <- if (is.null(dryRun)) .preDigestOfRows(thisRows[tagKey %in% "preDigest"]) else dryRun$preDigest
  if (!length(thisDigest))
    stop("Entry '", cacheId, "' recorded no per-element digests, so there is nothing to compare. ",
         "This happens for entries written by versions that did not record them.", call. = FALSE)

  fn <- unique(thisRows[tagKey %in% "function"][["tagValue"]])
  ## Only entries for the same function can explain this one.
  all <- if (!is.null(other)) {
    data.table::rbindlist(lapply(other, function(o)
      data.table::as.data.table(showCache(cachePath, cacheId = o, verbose = -2))), fill = TRUE)
  } else if (length(fn)) {
    data.table::as.data.table(showCache(cachePath, Function = fn, verbose = -2))
  } else if (exists("all", inherits = FALSE)) all else thisRows
  all <- all[!startsWith(cacheId, "preDigest_")]
  candidates <- if (!is.null(other)) other else setdiff(unique(all[["cacheId"]]), wantedId)
  if (!length(candidates)) {
    messageCache("No other entry for ", if (length(fn)) fn else "this function",
                 " to compare against: this call had nothing to reuse.", verbose = verbose)
    return(.emptyWhyNoCacheHit(cacheId, fn, 0L))
  }

  comparisons <- .compareAgainst(thisDigest, all, candidates)
  comparisons <- comparisons[lengths(comparisons) > 0]
  if (!length(comparisons)) {
    messageCache("None of the ", length(candidates), " other entries for this function recorded ",
                 "per-element digests, so there is nothing to compare against.", verbose = verbose)
    return(.emptyWhyNoCacheHit(cacheId, fn, length(candidates)))
  }
  ## Closest first: the entry that differs in the fewest elements is the one the
  ## call "should" have reused.
  comparisons <- comparisons[order(vapply(comparisons, NROW, numeric(1)))]
  out <- do.call(rbind, comparisons[seq_len(min(n, length(comparisons)))])
  attr(out, "cacheId") <- cacheId
  attr(out, "fn") <- fn
  attr(out, "nCandidates") <- length(candidates)
  class(out) <- c("whyNoCacheHit", class(out))
  out
}

.emptyWhyNoCacheHit <- function(cacheId, fn, nCandidates) {
  out <- data.frame(candidate = character(0), created = character(0), element = character(0),
                    status = character(0), thisCall = character(0), otherCall = character(0),
                    nDiff = numeric(0), nShared = numeric(0), stringsAsFactors = FALSE)
  attr(out, "cacheId") <- cacheId
  attr(out, "fn") <- fn
  attr(out, "nCandidates") <- nCandidates
  class(out) <- c("whyNoCacheHit", class(out))
  out
}

#' @export
#' @param x A `whyNoCacheHit` object.
#' @param ... Passed to `print.data.frame`.
#' @rdname whyNoCacheHit
print.whyNoCacheHit <- function(x, ...) {
  ## Whole-run mode: the useful summary is which element explains the most
  ## recomputation, not a walk through every entry.
  if (!is.null(attr(x, "since"))) {
    cat("Entries written after ", format(attr(x, "since")), ": ", attr(x, "nWritten"),
        "\n", sep = "")
    if (!NROW(x)) {
      cat("None had an earlier call of the same function to compare against.\n")
      return(invisible(x))
    }
    cat(attr(x, "nEntries"), " of them had a close earlier call. What differed:\n", sep = "")
    ## One changed value usually reaches many entries under different paths
    ## (a global parameter appears once per module), so group on the leaf name:
    ## "13 entries differ on .studyAreaName" is the finding, not thirteen lines.
    differs <- x[x$status %in% "differs", , drop = FALSE]
    if (NROW(differs)) {
      leaf <- sub("^.*\\.", "", differs$element)
      byLeaf <- sort(table(leaf), decreasing = TRUE)
      for (i in seq_along(byLeaf)) {
        nm <- names(byLeaf)[i]
        paths <- unique(differs$element[leaf %in% nm])
        cat("  ", nm, "  (", byLeaf[[i]], " entr", if (byLeaf[[i]] == 1L) "y" else "ies", ")\n", sep = "")
        for (pth in utils::head(paths, 3)) cat("      ", pth, "\n", sep = "")
        if (length(paths) > 3) cat("      ... and ", length(paths) - 3, " more path(s)\n", sep = "")
      }
    }
    other <- x[!x$status %in% "differs", , drop = FALSE]
    if (NROW(other))
      cat("  plus ", NROW(other), " element(s) present in only one of the two calls\n", sep = "")
    fns <- unique(x$fn)
    fns <- substr(gsub("[[:space:]]+", " ", fns), 1, 48)
    cat("Functions affected (", length(unique(x$fn)), "): ",
        paste(utils::head(fns, 5), collapse = ", "),
        if (length(fns) > 5) ", ..." , "\n", sep = "")
    return(invisible(x))
  }
  fn <- attr(x, "fn")
  cat("Cache entry ", attr(x, "cacheId"),
      if (length(fn)) paste0(" (", paste(fn, collapse = ", "), ")"), "\n", sep = "")
  if (!NROW(x)) {
    cat("Nothing to compare: ", attr(x, "nCandidates"),
        " other entr", if (identical(attr(x, "nCandidates"), 1L)) "y" else "ies",
        " for this function.\n", sep = "")
    return(invisible(x))
  }
  best <- x[x$candidate %in% x$candidate[[1L]], , drop = FALSE]
  cat("Closest previous call: ", best$candidate[[1L]],
      if (!is.na(best$created[[1L]])) paste0(" (", substr(best$created[[1L]], 1, 19), ")"), "\n", sep = "")
  cat(NROW(best), " of ", best$nShared[[1L]] + NROW(best),
      " digested elements differ:\n", sep = "")
  for (i in seq_len(NROW(best)))
    cat("  ", best$element[[i]], "  [", best$status[[i]], "]\n", sep = "")
  if (length(unique(x$candidate)) > 1L)
    cat("(", length(unique(x$candidate)) - 1L, " further candidate(s) in the returned data.frame)\n", sep = "")
  invisible(x)
}

## What Cache(dryRun = TRUE) hands back: the prospective call's identity and its
## element-by-element digest, so it can be compared against the repository
## without running or saving anything.
.cacheDryRunResult <- function(keyFull, functionName, metadata, cachePath) {
  pre <- metadata[metadata[["tagKey"]] %in% "preDigest", ]
  structure(list(cacheId = keyFull$key,
                 functionName = functionName,
                 preDigest = .preDigestOfRows(pre),
                 cachePath = cachePath),
            class = "cacheDryRun")
}

#' @export
#' @rdname whyNoCacheHit
print.cacheDryRun <- function(x, ...) {
  cat("A dry run of ", if (length(x$functionName)) x$functionName else "a Cache() call",
      "\n  cacheId it would use: ", x$cacheId,
      "\n  digested elements:    ", length(x$preDigest),
      "\n  Pass this to whyNoCacheHit() to see which element differs from the closest previous call.\n", sep = "")
  invisible(x)
}


## Stop the run at the first miss that looks accidental, and say why. The option
## exists because in a pipeline the Cache() calls belong to the modules: the user
## has no call to put `dryRun` on and no cacheId to pass afterwards, but can set
## an option before the run and be told at the moment it matters.
##
## A miss with no earlier call of the same function is not accidental -- it is
## new work -- so it never fires. `reproducible.stopOnCacheMiss` may also be a
## number k: fire only when the closest earlier call differs in at most k
## elements, i.e. when the miss looks like a slip rather than a different job.
.maybeStopOnCacheMiss <- function(setting, keyFull, functionName, metadata, cachePath, verbose) {
  if (isFALSE(setting) || is.null(setting)) return(invisible(NULL))
  dr <- .cacheDryRunResult(keyFull, functionName, metadata, cachePath)
  if (!length(dr$preDigest)) return(invisible(NULL))
  report <- try(whyNoCacheHit(dr, cachePath = cachePath, n = 1, verbose = -2), silent = TRUE)
  if (inherits(report, "try-error") || !NROW(report)) return(invisible(NULL))
  if (is.numeric(setting) && report$nDiff[[1L]] > setting) return(invisible(NULL))
  messageCache("reproducible.stopOnCacheMiss is set, and this call did not reuse the cache:",
               verbose = verbose)
  print(report)
  stop("Cache miss in ", if (length(functionName)) functionName else "a Cache() call",
       ": ", paste(unique(report$element), collapse = ", "),
       " differ(s) from the closest previous call. ",
       "Set options(reproducible.stopOnCacheMiss = FALSE) to continue past misses.",
       call. = FALSE)
}
