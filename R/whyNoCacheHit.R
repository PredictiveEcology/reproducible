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
#' @param cacheId The `cacheId` of the run that missed. Defaults to the most
#'   recently created entry in `cachePath`, i.e. "whatever my latest run wrote".
#' @param other Optional `cacheId`(s) to compare against. By default, every other
#'   entry for the same function, ranked so the closest comes first.
#' @param cachePath The cache repository. Defaults to
#'   `getOption("reproducible.cachePath")`.
#' @param n Report at most this many candidate entries.
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
#' @seealso [showCache()], [clearCache()]
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
                          n = 3, verbose = getOption("reproducible.verbose")) {
  if (is.null(cachePath) || !CacheIsACache(cachePath))
    stop("`cachePath` is not a cache repository: ", paste(cachePath, collapse = ", "), call. = FALSE)
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
  thisRows <- data.table::as.data.table(showCache(cachePath, cacheId = wantedId, verbose = -2))
  if (!NROW(thisRows)) stop("No cache entry with cacheId '", cacheId, "' in ", cachePath, call. = FALSE)
  thisDigest <- .preDigestOfRows(thisRows[tagKey %in% "preDigest"])
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

  comparisons <- lapply(candidates, function(cand) {
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
