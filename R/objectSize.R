#' Wrapper around `lobstr::obj_size`
#'
#' This function attempts to estimate the real object size of an object. If the object
#' has pass-by-reference semantics, it may not estimate the object size well without
#' a specific method developed. For the case of `terra` class objects, this will
#' be accurate (both RAM and file size), but only if it is not passed inside
#' a list or environment. To get an accurate size of these, they should be passed
#' individually.
#'
#' @return
#' This will return the result from `lobstr::obj_size`, i.e., a `lobstr_bytes`
#' which is a `numeric`. If `quick = FALSE`, it will also have an attribute,
#' "objSize", which will
#' be a list with each element being the `objSize` of the individual elements of `x`.
#' This is particularly useful if `x` is a `list` or `environment`.
#' However, because of the potential for shared memory, the sum of the individual
#' elements will generally not equal the value returned from this function.
#'
#' @param x An object
#' @param quick Logical. If `FALSE`, then an attribute, "objSize" will be added to
#'   the returned value, with each of the elements' object size returned also.
#' @param recursive Logical. If `TRUE`, then, in addition to evaluating the whole object,
#'   it will also return the recursive sizes of the elements of a list or environment.
#' @param ...  Additional arguments (currently unused), enables backwards compatible use.
#'
#' @export
#' @rdname objSize
#'
#' @examples
#' library(utils)
#'
#' foo <- new.env()
#' foo$b <- 1:10
#' foo$d <- 1:10
#'
#' objSize(foo) # all the elements in the environment
#' utils::object.size(foo) # different - only measuring the environment as an object
#'
#' utils::object.size(prepInputs) # only the function, without its enclosing environment
#' objSize(prepInputs) # the function, plus its enclosing environment
#'
#' os1 <- utils::object.size(as.environment("package:reproducible"))
#' (os1) # very small -- just the environment container
#'
#' @details
#' For functions, a user can include the enclosing environment as described
#' <https://www.r-bloggers.com/2015/03/using-closures-as-objects-in-r/> and
#' <http://adv-r.had.co.nz/memory.html>.
#' It is not entirely clear which estimate is better.
#' However, if the enclosing environment is the `.GlobalEnv`, it will
#' not be included even though `enclosingEnvs = TRUE`.
#'
#' @export
objSize <- function(x, quick = FALSE, recursive = FALSE, ...) {
  UseMethod("objSize", x)
}

#' @export
#' @importFrom lobstr obj_size
objSize.default <- function(x, quick = FALSE, ...) {
  FNs <- Filenames(x)
  if (!is.null(FNs) && length(FNs) && !isTRUE(quick)) {
    if (any(nzchar(FNs))) {
      FNs <- asPath(FNs)
      out2 <- objSize(FNs, quick = FALSE)
    }
  }
  if (.isSpat(x)) {
    if (.requireNamespace("terra")) {
      if (inherits(x, "SpatVector")) { # too slow for large SpatVectors
        x <- list(terra::geom(x), terra::values(x))
      } # approximate
      else {
        if (!isTRUE(quick))
          x <- list(terra::wrap(x))
      }
    }
  }
  out <- object.size(x)
  # out <- .objSizeWithTry(x)  # out <- try(obj_size(x), silent = TRUE) # Error in obj_size_(dots, env, size_node(), size_vector()) :
  #                                        # bad binding access
  if (exists("out2", inherits = FALSE)) {
    out <- sum(out, out2)
  }
  class(out) <- "lobstr_bytes"

  if (!quick) {
    attr(out, "objSize") <- list(obj = out)
  }
  out
}

#' @export
#' @importFrom lobstr obj_size
objSize.list <- function(x, quick = FALSE, recursive = FALSE, ...) {
  os <- .objSizeWithTry(x)
  # os <- try(obj_size(x), silent = TRUE) # need to get overall object size; not just elements;
  # but this doesn't work for e.g., terra

  if (!quick && (isTRUE(recursive) || (is.numeric(recursive) && recursive > 0))) {
    out <- Map(x = x, function(x) objSize(x, quick = quick, recursive = recursive - 1))
    os2 <- sum(unlist(out))
    os <- max(os2, os)
    class(os) <- "lobstr_bytes"
    attr(os, "objSize") <- out
  }

  os
}

#' @export
#' @importFrom lobstr obj_size
objSize.Path <- function(x, quick = FALSE, ...) {
  if (quick) {
    os <- .objSizeWithTry(x, FALSE)
  } else {
    os <- file.size(x)
  }

  if (!quick) {
    attr(os, "objSize") <- os
  }
  class(os) <- "lobstr_bytes"

  os
}

#' @export
#' @importFrom lobstr obj_size
objSize.environment <- function(x, quick = FALSE, recursive = FALSE, ...) {
  os <- .objSizeWithTry(x)
  if (!quick) {
    out <- lapply(as.list(x, all.names = TRUE), objSize, quick = quick)
    attr(os, "objSize") <- out
  }
  os
}

#' @export
#' @importFrom lobstr obj_size
objSize.function <- function(x, quick = FALSE, ...) {
  os <- .objSizeWithTry(x)
  if (!quick)
    attr(os, "objSize") <- os
  os
}


#' @param sumLevel Numeric, indicating at which depth in the list of objects should the
#'   object sizes be summed (summarized). Default is `Inf`, meaning no sums. Currently,
#'   the only option other than Inf is 1: `objSizeSession(1)`,
#'   which gives the size of each package.
#' @param enclosingEnvs Logical indicating whether to include enclosing environments.
#'                      Default `TRUE`.
#' @param .prevEnvirs For internal account keeping to identify and prevent duplicate counting
#'
#'
#' @details `objSizeSession` will give the size of the whole session, including loaded packages.
#' Because of the difficulties in calculating the object size of `base`
#' and `methods` packages and `Autoloads`, these are omitted.
#'
#' @export
#' @rdname objSize
objSizeSession <- function(sumLevel = Inf, enclosingEnvs = TRUE, .prevEnvirs = list()) {
  .Defunct("Please use lobstr::obj_size instead")
}


#' `lobstr::obj_size` with a `try` to address issue #72
#'
#' It is not clear why, but it appears that running `lobstr::obj_size` again, after
#' a bad binding error, it will work.
#' @export
#' @importFrom lobstr obj_size
#' @inheritParams objSize
#' @param useTry Logical. If `TRUE`, the default, then it will use `try`. Can optionally
#'   avoid this if set to `FALSE`. The `try` takes sufficient extra compute time
#'   that it is worth avoiding it if possible.
#' @return The size of an object, using `lobstr::obj_size` or `object.size` if the
#'   first fails
.objSizeWithTry <- function(x, useTry = TRUE) {
  for (i in 1:2) {
    if (isTRUE(useTry)) {
      out <- try(obj_size(x), silent = TRUE)
    } else {
      out <- obj_size(x)
    }

    if (inherits(out, "try-error")) {
      if (identical(i, 1L)) next
    } else {
      break
    }
    out <- try(length(serialize(x, NULL)), silent = TRUE)
    if (inherits(out, "try-error")) {
      out <- object.size(x)
    }
    class(out) <- "lobstr_bytes"
  }
  out
}
