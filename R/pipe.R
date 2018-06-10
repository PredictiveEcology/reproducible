#' Pipe that is Cache-aware, being deprecated
#'
#' This pipe will likely be deprecated, as it masks
#' other pipes in the R ecosystem. This is fine, except to work,
#' the reproducible package must be guaranteed to be first
#' on the search path, which is almost impossible in any realistic project. Please
#' see the \code{\%C\%} function ?pipe
#'
#' A pipe that works with Cache. The code for this is based on a verbatim copy from
#' \url{https://github.com/tidyverse/magrittr/blob/master/R/pipe.R} on Sep 8, 2017,
#' based on commit 81c2e2410ebb7c560a2b4a8384ef5c20946373c3, with enhancements
#' to make it Cache-aware.
#' This version is a drop-in replacement for \code{\link[magrittr]{\%>\%}} and will
#' work identically when there is no Cache. To use this, simply add \code{\%>\% Cache()}
#' to a pipe sequence. This can be in the middle or at the end. See examples. It has
#' been tested with multiple Cache calls within the same (long) pipe.
#'
#' If there is a Cache in the pipe,
#' the behaviour of the pipe is altered. In the magrittr pipe, each step of the
#' pipe chain is evaluated one at a time, in sequence. This will not allow any useful
#' type of caching. Here, if there is a call to \code{Cache} in the pipe sequence,
#' the entire pipe chain before the call to \code{Cache} will have its arguments
#' examined and digested. This digest is compared to the cache repository database.
#' If there is an identical pipe sequence in the Cache respository, then it will return
#' the final result of the entire pipe up to the Cache call. If there is no
#' identical copy in the cache repository, then it will evaluate the pipe sequence as per
#' normal, caching the return value at the point of the \code{Cache} call
#' into the cache repository for later use.
#'
#' @name pipe2
#' @aliases %>%
#' @importFrom utils getFromNamespace
#' @inheritParams magrittr::`%>%`
#' @importFrom magrittr freduce
#' @export
#' @seealso pipe
#' @rdname pipe2
#' @examples
#' \dontrun{
#' tmpdir <- file.path(tempdir(), "testCache")
#' checkPath(tmpdir, create = TRUE)
#' try(detach("package:magrittr", unload = TRUE)) # magrittr, if loaded, gives an error below
#' a <- rnorm(10, 16) %>% mean() %>% prod(., 6)
#' b <- rnorm(10, 16) %>% mean() %>% prod(., 6) %>% Cache(cacheRepo = tmpdir)
#' d <- rnorm(10, 16) %>% mean() %>% prod(., 6) %>% Cache(cacheRepo = tmpdir)
#' all.equal(b,d) # TRUE
#' all.equal(a,d) # different because 'a' uses a unique rnorm, 'd' uses the Cached rnorm
#'
#' # Can put Cache in the middle of a pipe -- f2 and f4 use "cached result" until Cache
#' f1 <- rnorm(10, 16) %>% mean() %>% prod(., runif(1)) %>% Cache(cacheRepo = tmpdir)
#' f2 <- rnorm(10, 16) %>% mean() %>% prod(., runif(1)) %>% Cache(cacheRepo = tmpdir)
#' f3 <- rnorm(10, 16) %>% mean() %>% Cache(cacheRepo = tmpdir) %>% prod(., runif(1))
#' f4 <- rnorm(10, 16) %>% mean() %>% Cache(cacheRepo = tmpdir) %>% prod(., runif(1))
#' all.equal(f1, f2) # TRUE because the runif is before the Cache
#' all.equal(f3, f4) # different because the runif is after the Cache
#'
#' unlink(tmpdir, recursive = TRUE)
#' }
`%>%` <- function(lhs, rhs) {
  # magrittr code below
  parent <- parent.frame()
  env <- new.env(parent = parent)
  mc <- match.call()
  chain_parts <- getFromNamespace("split_chain", ns = "magrittr")(mc, env = env) # nolint
  pipes <- chain_parts[["pipes"]]
  rhss <- chain_parts[["rhss"]]
  lhs <- chain_parts[["lhs"]]
  env[["_function_list"]] <- lapply(1:length(rhss), function(i) {
    getFromNamespace("wrap_function", ns = "magrittr")(rhss[[i]], pipes[[i]], parent)
  })
  env[["_fseq"]] <- `class<-`(eval(quote(function(value) {
    freduce(value, `_function_list`)
  }), env, env), c("fseq", "function"))
  env[["freduce"]] <- freduce

  if (getFromNamespace("is_placeholder", ns = "magrittr")(lhs)) {
    env[["_fseq"]]
  } else {

    # reproducible package code here until end of if statement
    whCache <- startsWith(as.character(rhss), "Cache")

    if (any(whCache)) {
      if (sum(whCache) > 1) whCache[-min(which(whCache))] <- FALSE
      whPreCache <- whCache
      whPreCache[seq(which(whCache), length(whCache))] <- TRUE

      cacheCall <- match.call(Cache, rhss[whCache][[1]])
      cacheArgs <- lapply(cacheCall, function(x) x)
      cacheArgs <- cacheArgs[names(cacheArgs) != "FUN"][-1] # remove FUN and Cache (i.e., the -1)

      args <- list(eval(lhs[[1]]),
                   ._pipe = parse(text = paste(c(lhs, rhss[!whPreCache]), collapse = " %>% ")),
                   ._pipeFn = as.character(lhs[[1]]),
                   ._lhs = quote(lhs),
                   ._rhss = quote(rhss[!whPreCache]),
                   ._envir = parent)
      args <- append(args, lapply(cacheArgs, eval, envir = parent, enclos = parent))

      result <- withVisible(do.call("Cache", args))

      if (!identical(whPreCache, whCache)) {
        # If Cache call is not at the end of the pipe
        postCacheCall <- parse(text = paste(c(result$value, rhss[(!whCache) & whPreCache]),
                                            collapse = " %>% "))
        result <- withVisible(eval(postCacheCall, envir = parent, enclos = parent))
      }
    } else {
      # end reproducible package code

      # magrittr code below
      env[["_lhs"]] <- eval(lhs, parent, parent)
      result <- withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
    }

    if (getFromNamespace("is_compound_pipe", ns = "magrittr")(pipes[[1L]])) {
      eval(call("<-", lhs, result[["value"]]), parent,
           parent)
    } else {
      if (result[["visible"]])
        result[["value"]]
      else invisible(result[["value"]])
    }
  }
}


#################
#' A cache-aware pipe that does not mask with \%>\%
#'
#' This pipe can only be used at the start of a pipe chain, and must
#' be preceeded by \code{Cache(...)} to allow other Cache arguments to be passed.
#'
#' This will take the input arguments of the
#' first function immediately following the \code{Cache() \%C\%} and the
#' entire pipe chain code, evaluate them both
#' against the \code{cacheRepo} argument in \code{Cache}. If they exist, then
#' the entire pipe chain will be skipped, and only the previous final result
#' will be given. If there is no previous cached copy of the initial function's
#' arguments, then all chain elements will be evaluated. The final result will
#' be cached for future use. The entire chain must be identical, therefore.
#' The required usage should be straight forward to insert into existing code
#' that uses pipes (\code{Cache() \%C\% ... remaining pipes}.
#' \code{This is still experimental; use with care}.
#'
#' @rdname pipe
#' @name pipe
#' @importFrom utils getFromNamespace
#' @inheritParams magrittr::`%>%`
#' @importFrom magrittr freduce
#' @seealso pipe2
#' @aliases %C%
#' @export
#' @examples
#'
#' # dontrun{ # these can't be automatically run due to package conflicts with magrittr
#' tmpdir <- file.path(tempdir(), "testCache")
#' checkPath(tmpdir, create = TRUE)
#' a <- rnorm(10, 16) %>%
#'      mean() %>%
#'      prod(., 6)
#' b <- Cache(cacheRepo = tmpdir) %C% # use of the %C% pipe!
#'      rnorm(10, 16) %>%
#'      mean() %>%
#'      prod(., 6)
#' d <- Cache(cacheRepo = tmpdir) %C%
#'      rnorm(10, 16) %>%
#'      mean() %>%
#'      prod(., 6)
#' e <- Cache(cacheRepo = tmpdir) %C%
#'      rnorm(10, 16) %>%
#'      mean() %>%
#'      prod(., 5) # changed
#' all.equal(b,d) # TRUE
#' all.equal(a,d) # different because 'a' uses a unique rnorm, 'd' uses the Cached rnorm
#'                #   because the arguments to rnorm, i.e., 10 and 16, and
#'                #   the subsequent functions in the chain, are identical
#' all.equal(a,e) # different because the final function, prod, has a changed argument.
#'
#' unlink(tmpdir, recursive = TRUE)
#' #}
`%C%` <- function(lhs, rhs) {
  # adapted from magrittr code below
  parent <- parent.frame()
  env <- new.env(parent = parent)
  sc <- sys.calls()
  wherePipe <- grepl(sc, pattern = "(Cache).*(%C%)")
  isPipe <- any(wherePipe)

  # do match call on all possible calls in the stack
  mcs <- lapply(sc[which(wherePipe)], function(ca) match.call(`%C%`, call = ca))
  # choose the ones that start with either %C% or %>%
  whPipeCall <- unlist(lapply(mcs, function(elem) as.character(elem[[1]]) %in% c("%C%", "%>%")))
  # Take the first one, which will be one with the whole pipe sequence
  mc <- mcs[whPipeCall][[1]]
  mc <- parse(text = gsub(deparse(mc), pattern = "%C%",
                                    replacement = "%>%"))[[1]]
  chain_parts <- getFromNamespace("split_chain", ns = "magrittr")(mc, env = env) # nolint
  pipes <- chain_parts[["pipes"]][-1]
  rhss <- chain_parts[["rhss"]][-1]
  lhs <- chain_parts[["rhss"]][1]
  lhs <- lhs[[1]][-2] # remove the .

  # Is the first element an object or a function, if object, rm the ()
  # isFirstElemAnObj <- exists(as.character(lhs[[1]][[1]]))
  # if (isFirstElemAnObj) {
  #   lhs[[1]] <- lhs[[1]][[1]]
  # }

  env[["_function_list"]] <- lapply(seq(rhss), function(i) {
    getFromNamespace("wrap_function", ns = "magrittr")(rhss[[i]], pipes[[i]], parent)
  })
  env[["_fseq"]] <- `class<-`(eval(quote(function(value) {
    freduce(value, `_function_list`)
  }), env, env), c("fseq", "function"))
  env[["freduce"]] <- freduce
  if (getFromNamespace("is_placeholder", ns = "magrittr")(lhs)) {
    env[["_fseq"]]
  } else {

    # reproducible package code here until end of if statement
    cacheCall <- match.call(Cache, chain_parts[["lhs"]])
    cacheArgs <- lapply(cacheCall, function(x) x)
    #rhss[[1]] <- rhss[[1]][-2]

    args <- list(eval(lhs[[1]]),
                 ._pipe = parse(text = paste(c(lhs, rhss), collapse = " %>% ")),
                 ._pipeFn = as.character(lhs[[1]]),
                 ._lhs = quote(lhs),
                 ._rhss = quote(rhss),
                 ._envir = parent)
    args <- append(args, lapply(cacheArgs, eval, envir = parent, enclos = parent))

    result <- withVisible(do.call("Cache", args))

    if (getFromNamespace("is_compound_pipe", ns = "magrittr")(pipes[[1L]])) {
      eval(call("<-", lhs, result[["value"]]), parent,
           parent)
    } else {
      if (result[["visible"]])
        result[["value"]]
      else invisible(result[["value"]])
    }
  }
}

#' The special assign operator \code{\%<\%} is equivalent to Cache. See examples at the end.
#'
#' @export
#' @rdname cache
#' @param lhs A name to assign to.
#' @param rhs A function call
#' @examples
#' # Equivalent
#' a <- Cache(rnorm, 1)
#' b %<% rnorm(1)
#'
`%<%` <- function(lhs, rhs) {
  lhsChar <- deparse(substitute(lhs))
  mc <- match.call()["rhs"]
  RHS <- as.list(mc)[[1]]
  assign(lhsChar, do.call(Cache, as.list(RHS)), envir = parent.frame())
  return(invisible(get(lhsChar, envir = parent.frame(), inherits = FALSE)))
}
