#################
#' A cache-aware pipe that does not mask with \code{\%>\%}
#'
#' \emph{STILL EXPERIMENTAL. THIS MAY NOT WORK AS ANTICIPATED.}
#'
#' This pipe can only be used at any point in a pipe chain, but must
#' be preceded by \code{Cache(...)} (which allows other \code{Cache} arguments to be passed).
#'
#' This will take the input arguments of the first function immediately following
#' the \code{Cache()} and the pipe chain until the special \code{\%C\%},
#' evaluate them both against the \code{cacheRepo} argument in \code{Cache}.
#' If they exist, then the entire pipe chain will be skipped, and only the
#' previous final result will be given.
#' If there is no previous cached copy of the initial function's arguments,
#' then all chain elements will be evaluated.
#' The final result will be cached for future use.
#' Therefore, the entire chain must be identical.
#' The required usage should be straight forward to insert into existing code
#' that uses pipes (\code{Cache() \%C\% ... remaining pipes}).
#'
#' @note \emph{This is still experimental; use with care.}
#'
#' @inheritParams magrittr::`%>%`
#'
#' @aliases %C%
#' @export
#' @importFrom magrittr freduce
#' @importFrom utils getFromNamespace
#' @name pipe
#' @rdname pipe
#'
#' @examples
#' library(magrittr) # standard pipe
#' # dontrun{ # these can't be automatically run due to package conflicts with magrittr
#' tmpdir <- file.path(tempdir(), "testCache")
#' checkPath(tmpdir, create = TRUE)
#' a <- rnorm(10, 16) %>%
#'      mean() %>%
#'      prod(., 6)
#' b <- Cache(cacheRepo = tmpdir) %C% # use of the %C% pipe!
#'      rnorm(10, 16) %>% # everything after here is NOT cached!
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
#' ###########
#' # multiple random elements shows Cached sequence up to %C%
#' a1 <- Cache(cacheRepo = tmpdir) %>%
#'        seq(1, 10) %>%
#'        rnorm(2, mean = .) %>%
#'        mean() %C%                # Cache pipe here --
#'                                  # means this pipe is the last one that is Cached
#'        rnorm(3, mean = .) %>%
#'        mean(.) %>%
#'        rnorm(4, mean = .)  # Random 4 numbers, the mean is same each time
#' a2 <- Cache(cacheRepo = tmpdir) %>%
#'        seq(1, 10) %>%
#'        rnorm(2, mean = .) %>%
#'        mean() %C%                # Cache pipe here --
#'                                  # means this pipe is the last one that is Cached
#'        rnorm(3, mean = .) %>%
#'        mean(.) %>%
#'        rnorm(4, mean = .)  # Random 4 numbers, the mean is same each time
#' sum(a1 - a2) # not 0 # i.e., numbers are different
#'
#' # NOW DO WITH CACHE AT END
#' b1 <- Cache(cacheRepo = tmpdir) %>%
#'        seq(1, 10) %>%
#'        rnorm(2, mean = .) %>%
#'        mean() %>%
#'                                  # means this pipe is the last one that is Cached
#'        rnorm(3, mean = .) %>%
#'        mean(.) %C%               # Cache pipe here --
#'        rnorm(4, mean = .)        # These are samethe mean is same each time
#' b2 <- Cache(cacheRepo = tmpdir) %>%
#'        seq(1, 10) %>%
#'        rnorm(2, mean = .) %>%
#'        mean() %>%
#'                                  # means this pipe is the last one that is Cached
#'        rnorm(3, mean = .) %>%
#'        mean(.) %C%               # Cache pipe here --
#'        rnorm(4, mean = .)        # These are samethe mean is same each time
#' sum(b1 - b2) # 0 # i.e., numbers are same
#'
#' unlink(tmpdir, recursive = TRUE)
#' #}
`%C%` <- function(lhs, rhs) {
  # adapted from magrittr code below
  parent <- parent.frame()
  env <- new.env(parent = parent)
  sc <- sys.calls()
  #wherePipe <- grepl(sc, pattern = "(Cache).*(%C%)")
  wherePipe <- grepl(sc, pattern = "%C%")
  isPipe <- any(wherePipe)

  # do match call on all possible calls in the stack
  mcs <- lapply(sc[which(wherePipe)], function(ca) match.call(`%C%`, call = ca))
  # choose the ones that start with either %C% or %>%
  whPipeCall <- unlist(lapply(mcs, function(elem) as.character(elem[[1]]) %in% c("%C%", "%>%")))
  # Take the first one, which will be one with the whole pipe sequence
  mc <- mcs[whPipeCall][[1]]
  maxNumPipes <- 1e3
  numPipes <- maxNumPipes
  if (sum(whPipeCall) > 1) {
    randStr <- "trwertrw"
    mc1 <- gsub(mc, pattern = "%C%", replacement = randStr)
    mc1Coll <- strsplit(paste0(mc1, collapse = " "), randStr)[[1]]
    penultimateCall <- mc1Coll[length(mc1Coll) - 1]
    penultimateCall <- gsub("(%>%)", randStr, penultimateCall)
    numPipes <- length(strsplit(gsub("(%>%)", randStr, penultimateCall), randStr)[[1]]) - 1
    #numPipes <- stringr::str_count(penultimateCall, "%>%")
  }

  mc <- parse(text = gsub(deparse(mc), pattern = "%C%", replacement = "%>%"))[[1]]
  chain_parts <- getFromNamespace("split_chain", ns = "magrittr")(mc, env = env) # nolint
  pipes <- chain_parts[["pipes"]][-1]
  rhss <- chain_parts[["rhss"]][-1]
  lhs <- chain_parts[["rhss"]][1]
  lhs <- lhs[[1]][-2] # remove the .
  if (numPipes < maxNumPipes)
    rhss <- rhss[seq(numPipes - 1)]

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
    cacheArgs <- lapply(cacheCall, function(x) x)[-1]
    #rhss[[1]] <- rhss[[1]][-2]

    args <- list(eval(lhs[[1]]),
                 ._pipe = parse(text = paste(c(lhs, rhss), collapse = " %>% ")),
                 ._pipeFn = as.character(lhs[[1]]),
                 ._lhs = quote(lhs),
                 ._rhss = quote(rhss),
                 ._envir = parent)
    args <- append(args, lapply(cacheArgs, eval, envir = parent, enclos = parent))

    result <- withVisible(do.call("Cache", args))

    cmpdPipe <- FALSE
    if (length(pipes) > 0) {
      if (identical(pipes[[1L]], quote(`%<>%`)))
        cmpdPipe <- TRUE
    }

     if (cmpdPipe) {
       ret <- eval(call("<-", lhs, result[["value"]]), parent, parent)
     } else {
      if (result[["visible"]])
        ret <- result[["value"]]
      else
        ret <- invisible(result[["value"]])
     }
  }
}

#' The special assign operator \code{\%<C-\%} is equivalent to Cache. See examples at the end.
#'
#' Still experimental and may change. This form cannot pass any arguments to
#' ]code{Cache}, such as \code{cacheRepo}, thus it is of limited utility. However,
#' it is a clean alternative for simple cases.
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
