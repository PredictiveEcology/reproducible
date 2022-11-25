#################
#' A cache-aware pipe (currently not working)
#'
#' @description
#' \if{html}{\figure{lifecycle-experimental.svg}{options: alt="experimental"}}
#'
#' With updates to `magrittr` to version 2.0, this Cache pipe is now broken.
#' We are working on an update.
#'
#' This pipe can only be used at any point in a pipe chain, but must
#' be preceded by `Cache(...)` (which allows other `Cache()
#' \%C\% ... remaining pipes`
#' arguments to be passed).
#'
#' This will take the input arguments of the first function immediately following
#' the `Cache()` and the pipe chain until the special `\%C\%`,
#' evaluate them both against the `cacheRepo` argument in `Cache`.
#' If they exist, then the entire pipe chain will be skipped, and only the
#' previous final result will be given.
#' If there is no previous cached copy of the initial function's arguments,
#' then all chain elements will be evaluated.
#' The final result will be cached for future use.
#' Therefore, the entire chain must be identical.
#' The required usage should be straight forward to insert into existing code
#' that uses pipes (`Cache() \%C\% ... remaining pipes`).
#'
#' @aliases %C%
#' @export
#' @importFrom magrittr freduce
#' @importFrom utils getFromNamespace
#' @name pipe
#' @rdname pipe
#'
`%C%` <- function(lhs, rhs) {
  stop("This %C% is currently broken due to magrittr 2.0 updates; working on a fix")
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
  chain_parts <- split_chain(mc, env = env) # nolint
  if (grepl("Cache", chain_parts[["lhs"]][1])) {
    rhss <- chain_parts[["rhss"]][-1]
    pipes <- chain_parts[["pipes"]][-1]
    lhs <- chain_parts[["rhss"]][1]
    lhs <- lhs[[1]][-2] # remove the .
    cacheCall <- match.call(Cache, chain_parts[["lhs"]])
    cacheArgs <- lapply(cacheCall, function(x) x)[-1]
  } else {
    rhss <- chain_parts[["rhss"]]
    pipes <- chain_parts[["pipes"]]
    lhs <- chain_parts[["lhs"]]
    cacheArgs <- list()
    # lhs <- lhs[[1]][-2] # remove the .

  }
  if (numPipes < maxNumPipes)
    rhss <- rhss[seq(numPipes - 1)]

  env[["_function_list"]] <- lapply(seq(rhss), function(i) {
    wrap_function(rhss[[i]], pipes[[i]], parent)
  })
  env[["_fseq"]] <- `class<-`(eval(quote(function(value) {
    freduce(value, `_function_list`)
  }), env, env), c("fseq", "function"))
  env[["freduce"]] <- freduce

  if (is_placeholder(lhs)) {
    env[["_fseq"]]
  } else {
    # reproducible package code here until end of if statement
    browser()
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

#' The special assign operator `\%<C-\%` is equivalent to Cache. See examples at the end.
#'
#' Still experimental and may change. This form cannot pass any arguments to
#' `Cache`, such as `cacheRepo`, thus it is of limited utility. However,
#' it is a clean alternative for simple cases.
#'
#' @export
#' @rdname pipe
#' @param lhs A name to assign to.
#' @param rhs A function call
`%<%` <- function(lhs, rhs) {
  stop("%<% is currently broken because of changes to magrittr internals")
  lhsChar <- deparse(substitute(lhs))
  mc <- match.call()["rhs"]
  RHS <- as.list(mc)[[1]]
  assign(lhsChar, do.call(Cache, as.list(RHS)), envir = parent.frame())
  return(invisible(get(lhsChar, envir = parent.frame(), inherits = FALSE)))
}

split_chain <- function(expr, env) {
  # lists for holding the right-hand sides and the pipe operators.
  rhss  <- list()
  pipes <- list()

  # Process the call, splitting it at each valid magrittr pipe operator.
  i <- 1L
  while(is.call(expr) && is_pipe(expr[[1L]])) {
    pipes[[i]] <- expr[[1L]]
    rhs <- expr[[3L]]

    if (is_parenthesized(rhs))
      rhs <- eval(rhs, env, env)

    rhss[[i]] <-
      if (is_dollar(pipes[[i]]) || is_funexpr(rhs))
        rhs
    else if (is_function(rhs))
      prepare_function(rhs)
    else if (is_first(rhs))
      prepare_first(rhs)
    else
      rhs

    # Make sure no anonymous functions without parentheses are used.
    if (is.call(rhss[[i]]) && identical(rhss[[i]][[1L]], quote(`function`)))
      stop("Anonymous functions myst be parenthesized", call. = FALSE)

    expr <- expr[[2L]]
    i <- i + 1L
  }

  # return the components; expr will now hold the left-most left-hand side.
  list(rhss = rev(rhss), pipes = rev(pipes), lhs = expr)
}

# Check whether a symbol is a valid magrittr pipe.
#
# @param pipe A quoted symbol
# @return logical - TRUE if a valid magrittr pipe, FALSE otherwise.
is_pipe <- function(pipe) {
  identical(pipe, quote(`%>%`))   ||
    identical(pipe, quote(`%T>%`))  ||
    identical(pipe, quote(`%<>%`))  ||
    identical(pipe, quote(`%$%`))
}


# Determine whether an non-evaluated call is parenthesized
#
# @param a non-evaluated expression
# @retun logical - TRUE if expression is parenthesized, FALSE otherwise.
is_parenthesized <- function(expr) {
  is.call(expr) && identical(expr[[1]], quote(`(`))
}

# Check whether a pipe is the dollar pipe.
#
# @param pipe A (quoted) pipe
# @return logical - TRUE if pipe is the dollar pipe, FALSE otherwise.
is_dollar <- function(pipe)
{
  identical(pipe, quote(`%$%`))
}

# Check whether expression is enclosed in curly braces.
#
# @param  expr An expression to be tested.
# @return logical - TRUE if expr is enclosed in `{`, FALSE otherwise.
is_funexpr <- function(expr)
{
  is.call(expr) && identical(expr[[1]], quote(`{`))
}

# Determine whether an expression counts as a function in a magrittr chain.
#
# @param a non-evaluated expression.
# @return logical - TRUE if expr represents a function, FALSE otherwise.
is_function <- function(expr)
{
  is.symbol(expr) || is.function(expr)
}

# Determine whether an expression is of the type that needs a first argument.
#
# @param a non-evaluated expression.
# @return logical - TRUE if expr is of "first-argument" type, FALSE otherwise.
is_first <- function(expr)
{
  !any(vapply(expr[-1], identical, logical(1), quote(.)))
}

# Prepare a magrittr rhs of "first-argument" type.
#
# @param a an expression which passes \code{is_first}
# @return an expression prepared for functional sequence construction.
prepare_first <- function(expr)
{
  as.call(c(expr[[1L]], quote(.), as.list(expr[-1L])))
}

prepare_function <- function(f)
{
  as.call(list(f, quote(.)))
}

# Wrap an expression in a function
#
# This function takes the "body" part of a function and wraps it in
# a function. The return value depends on whether the function is created
# for its side effect with the tee operator. If the operator is \code{\%$\%}
# then the expression will be evaluated in a \code{with(., )} statement.
#
# @param body an expression which will serve as function body in single-argument
#    function with an argument names \code{.} (a dot)
# @param pipe a quoted magrittr pipe, which determines how the function is made.
# @param env The environment in which to contruct the function.

# @details Currently, the only distinction made is whether the pipe is a tee
#   or not.
#
# @return a function of a single argument, named \code{.}.
wrap_function <- function(body, pipe, env)
{

  if (is_tee(pipe)) {
    body <- call("{", body, quote(.))
  } else if (is_dollar(pipe)) {
    body <- substitute(with(., b), list(b = body))
  }
  eval(call("function", as.pairlist(alist(.=)), body), env, env)
}

# Check whether a symbol is the magrittr placeholder.
#
# @param  symbol A (quoted) symbol
# @return logical - TRUE if symbol is the magrittr placeholder, FALSE otherwise.
is_placeholder <- function(symbol)
{
  identical(symbol, quote(.))
}

# Check whether a pipe is a tee.
#
# @param pipe A (quoted) pipe
# @return logical - TRUE if pipe is a tee, FALSE otherwise.
is_tee <- function(pipe)
{
  identical(pipe, quote(`%T>%`))
}
