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
#' @importFrom magrittr freduce
#' @importFrom utils getFromNamespace
#' @name pipe
#' @rdname pipe
#'
`%C%` <- function(lhs, rhs) {
  stop("This %C% is currently broken due to magrittr 2.0 updates; working on a fix; ",
       "use base pipe |> ")
}

#' The special assign operator `\%<C-\%` is equivalent to Cache. See examples at the end.
#'
#' Still experimental and may change. This form cannot pass any arguments to
#' `Cache`, such as `cacheRepo`, thus it is of limited utility. However,
#' it is a clean alternative for simple cases.
#'
#' @rdname pipe
#' @param lhs A name to assign to.
#' @param rhs A function call
`%<%` <- function(lhs, rhs) {
  stop("%<% is currently broken because of changes to magrittr internals")
}


