## Working out what call is being cached: capturing it, matching arguments
## against formals, normalising the many ways the same call can be written, and
## evaluating it. Nothing here touches the cache store.


#' Convert all ways of calling a function into canonical form, including defaults
#'
#' e.g., stats::rnorm(1) --> rnorm(n = 1, mean = 0, sd = 1)
#' @param call The full captured call as it was passed by user.
#' @param usesDots Logical. Whether the original `Cache` call used `...`
#' @param isSquiggly Logical. Whether there are curly braces e.g., as in a pipe sequence.
#' @param .callingEnv Environment. The environment from which `Cache` was called.
convertCallToCommonFormat <- function(call, usesDots, isSquiggly, .callingEnv) {

  if (requireNamespace("covr", quietly = TRUE) && covr::in_covr()) {
    strip_covr_wrappers <- function(expr) {
      while (is.call(expr) && identical(expr[[1]], as.name("{"))) expr <- expr[[length(expr)]]
      expr
    }
    call <- strip_covr_wrappers(call)
    if (length(call) >= 2L && is.language(call[[2]])) call[[2]] <- strip_covr_wrappers(call[[2]])
  }

  .functionName <- NULL
  # Check if the first argument is a function call
  func_full <- NULL

  func_call <- NULL
  if (is.call(call[[2]])) {

    func_call <- call[[2]]  # This is the actual function call (e.g., stats::rnorm)
    # Extract the function without the package prefix
    if (is.call(func_call[[1]]) && func_call[[1]][[1]] == quote(`::`)) {
      func <- func_full <- func_call[[1]]
      if (length(func_call) == 2)
        args <- func_call[[-1]]
      else
        args <- as.list(func_call)[-1]
      func_call <- as.call(c(func_call[[1]][[3]], args))
    } else {
      if (func_call[[1]] == quote(`::`) || func_call[[1]] == quote(`:::`)) {
        func_full <- func_call
        func <- func_call  # Package prefix, using FUN as name only
        args <- as.list(call[-(1:2)])
        func_call <- as.call(c(func_call[[3]], args))
      } else {
        if (isDollarOnlySqBr(func_call)) {
          func <- eval(func_call, envir = .callingEnv)  # No package prefix
          if (usesDots)
            func_call <- as.call(append(list(func), as.list(call[-(1:2)])))
        } else {
          # It is a complete call e.g., FUN = rnorm(1)
          func <- func_call[[1]]  # No package prefix
          if (isDollarOnlySqBr(func)) {
            func <- eval(func, envir = .callingEnv)  # No package prefix
          }
        }
        if (identical(func, quote(do.call))) {
          func_call <- undoDoCall(func_call, .callingEnv = .callingEnv)
          func <- func_call[[1]]  # Extract the function for do.call (e.g., rnorm)
        }
        args <- as.list(func_call)[-1]
      }
    }
  } else if (identical(call[[2]], quote(do.call))) {
    # Special handling for do.call to return the function unevaluated
    func <- call[[3]]  # Extract the function for do.call (e.g., rnorm)
    args <- eval(call[[4]], envir = .callingEnv)  # Evaluate the argument list
  } else {
    func <- call[[2]]  # This is the function (e.g., rnorm)
    args <- as.list(call[-(1:2)])  # These are the arguments (e.g., 1)
    func_call <- as.call(append(list(func), args))
    # Check for package prefix
    if (is.call(func) && func[[1]] == quote(`::`)) {
      func <- func[[3]]  # Get the actual function name (e.g., rnorm)
    }
  }

  if (is.call(func) || is.name(func)) {
    if (is.name(func))
      .functionName <- format(func)
    fun <- if (is.null(func_full)) func else func_full
    if (is.name(fun)) {
      infixes <- c("+", "-", "*", "/", "==", "!=", "<", ">", "<=", ">=", "&&", "||")
      areInfixes <- any(fun == infixes)
      if (!any(areInfixes)) {
        fun_chr <- as.character(fun)
        # Only parse if it's not a reserved word
        if (!(fun_chr %in% c("if", "function"))) {
          fun <- parse(text = fun_chr)
        }
        # else: leave fun as-is (symbol), so downstream logic can handle it
        # fun <- parse(text = fun)
      }
    }
    func <- eval(fun, envir = .callingEnv)
  }

  # deal with defunct arguments
  if (usesDots) { # any defunct argument will show up in the usesDots; need to keep them for defunct fn
    argsSupplied <- unique(names(call))[-1]
  } else {
    argsSupplied <- names(args)
  }
  defunct(setdiff(argsSupplied, formalArgs(func))) # pull the plug if args are defunct, and not used in FUN
  argsRm <- names(args) %in% setdiff(names(.formalsCache), names(formals(func)))
  if (any(argsRm %in% TRUE))
    args <- args[!argsRm %in% TRUE]

  # build new call from func and args; both must be correct by here
  new_call <- as.call(append(list(func), args))
  # This matches call on the FUN, not a duplicate of matchCall2
  matched_call <- match_call_primitive(func, new_call, expand.dots = TRUE, envir = .callingEnv)

  if (isSquiggly) {
    FUNcaptured <- recursiveEvalNamesOnly(matched_call, envir = .callingEnv) # deals with e.g., stats::rnorm, b$fun, b[[fun]]
    args <- as.list(FUNcaptured[-1])
  } else {
    args <- as.list(matched_call)[-1]
    args <- evaluate_args(args, envir = .callingEnv)
  }
  combined_args <- combine_clean_args(func, args, .objects = NULL, .callingEnv)

  # Check for arguments that are in both Cache and the FUN
  matched_call <- checkOverlappingArgs(call, combined_args, dotsCaptured = args,
                                       functionName = "outer", matched_call, whichCache = "cache2")

  if (is.null(func_call)) func_call <- new_call
  func_call2 <- as.call(c(func_call[[1]], args))
  attr(matched_call, ".Cache")$func_call <- func_call2
  attr(matched_call, ".Cache")$args_w_defaults <- combined_args
  attr(matched_call, ".Cache")$method <- func
  attr(matched_call, ".Cache")$.functionName <- .functionName

  return(matched_call)
}


evaluate_args <- function(args, envir) {
  lapply(args, function(arg) {
    if (is.call(arg)) {
      arg <- tryCatch(eval(arg, envir = envir), error = function(err) { # can't be tryCatch2 --> this must always be a tryCatch
        # If it's a call that cannot be evaluated, evaluate recursively
        fail <- "fail"
        newPossArgMinus1 <- tryCatch(evaluate_args(as.list(arg[-1]), envir), error = function(err) {
          fail
        })
        if (!identical(newPossArgMinus1, fail)) {
          arg <- as.call(c(arg, as.list(newPossArgMinus1[-1])))
        }
        arg
      })
    } else if (is.symbol(arg)) {
      # If it's a symbol, evaluate it in the specified environment
      arg <- eval(arg, envir)
    }
    return(arg)
  })
}


combine_clean_args <- function(FUN, args, .objects, .callingEnv) {
  # has to be after match.call --> relies on name matched arguments
  defaults <- get_function_defaults(eval(FUN, .callingEnv))
  combined_args <- reorder_arguments(defaults, args)
  empties <- vapply(combined_args, function(ca) if (is.symbol(ca)) capture.output(ca) else "Normal", character(1))
  empties <- !nzchar(empties)
  if (isTRUE(any(empties)))
    combined_args <- combined_args[!empties]

  # Process the .objects argument using the helper function
  if (!is.null(.objects)) {
    combined_args <- filter_objects(combined_args, .objects)
  }

  combined_args
}


# Helper function to filter arguments based on .objects
filter_objects <- function(evaluated_args, .objects) {
  list_or_env_arg <- NULL
  for (name in names(evaluated_args)) {
    if (is.list(evaluated_args[[name]]) || is.environment(evaluated_args[[name]])) {
      list_or_env_arg <- name
      break
    }
  }

  if (!is.null(list_or_env_arg)) {
    actual_list <- evaluated_args[[list_or_env_arg]]
    filtered_elements <- actual_list[.objects]
    filtered_list <- actual_list
    filtered_list[names(filtered_list) %in% .objects] <- filtered_elements
    filtered_list <- filtered_list[.objects]
    evaluated_args[[list_or_env_arg]] <- filtered_list
  }

  return(evaluated_args)
}


# Function to normalize the call to handle `do.call`
undoDoCall <- function(call, .callingEnv) {
  if (is.call(call) && all(as.character(call[[1]]) == "do.call")) {
    func <- call[[2]]
    args <- call[[3]]

    if (isTRUE(is.call(args)) && isTRUE(as.character(args[[1]]) == "list")) {
      args <- as.list(args[-1])
    }
    if (is.name(args))
      args <- recursiveEvalNamesOnly(args, envir = .callingEnv)

  } else {
    func <- call[[1]]
    args <- as.list(call[-1])
  }
  return(as.call2(func, args))
}


# Helper function to get function defaults
get_function_defaults <- function(func) {
  if (is.primitive(func)) {
    formals_list <- formals(args(list))
  } else {
    formals_list <- formals(func)
  }
  return(as.list(formals_list))
}


# Helper function to reorder arguments based on formal arguments, combining defaults and user args
reorder_arguments <- function(formals, args) {
  # Combine defaults and args: user args override defaults

  areDots <- names(args) %in% "..."
  if (any(areDots)) {
    args2 <- args
    args2[[which(areDots)]] <- NULL
    args <- append(args2, args[[which(areDots)]])
  }

  if (FALSE) {
    # areDots <- names(args) %in% "..."
    namesOfArgs <- names(args) %in% "..."
    areDots <- any(names(formals) %in% "...") || any(namesOfArgs)
    if (any(areDots)) {
      if (length(namesOfArgs)) {
        args2 <- args
        for (wh in which(namesOfArgs)) {
          args2[[wh]] <- NULL
          args <- append(args2, args[[which(namesOfArgs)]])
        }

      } else {
        # these are unnamed args in the dots
      }
    }


  }

  if (length(formals) == 1 && all(names(formals) %in% "...")) {
    # This is case of things like `list`, `file.path`
    ordered_args <- args
  } else {
    # This will remove unnamed elements; which isn't right
    combined_args <- modifyList(formals, args, keep.null = TRUE)
    emptyNams <- names(args) %in% ""
    if (any(emptyNams)) {
      combined_args <- append(combined_args, args[emptyNams])
    }
    areDots <- names(combined_args) %in% "..."
    if (any(areDots)) {

      # argPlaceInsert <- which(!names(args) %in% names(formals))

      # needArgs <- !names(args) %in% names(combined_args)
      combined_args[areDots] <- NULL
      ordered_args <- combined_args
      # combined_args <- append(combined_args, args[needArgs])
      # areDots2 <- names(formals) %in% "..."
      # whNotDots <- which(!areDots2)
      # whDots <- which(areDots2)
      # first <- if (whDots > 1) seq(whDots - 1) else numeric()
      # anySeconds <- !whDots > whNotDots
      # second <- if (any(anySeconds)) whNotDots[anySeconds] else numeric()
      # ordered_args <- c(args[argPlaceInsert], formals[second])
    } else {
      ordered_args <- combined_args[union(names(formals), names(combined_args))]
    }
  }
  # Preserve the order of the formals

  return(ordered_args)
}


match_call_primitive <- function(definition = sys.function(sys.parent()),
                                 call = sys.call(sys.parent()),
                                 expand.dots = TRUE,
                                 envir = parent.frame()) {
  # Check if the function is a primitive infix operator
  if (is.primitive(definition)) {
    # For infix operators like +, -, *, etc., they are not called in the standard way
    infixes <- c(`+`, `-`, `*`, `/`, `==`, `!=`, `<`, `>`, `<=`, `>=`, `&&`, `||`)
    areInfixes <- vapply(infixes, function(i) identical(i, definition), FUN.VALUE = logical(1))

    if (isTRUE(any(areInfixes))) {
      # Handle infix operators by keeping the call intact
      return(call)
    }

    # For other primitives, match as best as possible
    args <- as.list(call)[-1]  # remove the function name
    if (expand.dots) {
      args <- lapply(args, eval, envir = envir)
    }
    # Construct the matched call manually for primitive
    matched <- as.call(c(definition, args))
    return(matched)
  } else {
    # Non-primitive function: fall back to regular match.call
    return(base::match.call(definition = definition,
                            call = call,
                            expand.dots = expand.dots,
                            envir = envir))
  }
}


as.call2 <- function(func, args) {
  as.call(c(as.name(deparse(func)), args))
}


callIsQuote <- function(call) {
  if (length(call$FUN) > 1) # just a function
    if (identical(call$FUN[[1]], quote(quote))) {
      call$FUN <- as.list(call$FUN)[[-1]] # unquote it
    }
  call
}


convertCallWithSquigglyBraces <- function(call, usesDots) {
  ## `x[[-1]]` is only valid when length(x) == 2 -- there it unambiguously means
  ## "the other element". On a longer object R raises the opaque
  ## "invalid negative subscript in get1index <real>". A braced block with more
  ## than one statement is exactly that longer object, so it must be rejected
  ## with the informative message BEFORE any [[-1]] is attempted. Previously
  ## only the length-2 call shape checked, so `Cache({a <- 1; a + 1})` reported
  ## the intended message while `Cache({a <- 1; a + 1}, cachePath = x)` -- the
  ## same unsupported code, merely with another argument -- surfaced the
  ## subscript error instead.
  stopIfMultiStep <- function(braced) {
    if (length(braced) > 2)
      stop("Cache does not yet support multi-step caching unless using the pipe (|>)")
    braced
  }
  if (length(call) == 2) {
    braced <- stopIfMultiStep(call[[-1]])
    call <- as.call(c(call[[1]], braced[[-1]]))
  } else if ((length(call) > 2) && isFALSE(usesDots)) {
    braced <- stopIfMultiStep(as.list(call[-1])[[1]])
    call <- as.call(c(call[[1]], FUN = braced[[-1]], as.list(call[-1])[-1]))
  }
  call
}





#' Remove `quote` and determine if call uses `...`
#'
#' Minor cleaning up of the `FUN` and `...` to be used subsequently. This does only very minor
#' things as it is run even if `useCache = FALSE`, i.e., even if the `Cache` is skipped.
#'
#' @inheritParams Cache
#' @inheritParams base::match.call
#' @param envir2 Environment. The environment where `matchCall2` was called.
#' @return A named list with `call` (the original call, without `quote`),
#' `FUNorig`, the original value passed by user to `FUN`, and `usesDots` which
#' is a logical indicating whether the `...` are used.
matchCall2 <- function(definition, call, envir, envir2 = parent.frame(), FUN) {
  if (missing(FUN)) {
    stop(.message$CacheRequiresFUNtxt())
  } else {
    FUNcaptured <- substitute(FUN, env = envir2)
    # This matches call for Cache
    call <- match.call(definition, call = call, expand.dots = TRUE, envir = envir)
    # call <- callIsQuote(call) # stip `quote`
    FUNorig <- call$FUN

    usesDots <- sum(!nzchar(names(call))) > 1 || sum(!names(call) %in% .namesCacheFormals) > 2
  }
  list(call = call, FUNorig = FUNorig, usesDots = usesDots, FUNcaptured = FUNcaptured)
}


#' Harmonize all forms of call
#'
#' This will convert all known (imagined) calls so that they have the same canonical
#' format i.e., `rnorm(n = 1, mean = 0, sd = 1)`
#'
#' @param callList A named list with elements `call`, `usesDots` and `FUNorig`
#' @param .callingEnv The calling environment where `Cache` was called from
#' @param .functionName A possible function name. If omitted, then it will be deduced
#'   from the `callList` and may be inaccurate.
#' @return A named list. We illustrate with the example `rnorm(1)`. The named
#' list will have the original `callList` (`call` (the original call, without `quote`),
#' `FUNorig`, the original value passed by user to `FUN`, and `usesDots` which
#' is a logical indicating whether the `...` are used), and appended with `new_call`
#' (the harmonized call, with the function and arguments evaluated, e.g.,
#' `(function (n, mean = 0, sd = 1) .Call(C_rnorm, n, mean, sd))(1)`), `func_call`, the same harmonized call
#' with neither function nor arguments not evaluated (e.g., `rnorm(1)`), `func` which
#' will be function or method definition
#' `function (n, mean = 0, sd = 1) .Call(C_rnorm, n, mean, sd)`,
#' and `.functionName`, which will be the function name as a character string (`rnorm`)
#' either directly passed from the user's `.functionName` or deduced from the `func_call`.
harmonizeCall <- function(callList, .callingEnv, .functionName = NULL) {
  callList$call <- callIsQuote(callList$call) # stip `quote`

  isSquiggly <- isSquigglyCall(callList$FUNorig)
  # isSquiggly <- is(callList$FUNorig, "{")
  if (isTRUE(isSquiggly))
    callList$call <- convertCallWithSquigglyBraces(callList$call, callList$usesDots)
  new_call <- convertCallToCommonFormat(callList$call, callList$usesDots, isSquiggly, .callingEnv) # evaluated arguments
  func_call <- attr(new_call, ".Cache")$func_call         # not evaluated arguments
  .functionNamePoss <- attr(new_call, ".Cache")$.functionName
  func <- as.list(new_call)[[1]]

  if (!is.null(.functionName)) {
    dotFnGrep <- "\\.functionName"
    hasDotFNLogical <- grepl(dotFnGrep, .functionName)
    hasDotFN <- isTRUE(any(hasDotFNLogical) )
    if (hasDotFN)
      .functionName <- gsub(dotFnGrep, .functionNamePoss, .functionName)
  }

  # Try to identify the .functionName; if can't just use the matched call callList$FUNorig
  if (is.null(.functionName)) {
    if (!is.null(.functionNamePoss))
      .functionName <- .functionNamePoss
    else
      .functionName <- getFunctionName2(func_call)# as.character(normalized_FUN[[1]])
  }
  if (!isTRUE(any(nzchar(.functionName)))) {
    .functionName <- format(callList$FUNorig)
  }
  append(callList, list(new_call = new_call, func_call = func_call,
                           func = func, .functionName = .functionName))
}


identical2 <- function(a, b) {
  id <- identical(a, b)
  if (isTRUE(id))
    return(TRUE)
  isTRUE(all.equal(a, b, check.attributes = FALSE))
}




evalTheFunAndAddChanged <- function(callList, keyFull, outputObjects, length, algo, quick,
                                    classOptions, .callingEnv, verbose, ...) {
  outputFromEvaluate <- evalTheFun(callList$FUNcaptured, !callList$usesDots,
                                   matchedCall = callList$call, envir = .callingEnv,
                                   verbose = verbose, ...)

  # Because this has be run, it means that it has changed; add an attribute to say that
  outputFromEvaluate <- .addChangedAttr(outputFromEvaluate, keyFull$preDigest,
                                        origArguments = attr(callList$new_call, ".Cache")$args_w_defaults,
                                        .objects = outputObjects, length = length,
                                        algo = algo, quick = quick, classOptions = classOptions, ...
  )
  outputFromEvaluate
}




isSquigglyCall <- function(x) {
  is(x, "{")
}



#' @keywords internal
.unlistToCharacter <- function(l, max.level = 1) {
  if (max.level > 0) {
    lapply(l, function(l1) {
      if (is.character(l1)) {
        l1
      } else {
        if (is.list(l1)) {
          .unlistToCharacter(l1, max.level = max.level - 1)
        } else {
          "not list"
        }
      }
    })
  } else {
    if (is.list(l)) {
      unlist(l)
    } else {
      "not list2"
    }
    # unlist(l)
  }
}



isDollarSqBrPkgColon <- function(args) {
  ret <- FALSE
  if (length(args) == 3 || length(args) == 1) { # i.e., only possible if it is just b$fun or stats::runif, not stats::runif(1) or b$fun(1)
    ret <- isTRUE(any(try(grepl("^\\$|\\[|\\:\\:", args)[1], silent = TRUE)))
  }
  ret
}


isPkgColon <- function(args) {
  ret <- FALSE
  if (length(args) == 3) { # i.e., only possible if it is just b$fun or stats::runif, not stats::runif(1) or b$fun(1)
    ret <- isTRUE(any(try(grepl("\\:\\:", args)[1], silent = TRUE)))
  }
  ret
}


isDollarOnlySqBr <- function(args) {
  ret <- FALSE
  if (length(args) == 3) { # i.e., only possible if it is just b$fun or stats::runif, not stats::runif(1) or b$fun(1)
    ret <- isTRUE(any(try(grepl("^\\$|\\[", args)[1], silent = TRUE)))
  }
  ret
}


recursiveEvalNamesOnly <- function(args, envir = parent.frame(), outer = TRUE, recursive = TRUE) {

  needsEvaling <- (length(args) > 1) || (length(args) == 1 && is.call(args)) # second case is fun() i.e., no args
  if (isTRUE(needsEvaling)) {
    if (is.call(args[[1]])) { # e.g., a$fun, stats::runif
      args[[1]] <- eval(args[[1]], envir)
    }

    isStandAlone <- FALSE
    if (length(args) == 3) { # e.g., status::runif or fun(1, 2); second case requires subsequent efforts
      if (!is.function(args[[1]])) { # this removes fun(1, 2) case
        isStandAlone <- isDollarSqBrPkgColon(args[[1]])
      }
    } else if (length(args[[1]]) == 3) {
      isStandAlone <- isDollarSqBrPkgColon(args[[1]])
    }

    if (identical(quote(`function`), args[[1]])) # if it is function definition, then leave the inside unevaluated
      isStandAlone <- TRUE

    if (identical(as.name("<-"), args[[1]])) {
      args <- as.list(args[-(1:2)])[[1]]
    }

    if (identical(quote(parse), args[[1]])) {
      args <- eval(args)
    }

    if (!isTRUE(recursive)) {
      isStandAlone <- TRUE
    }

    if (!any(isStandAlone)) {
      out <- lapply(args, function(xxxx) {
        if (is.name(xxxx)) {
          # exists(xxxx, envir = envir, inherits = FALSE)
          if (exists(xxxx, envir)) { # looks like variables that are in ... in the `envir` are not found; would need .whereInStack
            evd <- try(eval(xxxx, envir), silent = TRUE)
            isPrim <- is.primitive(evd)
            if (isPrim) {
              eval(xxxx)
            } else {
              isQuo <- is(evd, "quosure")
              if (isQuo) {
                evd <- rlang::eval_tidy(evd)
              }
              if (is(evd, "list")) {
                evd <- recursiveEvalNamesOnly(evd, envir, outer = FALSE)
              }
              evd
            }
          } else {
            ret <- xxxx
            ret
          }
        } else {
          if (is.call(xxxx)) {
            if (identical(quote(eval), xxxx[[1]])) { # basically "eval" should be evaluated
              message(
                "There is an `eval` call in a chain of calls for Cache; ",
                "\n  eval is evaluated before Cache which may be undesired. ",
                "\n  Perhaps use `do.call` if the evaluation should not occur prior to Cache"
              )
              ret <- eval(xxxx, envir = envir)
            } else {
              ret <- recursiveEvalNamesOnly(xxxx, envir, outer = FALSE)
            }
          } else {
            ret <- xxxx
          }
          ret
        }
      })



      args <- as.call(out)
      # args <- if (isTRUE(outer)) try(as.call(out)) else out
      if (is.function(args[[1]])) {
        args <- match_call_primitive(args[[1]], args, expand.dots = FALSE, envir = envir)
        args[[1]] <- getMethodAll(args, envir)
      }
    } else {
      # paths$inputPath comes here to be evaluated to its path
      args <- eval(args, envir)
    }
  } else {
    if (length(args) == 1 && is.name(args)) {
      args <- eval(args, envir)
    }
  }
  args
}



matchCall <- function(FUNcaptured, envir = parent.frame(), fnName) {
  if (length(FUNcaptured) > 1) {
    FUN <- FUNcaptured[[1]]
    args <- as.list(FUNcaptured[-1])
    if (is.call(FUN)) FUN <- eval(FUN, envir)
    if (is.function(FUN)) {
      forms <- if (is.primitive(FUN)) formals(args(FUN)) else formals(FUN)
      if (length(forms) == 0) {
        mc <- list(FUN)
      } else {
        if (is.primitive(FUN)) {
          # Must test for "non-normal non-positional matching", like round and round.POSIXt, ... see ?match.call
          #  can't tell a priori that a primitive doesn't have methods, so must test first.
          #  These will always be in base, so can just get0 directly, which is WAY faster than any other way
          nonPrimMeth <- NULL
          if (!is.null(fnName)) {
            cls <- is(args[[1]])
            # use for loop, so can break out if a method is found quickly
            for (classHere in cls) {
              nonPrimMeth <- get0(paste0(fnName, ".", classHere))
              if (!is.null(nonPrimMeth)) break
            }
          }
          if (length(nonPrimMeth)) {
            args2 <- formals(nonPrimMeth)
          } else {
            args2 <- forms
          }
          args2[seq(args)] <- args
          args2 <- args2[seq_along(args)] # chop off any trailing args
          mc <- append(list(FUN), args2)
        } else {
          mc <- match.call(FUN, FUNcaptured)
        }
      }
    } else {
      mc <- FUNcaptured
    }
  } else {
    mc <- list(FUNcaptured)
  }
  mc
}


#' @importFrom methods .S4methods
#' @importFrom utils getFromNamespace
getMethodAll <- function(FUNcaptured, callingEnv) {
  FUN <- FUNcaptured[[1]]
  if (!is.function(FUN))
    FUN <- tryCatch(eval(FUN, envir = callingEnv),
                    error = function(FU) eval(parse(text = FUN), envir = callingEnv))
  if (isS4(FUN)) {
    functionName <- FUN@generic
    # Not easy to selectMethod -- can't have trailing "ANY" -- see ?selectMethod last
    #  paragraph of "Using findMethod()" which says:
    # "Notice also that the length of the signature must be what the corresponding
    #  package used. If thisPkg had only methods for one argument, only length-1
    # signatures will match (no trailing "ANY"), even if another currently loaded
    # package had signatures with more arguments.
    numArgsInSig <- try(
      {
        suppressWarnings({
          info <- attr(methods::.S4methods(functionName), "info") # from hadley/sloop package s3_method_generic
          # info <- attr(utils::methods(functionName), "info")# from hadley/sloop package s3_method_generic
        })
        max(unlist(lapply(strsplit(rownames(info), split = ","), length)) - 1)
      },
      silent = TRUE
    )
    matchOn <- FUN@signature[seq(numArgsInSig)]

    argsClassesList <- lapply(FUNcaptured, function(x) class(x))
    # argsClasses <- unlist(argsClassesList)#[1]))
    argsClasses <- unlist(unname(argsClassesList[names(argsClassesList) %in% matchOn]))
    missingArgs <- matchOn[!(matchOn %in% names(argsClassesList))]

    missings <- rep("missing", length(missingArgs))
    names(missings) <- missingArgs
    argsClasses <- c(argsClasses, missings)

    argClassesAreCall <- argsClasses %in% "call" # maybe wasn't evaluated enough to know what it is; force eval
    if (any(argClassesAreCall)) {
      argsClasses <- "ANY"
      #whAreCall <- names(argsClasses[argClassesAreCall])
      #argsClasses <- Map(wac = whAreCall, function(wac) is(eval(FUNcaptured[[wac]], envir = callingEnv)))
    } else {
      FUN <- selectMethod(functionName, signature = argsClasses)
    }
    updatedFUN <- TRUE
  } else {
    isS3 <- isS3stdGeneric(FUN)
    if (!is.null(names(isS3))) {
      fnNameInitAlt <- names(isS3)
    }
    if (isS3) {
      updatedFUN <- TRUE
      classes <- is(FUNcaptured[[2]])
      for (cla in classes) {
        FUNposs <- utils::getS3method(fnNameInitAlt, cla, optional = TRUE) # S3 matches on 1st arg: FUNcaptured[[2]]
        if (!is.null(FUNposs)) {
          break
        }
      }

      # if generic fn was not exported, then getS3method won't find it above; try directly in NS
      if (is.null(FUNposs)) {
        envNam <- environmentName(environment(FUN))
        FUNpossGen <- get0(fnNameInitAlt, envir = asNamespace(envNam))
        for (cla in classes) {
          possMeth <- paste0(fnNameInitAlt, ".", cla)
          FUNposs <- try(getFromNamespace(possMeth, ns = envNam), silent = TRUE)
          if (!is(FUNposs, "try-error")) {
            break
          } else {
            FUNposs <- NULL
          }
        }
        if (is.null(FUNposs)) {
          FUNposs <- FUNpossGen
        }
      }

      if (is.null(FUNposs)) {
        FUNposs <- get0(fnNameInitAlt, envir = callingEnv)
        if (is.null(FUNposs) || isS4(FUNposs)) { # there are cases e.g., print that are both S4 & S3; this forces S3
          FUNposs <- get0(paste0(fnNameInitAlt, ".default"), envir = callingEnv)
        }
      }
      FUN <- FUNposs
    }
  }
  FUN
}


formals2 <- function(FUNcaptured) {
  modifiedDots <- as.list(FUNcaptured[-1])
  FUN <- FUNcaptured[[1]]
  modifiedDots <- formals3(FUN, modifiedDots, removeNulls = TRUE)
  modifiedDots
}



formals3 <- function(FUN, modifiedDots = list(), removeNulls = FALSE) {
  forms1 <- formals(FUN) # primitives don't have formals
  if (!is.null(forms1)) {
    forms1 <- forms1[setdiff(names(forms1), "...")]
    if (NROW(forms1)) {
      defaults <- setdiff(names(forms1), names(modifiedDots))
      if (removeNulls) {
        theNulls <- unlist(lapply(forms1[defaults], is.null))
        if (any(theNulls)) {
          defaults <- defaults[!theNulls]
        }
      }

      if (NROW(defaults)) { # there may be some arguments that are not specified

        # get the values of args that are eg. coming from options
        forms1[defaults] <- lapply(forms1[defaults], function(xxx) {
          yyy <- "default"
          if (length(xxx) > 0) {
            if (length(xxx) == 1) {
              if (isTRUE(nchar(xxx) == 0)) {
                yyy <- NULL
              }
            }
          }
          if (!is.null(yyy)) {
            # Some are used by other args, yet are undefined in the args ... because "missing"
            # ex is seq.default() # by is (from - to)/(length.out - 1), but length.out is NULL in args
            # so need try
            yyy <- try(eval(xxx, envir = modifiedDots), silent = TRUE)
            if (is(yyy, "try-error")) {
              yyy <- NULL
            }
          }
          yyy
        })
      }

      # Have to get rid of NULL because CacheDigest
      if (removeNulls) {
        forms1 <- forms1[!unlist(lapply(forms1, is.null))]
      }
      modifiedDots <- modifyList(forms1, modifiedDots)
      forms <- names(forms1)
    }
  }
  modifiedDots
}


# This is taken from Rdpack::S4formals
formals4reproducible <- function (fun, ...) {
  if (!is(fun, "MethodDefinition"))
    fun <- getMethod(fun, ...)
  fff <- fun@.Data
  funbody <- body(fff)
  if (length(funbody) == 3 && identical(funbody[[1]], as.name("{")) &&
      length(funbody[[2]]) == 3 && identical(funbody[[c(2,
                                                        1)]], as.name("<-")) && identical(funbody[[c(2, 2)]],
                                                                                          as.name(".local")) && is.function(funbody[[c(2, 3)]])) {
    formals(funbody[[c(2, 3)]])
  }
  else {
    formals(fff)
  }
}


getFunctionName2 <- function(mc) {
  if (length(mc) > 1) {
    if (identical(as.name("<-"), mc[[1]])) {
      mc <- mc[-(1:2)]
    }
    coloncolon <- .grepSysCalls(list(mc), "^\\$|\\[|\\:\\:")
    coloncoloncolon <- .grepSysCalls(list(mc), "^\\$|\\[|\\:\\:\\:")
    if (length(coloncolon)) { # stats::runif -- has to be first one, not some argument in middle
      if (length(coloncolon) && length(mc) != 3 || length(coloncoloncolon)) { # stats::runif

        #if (any(grepl("^\\$|\\[|\\:\\:", mc)[1])) { # stats::runif -- has to be first one, not some argument in middle
        #  if (any(grepl("^\\$|\\[|\\:\\:", mc[[1]])) && length(mc) != 3) { # stats::runif
        fnNameInit <- deparse(mc[[1]])
      } else {
        fnNameInit <- deparse(mc)
      }
    } else {
      fnNameInit <- deparse(as.list(mc[[1]])[[1]]) # fun() and fun could both be here in first slot
    }
  } else {
    fnNameInit <- deparse(mc)
  }
  fnNameInit
}


#' @importFrom utils modifyList isS3stdGeneric methods
.fnCleanup <- function(FUN, ..., callingFun, FUNcaptured = NULL, CacheMatchedCall,
                       .functionName = NULL, callingEnv = parent.frame(2), .fnCleanup,
                       omitArgs = "") {
  if (is.null(FUNcaptured)) {
    FUNcaptured <- substitute(FUN)
  }

  FUNcapturedOrig <- FUNcaptured

  whCharName <- is.function(FUNcaptured) # this is bad; it means that it was not captured. Happens when user
  #  erroneously does do.call(Cache, args)
  if (all(whCharName %in% TRUE)) {
    stop(
      "It looks like Cache is called incorrectly, possibly something like do.call(Cache, args); \n",
      "Cache should be the outermost function. See examples for correct ways to use Cache"
    )
  }
  # Remove `quote`
  isQuoted <- any(grepl("^quote", FUNcaptured)[1]) # won't work for complicated quote
  if (isQuoted) {
    FUNcaptured <- FUNcaptured[[2]]
  }

  dotsCaptured <- substitute(list(...))
  dotsCaptured <- as.list(dotsCaptured[-1]) # need to remove the `list` on the inside of the substitute

  # Backward compatibility; has no effect now
  userTagsOtherFunctions <- NULL

  if (isDollarSqBrPkgColon(FUNcaptured)) {
    if (isPkgColonFn(FUNcaptured)) {
      FUNcaptured <- eval(FUNcaptured, envir = callingEnv)
    } else if (isPkgColon(FUNcaptured)) { # this is TRUE ONLY if it is *just* b$fun or stats::runif, i.e., not b$fun(1)
      FUNcaptured[[1]] <- eval(FUNcaptured[[1]], envir = callingEnv)
    } else if (isDollarOnlySqBr(FUNcaptured)) {
      FUNcaptured <- eval(FUNcaptured, envir = callingEnv)
    }
  }

  if (length(FUNcaptured) > 1) { # this will cover the cases where previous misses, e.g.,
    if (isDollarSqBrPkgColon(FUNcaptured[[1]])) { # this is TRUE ONLY if it is *just* b$fun(1), stats::runif(1)
      FUNcaptured[[1]] <- eval(FUNcaptured[[1]], envir = callingEnv)
    }
  }

  if (!is.call(FUNcaptured)) { # isDollarSqBrPkgColon(FUNcaptured)) { # turn the rnorm, 1, 2 into rnorm(1, 2)
    FUNcaptured <- as.call(append(list(FUNcaptured), dotsCaptured))
  }

  whCharName <- unlist(lapply(FUNcaptured, function(x) is.call(x) || is.name(x) || is.function(x) || is.character(x)))
  isDoCall <- if (any(whCharName)) {
    isTRUE(grepl("^do\\.call", FUNcaptured[whCharName])[[1]]) ||
      identical(do.call, FUNcaptured[[1]])
  } else {
    FALSE
  }
  needRmList <- FALSE
  fnNameInit <- NULL
  if (isDoCall) {
    mc <- match.call(do.call, FUNcaptured)
    fnNameInit <- deparse(mc$what)
    if (length(mc$args) > 1) {
      argsForWhat <- mc$args[-1]
    } else {
      needRmList <- TRUE
      argsForWhat <- mc$args # mc$args will be a list; needs to be evaluated to be unlisted; do below
    }
    FUNcaptured <- try(as.call(append(list(mc$what), as.list(argsForWhat))))
  }

  isSquiggly <- FALSE
  if (!is.function(FUNcaptured[[1]])) { # e.g., just the name, such as rnorm --> convert to the actual function code
    if (is(FUNcaptured[[1]], "(")) {
      fnNameInit <- "headless"
    }
    FUNcaptured[[1]] <- eval(FUNcaptured[[1]], envir = callingEnv)
  }

  if (length(FUNcaptured) > 1) isSquiggly <- identical(`{`, FUNcaptured[[1]])

  if (isSquiggly) {
    # Get rid of squiggly
    FUNcaptured <- as.list(FUNcaptured[-1]) # [[1]] ... if it has many calls... pipe will be just one; but others will be more
    if (length(FUNcaptured) > 1) {
      stop("Cache can only handle curly braces if all internal code uses base pipe |>; see examples")
    }
    FUNcaptured <- FUNcaptured[[1]]
    FUNcapturedNamesEvaled <- recursiveEvalNamesOnly(FUNcaptured, envir = callingEnv) # deals with e.g., stats::rnorm, b$fun, b[[fun]]
    mc1 <- matchCall(FUNcaptured, envir = callingEnv, fnName = fnNameInit)
    if (is.null(fnNameInit)) {
      fnNameInit <- getFunctionName2(mc1[[1]])
    }
    FUNcapturedNamesEvaled <- matchCall(FUNcapturedNamesEvaled, envir = callingEnv, fnName = fnNameInit)
  } else {
    if (is.null(fnNameInit)) {
      fnNameInit <- getFunctionName2(FUNcapturedOrig)
    }
    if (length(FUNcaptured) > 1) {
      # The next line works for any object that is NOT in a ..., because the
      #   object never shows up in the environment; it is passed through
      # mced <- names(CacheMatchedCall)

      # if (!is.null(unlist(argsToKeep))) {
      FUNcapturedList <- as.list(FUNcaptured[-1])
      nams <- names(FUNcapturedList)
      if (is.null(nams))
        nams <- sapply(seq_along(FUNcapturedList), function(x) paste0(sample(LETTERS, 14), collapse = ""))
      FUNcapturedArgs <- Map(
        ee = FUNcapturedList, nam = nams, function(ee, nam) {

            out <- try(eval(ee, envir = callingEnv), silent = TRUE)
            if (is(out, "try-error")) {
              if (identical(as.name("..."), ee)) {
                out <- "..."
              } else {
                env2 <- try(if (isDollarSqBrPkgColon(ee)) {
                  .whereInStack(ee[[2]])
                } else {
                  .whereInStack(ee)
                }, silent = TRUE)
                if (is(env2, "try-error")) {
                  out <- try(paste(format(ee$destinationPath), collapse = " "), silent = TRUE)
                  if (is(out, "try-error"))
                    stop(env2)
                } else {
                  out <- try(eval(ee, envir = env2), silent = TRUE)
                  if (is(out, "try-error")) {
                    out <- as.character(parse(text = ee))
                  }
                }
              }
            }
          # }

          out
        }) # may be slow as it is evaluating the args
      if (needRmList) { # it has one too many list elements # not sure about the length(out) == 1
        FUNcapturedArgs <- FUNcapturedArgs[[1]]
      }
      # }

      FUNcapturedNamesEvaled <- as.call(append(list(FUNcaptured[[1]]), FUNcapturedArgs))
      FUNcapturedNamesEvaled <- matchCall(FUNcapturedNamesEvaled, callingEnv, fnName = fnNameInit)
                             } else { # this is a function called with no arguments
                               FUNcapturedNamesEvaled <- FUNcaptured
    }
  }



  # Now FUNcaptured will always have at least 1 element, because it is a call


  FUN <- FUNcapturedNamesEvaled[[1]] # This will be wrong if a fn has no args
  if (is.call(FUN)) { # This will only happen if there are no args to FUN e.g., fun()... anything else is a name fun(1)
    FUN <- FUN[[1]]
    FUNcapturedNamesEvaled[[1]] <- FUN
  }

  fnDetails <- list(
    functionName = fnNameInit,
    .FUN = FUN,
    nestLevel = 1
  )

  modifiedDots <- as.list(FUNcapturedNamesEvaled[-1]) # this is prior to filling in with defaults
  if (is.function(FUN)) {
    FUN <- getMethodAll(FUNcapturedNamesEvaled, callingEnv)
    forms <- if (is.primitive(FUN)) formals(args(FUN)) else formals(FUN)
    FUNcapturedNamesEvaled[[1]] <- FUN # may be same if it was a primitive or just a function
    fnDetails$.FUN <- FUN

    if (!is.primitive(FUN) && (length(forms) > 0)) {
      modifiedDots <- formals2(FUNcapturedNamesEvaled) # this gets default values for methods;
    }
  } else {
    # This comes from `CacheDigest(something$something)`
    FUNcapturedNamesEvaled <- append(list(NULL), FUNcaptured) # the first arg is supposed to be a function below; put NULL as placeholder
    forms <- names(FUNcapturedNamesEvaled[-1])
  }

  FUNcapturedNamesEvaled <- checkOverlappingArgs(CacheMatchedCall, forms, dotsCaptured,
                                                 functionName = fnDetails$functionName, FUNcapturedNamesEvaled)

  # # Check for args that are passed to both Cache and the FUN -- if any overlap; pass to both
  # possibleOverlap <- names(formals(args(Cache)))
  # possibleOverlap <- intersect(names(CacheMatchedCall), possibleOverlap)
  # actualOverlap <- intersect(names(forms), possibleOverlap)
  # if (length(actualOverlap) && !identical(list(), dotsCaptured)) { # e.g., useCache, verbose; but if not in dots, then OK because were separate already
  #   message(
  #     "The following arguments are arguments for both Cache and ", fnDetails$functionName, ":\n",
  #     paste0(actualOverlap, collapse = ", "),
  #     "\n...passing to both. If more control is needed, pass as a call, e.g., ",
  #     "Cache(", fnDetails$functionName, "(...))"
  #   )
  #   overlappingArgsAsList <- as.list(CacheMatchedCall)[actualOverlap]
  #   FUNcapturedNamesEvaled <- as.call(append(as.list(FUNcapturedNamesEvaled), overlappingArgsAsList))
  # }

  if (!is.null(.functionName)) {
    fnDetails$functionName <- .functionName
  }

  return(append(fnDetails, list(
    FUN = FUN, matchedCall = FUNcapturedNamesEvaled,
    modifiedDots = modifiedDots, # isDoCall = isDoCall,
    formalArgs = forms,
    userTags = userTagsOtherFunctions
  )))
}


isPkgColonFn <- function(x) {
  identical(x[[1]], quote(`::`))
}


evalTheFun <- function(FUNcaptured, isCapturedFUN, matchedCall, envir = parent.frame(),
                       verbose = getOption("reproducible.verbose"), ...) {
  .message$IndentUpdate()
  withCallingHandlers(
    {
      out <- eval(FUNcaptured, envir = envir)
      if (is.function(out)) { # if is wasn't "captured", then it is just a function, so now use it on the ...
        out <- out(...)
      }
    },
    warning = function(w) {
      asch <- format(w$call[[1]])
      warning("In ", format(matchedCall), ": ", w$message, call. = FALSE)
      invokeRestart("muffleWarning")
      #    }
    }
  )

  out
}
