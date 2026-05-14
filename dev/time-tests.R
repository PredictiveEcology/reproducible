## Time each test_that block in a single test file using a testthat Reporter.
## Usage:
##   source("dev/time-tests.R")
##   time_tests("tests/testthat/test-prepInputs.R")
##   time_tests("tests/testthat/test-prepInputs.R", trace_downloads = TRUE)
##   time_tests("tests/testthat/test-prepInputs.R", force_interactive = TRUE)

time_tests <- function(path, trace_downloads = FALSE, force_interactive = FALSE,
                       filter = NULL) {
  stopifnot(file.exists(path))
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)

  pkgload::load_all(quiet = TRUE)
  suppressMessages(library(testthat))

  if (isTRUE(force_interactive)) {
    fn <- function() TRUE
    ## isInteractive is internal; mock it in the namespace, the attached env
    ## (if testthat exposes it there), and globalenv as a search-path shadow.
    ns <- asNamespace("reproducible")
    try(unlockBinding("isInteractive", ns), silent = TRUE)
    assign("isInteractive", fn, envir = ns)
    if ("package:reproducible" %in% search()) {
      e <- as.environment("package:reproducible")
      if (exists("isInteractive", envir = e, inherits = FALSE)) {
        try(unlockBinding("isInteractive", e), silent = TRUE)
        assign("isInteractive", fn, envir = e)
      }
    }
    assign("isInteractive", fn, envir = globalenv())
    cat("[debug] reproducible:::isInteractive() ->",
        getFromNamespace("isInteractive", "reproducible")(), "\n")
  }

  ## Patch runTest helper (sourced into globalenv from helper-allEqual.R) so a
  ## length(files) != numFiles failure prints the actual directory contents.
  ## Hook this in just before test_file by trapping the post-source state.
  ## No longer needed: helper-allEqual.R::runTest now dumps file lists via
  ## message() on length mismatch, which survives capture.output().
  .patch_runTest <- function() invisible(FALSE)

  if (isTRUE(trace_downloads)) .trace_downloads_on()
  on.exit(if (isTRUE(trace_downloads)) .trace_downloads_off(), add = TRUE)

  rep <- TimingReporter$new(max_fail = Inf)
  cat("[debug] reporter$max_fail =", rep$max_fail, "\n")
  cat(sprintf("\n== timing %s ==\n", path)); flush.console()
  ## source helper files explicitly so runTest exists in globalenv before patching
  for (h in list.files(dirname(path), pattern = "^helper-.*\\.R$", full.names = TRUE))
    sys.source(h, envir = globalenv())
  .patch_runTest()
  cat("[debug] entering test_file\n"); flush.console()
  err <- tryCatch(
    testthat::test_file(path, reporter = rep, stop_on_failure = FALSE,
                        stop_on_warning = FALSE,
                        desc = filter),
    error = function(e) {
      cat(sprintf("[FATAL test_file] %s\n", conditionMessage(e)))
      flush.console()
      e
    }
  )
  cat("[debug] left test_file\n"); flush.console()

  df <- rep$timings_df()
  if (NROW(df)) {
    df <- df[order(-df$secs), ]
    cat("\n== slowest test_that blocks ==\n")
    for (i in seq_len(nrow(df))) {
      tag <- ""
      if (df$skipped[i])  tag <- "  [SKIP]"
      if (df$error[i])    tag <- paste0(tag, "  [ERROR]")
      if (df$failures[i]) tag <- paste0(tag, sprintf("  [%dF]", df$failures[i]))
      cat(sprintf("%7.2fs  %s%s\n", df$secs[i], df$desc[i], tag))
    }
  }
  invisible(df)
}

TimingReporter <- R6::R6Class(
  "TimingReporter",
  inherit = testthat::ProgressReporter,
  public = list(
    timings = list(),
    .t0 = NULL,
    .cur_failures = 0L,
    .cur_skip = FALSE,
    .cur_error = FALSE,

    initialize = function(...) {
      super$initialize(...)
    },

    start_test = function(context, test) {
      cat(sprintf("\n>>> %s\n", test)); flush.console()
      self$.t0 <- Sys.time()
      self$.cur_failures <- 0L
      self$.cur_skip <- FALSE
      self$.cur_error <- FALSE
      super$start_test(context, test)
    },

    add_result = function(context, test, result) {
      if (inherits(result, "expectation_failure")) self$.cur_failures <- self$.cur_failures + 1L
      if (inherits(result, "expectation_skip"))    self$.cur_skip <- TRUE
      if (inherits(result, "expectation_error"))   self$.cur_error <- TRUE
      super$add_result(context, test, result)
    },

    end_test = function(context, test) {
      dt <- as.numeric(Sys.time() - self$.t0, units = "secs")
      tag <- ""
      if (self$.cur_skip)  tag <- "  [SKIP]"
      if (self$.cur_error) tag <- paste0(tag, "  [ERROR]")
      if (self$.cur_failures) tag <- paste0(tag, sprintf("  [%dF]", self$.cur_failures))
      cat(sprintf("<<< %7.2fs  %s%s\n", dt, test, tag)); flush.console()
      self$timings[[length(self$timings) + 1L]] <- list(
        desc = test, secs = dt, failures = self$.cur_failures,
        skipped = self$.cur_skip, error = self$.cur_error
      )
      super$end_test(context, test)
    },

    timings_df = function() {
      if (!length(self$timings)) return(NULL)
      do.call(rbind, lapply(self$timings, as.data.frame, stringsAsFactors = FALSE))
    }
  )
)

## Trace downloadFile + key network helpers to log start/elapsed per call.
.trace_downloads_on <- function() {
  fns <- c("downloadFile", "preProcess", "extractFromArchive",
           "getRemoteMetadata", ".driveDownloadFile")
  ns <- asNamespace("reproducible")
  for (fn in fns) {
    if (exists(fn, envir = ns, inherits = FALSE)) {
      tracer <- bquote({
        .t0_dl <- Sys.time()
        cat(sprintf("  [%s] ENTER %s\n", format(.t0_dl, "%H:%M:%S"), .(fn)))
        flush.console()
      })
      exiter <- bquote({
        cat(sprintf("  [%s] EXIT  %s   (%.2fs)\n",
                    format(Sys.time(), "%H:%M:%S"), .(fn),
                    as.numeric(Sys.time() - .t0_dl, units = "secs")))
        flush.console()
      })
      try(suppressMessages(trace(fn, where = ns, tracer = tracer,
                                 exit = exiter, print = FALSE)), silent = TRUE)
    }
  }
}

.trace_downloads_off <- function() {
  fns <- c("downloadFile", "preProcess", "extractFromArchive",
           "getRemoteMetadata", ".driveDownloadFile")
  ns <- asNamespace("reproducible")
  for (fn in fns) {
    try(suppressMessages(untrace(fn, where = ns)), silent = TRUE)
  }
}
