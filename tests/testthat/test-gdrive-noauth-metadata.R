# assessGoogle() resolves a PUBLIC Google Drive file's metadata without launching
# interactive OAuth. The decision + deauthorize helpers are tested offline by
# mocking the token check and googledrive's auth functions.

# --- .gdriveAuthCandidates: which identities to try, in order, only if present ---

test_that(".gdriveAuthCandidates: email first, then service account; absent ones skipped", {
  skip_if_not_installed("googledrive")
  sa <- withr::local_tempfile(fileext = ".json"); writeLines("{}", sa)

  withr::with_options(list(gargle_oauth_email = "a@b.com"), {
    withr::with_envvar(list(GOOGLEDRIVE_AUTH = sa, GARGLE_SERVICE_ACCOUNT = ""), {
      cands <- reproducible:::.gdriveAuthCandidates()
      expect_identical(vapply(cands, `[[`, "", "kind"),
                       c("email", "service_account"))      # email BEFORE service account
      expect_identical(cands[[1]]$email, "a@b.com")
      expect_identical(cands[[2]]$envvar, "GOOGLEDRIVE_AUTH")
    })
  })

  # no email option -> email rung skipped entirely
  withr::with_options(list(gargle_oauth_email = NULL), {
    withr::with_envvar(list(GOOGLEDRIVE_AUTH = sa, GARGLE_SERVICE_ACCOUNT = ""), {
      cands <- reproducible:::.gdriveAuthCandidates()
      expect_identical(vapply(cands, `[[`, "", "kind"), "service_account")
    })
    # no email AND no service account -> nothing to try
    withr::with_envvar(list(GOOGLEDRIVE_AUTH = "", GARGLE_SERVICE_ACCOUNT = ""), {
      expect_length(reproducible:::.gdriveAuthCandidates(), 0L)
    })
    # GARGLE_SERVICE_ACCOUNT is honoured when GOOGLEDRIVE_AUTH is unset
    withr::with_envvar(list(GOOGLEDRIVE_AUTH = "", GARGLE_SERVICE_ACCOUNT = sa), {
      cands <- reproducible:::.gdriveAuthCandidates()
      expect_identical(cands[[1]]$envvar, "GARGLE_SERVICE_ACCOUNT")
    })
  })
})

# --- .gdriveProbe: only accepts an identity that can actually read the file -----

test_that(".gdriveProbe is TRUE iff the metadata read succeeds (silently)", {
  skip_if_not_installed("googledrive")
  testthat::local_mocked_bindings(as_id = function(x) x, .package = "googledrive")

  testthat::local_mocked_bindings(drive_get = function(...) invisible(TRUE),
                                  .package = "googledrive")
  expect_true(reproducible:::.gdriveProbe("someUrl"))

  testthat::local_mocked_bindings(drive_get = function(...) stop("404 File not found"),
                                  .package = "googledrive")
  expect_false(reproducible:::.gdriveProbe("someUrl"))   # error -> FALSE, never propagates
})

# --- .gdrivePrepareAuth cascade -------------------------------------------------

test_that(".gdrivePrepareAuth: a loaded token wins outright -> 'token' (no attempts, no deauth)", {
  skip_if_not_installed("googledrive")
  withr::local_options(reproducible.gdriveNoAuth = NULL)
  deauthed <- FALSE; authed <- FALSE
  testthat::local_mocked_bindings(.gdriveHasToken = function() TRUE)
  testthat::local_mocked_bindings(
    drive_auth = function(...) authed <<- TRUE,
    drive_deauth = function(...) deauthed <<- TRUE,
    .package = "googledrive")
  expect_identical(suppressMessages(reproducible:::.gdrivePrepareAuth("u")), "token")
  expect_false(authed)       # already authenticated; no trials
  expect_false(deauthed)     # token left intact
})

test_that(".gdrivePrepareAuth: gdriveNoAuth=TRUE -> 'anon', deauthorize, no trials", {
  skip_if_not_installed("googledrive")
  withr::local_options(reproducible.gdriveNoAuth = TRUE, gargle_oauth_email = "a@b.com")
  deauthed <- FALSE; authed <- FALSE
  testthat::local_mocked_bindings(.gdriveHasToken = function() FALSE)
  testthat::local_mocked_bindings(
    drive_auth = function(...) authed <<- TRUE,
    drive_deauth = function(...) deauthed <<- TRUE,
    .package = "googledrive")
  expect_identical(suppressMessages(reproducible:::.gdrivePrepareAuth("u")), "anon")
  expect_false(authed)       # forced anon never attempts auth
  expect_true(deauthed)
})

test_that(".gdrivePrepareAuth: configured email that can read the file -> 'token', no deauth", {
  skip_if_not_installed("googledrive")
  withr::local_options(reproducible.gdriveNoAuth = NULL, gargle_oauth_email = "a@b.com")
  withr::local_envvar(GOOGLEDRIVE_AUTH = "", GARGLE_SERVICE_ACCOUNT = "")
  events <- character()
  testthat::local_mocked_bindings(.gdriveHasToken = function() FALSE)
  testthat::local_mocked_bindings(
    as_id = function(x) x,
    drive_auth = function(...) { events <<- c(events, "auth:email"); invisible() },
    drive_get = function(...) { events <<- c(events, "probe"); invisible(TRUE) },
    drive_deauth = function(...) { events <<- c(events, "deauth") },
    .package = "googledrive")
  expect_identical(suppressMessages(reproducible:::.gdrivePrepareAuth("u")), "token")
  expect_identical(events, c("auth:email", "probe"))   # authed as email, probe OK, NO deauth
})

test_that(".gdrivePrepareAuth: email cannot read -> falls back to service account that can", {
  skip_if_not_installed("googledrive")
  sa <- withr::local_tempfile(fileext = ".json"); writeLines("{}", sa)
  withr::local_options(reproducible.gdriveNoAuth = NULL, gargle_oauth_email = "a@b.com")
  withr::local_envvar(GOOGLEDRIVE_AUTH = sa, GARGLE_SERVICE_ACCOUNT = "")
  events <- character(); cur <- NULL
  testthat::local_mocked_bindings(.gdriveHasToken = function() FALSE)
  testthat::local_mocked_bindings(
    as_id = function(x) x,
    drive_auth = function(...) {
      a <- list(...); cur <<- if (!is.null(a$email)) "email" else "sa"
      events <<- c(events, paste0("auth:", cur)); invisible()
    },
    drive_get = function(...) {                # email identity is denied; SA can read
      events <<- c(events, paste0("probe:", cur))
      if (identical(cur, "email")) stop("404 File not found") else invisible(TRUE)
    },
    drive_deauth = function(...) events <<- c(events, "deauth"),
    .package = "googledrive")
  expect_identical(suppressMessages(reproducible:::.gdrivePrepareAuth("u")), "token")
  # email tried + probed, its poisoning token cleared, THEN service account wins
  expect_identical(events,
                   c("auth:email", "probe:email", "deauth", "auth:sa", "probe:sa"))
})

test_that(".gdrivePrepareAuth: no email option -> service-account rung tried directly", {
  skip_if_not_installed("googledrive")
  sa <- withr::local_tempfile(fileext = ".json"); writeLines("{}", sa)
  withr::local_options(reproducible.gdriveNoAuth = NULL, gargle_oauth_email = NULL)
  withr::local_envvar(GOOGLEDRIVE_AUTH = sa, GARGLE_SERVICE_ACCOUNT = "")
  authArgs <- list()
  testthat::local_mocked_bindings(.gdriveHasToken = function() FALSE)
  testthat::local_mocked_bindings(
    as_id = function(x) x,
    drive_auth = function(...) { authArgs[[length(authArgs) + 1L]] <<- list(...); invisible() },
    drive_get = function(...) invisible(TRUE),
    drive_deauth = function(...) NULL,
    .package = "googledrive")
  expect_identical(suppressMessages(reproducible:::.gdrivePrepareAuth("u")), "token")
  expect_length(authArgs, 1L)                       # email rung skipped (no option)
  expect_null(authArgs[[1]]$email)                  # ...went straight to the JSON path
  expect_identical(normalizePath(authArgs[[1]]$path, mustWork = FALSE),
                   normalizePath(sa, mustWork = FALSE))
})

test_that(".gdrivePrepareAuth: nothing configured -> 'anon', deauthorize, no auth attempt", {
  skip_if_not_installed("googledrive")
  withr::local_options(reproducible.gdriveNoAuth = NULL, gargle_oauth_email = NULL)
  withr::local_envvar(GOOGLEDRIVE_AUTH = "", GARGLE_SERVICE_ACCOUNT = "")
  authed <- FALSE; deauthed <- FALSE
  testthat::local_mocked_bindings(.gdriveHasToken = function() FALSE)
  testthat::local_mocked_bindings(
    as_id = function(x) x,
    drive_auth = function(...) authed <<- TRUE,
    drive_get = function(...) invisible(TRUE),
    drive_deauth = function(...) deauthed <<- TRUE,
    .package = "googledrive")
  expect_identical(suppressMessages(reproducible:::.gdrivePrepareAuth("u")), "anon")
  expect_false(authed)       # no configured identity -> straight to anonymous
  expect_true(deauthed)
})

test_that(".gdrivePrepareAuth: configured email that cannot read + no SA -> 'anon'", {
  skip_if_not_installed("googledrive")
  withr::local_options(reproducible.gdriveNoAuth = NULL, gargle_oauth_email = "a@b.com")
  withr::local_envvar(GOOGLEDRIVE_AUTH = "", GARGLE_SERVICE_ACCOUNT = "")
  testthat::local_mocked_bindings(.gdriveHasToken = function() FALSE)
  testthat::local_mocked_bindings(
    as_id = function(x) x,
    drive_auth = function(...) invisible(),
    drive_get = function(...) stop("404 File not found"),   # email denied
    drive_deauth = function(...) NULL,
    .package = "googledrive")
  # exhausted configured identities -> public read (caller's drive_get raises if private)
  expect_identical(suppressMessages(reproducible:::.gdrivePrepareAuth("u")), "anon")
})

test_that(".gdrivePrepareAuth: trials are non-interactive and announce each rung", {
  skip_if_not_installed("googledrive")
  withr::local_options(reproducible.gdriveNoAuth = NULL,
                       gargle_oauth_email = "a@b.com", rlang_interactive = TRUE,
                       reproducible.verbose = 1)
  withr::local_envvar(GOOGLEDRIVE_AUTH = "", GARGLE_SERVICE_ACCOUNT = "")
  seenInteractive <- NA
  testthat::local_mocked_bindings(.gdriveHasToken = function() FALSE)
  testthat::local_mocked_bindings(
    as_id = function(x) x,
    drive_auth = function(...) { seenInteractive <<- getOption("rlang_interactive"); invisible() },
    drive_get = function(...) invisible(TRUE),
    drive_deauth = function(...) NULL,
    .package = "googledrive")
  expect_message(reproducible:::.gdrivePrepareAuth("u"), "gargle_oauth_email")
  expect_false(isTRUE(seenInteractive))    # forced non-interactive: no browser prompt mid-cascade
})

# --- error messages carry the full, pasteable URL (not just the bare fileId) ---

test_that(".gdriveBrowserUrl returns a pasteable URL for URLs and bare IDs", {
  fileUrl <- "https://drive.google.com/file/d/13-atqi_7ogRPIFxOoJZoUDYdQCJ5-a_u/view?usp=share_link"
  expect_identical(reproducible:::.gdriveBrowserUrl(fileUrl), fileUrl)         # full URL echoed
  folderUrl <- "https://drive.google.com/drive/folders/199oEp-TVaCyacwqS4PPf3XWMbhPe4YBN"
  expect_identical(reproducible:::.gdriveBrowserUrl(folderUrl), folderUrl)
  # a bare 33-char Drive ID -> the file viewer URL
  id <- "13-atqi_7ogRPIFxOoJZoUDYdQCJ5-a_u"
  expect_identical(reproducible:::.gdriveBrowserUrl(id),
                   "https://drive.google.com/file/d/13-atqi_7ogRPIFxOoJZoUDYdQCJ5-a_u")
  expect_true(is.na(reproducible:::.gdriveBrowserUrl(NA_character_)))
  expect_true(is.na(reproducible:::.gdriveBrowserUrl("")))
})

test_that(".stopGoogleDriveAccess surfaces the full URL and the original error", {
  id <- "13-atqi_7ogRPIFxOoJZoUDYdQCJ5-a_u"
  err <- simpleError("Client error: (404) Not Found\nFile not found: 13-atqi_7ogRPIFxOoJZoUDYdQCJ5-a_u.")
  # a bare ID becomes a pasteable viewer URL in the message
  expect_error(reproducible:::.stopGoogleDriveAccess(id, err),
               "drive\\.google\\.com/file/d/13-atqi")
  # ...and the original googledrive detail is preserved
  expect_error(reproducible:::.stopGoogleDriveAccess(id, err), "File not found")
})

test_that(".gdrivePrepareAuth: a transient probe failure is retried, not demoted to anonymous", {
  skip_if_not_installed("googledrive")
  withr::local_options(reproducible.gdriveNoAuth = NULL, gargle_oauth_email = "a@b.com",
                       reproducible.gdriveAuthRetries = 1L)
  withr::local_envvar(GOOGLEDRIVE_AUTH = "", GARGLE_SERVICE_ACCOUNT = "")
  events <- character(); n <- 0L
  testthat::local_mocked_bindings(.gdriveHasToken = function() FALSE)
  testthat::local_mocked_bindings(
    as_id = function(x) x,
    drive_auth = function(...) { events <<- c(events, "auth"); invisible() },
    drive_get = function(...) {                 # first probe: refresh/network blip; second: fine
      n <<- n + 1L; events <<- c(events, paste0("probe", n))
      if (n == 1L) stop("Failed to connect to oauth2.googleapis.com port 443: Timeout was reached")
      invisible(TRUE)
    },
    drive_deauth = function(...) events <<- c(events, "deauth"),
    .package = "googledrive")
  expect_identical(suppressMessages(reproducible:::.gdrivePrepareAuth("u")), "token")
  expect_identical(events, c("auth", "probe1", "auth", "probe2"))   # retried; never deauthorized
})

test_that(".gdrivePrepareAuth: a 403/404 denial is final -- no retry -- and is reported", {
  skip_if_not_installed("googledrive")
  withr::local_options(reproducible.gdriveNoAuth = NULL, gargle_oauth_email = "a@b.com",
                       reproducible.gdriveAuthRetries = 2L)
  withr::local_envvar(GOOGLEDRIVE_AUTH = "", GARGLE_SERVICE_ACCOUNT = "")
  probes <- 0L
  testthat::local_mocked_bindings(.gdriveHasToken = function() FALSE)
  testthat::local_mocked_bindings(
    as_id = function(x) x,
    drive_auth = function(...) invisible(),
    drive_get = function(...) { probes <<- probes + 1L; stop("Client error: (404) Not Found. File not found: u") },
    drive_deauth = function(...) invisible(),
    .package = "googledrive")
  expect_identical(suppressMessages(reproducible:::.gdrivePrepareAuth("u")), "anon")
  expect_identical(probes, 1L)
  err <- tryCatch(reproducible:::.stopGoogleDriveAccess("u", simpleError("File not found: u")),
                  error = function(e) conditionMessage(e))
  expect_match(err, "Authenticated access was tried first and failed", fixed = TRUE)
  expect_match(err, "gargle_oauth_email = 'a@b.com': Client error: (404)", fixed = TRUE)
})

test_that(".gdriveIsDenial separates 403/404 from transient failures", {
  f <- reproducible:::.gdriveIsDenial
  expect_true(f(simpleError("Client error: (404) Not Found")))
  expect_true(f(simpleError("404 File not found")))
  expect_true(f(simpleError("Client error: (403) Forbidden: insufficientPermissions")))
  expect_false(f(simpleError("Failed to connect to oauth2.googleapis.com port 443: Timeout was reached")))
  expect_false(f(simpleError("Server error: (503) Service Unavailable")))
})

test_that(".gdrivePrepareAuth: a transient exhaustion never deauthorizes the session", {
  skip_if_not_installed("googledrive")
  withr::local_options(reproducible.gdriveNoAuth = NULL, gargle_oauth_email = "a@b.com",
                       reproducible.gdriveAuthRetries = 0L)
  withr::local_envvar(GOOGLEDRIVE_AUTH = "", GARGLE_SERVICE_ACCOUNT = "")
  events <- character()
  testthat::local_mocked_bindings(.gdriveHasToken = function() FALSE)
  testthat::local_mocked_bindings(
    as_id = function(x) x,
    drive_auth = function(...) { events <<- c(events, "auth"); invisible() },
    drive_get = function(...) stop("Timeout was reached [oauth2.googleapis.com]"),
    drive_deauth = function(...) events <<- c(events, "deauth"),
    .package = "googledrive")
  expect_identical(suppressMessages(reproducible:::.gdrivePrepareAuth("u")), "anon")
  expect_identical(events, "auth")            # no deauth: LandR's later drive_ls() must not go anonymous
  expect_false(isTRUE(reproducible:::.gdriveLastAuth$deauthed))
  expect_false(reproducible:::.gdriveRestoreAuth())   # nothing to restore
})

test_that(".gdrivePrepareAuth: after a denial, .gdriveRestoreAuth() puts the email identity back", {
  skip_if_not_installed("googledrive")
  withr::local_options(reproducible.gdriveNoAuth = NULL, gargle_oauth_email = "a@b.com",
                       reproducible.gdriveAuthRetries = 2L)
  withr::local_envvar(GOOGLEDRIVE_AUTH = "", GARGLE_SERVICE_ACCOUNT = "")
  events <- character()
  testthat::local_mocked_bindings(.gdriveHasToken = function() FALSE)
  testthat::local_mocked_bindings(
    as_id = function(x) x,
    drive_auth = function(...) { events <<- c(events, paste0("auth:", list(...)$email)); invisible() },
    drive_get = function(...) stop("Client error: (404) Not Found. File not found: u"),
    drive_deauth = function(...) events <<- c(events, "deauth"),
    .package = "googledrive")
  expect_identical(suppressMessages(reproducible:::.gdrivePrepareAuth("u")), "anon")
  expect_identical(events, c("auth:a@b.com", "deauth"))   # denial: deauth once, no retry
  expect_true(reproducible:::.gdriveRestoreAuth())
  expect_identical(events, c("auth:a@b.com", "deauth", "auth:a@b.com"))
  expect_false(isTRUE(reproducible:::.gdriveLastAuth$deauthed))
})
