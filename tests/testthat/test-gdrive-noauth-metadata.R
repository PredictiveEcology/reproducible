# assessGoogle() resolves a PUBLIC Google Drive file's metadata without launching
# interactive OAuth. The decision + deauthorize helpers are tested offline by
# mocking the token check and googledrive's auth functions.

test_that(".gdriveTryAuthQuietly authenticates via a service-account JSON when present", {
  skip_if_not_installed("googledrive")
  withr::local_options(gargle_oauth_email = NULL)
  sa <- withr::local_tempfile(fileext = ".json")
  writeLines("{}", sa)                        # a file that exists
  withr::local_envvar(GOOGLEDRIVE_AUTH = sa, GARGLE_SERVICE_ACCOUNT = "")

  authPath <- NULL
  testthat::local_mocked_bindings(
    drive_auth = function(...) { authPath <<- list(...)$path; invisible() },
    drive_has_token = function(...) TRUE,
    .package = "googledrive"
  )
  expect_true(reproducible:::.gdriveTryAuthQuietly())
  # it honoured GOOGLEDRIVE_AUTH by passing path = the service-account JSON
  expect_identical(normalizePath(authPath, mustWork = FALSE),
                   normalizePath(sa, mustWork = FALSE))
})

test_that(".gdriveTryAuthQuietly fails QUIETLY (no prompt) when no token is available", {
  skip_if_not_installed("googledrive")
  withr::local_options(gargle_oauth_email = NULL)
  withr::local_envvar(GOOGLEDRIVE_AUTH = "", GARGLE_SERVICE_ACCOUNT = "")
  # gargle errors (rather than prompting) under forced non-interactive when it has
  # no token; emulate that here.
  testthat::local_mocked_bindings(
    drive_auth = function(...) stop("Can't get Google credentials"),
    drive_has_token = function(...) FALSE,
    .package = "googledrive"
  )
  expect_false(reproducible:::.gdriveTryAuthQuietly())
})

test_that(".gdrivePrepareAuth: a loaded token -> 'token' (no auth attempt, no deauth)", {
  skip_if_not_installed("googledrive")
  withr::local_options(reproducible.gdriveNoAuth = NULL)
  deauthed <- FALSE; tried <- FALSE
  testthat::local_mocked_bindings(
    .gdriveHasToken = function() TRUE,
    .gdriveTryAuthQuietly = function() { tried <<- TRUE; FALSE })
  testthat::local_mocked_bindings(drive_deauth = function(...) deauthed <<- TRUE,
                                  .package = "googledrive")
  expect_identical(reproducible:::.gdrivePrepareAuth(), "token")
  expect_false(tried)        # already authenticated; no need to try
  expect_false(deauthed)     # token left intact
})

test_that(".gdriveTryAuthQuietly: a configured gargle email is NOT forced non-interactive", {
  skip_if_not_installed("googledrive")
  # The user has set gargle_oauth_email but has not yet run drive_auth(). The auth
  # attempt must respect the session's interactivity so a missing token can
  # complete OAuth (or load a cached token) -- NOT be forced to a quiet failure
  # that downgrades a private Drive folder to an anonymous 404.
  withr::local_options(gargle_oauth_email = "someone@example.com", rlang_interactive = TRUE)
  withr::local_envvar(GOOGLEDRIVE_AUTH = "", GARGLE_SERVICE_ACCOUNT = "")
  seenInteractive <- NA
  testthat::local_mocked_bindings(
    drive_auth = function(...) { seenInteractive <<- getOption("rlang_interactive"); invisible() },
    drive_has_token = function(...) TRUE,
    .package = "googledrive")
  expect_true(reproducible:::.gdriveTryAuthQuietly())
  expect_true(isTRUE(seenInteractive))   # interactivity preserved -> OAuth can complete
})

test_that(".gdriveTryAuthQuietly: an unconfigured session IS forced non-interactive", {
  skip_if_not_installed("googledrive")
  # No email, no service account -> the public-file case: never prompt.
  withr::local_options(gargle_oauth_email = NULL, rlang_interactive = TRUE)
  withr::local_envvar(GOOGLEDRIVE_AUTH = "", GARGLE_SERVICE_ACCOUNT = "")
  seenInteractive <- NA
  testthat::local_mocked_bindings(
    drive_auth = function(...) { seenInteractive <<- getOption("rlang_interactive"); invisible() },
    drive_has_token = function(...) TRUE,
    .package = "googledrive")
  expect_true(reproducible:::.gdriveTryAuthQuietly())
  expect_false(isTRUE(seenInteractive))  # forced FALSE -> a public file never prompts
})

test_that(".gdrivePrepareAuth: no token + quiet auth succeeds -> 'token'", {
  skip_if_not_installed("googledrive")
  withr::local_options(reproducible.gdriveNoAuth = NULL)
  deauthed <- FALSE
  testthat::local_mocked_bindings(
    .gdriveHasToken = function() FALSE,
    .gdriveTryAuthQuietly = function() TRUE)
  testthat::local_mocked_bindings(drive_deauth = function(...) deauthed <<- TRUE,
                                  .package = "googledrive")
  expect_identical(reproducible:::.gdrivePrepareAuth(), "token")
  expect_false(deauthed)
})

test_that(".gdrivePrepareAuth: no token + quiet auth fails -> deauthorize, 'anon'", {
  skip_if_not_installed("googledrive")
  withr::local_options(reproducible.gdriveNoAuth = NULL)
  deauthed <- FALSE
  testthat::local_mocked_bindings(
    .gdriveHasToken = function() FALSE,
    .gdriveTryAuthQuietly = function() FALSE)
  testthat::local_mocked_bindings(drive_deauth = function(...) deauthed <<- TRUE,
                                  .package = "googledrive")
  expect_identical(reproducible:::.gdrivePrepareAuth(), "anon")
  expect_true(deauthed)      # fell back to anonymous/public
})

test_that(".gdrivePrepareAuth: gdriveNoAuth=TRUE + no token -> 'anon' without trying auth", {
  skip_if_not_installed("googledrive")
  withr::local_options(reproducible.gdriveNoAuth = TRUE)
  deauthed <- FALSE; tried <- FALSE
  testthat::local_mocked_bindings(
    .gdriveHasToken = function() FALSE,
    .gdriveTryAuthQuietly = function() { tried <<- TRUE; TRUE })
  testthat::local_mocked_bindings(drive_deauth = function(...) deauthed <<- TRUE,
                                  .package = "googledrive")
  expect_identical(reproducible:::.gdrivePrepareAuth(), "anon")
  expect_false(tried)        # forced anon never attempts auth
  expect_true(deauthed)
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
