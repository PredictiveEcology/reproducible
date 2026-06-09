# assessGoogle() resolves a PUBLIC Google Drive file's metadata without launching
# interactive OAuth. The decision + deauthorize helpers are tested offline by
# mocking the token check and googledrive's auth functions.

test_that(".gdriveCanAuthNonInteractively detects a configured email / service account", {
  withr::local_options(gargle_oauth_email = NULL)
  withr::local_envvar(GOOGLEDRIVE_AUTH = "", GARGLE_SERVICE_ACCOUNT = "")
  expect_false(reproducible:::.gdriveCanAuthNonInteractively())

  withr::local_options(gargle_oauth_email = "someone@example.com")
  expect_true(reproducible:::.gdriveCanAuthNonInteractively())

  withr::local_options(gargle_oauth_email = NULL)
  withr::local_envvar(GOOGLEDRIVE_AUTH = "/path/to/sa.json")
  expect_true(reproducible:::.gdriveCanAuthNonInteractively())
})

test_that(".gdriveShouldGoAnon: anonymous only when token-less AND no usable auth config", {
  withr::local_options(reproducible.gdriveNoAuth = NULL, gargle_oauth_email = NULL)
  withr::local_envvar(GOOGLEDRIVE_AUTH = "", GARGLE_SERVICE_ACCOUNT = "")

  # token-less, no gargle config -> anonymous (cloud-reader / public-file case)
  expect_true(reproducible:::.gdriveShouldGoAnon(hadToken = FALSE))
  # token present -> keep existing (authenticated) behaviour
  expect_false(reproducible:::.gdriveShouldGoAnon(hadToken = TRUE))

  # explicit opt-in forces anonymous even when a token is present
  withr::local_options(reproducible.gdriveNoAuth = TRUE)
  expect_true(reproducible:::.gdriveShouldGoAnon(hadToken = TRUE))
})

test_that(".gdriveShouldGoAnon does NOT go anonymous when gargle can auth (regression)", {
  withr::local_options(reproducible.gdriveNoAuth = NULL)
  withr::local_envvar(GOOGLEDRIVE_AUTH = "", GARGLE_SERVICE_ACCOUNT = "")

  # a configured OAuth email -> authenticate (load cached token), not anonymous
  withr::local_options(gargle_oauth_email = "someone@example.com")
  expect_false(reproducible:::.gdriveShouldGoAnon(hadToken = FALSE))

  # a service-account JSON -> authenticate
  withr::local_options(gargle_oauth_email = NULL)
  withr::local_envvar(GOOGLEDRIVE_AUTH = "/path/to/sa.json")
  expect_false(reproducible:::.gdriveShouldGoAnon(hadToken = FALSE))

  # ...but an explicit gdriveNoAuth still forces anonymous
  withr::local_options(gargle_oauth_email = "someone@example.com",
                       reproducible.gdriveNoAuth = TRUE)
  withr::local_envvar(GOOGLEDRIVE_AUTH = "")
  expect_true(reproducible:::.gdriveShouldGoAnon(hadToken = FALSE))
})

test_that(".gdriveDeauthForPublic deauthorizes when there is no token (and no auth config)", {
  skip_if_not_installed("googledrive")
  withr::local_options(reproducible.gdriveNoAuth = NULL, gargle_oauth_email = NULL)
  withr::local_envvar(GOOGLEDRIVE_AUTH = "", GARGLE_SERVICE_ACCOUNT = "")

  deauthed <- FALSE
  testthat::local_mocked_bindings(.gdriveHasToken = function() FALSE)
  testthat::local_mocked_bindings(
    drive_deauth = function(...) deauthed <<- TRUE,
    drive_token = function(...) stop("must not fetch a token"),
    .package = "googledrive"
  )

  res <- reproducible:::.gdriveDeauthForPublic()
  expect_true(res$deauthed)
  expect_null(res$token)        # nothing to restore (there was no token)
  expect_true(deauthed)         # googledrive::drive_deauth() was called
})

test_that(".gdriveDeauthForPublic is a no-op when a token is present (no opt-in)", {
  skip_if_not_installed("googledrive")
  withr::local_options(reproducible.gdriveNoAuth = NULL)

  deauthed <- FALSE
  testthat::local_mocked_bindings(.gdriveHasToken = function() TRUE)
  testthat::local_mocked_bindings(
    drive_deauth = function(...) deauthed <<- TRUE,
    .package = "googledrive"
  )

  res <- reproducible:::.gdriveDeauthForPublic()
  expect_false(res$deauthed)
  expect_false(deauthed)        # a cloud writer's token is left untouched
})

test_that(".gdriveDeauthForPublic preserves a token to restore when gdriveNoAuth forces anon", {
  skip_if_not_installed("googledrive")
  withr::local_options(reproducible.gdriveNoAuth = TRUE)

  deauthed <- FALSE
  testthat::local_mocked_bindings(.gdriveHasToken = function() TRUE)
  testthat::local_mocked_bindings(
    drive_deauth = function(...) deauthed <<- TRUE,
    drive_token = function(...) "TOKEN",
    .package = "googledrive"
  )

  res <- reproducible:::.gdriveDeauthForPublic()
  expect_true(res$deauthed)
  expect_identical(res$token, "TOKEN")  # caller restores this on.exit
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
