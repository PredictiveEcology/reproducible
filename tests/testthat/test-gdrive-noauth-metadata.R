# assessGoogle() resolves a PUBLIC Google Drive file's metadata without launching
# interactive OAuth. The decision + deauthorize helpers are tested offline by
# mocking the token check and googledrive's auth functions.

test_that(".gdriveShouldGoAnon: anonymous when no token or when gdriveNoAuth", {
  withr::local_options(reproducible.gdriveNoAuth = NULL)
  # no token -> anonymous (the typical cloud-reader / public-file case)
  expect_true(reproducible:::.gdriveShouldGoAnon(hadToken = FALSE))
  # token present -> keep existing (authenticated) behaviour
  expect_false(reproducible:::.gdriveShouldGoAnon(hadToken = TRUE))

  # explicit opt-in forces anonymous even when a token is present
  withr::local_options(reproducible.gdriveNoAuth = TRUE)
  expect_true(reproducible:::.gdriveShouldGoAnon(hadToken = TRUE))
  expect_true(reproducible:::.gdriveShouldGoAnon(hadToken = FALSE))
})

test_that(".gdriveDeauthForPublic deauthorizes when there is no token", {
  skip_if_not_installed("googledrive")
  withr::local_options(reproducible.gdriveNoAuth = NULL)

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
