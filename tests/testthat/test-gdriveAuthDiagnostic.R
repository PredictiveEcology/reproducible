## A configured Google Drive credential must actually work.
##
## Every other Drive test is written to skip() when authentication is
## unavailable -- correct behaviour for CRAN, forks and contributors, but it
## means a *broken* credential is indistinguishable from *no* credential: both
## just skip. Under covr it is worse, because covr neither fails on test errors
## nor prints a skip summary, so the run goes green and the only symptom is a
## coverage number that quietly stays flat.
##
## That is not hypothetical: CacheGeo() sat at 0/176 lines and the cloud.R
## round-trip functions at 0 for months, with nothing anywhere saying why.
##
## So: skip when nothing is configured, but FAIL -- loudly, with the reason --
## when something is configured and does not work.

test_that("a configured Google Drive credential is usable", {
  skip_on_cran()
  skip_if_not_installed("googledrive")

  tokenFile <- Sys.getenv("GDRIVE_OAUTH_TOKEN")
  saCred    <- Sys.getenv("GOOGLEDRIVE_AUTH")
  if (!nzchar(tokenFile) && !nzchar(saCred)) {
    skip("No Drive credential configured (GDRIVE_OAUTH_TOKEN / GOOGLEDRIVE_AUTH unset)")
  }

  ## Authenticate BEFORE testInit(needGoogleDriveAuth = TRUE): that skips when
  ## no token loads, which would turn a broken credential back into a skip.
  testInit("googledrive", needInternet = TRUE)
  why <- .gdriveAuthFromEnv()
  expect_null(why)
  expect_true(googledrive::drive_has_token())

  ## drive_has_token() only reports that a token *object* exists. It is TRUE for
  ## a scope-limited token that 403s on first use, and for one whose refresh is
  ## broken -- both of which look like success right up until the tests silently
  ## produce nothing. Make one real API call.
  who <- tryCatch(googledrive::drive_user()$emailAddress,
                  error = function(e) paste("ERROR:", conditionMessage(e)))
  expect_false(startsWith(who, "ERROR:"),
               label = paste("drive_user() failed --", who))
  expect_true(nzchar(who))

  ## Report what is actually in use. These land in the test log and are the
  ## difference between "Drive tests skipped" and knowing *which* identity,
  ## client and scopes were in play.
  info <- c(
    paste("credential:", if (nzchar(tokenFile)) "user OAuth token" else "service account"),
    paste("identity:  ", who),
    paste("is service account:", grepl("gserviceaccount", who))
  )
  message(paste(info, collapse = "\n"))

  ## A service account cannot upload into a folder in a person's My Drive (no
  ## storage quota), so the upload tests fail unless their folder is on a Shared
  ## Drive. Not a failure here -- reading works -- but it must be visible.
  if (grepl("gserviceaccount", who)) {
    message("NOTE: authenticated as a service account. Uploads into a folder in ",
            "a person's My Drive will fail; a Shared Drive folder works.")
  }
})
