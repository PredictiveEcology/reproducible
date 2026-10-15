## A cloud cache folder shared "Anyone with the link" is read without a Google sign-in; with no sign-in
## nothing is uploaded and no folder is made. Drive is a directory; googledrive is mocked.

fakeCloudDrive <- function(token = FALSE, env = parent.frame()) {
  fd <- withr::local_tempdir(.local_envir = env)
  store <- new.env()
  store$token <- token
  store$uploads <- character()
  store$mkdir <- 0L
  folderId <- "1abcdefghijklmnopqrstuvwxyzABCDEF" # 33 characters, as a Drive id
  idOf <- function(names) gsub("[^A-Za-z0-9_-]", "_", names) # a Drive id has no dot
  dribble <- function(names) {
    x <- data.frame(name = names, id = idOf(names), stringsAsFactors = FALSE)
    x$drive_resource <- replicate(length(names), list(list()), simplify = FALSE)
    class(x) <- c("dribble", "tbl_df", "tbl", "data.frame")
    x
  }
  testthat::local_mocked_bindings(
    drive_has_token = function() store$token,
    drive_deauth = function(...) invisible(),
    drive_auth = function(...) invisible(),
    drive_get = function(path, ...) {
      x <- dribble("cacheFolder"); x$id <- folderId; x
    },
    drive_mkdir = function(...) { store$mkdir <- store$mkdir + 1L; stop("no sign-in") },
    drive_ls = function(path, pattern = NULL, q = NULL, ...) {
      f <- list.files(fd)
      if (length(pattern) && nzchar(pattern)) f <- f[grepl(pattern, f)]
      dribble(f)
    },
    drive_upload = function(media, path, name, ...) {
      store$uploads <- c(store$uploads, name)
      file.copy(media, file.path(fd, name), overwrite = TRUE)
      dribble(name)
    },
    drive_download = function(file, path, ...) {
      f <- list.files(fd)
      file.copy(file.path(fd, f[idOf(f) == as.character(if (is.character(file)) file else file$id)]), path, overwrite = TRUE)
      data.frame(local_path = path)
    },
    .package = "googledrive", .env = env)
  store$folderId <- folderId
  store
}

test_that("useCloud: no Google sign-in reads a public folder, skips the upload, and says so", {
  skip_if_not_installed("googledrive")
  testInit(needInternet = FALSE)
  withr::local_options(rlang_interactive = FALSE, reproducible.useMemoise = FALSE)
  drive <- fakeCloudDrive(token = TRUE)
  f <- function(x) x + 1
  cp <- function(nm) checkPath(file.path(tmpdir, nm), create = TRUE)

  ## with a token (unchanged): the miss is uploaded
  expect_equal(as.numeric(Cache(f(1), useCloud = TRUE, cloudFolderID = drive$folderId,
                                cachePath = cp("A"), verbose = 0)), 2)
  expect_true(length(drive$uploads) > 0L)

  ## no sign-in, a new local cache: the entry is read from the public folder
  drive$token <- FALSE
  nUp <- length(drive$uploads)
  expect_message(
    out <- Cache(f(1), useCloud = TRUE, cloudFolderID = drive$folderId, cachePath = cp("B"), verbose = 1),
    "No Google Drive sign-in")
  expect_equal(as.numeric(out), 2)
  expect_equal(length(drive$uploads), nUp)  # nothing uploaded; the hit came from the folder
  expect_equal(drive$mkdir, 0L)

  ## no sign-in and a miss: computed locally, upload and folder creation skipped, one message
  expect_message(
    out2 <- Cache(f(5), useCloud = TRUE, cloudFolderID = drive$folderId, cachePath = cp("C"), verbose = 1),
    "not uploaded")
  expect_equal(as.numeric(out2), 6)
  expect_equal(length(drive$uploads), nUp)
  expect_equal(drive$mkdir, 0L)
})

test_that(".cloudAuth(): token kept; none configured is anonymous; a name or interactive session is left alone", {
  skip_if_not_installed("googledrive")
  withr::local_options(rlang_interactive = FALSE, gargle_oauth_email = NULL)
  withr::local_envvar(GOOGLEDRIVE_AUTH = NA, GARGLE_SERVICE_ACCOUNT = NA)
  drive <- fakeCloudDrive(token = TRUE)
  id <- drive$folderId
  expect_identical(reproducible:::.cloudAuth(id, verbose = 0), "token")
  drive$token <- FALSE
  expect_identical(reproducible:::.cloudAuth(id, verbose = 0), "anon")
  expect_identical(reproducible:::.cloudAuth(paste0("https://drive.google.com/drive/folders/", id), verbose = 0), "anon")
  expect_identical(reproducible:::.cloudAuth("cloudCacheFolderName", verbose = 0), "token")
  expect_identical(reproducible:::.cloudAuth(NULL, verbose = 0), "token")
})
