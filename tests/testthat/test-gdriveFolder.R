## A Google Drive folder url: which of the folder's files are fetched, and how the folder is listed.
## No network: the listing comes from mocked googledrive calls, and dlGoogle() is replaced by a stub
## that records the ids it is asked for and writes a small file in their place.

gdriveFolderUrl <- "https://drive.google.com/drive/folders/1AbCdEfGhIjKlMnOpQrStUvWxYz012345"

## `files`: the folder's files, as name = id. Returns an environment whose `fetched` holds the ids
## dlGoogle() was asked for, in order.
localGdriveFolder <- function(files, env = parent.frame()) {
  rec <- new.env()
  rec$fetched <- character()
  local_mocked_bindings(
    isGoogleDriveDirectory = function(...) TRUE,
    .driveDirRemap = function(...) NA_character_,
    dlGoogle = function(url, destinationPath, ...) {
      rec$fetched <- c(rec$fetched, url)
      nm <- names(files)[files == url]
      checkPath(destinationPath, create = TRUE)
      f <- file.path(destinationPath, nm)
      writeLines(nm, f)
      list(destFile = f, needChecksums = 0)
    },
    .env = env
  )
  rec
}

fetchFromGdriveFolder <- function(dest, archive = NULL, targetFile = NULL, alsoExtract = NULL)
  downloadRemote(url = gdriveFolderUrl, archive = archive, targetFile = targetFile,
                 checkSums = .emptyChecksumsResult, fileToDownload = NULL,
                 messSkipDownload = "", destinationPath = dest, overwrite = FALSE,
                 needChecksums = 0, .tempPath = tempdir2(rndstr(1, 6)), preDigest = NULL,
                 alsoExtract = alsoExtract, purge = FALSE, verbose = 0)

modelFolder <- c("m_full.zip" = "id-m", "m_fit_full.zip" = "id-fit", "m_season.zip" = "id-season")
listing <- function(files) data.frame(name = names(files), id = unname(files), stringsAsFactors = FALSE)

test_that("a Drive folder url with an archive fetches that archive only", {
  skip_if_not_installed("googledrive")
  testInit(opts = list(reproducible.inputPaths = NULL, reproducible.destinationPathShared = NULL))
  rec <- localGdriveFolder(modelFolder)
  local_mocked_bindings(drive_ls = function(...) listing(modelFolder), .package = "googledrive")
  dest <- checkPath(file.path(tmpdir, "dest"), create = TRUE)

  fetchFromGdriveFolder(dest, archive = "m_full.zip", targetFile = "m_full_fixed.csv")
  expect_identical(rec$fetched, "id-m")
})

test_that("a Drive folder url with a targetFile fetches it and its companions, as alsoExtract says", {
  skip_if_not_installed("googledrive")
  testInit(opts = list(reproducible.inputPaths = NULL, reproducible.destinationPathShared = NULL))
  shp <- c("a.shp" = "id-ashp", "a.dbf" = "id-adbf", "a.shx" = "id-ashx", "b.shp" = "id-bshp",
           "readme.txt" = "id-readme")
  local_mocked_bindings(drive_ls = function(...) listing(shp), .package = "googledrive")
  fetched <- function(alsoExtract) {
    rec <- localGdriveFolder(shp)
    dest <- checkPath(file.path(tmpdir, rndstr(1, 6)), create = TRUE)
    fetchFromGdriveFolder(dest, targetFile = "a.shp", alsoExtract = alsoExtract)
    sort(names(shp)[match(rec$fetched, shp)])
  }
  expect_identical(fetched(NULL), c("a.dbf", "a.shp", "a.shx"))
  expect_identical(fetched("similar"), c("a.dbf", "a.shp", "a.shx"))
  expect_identical(fetched(NA), "a.shp")
  expect_identical(fetched("none"), "a.shp")
  expect_identical(fetched("readme.txt"), c("a.shp", "readme.txt"))
  expect_identical(fetched("all"), sort(names(shp)))
})

test_that("a Drive folder url with neither archive nor targetFile still fetches the whole folder", {
  skip_if_not_installed("googledrive")
  testInit(opts = list(reproducible.inputPaths = NULL, reproducible.destinationPathShared = NULL))
  rec <- localGdriveFolder(modelFolder)
  local_mocked_bindings(drive_ls = function(...) listing(modelFolder), .package = "googledrive")
  dest <- checkPath(file.path(tmpdir, "dest"), create = TRUE)

  fetchFromGdriveFolder(dest)
  expect_identical(rec$fetched, unname(modelFolder))
})

test_that("a Drive folder on a shared drive the account is not a member of is listed by a search", {
  skip_if_not_installed("googledrive")
  testInit(opts = list(reproducible.inputPaths = NULL, reproducible.destinationPathShared = NULL))
  rec <- localGdriveFolder(modelFolder)
  seen <- NULL
  local_mocked_bindings(
    ## drive_ls() lists through the folder's shared drive, which a non-member cannot open
    drive_ls = function(...) stop("Client error: (404) Not Found\nShared drive not found: 0AXXXXXXXXXXXXXXXXX"),
    drive_find = function(q = NULL, corpus = NULL, ...) {
      seen <<- list(q = q, corpus = corpus)
      listing(modelFolder)
    },
    .package = "googledrive"
  )
  dest <- checkPath(file.path(tmpdir, "dest"), create = TRUE)

  fetchFromGdriveFolder(dest, archive = "m_full.zip", targetFile = "m_full_fixed.csv")
  expect_identical(rec$fetched, "id-m")
  expect_identical(seen$corpus, "allDrives")
  expect_true("'1AbCdEfGhIjKlMnOpQrStUvWxYz012345' in parents" %in% seen$q)
})

test_that("a Drive folder that drive_ls() can list is not searched, and a failed search keeps drive_ls()'s error", {
  skip_if_not_installed("googledrive")
  testInit(opts = list(reproducible.inputPaths = NULL, reproducible.destinationPathShared = NULL))
  rec <- localGdriveFolder(modelFolder)
  searched <- FALSE
  local_mocked_bindings(
    drive_ls = function(...) listing(modelFolder),
    drive_find = function(...) {
      searched <<- TRUE
      stop("not expected")
    },
    .package = "googledrive"
  )
  dest <- checkPath(file.path(tmpdir, "dest"), create = TRUE)
  fetchFromGdriveFolder(dest, archive = "m_full.zip")
  expect_false(searched)

  local_mocked_bindings(
    drive_ls = function(...) stop("Client error: (403) Forbidden: the listing"),
    drive_find = function(...) stop("Client error: (403) Forbidden: the search"),
    .package = "googledrive"
  )
  expect_error(fetchFromGdriveFolder(dest, archive = "m_full.zip"), "the listing")
})
