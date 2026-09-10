# checkAndMakeCloudFolderID() must take the folder id out of a Drive folder URL
# whatever follows the id. Its regex required at least one character after the
# id, so a URL ending at the id -- the form Drive's address bar gives -- lost the
# id's last character: "https://.../folders/<33-char id>" became a 32-char id,
# which still looked like an id, so drive_get() was asked for a folder that does
# not exist and the call stopped with "File not found". googledrive's drive_get
# is mocked to record what it is asked for (no network/auth).

test_that("checkAndMakeCloudFolderID extracts the whole id from a Drive folder URL", {
  skip_if_not_installed("googledrive")
  id33 <- "1X9-mRjyLMNpgkP_cfqhbr_AQEPOsVCHf"
  id32 <- strrep("A", 32)
  cases <- list(
    "URL ending at a 33-char id" = list(paste0("https://drive.google.com/drive/folders/", id33), id33),
    "URL with a query"           = list(paste0("https://drive.google.com/drive/folders/", id33, "?usp=drive_link"), id33),
    "URL with a trailing slash"  = list(paste0("https://drive.google.com/drive/folders/", id33, "/"), id33),
    "URL ending at a 32-char id" = list(paste0("https://drive.google.com/drive/u/0/folders/", id32), id32),
    "bare 33-char id"            = list(id33, id33),
    "bare 32-char id"            = list(id32, id32)
  )
  for (nm in names(cases)) {
    asked <- NULL
    testthat::with_mocked_bindings(
      suppressWarnings( # the mock resolves nothing, which warns; only the lookup matters here
        reproducible:::checkAndMakeCloudFolderID(cases[[nm]][[1]], cachePath = tempdir(),
                                                 create = FALSE, verbose = 0)
      ),
      drive_get = function(x, ...) {
        if (is.null(asked)) asked <<- as.character(x)
        data.frame(name = character(0), id = character(0))
      },
      .package = "googledrive"
    )
    expect_identical(asked, cases[[nm]][[2]], label = nm)
  }
})
