test_that("urlLog: destinationPath is normalized to an absolute path", {
  testInit()
  withr::local_options(reproducible.urlLog = TRUE)
  clearUrlLog()
  reproducible:::.logUrlAccess("prepInputs", "https://example.com/a.tif",
                               destinationPath = ".")
  rec <- prepInputsLog()[[1]]
  expect_true(nzchar(rec$destinationPath))
  expect_false(identical(rec$destinationPath, "."))   # was expanded
  expect_true(startsWith(rec$destinationPath, "/") ||
              grepl("^[A-Za-z]:/", rec$destinationPath))  # absolute
})

test_that("urlLog: core record carries targetFile/archive/alsoExtract", {
  testInit()
  withr::local_options(reproducible.urlLog = TRUE)
  clearUrlLog()
  reproducible:::.logUrlAccess(
    "prepInputs", "https://example.com/a.zip",
    targetFile = "raster.tif",
    archive    = "raster.zip",
    alsoExtract = c("aux1", "aux2"),
    destinationPath = "/tmp"
  )
  rec <- prepInputsLog()[[1]]
  expect_equal(rec$targetFile, "raster.tif")
  expect_equal(rec$archive,    "raster.zip")
  expect_equal(rec$alsoExtract, "aux1; aux2")   # vector collapsed
  # The record stores the normPath()-resolved path (see .absPathOrNA), so the
  # expectation must normalize too: "/tmp" -> "/private/tmp" on macOS,
  # "C:/tmp" on Windows. Comparing to the literal "/tmp" only passes on Linux.
  expect_equal(rec$destinationPath, normPath("/tmp"))
})

test_that("urlLog: env sink merges sink$extra into each record (SpaDES module/event)", {
  testInit()
  e <- new.env(parent = emptyenv())
  e$extra <- list(module = "ageModule", event = "init")
  withr::local_options(reproducible.urlLog = e)
  reproducible:::.logUrlAccess("prepInputs", "https://example.com/a.tif")

  rec <- e$records[[1]]
  expect_equal(rec$module, "ageModule")
  expect_equal(rec$event,  "init")

  ## Dynamic update: next event sets new module/event, picked up immediately.
  e$extra <- list(module = "vegModule", event = "step")
  reproducible:::.logUrlAccess("prepInputs", "https://example.com/b.tif")
  rec2 <- e$records[[2]]
  expect_equal(rec2$module, "vegModule")
  expect_equal(rec2$event,  "step")

  ## Core columns win on key collisions: caller's `fn` doesn't override.
  e$extra <- list(fn = "hijacked", custom = "ok")
  reproducible:::.logUrlAccess("preProcess", "https://example.com/c.tif")
  rec3 <- e$records[[3]]
  expect_equal(rec3$fn,    "preProcess")  # core wins
  expect_equal(rec3$custom, "ok")
})

test_that("urlLog: one-time 'how to view' hint fires once per session", {
  testInit()
  withr::local_options(reproducible.urlLog = TRUE, reproducible.verbose = 1)
  clearUrlLog()
  ## reset the session-level announce flag so this test controls it
  assign("announced", FALSE, envir = reproducible:::.urlLogEnv)

  msg1 <- capture_messages(
    reproducible:::.logUrlAccess("prepInputs", "https://example.com/a.tif"))
  expect_true(any(grepl("prepInputsLog\\(\\)|showCache", msg1)))

  ## second access: no repeat hint
  msg2 <- capture_messages(
    reproducible:::.logUrlAccess("prepInputs", "https://example.com/b.tif"))
  expect_false(any(grepl("prepInputsLog\\(\\)|showCache", msg2)))
})

test_that("urlLog: FALSE is the kill switch", {
  testInit()
  withr::local_options(reproducible.urlLog = FALSE)
  clearUrlLog()
  reproducible:::.logUrlAccess("prepInputs", "https://example.com/x.tif")
  expect_length(prepInputsLog(), 0L)
})

test_that("urlLog: default (NULL) captures bare prepInputs into in-memory log", {
  testInit()
  withr::local_options(reproducible.urlLog = NULL)
  clearUrlLog()
  ## bare prepInputs (no Cache, no explicit sink) still recorded by default
  reproducible:::.logUrlAccess("prepInputs", "https://example.com/x.tif")
  reproducible:::.logUrlAccess("prepInputs", "https://example.com/x.tif")  # dedup
  reproducible:::.logUrlAccess("preProcess", "https://example.com/y.tif")
  log <- prepInputsLog()
  expect_length(log, 2L)
  expect_setequal(vapply(log, function(r) r$url, character(1)),
                  c("https://example.com/x.tif", "https://example.com/y.tif"))
})

test_that("urlLog: TRUE sink + idempotency + clear", {
  testInit()
  withr::local_options(reproducible.urlLog = TRUE)
  clearUrlLog()

  reproducible:::.logUrlAccess("prepInputs", "https://example.com/a.tif",
                               destinationPath = "/tmp", cacheId = "abc")
  reproducible:::.logUrlAccess("prepInputs", "https://example.com/a.tif",
                               destinationPath = "/tmp", cacheId = "abc")
  reproducible:::.logUrlAccess("prepInputs", "https://example.com/b.tif",
                               destinationPath = "/tmp", cacheId = "abc")

  log <- prepInputsLog()
  expect_length(log, 2L)
  expect_equal(log[[1]]$url, "https://example.com/a.tif")
  expect_equal(log[[2]]$url, "https://example.com/b.tif")

  clearUrlLog()
  expect_length(prepInputsLog(), 0L)
})

test_that("urlLog: environment sink populates env$records and env$seen", {
  testInit()
  e <- new.env(parent = emptyenv())
  withr::local_options(reproducible.urlLog = e)

  ## Same (fn, url, cacheId) collapses to one record regardless of miss/hit.
  reproducible:::.logUrlAccess("prepInputs", "https://example.com/a.tif",
                               cacheId = "abc")
  reproducible:::.logUrlAccess("prepInputs", "https://example.com/a.tif",
                               cacheId = "abc")
  expect_length(e$records, 1L)
  expect_equal(e$records[[1]]$url, "https://example.com/a.tif")
  expect_equal(e$records[[1]]$fn,  "prepInputs")
  expect_equal(e$records[[1]]$cacheId, "abc")

  ## Different cacheId -> new record.
  reproducible:::.logUrlAccess("prepInputs", "https://example.com/a.tif",
                               cacheId = "DIFFERENT")
  expect_length(e$records, 2L)
})

test_that("urlLog: preProcess head labels caller as 'prepInputs' when .tempPath supplied", {
  testInit()
  withr::local_options(reproducible.urlLog = TRUE)
  clearUrlLog()

  ## Direct preProcess() call -> .tempPath missing -> labelled "preProcess"
  ## (we exercise this without network by stubbing as a minimal recorder
  ## that mirrors what the real preProcess head does.)
  fakePreProcess <- function(url, destinationPath = ".", .tempPath) {
    reproducible:::.logUrlAccess(
      if (missing(.tempPath)) "preProcess" else "prepInputs",
      url, destinationPath = destinationPath)
  }

  fakePreProcess(url = "https://example.com/a.tif")
  fakePreProcess(url = "https://example.com/b.tif", .tempPath = tempdir())

  log <- prepInputsLog()
  expect_length(log, 2L)
  expect_equal(log[[1]]$fn, "preProcess")
  expect_equal(log[[2]]$fn, "prepInputs")
})

test_that("urlLog: function callback sink invoked with each record", {
  testInit()
  collected <- list()
  cb <- function(rec) collected[[length(collected) + 1L]] <<- rec
  withr::local_options(reproducible.urlLog = cb)
  reproducible:::.logUrlAccess("prepInputs", "https://example.com/a.tif")
  reproducible:::.logUrlAccess("prepInputs", "https://example.com/b.tif")
  expect_length(collected, 2L)
  expect_equal(collected[[1]]$url, "https://example.com/a.tif")
})

test_that("urlLog: NULL url is ignored", {
  testInit()
  withr::local_options(reproducible.urlLog = TRUE)
  clearUrlLog()
  reproducible:::.logUrlAccess("prepInputs", NULL)
  reproducible:::.logUrlAccess("prepInputs", character(0))
  expect_length(prepInputsLog(), 0L)
})

test_that("urlLog: idempotency key handles NA cacheId", {
  testInit()
  e <- new.env(parent = emptyenv())
  withr::local_options(reproducible.urlLog = e)
  reproducible:::.logUrlAccess("prepInputs", "https://example.com/a.tif",
                               cacheId = NA_character_)
  reproducible:::.logUrlAccess("prepInputs", "https://example.com/a.tif",
                               cacheId = NA_character_)
  expect_length(e$records, 1L)
})

test_that("urlLog: Cache(Map(...prepInputs(url=url))) attaches urls via frame on miss + replays on hit", {
  testInit("terra")
  withr::local_options(reproducible.cachePath = tmpdir)
  e <- new.env(parent = emptyenv())
  withr::local_options(reproducible.urlLog = e)

  urls <- c("https://example.com/a.tif", "https://example.com/b.tif")

  ## Stub prepInputs that calls the real function-head hook, so the frame
  ## mechanism gets exercised without touching the network.
  fakePrepInputs <- function(url, destinationPath = ".") {
    reproducible:::.logUrlAccess("prepInputs", url,
                                 destinationPath = destinationPath)
    url
  }
  prepInputs <- fakePrepInputs

  ## First call: cache miss. Inner stub fires hook -> urls pushed to Cache
  ## frame -> Cache attaches tags + emits session records on save.
  r1 <- Cache(Map(url = urls, function(url) prepInputs(url = url)))

  expect_length(e$records, 2L)
  cids <- unique(vapply(e$records, function(r) r$cacheId, character(1)))
  expect_length(cids, 1L)
  expect_setequal(vapply(e$records, function(r) r$url, character(1)), urls)

  sc <- showCache(tmpdir, cacheId = cids)
  expect_true("reproducible.url"          %in% sc$tagKey)
  expect_true("reproducible.urlHitCount"  %in% sc$tagKey)

  ## Second call: cache hit. Fresh env -> replay from DB tags.
  e2 <- new.env(parent = emptyenv())
  withr::local_options(reproducible.urlLog = e2)
  r2 <- Cache(Map(url = urls, function(url) prepInputs(url = url)))

  expect_length(e2$records, 2L)
  expect_setequal(vapply(e2$records, function(r) r$url, character(1)), urls)

  sc2 <- showCache(tmpdir, cacheId = cids)
  hc <- as.integer(sc2$tagValue[sc2$tagKey == "reproducible.urlHitCount"])
  expect_true(any(hc >= 1L))
})

test_that("urlLog: Cache(Map(..., function(url) prepInputs(url=url))) does not error", {
  testInit("terra")
  withr::local_options(
    reproducible.urlLog   = TRUE,
    reproducible.cachePath = tmpdir
  )
  clearUrlLog()

  ## Mask prepInputs locally with a stub that invokes the real hook
  ## (so the frame mechanism gets exercised).
  fakePrepInputs <- function(url, destinationPath = ".") {
    reproducible:::.logUrlAccess("prepInputs", url,
                                 destinationPath = destinationPath)
    url
  }
  prepInputs <- fakePrepInputs
  urls <- c("https://example.com/a.tif", "https://example.com/b.tif")

  expect_no_error({
    Cache(Map(url = urls, function(url) prepInputs(url = url)))
  })
})

test_that("urlLog: Cache hooks tag cacheId with reproducible.url* tags", {
  testInit("terra", needInternet = FALSE)
  withr::local_options(
    reproducible.urlLog   = TRUE,
    reproducible.cachePath = tmpdir
  )
  clearUrlLog()

  ## Drive the Cache hooks via a synthetic call that does NOT touch the network:
  ## the stub invokes the real function-head hook so the frame mechanism
  ## carries the URL into the Cache tag-write path.
  fakePrepInputs <- function(url, destinationPath = ".") {
    reproducible:::.logUrlAccess("prepInputs", url,
                                 destinationPath = destinationPath)
    "ok"
  }
  prepInputs <- fakePrepInputs

  out1 <- Cache(prepInputs(url = "https://example.com/a.tif"))
  out2 <- Cache(prepInputs(url = "https://example.com/a.tif"))  # hit
  expect_equal(as.character(out1), "ok")
  expect_equal(as.character(out2), "ok")

  sc <- showCache(tmpdir, userTags = "reproducible.url")
  expect_true(NROW(sc) > 0L)
  expect_true("reproducible.url"          %in% sc$tagKey)
  expect_true("reproducible.urlFn"        %in% sc$tagKey)
  expect_true("reproducible.urlFirstSeen" %in% sc$tagKey)
  expect_true("reproducible.urlLastSeen"  %in% sc$tagKey)
  expect_true("reproducible.urlHitCount"  %in% sc$tagKey)

  ## hitCount should be >= 1 after the second (hit) call
  hc <- as.integer(sc$tagValue[sc$tagKey == "reproducible.urlHitCount"])
  expect_true(any(hc >= 1L))

  ## Session log: miss + subsequent hit dedup to one record per cacheId.
  log <- prepInputsLog()
  expect_length(log, 1L)
})

test_that("urlLog: legacy cache hit (no tags) recovers url from matched call + self-heals", {
  testInit("terra")
  withr::local_options(reproducible.cachePath = tmpdir)

  fakePrepInputs <- function(url, destinationPath = ".") "ok"
  prepInputs <- fakePrepInputs

  ## Create a "legacy" entry with the feature OFF -> no reproducible.url* tags.
  withr::local_options(reproducible.urlLog = FALSE)
  invisible(Cache(prepInputs(url = "https://example.com/legacy.tif")))
  sc0 <- showCache(tmpdir, userTags = "reproducible.url")
  expect_equal(NROW(sc0), 0L)   # confirm no url tags yet

  ## Now turn the feature on (default) and hit the same entry. Option 1 should
  ## recover the url from the matched call, emit a record, and back-fill tags.
  withr::local_options(reproducible.urlLog = TRUE)
  clearUrlLog()
  out <- Cache(prepInputs(url = "https://example.com/legacy.tif"))  # hit
  expect_equal(as.character(out), "ok")

  log <- prepInputsLog()
  expect_length(log, 1L)
  expect_equal(log[[1]]$url, "https://example.com/legacy.tif")

  ## Self-heal: tags now exist, so a further hit replays from the DB.
  sc1 <- showCache(tmpdir, userTags = "reproducible.url")
  expect_true("reproducible.url" %in% sc1$tagKey)
  expect_true(any(sc1$tagValue == "https://example.com/legacy.tif"))
})

## ---- urlLog(): accessor for the url tags on a cache ----------------------

.urlLogCache <- function(urls = "https://example.com/a.tif") {
  ## stub prepInputs that logs the url(s) without touching the network
  fakePrepInputs <- function(url, opts = "x") {
    reproducible:::.logUrlAccess("prepInputs", url)
    "ok"
  }
  prepInputs <- fakePrepInputs
  Cache(prepInputs(url = urls))
}

test_that("urlLog(): returns url and function, selects/orders columns by `which`", {
  testInit(verbose = 0)
  withr::local_options(reproducible.urlLog = TRUE, reproducible.cachePath = tmpdir)
  clearUrlLog()
  .urlLogCache()
  ul <- urlLog(tmpdir)
  expect_s3_class(ul, "data.table")
  expect_identical(colnames(ul), c("function", "module", "url"))
  expect_identical(ul$url, "https://example.com/a.tif")
  expect_identical(ul[["function"]], "prepInputs")
  expect_true(is.na(ul$module))

  ## default cachePath comes from the option
  expect_identical(urlLog()$url, ul$url)

  ul2 <- urlLog(tmpdir, which = c("url", "caller", "hitCount", "cacheId", "createdDate"))
  expect_identical(colnames(ul2), c("url", "caller", "hitCount", "cacheId", "createdDate"))
  expect_identical(ul2$caller, "prepInputs")
  expect_error(urlLog(tmpdir, which = "nonsense"))
})

test_that("urlLog(): never returns preDigest values that merely contain 'url'", {
  testInit(verbose = 0)
  withr::local_options(reproducible.urlLog = TRUE, reproducible.cachePath = tmpdir)
  clearUrlLog()
  .urlLogCache()
  ## a Cache entry with no url ledger whose argument digests mention "url"
  f <- function(urlThing) urlThing
  Cache(f, urlThing = "url-like-value")
  sc <- showCache(tmpdir, userTags = "url", verbose = FALSE)
  expect_true(any(grepl("url", sc$tagValue[sc$tagKey == "preDigest"], ignore.case = TRUE)))
  ul <- urlLog(tmpdir, which = c("cacheId", "function", "url"))
  expect_identical(NROW(ul), 1L)
  expect_identical(ul$url, "https://example.com/a.tif")
})

test_that("urlLog(): several urls on one entry give several rows", {
  testInit(verbose = 0)
  withr::local_options(reproducible.urlLog = TRUE, reproducible.cachePath = tmpdir)
  clearUrlLog()
  .urlLogCache(c("https://example.com/a.tif", "https://example.com/b.tif"))
  ul <- urlLog(tmpdir, which = c("cacheId", "url"))
  expect_identical(NROW(ul), 2L)
  expect_identical(length(unique(ul$cacheId)), 1L)
  expect_setequal(ul$url, c("https://example.com/a.tif", "https://example.com/b.tif"))
})

test_that("urlLog(): empty cache gives a zero-row table with the requested columns", {
  testInit(verbose = 0)
  withr::local_options(reproducible.cachePath = tmpdir)
  ul <- urlLog(tmpdir, which = c("url", "lastSeen"))
  expect_s3_class(ul, "data.table")
  expect_identical(NROW(ul), 0L)
  expect_identical(colnames(ul), c("url", "lastSeen"))
})

test_that("urlLog(): sorted by lastSeen descending", {
  testInit(verbose = 0)
  withr::local_options(reproducible.urlLog = TRUE, reproducible.cachePath = tmpdir)
  clearUrlLog()
  .urlLogCache("https://example.com/a.tif")
  Sys.sleep(1.1)
  .urlLogCache("https://example.com/b.tif")
  ul <- urlLog(tmpdir, which = c("url", "lastSeen"))
  expect_identical(ul$url, c("https://example.com/b.tif", "https://example.com/a.tif"))
})

test_that("logUrlAccess(): a download by other means lands in the session log under the caller's name", {
  testInit()
  withr::local_options(reproducible.urlLog = NULL)
  clearUrlLog()
  myFetch <- function() logUrlAccess("https://example.com/other.csv", destinationPath = ".")
  myFetch()
  rec <- prepInputsLog()[[1]]
  expect_equal(rec$url, "https://example.com/other.csv")
  expect_equal(rec$fn, "myFetch")
  expect_true(is.na(rec$cacheId))
  logUrlAccess("https://example.com/two.csv", fn = "byHand")
  expect_equal(prepInputsLog()[[2]]$fn, "byHand")
})

test_that("logUrlAccess(): inside Cache() it tags the cacheId and replays on a hit", {
  testInit(needInternet = FALSE)
  withr::local_options(reproducible.urlLog = TRUE, reproducible.cachePath = tmpdir)
  clearUrlLog()
  fetcher <- function(x) {
    logUrlAccess("https://example.com/viaCache.csv")
    x
  }
  Cache(fetcher(1))
  expect_equal(prepInputsLog()[[1]]$url, "https://example.com/viaCache.csv")
  expect_false(is.na(prepInputsLog()[[1]]$cacheId))
  ul <- urlLog(tmpdir, which = c("url", "caller"))
  expect_equal(ul$url, "https://example.com/viaCache.csv")
  expect_equal(ul$caller, "fetcher")

  clearUrlLog()
  Cache(fetcher(1)) # hit: fetcher() does not run, the tags are replayed
  rec <- prepInputsLog()
  expect_length(rec, 1L)
  expect_equal(rec[[1]]$url, "https://example.com/viaCache.csv")
  expect_equal(rec[[1]]$fn, "fetcher")
})

test_that("logUrlAccess(): reproducible.urlLog = FALSE turns it off", {
  testInit(needInternet = FALSE)
  withr::local_options(reproducible.urlLog = FALSE, reproducible.cachePath = tmpdir)
  clearUrlLog()
  logUrlAccess("https://example.com/off.csv")
  fetcher <- function(x) { logUrlAccess("https://example.com/off.csv"); x }
  Cache(fetcher(1))
  expect_length(prepInputsLog(), 0L)
  expect_equal(NROW(showCache(tmpdir, userTags = "reproducible.url", verbose = FALSE)), 0L)
})

test_that("CacheGeo's Drive ledger read and write are recorded in the url ledger", {
  skip_if_not_installed("googledrive")
  testInit()
  withr::local_options(reproducible.urlLog = NULL)
  clearUrlLog()
  local_mocked_bindings(
    drive_download = function(file, path, overwrite, ...) {
      file.create(path)
      data.frame(local_path = path)
    },
    with_drive_quiet = function(code) code,
    drive_put = function(media, path, name, ...) {
      structure(list(id = "ABC123"), class = "fakeDribble")
    },
    drive_link = function(file) paste0("https://drive.google.com/file/d/", file$id, "/view"),
    .package = "googledrive")
  reproducible:::.geoDriveDownload("XYZ", file.path(tmpdir, "ledger.rds"),
                                   url = "https://drive.google.com/file/d/XYZ/view")
  ledger <- list(remoteType = "drive", remote = "folder", file = "ledger.rds",
                 path = file.path(tmpdir, "ledger.rds"))
  local_mocked_bindings(.geoDriveFolder = function(ledger, create) list(id = "FOLDER"))
  reproducible:::.geoPush(ledger)
  urls <- vapply(prepInputsLog(), `[[`, character(1), "url")
  expect_true("https://drive.google.com/file/d/XYZ/view" %in% urls)
  expect_true("https://drive.google.com/file/d/ABC123/view" %in% urls)
})

## ---- Completeness: every access reaches prepInputsLog(), however it was called or cached ----

test_that("urlLog: an inner Cache hit inside an outer miss tags the outer entry, so its hit replays the url", {
  testInit(needInternet = FALSE)
  withr::local_options(reproducible.urlLog = TRUE, reproducible.cachePath = tmpdir)
  fetch <- function(u) { logUrlAccess(u, fn = "prepInputs", targetFile = "a.tif"); u }
  inner <- function(u) Cache(fetch(u))
  outer <- function(u, k) { inner(u); k }
  inner("https://example.com/nested.tif")          # inner entry exists before the outer runs
  clearUrlLog()
  Cache(outer("https://example.com/nested.tif", 1)) # outer miss, inner hit
  expect_true("https://example.com/nested.tif" %in% urlLog(tmpdir, which = c("cacheId", "url"))$url[
    urlLog(tmpdir, which = c("cacheId", "url", "function"))$`function` == "outer"])
  clearUrlLog()
  Cache(outer("https://example.com/nested.tif", 1)) # outer hit: nothing inside runs
  recs <- prepInputsLog()
  expect_length(recs, 1L)
  expect_equal(recs[[1]]$url, "https://example.com/nested.tif")
  expect_equal(recs[[1]]$targetFile, "a.tif")
})

test_that("urlLog: an access inside a Cache() that then fails is still recorded", {
  testInit(needInternet = FALSE)
  withr::local_options(reproducible.urlLog = TRUE, reproducible.cachePath = tmpdir)
  clearUrlLog()
  boom <- function(u) { logUrlAccess(u, fn = "prepInputs"); stop("fails after the download") }
  expect_error(Cache(boom("https://example.com/failed.tif")), "fails after")
  recs <- prepInputsLog()
  expect_length(recs, 1L)
  expect_equal(recs[[1]]$url, "https://example.com/failed.tif")
  expect_true(is.na(recs[[1]]$cacheId))
})

test_that("urlLog: a hit replays each url's own fn, targetFile, archive and destinationPath", {
  testInit(needInternet = FALSE)
  withr::local_options(reproducible.urlLog = TRUE, reproducible.cachePath = tmpdir)
  dp <- file.path(tmpdir, "inputs")
  two <- function() {
    logUrlAccess("https://example.com/one.zip", fn = "prepInputs", targetFile = "one.tif",
                 archive = "one.zip", destinationPath = dp)
    logUrlAccess("https://example.com/two.csv", fn = "getClimate", targetFile = "two.csv",
                 destinationPath = dp)
    1
  }
  clearUrlLog()
  Cache(two())
  miss <- data.table::rbindlist(prepInputsLog(), fill = TRUE)
  clearUrlLog()
  Cache(two())
  hit <- data.table::rbindlist(prepInputsLog(), fill = TRUE)
  cols <- c("fn", "url", "targetFile", "archive", "alsoExtract", "destinationPath", "cacheId")
  expect_equal(hit[order(url), ..cols], miss[order(url), ..cols])
  ul <- urlLog(tmpdir, which = c("url", "caller", "targetFile", "destinationPath"))
  expect_equal(ul[order(url)]$caller, c("prepInputs", "getClimate"))
  expect_equal(ul[order(url)]$targetFile, c("one.tif", "two.csv"))
  expect_equal(unique(ul$destinationPath), normPath(dp))
})

test_that("urlLog: an environment or function sink does not hide records from prepInputsLog()", {
  testInit(needInternet = FALSE)
  clearUrlLog()
  e <- new.env(parent = emptyenv())
  e$extra <- list(module = "modA", event = "init")
  withr::with_options(list(reproducible.urlLog = e),
                      logUrlAccess("https://example.com/env.tif", fn = "prepInputs"))
  got <- list()
  withr::with_options(list(reproducible.urlLog = function(rec) got[[length(got) + 1L]] <<- rec),
                      logUrlAccess("https://example.com/fun.tif", fn = "prepInputs"))
  expect_length(e$records, 1L)
  expect_length(got, 1L)
  recs <- prepInputsLog()
  expect_equal(vapply(recs, `[[`, "", "url"),
               c("https://example.com/env.tif", "https://example.com/fun.tif"))
  expect_equal(recs[[1]]$module, "modA")
})

test_that("urlLog: accesses made in forked children (mclapply) reach the parent's log and Cache", {
  skip_on_os("windows")
  testInit(needInternet = FALSE)
  withr::local_options(reproducible.urlLog = TRUE, reproducible.cachePath = tmpdir)
  fetch <- function(u) { logUrlAccess(u, fn = "prepInputs"); u }
  clearUrlLog()
  invisible(parallel::mclapply(c("https://example.com/f1", "https://example.com/f2"), fetch,
                               mc.cores = 2))
  expect_setequal(vapply(prepInputsLog(), `[[`, "", "url"),
                  c("https://example.com/f1", "https://example.com/f2"))

  par <- function(us) unlist(parallel::mclapply(us, fetch, mc.cores = 2))
  clearUrlLog()
  Cache(par(c("https://example.com/f3", "https://example.com/f4")))
  recs <- prepInputsLog()
  expect_setequal(vapply(recs, `[[`, "", "url"), c("https://example.com/f3", "https://example.com/f4"))
  expect_false(anyNA(vapply(recs, `[[`, "", "cacheId")))
  clearUrlLog()
  Cache(par(c("https://example.com/f3", "https://example.com/f4"))) # hit: replayed from the tags
  expect_setequal(vapply(prepInputsLog(), `[[`, "", "url"),
                  c("https://example.com/f3", "https://example.com/f4"))
})

test_that("urlLog: prepInputs(dlFun = ) with no url is recorded", {
  testInit(needInternet = FALSE, opts = list(reproducible.inputPaths = NULL))
  withr::local_options(reproducible.urlLog = TRUE)
  clearUrlLog()
  getMyData <- function() data.frame(x = 1:3)
  prepInputs(destinationPath = tmpdir, dlFun = getMyData())
  recs <- prepInputsLog()
  expect_length(recs, 1L)
  expect_match(recs[[1]]$url, "getMyData()", fixed = TRUE)
  expect_match(recs[[1]]$fn, "dlFun")
})

test_that("urlLog: a local file read with no url is recorded as local:<path>, and replayed on a hit", {
  testInit(needInternet = FALSE, opts = list(reproducible.inputPaths = NULL))
  withr::local_options(reproducible.urlLog = TRUE, reproducible.cachePath = file.path(tmpdir, "cache"))
  writeLines("a,b\n1,2", file.path(tmpdir, "local.csv"))
  want <- paste0("local:", normPath(file.path(tmpdir, "local.csv")))

  clearUrlLog()
  prepInputs(targetFile = "local.csv", destinationPath = tmpdir, fun = NA)
  recs <- prepInputsLog()
  expect_length(recs, 1L)
  expect_equal(recs[[1]]$url, want)
  expect_equal(recs[[1]]$fn, "prepInputs (local)")
  expect_equal(recs[[1]]$targetFile, "local.csv")

  clearUrlLog()
  preProcess(targetFile = file.path(tmpdir, "local.csv"), destinationPath = tmpdir, fun = NA)
  expect_equal(prepInputsLog()[[1]]$url, want)
  expect_equal(prepInputsLog()[[1]]$fn, "preProcess (local)")

  readIt <- function(dp) prepInputs(targetFile = "local.csv", destinationPath = dp, fun = NA)
  Cache(readIt(tmpdir))
  clearUrlLog()
  Cache(readIt(tmpdir)) # hit
  recs <- prepInputsLog()
  expect_length(recs, 1L)
  expect_equal(recs[[1]]$url, want)
  expect_equal(recs[[1]]$fn, "prepInputs (local)")
  expect_false(is.na(recs[[1]]$cacheId))
})
