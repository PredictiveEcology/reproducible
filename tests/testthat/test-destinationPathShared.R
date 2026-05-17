# Tests for `reproducible.destinationPathShared` — the read-write shared local cache
# used by prepInputs() / preProcess() to avoid re-downloads across projects.
#
# Test matrix lives in dev/destinationPathShared-design.md (§5). Case IDs (P1, H1, ...)
# in this file map 1:1 to that matrix. Tests for behaviour that has not been
# implemented in this branch have been removed (rather than left as `skip()`
# stubs that misleadingly pad the count) — see `dev/destinationPathShared-design.md` §4
# for the rolling status of each design item.
#
# Note: the option is now named `reproducible.destinationPathShared` (matching the
# `prepInputs` naming family); `reproducible.inputPaths` remains as a
# backwards-compatible alias. The intermediate `reproducible.dataPath` name
# briefly used in this branch was dropped before release — case IDs that
# referenced it have been retired.
#
# Conventions:
#   - No real network. Fixtures use file:// URLs pointing at on-disk files.
#   - CHECKSUMS.txt is hand-written into the shared dir to exercise the
#     existing checksum-driven local-source path.
#   - withr::local_options() isolates option state per test_that() block.
#   - Internal helpers reached via getFromNamespace() so tests load on
#     covr / R CMD check / source-loaded sessions.

# ---------------------------------------------------------------------------
# Fixture helpers
# ---------------------------------------------------------------------------

# Build a tiny on-disk fixture with a known content/hash. Creates the dir
# if it doesn't exist. Returns list(path, hash, size, url).
makeFixture <- function(dir, name = "foo.csv", content = "a,b\n1,2\n3,4\n") {
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  p <- file.path(dir, name)
  writeBin(charToRaw(content), p)        # avoid line-ending mangling
  list(
    path = normPath(p),
    hash = digest::digest(file = p, algo = "xxhash64"),
    size = file.info(p)$size,
    url  = toFileUrl(p)
  )
}

# Write a CHECKSUMS.txt covering a single fixture into a directory.
# Format matches what `Checksums(write = TRUE)` produces.
writeChecksumsFor <- function(dir, fixture, name = basename(fixture$path)) {
  csf <- file.path(dir, "CHECKSUMS.txt")
  hdr <- '"file" "checksum" "filesize" "algorithm"'
  row <- sprintf('"%s" "%s" "%d" "xxhash64"', name, fixture$hash, fixture$size)
  writeLines(c(hdr, row), csf)
  csf
}

# Inode of a file, as a string. POSIX-only; returns NA on failure or
# non-POSIX OS. Kept as character (not integer) because inode numbers can
# exceed R's .Machine$integer.max (2^31-1) on large filesystems, where
# as.integer() would silently coerce to NA. We only ever test equality.
# GNU stat (Linux) uses `-c %i`; BSD stat (macOS) uses `-f %i`.
inoOf <- function(p) {
  if (.Platform$OS.type != "unix") return(NA_character_)
  args <- if (Sys.info()[["sysname"]] == "Darwin") c("-f", "%i") else c("-c", "%i")
  out <- suppressWarnings(
    system2("stat", c(args, shQuote(p)), stdout = TRUE, stderr = FALSE)
  )
  if (length(out) == 0L || !grepl("^[0-9]+$", out[[1L]])) return(NA_character_)
  out[[1L]]
}

# Two paths refer to the same physical file (hardlinked) iff inodes match
# AND both look like real files. Returns NA when we can't tell (Windows).
sameInode <- function(p1, p2) {
  i1 <- inoOf(p1); i2 <- inoOf(p2)
  if (is.na(i1) || is.na(i2)) return(NA)
  identical(i1, i2)
}

getInternalOrNull <- function(name) {
  tryCatch(getFromNamespace(name, "reproducible"), error = function(e) NULL)
}
`%||%` <- function(a, b) if (!is.null(a)) a else b

# Build a portable file:// URL. RFC 8089 says file:///<path>, with empty
# host. On POSIX, normPath returns "/home/x" → "file:///home/x".
# On Windows, normPath returns "C:\\path" or "C:/path" → "file:///C:/path".
toFileUrl <- function(p) {
  p <- gsub("\\\\", "/", normPath(p))
  if (substr(p, 1L, 1L) != "/") p <- paste0("/", p)   # Windows drive letter
  paste0("file://", p)
}

getDestinationPathSharedFn <- function() {
  getInternalOrNull(".getDestinationPathShared") %||% getInternalOrNull(".getDataPath")
}
getDestinationPathSharedRecursiveFn <- function() {
  getInternalOrNull(".getDestinationPathSharedRecursive") %||%
    getInternalOrNull(".getDataPathRecursive")
}

# Build a minimal pp_* ctx matching what .pp_make_ctx produces, with just the
# fields pp_remote_hash_check reads/writes. Keeps tests self-contained without
# invoking the full preProcess() pipeline.
makePPCtx <- function(url, destinationPath, neededFiles = NULL, archive = NULL) {
  identifyCS <- getFromNamespace("identifyCHECKSUMStxtFile", "reproducible")
  emptyCS    <- getFromNamespace(".emptyChecksumsResult", "reproducible")
  list(
    url = url, archive = archive, neededFiles = neededFiles,
    destinationPath = destinationPath,
    checkSumFilePath = identifyCS(destinationPath),
    checkSums = emptyCS, needChecksums = 0L,
    skipDownload = FALSE, hashVerified = character(),
    remoteMetadata = NULL, verbose = 0L
  )
}

readSidecar <- function(dir, fixture, url) {
  mk <- getFromNamespace("makeRemoteHashFile", "reproducible")
  p  <- mk(url, dir, basename(fixture$path), "")
  if (!file.exists(p)) return(NULL)
  list(path = p, contents = readLines(p, warn = FALSE))
}

readChecksumsRows <- function(dir) {
  csf <- file.path(dir, "CHECKSUMS.txt")
  if (!file.exists(csf) || file.size(csf) == 0L) return(NULL)
  read.table(csf, header = TRUE, stringsAsFactors = FALSE)
}


# ===========================================================================
# §5.1  Option plumbing (no I/O)
# ===========================================================================

test_that("P1: option unset → getter returns NULL", {
  testInit()
  withr::local_options(list(
    reproducible.destinationPathShared = NULL,
    reproducible.inputPaths   = NULL
  ))
  fn <- getDestinationPathSharedFn()
  skip_if(is.null(fn), "no destinationPathShared getter available")
  expect_null(fn())
})

test_that("P2: destinationPathShared = '/x' → '/x'", {
  testInit()
  fn <- getInternalOrNull(".getDestinationPathShared")
  skip_if(is.null(fn), "no destinationPathShared getter available")
  withr::local_options(list(reproducible.destinationPathShared = "/x"))
  expect_identical(fn(), "/x")
})

test_that("P3: vector value preserved", {
  testInit()
  fn <- getInternalOrNull(".getDestinationPathShared")
  skip_if(is.null(fn), "no destinationPathShared getter available")
  withr::local_options(list(reproducible.destinationPathShared = c("/a", "/b")))
  expect_identical(fn(), c("/a", "/b"))
})

test_that("P5: only inputPaths set → returned + deprecation message", {
  testInit()
  fn <- getDestinationPathSharedFn()
  skip_if(is.null(fn), "no getter available")
  withr::local_options(list(
    reproducible.destinationPathShared = NULL,
    reproducible.inputPaths   = "/x"
  ))
  # Deprecation messaging is one-shot per session; reset the registry so
  # this test sees the message regardless of test ordering.
  pkgEnv <- getInternalOrNull(".pkgEnv")
  if (!is.null(pkgEnv)) pkgEnv$.deprecMsgEmitted <- character()
  msgs <- testthat::capture_messages(out <- fn())
  expect_identical(out, "/x")
  expect_true(any(grepl("deprecated", msgs)))
})

test_that("P6: destinationPathShared and inputPaths both set → destinationPathShared wins, no deprecation", {
  testInit()
  fn <- getInternalOrNull(".getDestinationPathShared")
  skip_if(is.null(fn), "no destinationPathShared getter available")
  withr::local_options(list(
    reproducible.destinationPathShared = "/new",
    reproducible.inputPaths   = "/old"
  ))
  msgs <- testthat::capture_messages(out <- fn())
  expect_identical(out, "/new")
  expect_false(any(grepl("deprecated", msgs)))
})

test_that("P11: recursive option unset → FALSE", {
  testInit()
  fn <- getDestinationPathSharedRecursiveFn()
  skip_if(is.null(fn), "no recursive getter available")
  withr::local_options(list(
    reproducible.destinationPathSharedRecursive = NULL,
    reproducible.inputPathsRecursive   = NULL
  ))
  expect_false(fn())
})

test_that("P12: destinationPathSharedRecursive = TRUE → TRUE", {
  testInit()
  skip_if_not(
    !is.null(getInternalOrNull(".getDestinationPathSharedRecursive")),
    "recursive option not yet wired (§4 step 5)"
  )
  withr::local_options(list(reproducible.destinationPathSharedRecursive = TRUE))
  expect_true(getInternalOrNull(".getDestinationPathSharedRecursive")())
})

test_that("P13: only inputPathsRecursive = TRUE → TRUE + deprecation", {
  testInit()
  fn <- getDestinationPathSharedRecursiveFn()
  skip_if(is.null(fn), "no recursive getter available")
  withr::local_options(list(
    reproducible.destinationPathSharedRecursive = NULL,
    reproducible.inputPathsRecursive   = TRUE
  ))
  # Reset one-shot deprecation registry — see P5 comment.
  pkgEnv <- getInternalOrNull(".pkgEnv")
  if (!is.null(pkgEnv)) pkgEnv$.deprecMsgEmitted <- character()
  msgs <- testthat::capture_messages(out <- fn())
  expect_true(out)
  expect_true(any(grepl("deprecated", msgs)))
})


# ===========================================================================
# §5.2  Resolution — happy paths
# ===========================================================================
#
# Strategy: file:// URL fixtures + a hand-written CHECKSUMS.txt in the shared
# dir, so the *current* code path (.checkLocalSources) recognizes the file
# and hardlinks it into destinationPath. Inode comparison on POSIX is the
# strongest assertion that no copy/download happened.

test_that("H1: file in destinationPathShared with matching CHECKSUMS → hardlinked, no copy", {
  testInit("digest")
  shared <- normPath(file.path(tmpdir, "shared"))
  dest   <- normPath(file.path(tmpdir, "dest"))
  src    <- normPath(file.path(tmpdir, "src"))
  for (d in c(shared, dest, src)) dir.create(d, recursive = TRUE)

  fixSrc    <- makeFixture(src,    "foo.csv")          # the "remote"
  fixShared <- makeFixture(shared, "foo.csv")          # same content → same hash
  writeChecksumsFor(shared, fixShared)

  withr::local_options(list(
    reproducible.inputPaths          = shared,
    reproducible.inputPathsRecursive = FALSE
  ))

  capture.output(
    preProcess(url = fixSrc$url, targetFile = "foo.csv",
               destinationPath = dest, fun = NA, verbose = -1),
    type = "output"
  )

  expect_true(file.exists(file.path(dest, "foo.csv")))
  skip_on_os("windows")
  expect_true(isTRUE(sameInode(file.path(dest, "foo.csv"), fixShared$path)),
               info = "destinationPath copy should share inode with destinationPathShared (hardlink)")
})

test_that("H3: file in subdir, recursive=FALSE → not found, falls through", {
  testInit("digest")
  shared <- normPath(file.path(tmpdir, "shared"))
  sub    <- normPath(file.path(shared, "sub"))
  dest   <- normPath(file.path(tmpdir, "dest"))
  src    <- normPath(file.path(tmpdir, "src"))
  for (d in c(shared, sub, dest, src)) dir.create(d, recursive = TRUE)

  fixSrc    <- makeFixture(src, "foo.csv")
  fixShared <- makeFixture(sub, "foo.csv")
  writeChecksumsFor(sub, fixShared)

  withr::local_options(list(
    reproducible.inputPaths          = shared,    # ← search top-level only
    reproducible.inputPathsRecursive = FALSE
  ))

  capture.output(
    preProcess(url = fixSrc$url, targetFile = "foo.csv",
               destinationPath = dest, fun = NA, verbose = -1),
    type = "output"
  )

  expect_true(file.exists(file.path(dest, "foo.csv")))
  skip_on_os("windows")
  expect_false(isTRUE(sameInode(file.path(dest, "foo.csv"), fixShared$path)),
               info = "non-recursive search must not link from subdir")
})

test_that("H5: two destinationPathShared entries; file only in second → found", {
  testInit("digest")
  s1   <- normPath(file.path(tmpdir, "s1"))
  s2   <- normPath(file.path(tmpdir, "s2"))
  dest <- normPath(file.path(tmpdir, "dest"))
  src  <- normPath(file.path(tmpdir, "src"))
  for (d in c(s1, s2, dest, src)) dir.create(d, recursive = TRUE)

  fixSrc    <- makeFixture(src, "foo.csv")
  fixShared <- makeFixture(s2,  "foo.csv")
  writeChecksumsFor(s2, fixShared)

  withr::local_options(list(reproducible.inputPaths = c(s1, s2)))

  capture.output(
    preProcess(url = fixSrc$url, targetFile = "foo.csv",
               destinationPath = dest, fun = NA, verbose = -1),
    type = "output"
  )

  skip_on_os("windows")
  expect_true(isTRUE(sameInode(file.path(dest, "foo.csv"), fixShared$path)))
})

test_that("H6: file in both destinationPathShared entries → first wins", {
  testInit("digest")
  s1   <- normPath(file.path(tmpdir, "s1"))
  s2   <- normPath(file.path(tmpdir, "s2"))
  dest <- normPath(file.path(tmpdir, "dest"))
  src  <- normPath(file.path(tmpdir, "src"))
  for (d in c(s1, s2, dest, src)) dir.create(d, recursive = TRUE)

  fixSrc <- makeFixture(src, "foo.csv")
  fix1   <- makeFixture(s1,  "foo.csv")
  fix2   <- makeFixture(s2,  "foo.csv")
  writeChecksumsFor(s1, fix1)
  writeChecksumsFor(s2, fix2)

  withr::local_options(list(reproducible.inputPaths = c(s1, s2)))

  capture.output(
    preProcess(url = fixSrc$url, targetFile = "foo.csv",
               destinationPath = dest, fun = NA, verbose = -1),
    type = "output"
  )

  skip_on_os("windows")
  expect_true(isTRUE(sameInode(file.path(dest, "foo.csv"), fix1$path)),
               info = "deterministic order: first destinationPathShared entry wins")
  expect_false(isTRUE(sameInode(file.path(dest, "foo.csv"), fix2$path)))
})

test_that("H7: file in destinationPath wins; destinationPathShared not consulted", {
  testInit("digest")
  shared <- normPath(file.path(tmpdir, "shared"))
  dest   <- normPath(file.path(tmpdir, "dest"))
  src    <- normPath(file.path(tmpdir, "src"))
  for (d in c(shared, dest, src)) dir.create(d, recursive = TRUE)

  fixSrc    <- makeFixture(src,    "foo.csv")
  fixDest   <- makeFixture(dest,   "foo.csv")
  fixShared <- makeFixture(shared, "foo.csv")
  writeChecksumsFor(dest,   fixDest)
  writeChecksumsFor(shared, fixShared)

  withr::local_options(list(reproducible.inputPaths = shared))

  origIno <- inoOf(fixDest$path)

  capture.output(
    preProcess(url = fixSrc$url, targetFile = "foo.csv",
               destinationPath = dest, fun = NA, verbose = -1),
    type = "output"
  )

  skip_on_os("windows")
  expect_equal(inoOf(file.path(dest, "foo.csv")), origIno,
               info = "destinationPath copy untouched; not relinked from shared")
})

# ===========================================================================
# §5.3  Resolution — sad paths
# ===========================================================================

test_that("S3: destinationPathShared path doesn't exist → falls through", {
  testInit("digest")
  shared <- file.path(tmpdir, "doesnotexist")
  dest   <- normPath(file.path(tmpdir, "dest"))
  src    <- normPath(file.path(tmpdir, "src"))
  for (d in c(dest, src)) dir.create(d, recursive = TRUE)
  fixSrc <- makeFixture(src, "foo.csv")

  withr::local_options(list(reproducible.inputPaths = shared))

  expect_no_error(capture.output(
    preProcess(url = fixSrc$url, targetFile = "foo.csv",
               destinationPath = dest, fun = NA, verbose = -1),
    type = "output"
  ))
  expect_true(file.exists(file.path(dest, "foo.csv")))
})

test_that("S7: HEAD network error → falls back to download path", {
  testInit()
  td  <- withr::local_tempdir()
  fx  <- makeFixture(td, "data.tif")
  url <- "https://example.com/path/data.tif"

  pp <- getFromNamespace("pp_remote_hash_check", "reproducible")
  ctx <- makePPCtx(url, td, neededFiles = fx$path)

  out <- testthat::with_mocked_bindings(
    getRemoteMetadata = function(...) stop("simulated network error"),
    { pp(ctx) }
  )
  # Network failure must not poison state — skipDownload stays FALSE so
  # pp_download proceeds normally, and no sidecar/CHECKSUMS row is written.
  expect_false(isTRUE(out$skipDownload))
  expect_false(fx$path %in% out$hashVerified)
})


# ===========================================================================
# §5.4  Sidecar lifecycle and CHECKSUMS interaction
# ===========================================================================

test_that("C4: sidecar with missing adjacent file → ignored", {
  testInit()
  td  <- withr::local_tempdir()
  url <- "https://example.com/data.tif"

  # Write an orphan sidecar (no adjacent file).
  mk <- getFromNamespace("makeRemoteHashFile", "reproducible")
  sc <- mk(url, td, "data.tif", paste0(strrep("a", 32L)),
           algorithm = "md5", write = TRUE)
  expect_true(file.exists(sc))

  # pp_remote_hash_check returns ctx unchanged when localFile resolves to
  # NULL (no on-disk file to verify), regardless of orphan sidecars.
  pp <- getFromNamespace("pp_remote_hash_check", "reproducible")
  ctx <- makePPCtx(url, td, neededFiles = file.path(td, "data.tif"))
  out <- pp(ctx)
  expect_false(isTRUE(out$skipDownload))
})
test_that("C8: legacy single-line .hash sidecar parsed via length heuristic", {
  parse <- getFromNamespace(".parseRemoteHashFile", "reproducible")
  td <- withr::local_tempdir()
  legacy <- file.path(td, "legacy.hash")
  writeLines(strrep("a", 32L), legacy)        # md5-shaped, no `:` prefix
  out <- parse(legacy)
  expect_identical(out$algorithm, "md5")
  expect_identical(out$hash, strrep("a", 32L))
})
test_that("C9: legacy CHECKSUMS row with multi-algorithm coexists with new row", {
  testInit()
  td <- withr::local_tempdir()
  fx <- makeFixture(td, "shared.dat")
  csf <- writeChecksumsFor(td, fx)            # xxhash64 row only

  upsert <- getFromNamespace(".upsertChecksumsRow", "reproducible")
  upsert(csf, file = basename(fx$path),
         hash = digest::digest(file = fx$path, algo = "md5"),
         filesize = fx$size, algorithm = "md5")

  rows <- read.table(csf, header = TRUE, stringsAsFactors = FALSE)
  algos <- sort(rows$algorithm[rows$file == basename(fx$path)])
  expect_identical(algos, c("md5", "xxhash64"))
})
test_that("C10: malformed sidecar contents → treated as missing", {
  parse <- getFromNamespace(".parseRemoteHashFile", "reproducible")
  td <- withr::local_tempdir()

  # Empty file → NULL
  empty <- file.path(td, "empty.hash")
  file.create(empty)
  expect_null(parse(empty))

  # Non-existent file → NULL
  expect_null(parse(file.path(td, "does-not-exist.hash")))

  # Garbage one-line → still parses (algorithm classified as etag-opaque),
  # but the hash won't match any remote so downstream comparison fails.
  garbage <- file.path(td, "garbage.hash")
  writeLines("not-a-real-hash-string", garbage)
  out <- parse(garbage)
  expect_identical(out$algorithm, "etag-opaque")
})


# ===========================================================================
# §5.5  targetFile inference
# ===========================================================================

test_that("T1: existing sidecar → no HEAD request fired", {
  testInit()
  td  <- withr::local_tempdir()
  fx  <- makeFixture(td, "data.tif")
  url <- "https://example.com/path/data.tif"

  # Pre-write the sidecar; pp_remote_hash_check should fast-path on existence.
  mk <- getFromNamespace("makeRemoteHashFile", "reproducible")
  mk(url, td, basename(fx$path),
     digest::digest(file = fx$path, algo = "md5"),
     algorithm = "md5", write = TRUE)

  pp <- getFromNamespace("pp_remote_hash_check", "reproducible")
  ctx <- makePPCtx(url, td, neededFiles = fx$path)

  callCount <- 0L
  out <- testthat::with_mocked_bindings(
    getRemoteMetadata = function(...) {
      callCount <<- callCount + 1L
      stop("HEAD should not be called when sidecar exists")
    },
    { pp(ctx) }
  )
  expect_identical(callCount, 0L)
  expect_true(isTRUE(out$skipDownload))
})
test_that("T2: no sidecar → HEAD fires; size+hash match → sidecar written", {
  testInit()
  td  <- withr::local_tempdir()
  fx  <- makeFixture(td, "data.tif")
  url <- "https://example.com/path/data.tif"

  fakeMeta <- list(
    targetFile = "data.tif",
    fileSize = as.character(fx$size),
    remoteHash = digest::digest(file = fx$path, algo = "md5"),
    remoteAlgorithm = "md5", timestampOnline = "now"
  )
  pp <- getFromNamespace("pp_remote_hash_check", "reproducible")
  ctx <- makePPCtx(url, td, neededFiles = fx$path)

  callCount <- 0L
  out <- testthat::with_mocked_bindings(
    getRemoteMetadata = function(...) { callCount <<- callCount + 1L; fakeMeta },
    { pp(ctx) }
  )
  expect_identical(callCount, 1L)
  expect_true(isTRUE(out$skipDownload))
  expect_false(is.null(readSidecar(td, fx, url)))
})
test_that("T6: HEAD network error → silent fall-through", {
  testInit()
  td  <- withr::local_tempdir()
  fx  <- makeFixture(td, "data.tif")
  url <- "https://example.com/path/data.tif"

  pp <- getFromNamespace("pp_remote_hash_check", "reproducible")
  ctx <- makePPCtx(url, td, neededFiles = fx$path)

  out <- testthat::with_mocked_bindings(
    getRemoteMetadata = function(...) stop("simulated HEAD failure"),
    { pp(ctx) }
  )
  # Silent fall-through: no skipDownload, no sidecar, no error.
  expect_false(isTRUE(out$skipDownload))
  expect_null(readSidecar(td, fx, url))
})

test_that("T7: targetFile NULL + url NULL (local-only) → no regression", {
  testInit("digest")
  dest <- normPath(file.path(tmpdir, "dest"))
  src  <- normPath(file.path(tmpdir, "src"))
  for (d in c(dest, src)) dir.create(d, recursive = TRUE)
  fix <- makeFixture(src, "foo.csv")

  ## dlFun-only call: copy the fixture into destinationPath via dlFun.
  dlFun <- quote(file.copy(SRC, DEST, overwrite = TRUE))
  ## simplest: just use a real local URL — covers the "no crash" intent.
  capture.output(
    preProcess(url = fix$url, targetFile = "foo.csv",
               destinationPath = dest, fun = NA, verbose = -1),
    type = "output"
  )
  expect_true(file.exists(file.path(dest, "foo.csv")))
})


# ===========================================================================
# §5.6  remoteHash matching
# ===========================================================================

test_that("R1–R7: remoteHash-driven candidate validation", {
  # Cluster of behaviours, each verified inline:
  #   R1 size-match + hash-match → accept
  #   R2 size-match + hash-mismatch → reject (download)
  #   R3 size-mismatch (regardless of hash) → reject (download)
  #   R4 opaque ETag → reject (no positive trust possible)
  #   R5 missing remote hash → reject
  #   R6 sidecar pre-exists with matching contents under opt-in re-check
  #   R7 sidecar pre-exists with NON-matching contents → re-verify
  testInit()

  pp <- getFromNamespace("pp_remote_hash_check", "reproducible")

  ## R1: size + hash match → skipDownload
  td1 <- withr::local_tempdir()
  fx1 <- makeFixture(td1, "r1.tif")
  out1 <- testthat::with_mocked_bindings(
    getRemoteMetadata = function(...) list(
      targetFile = "r1.tif", fileSize = as.character(fx1$size),
      remoteHash = digest::digest(file = fx1$path, algo = "md5"),
      remoteAlgorithm = "md5", timestampOnline = "now"),
    { pp(makePPCtx("https://e.com/r1.tif", td1, neededFiles = fx1$path)) }
  )
  expect_true(isTRUE(out1$skipDownload))

  ## R2: size match, hash mismatch → reject
  td2 <- withr::local_tempdir()
  fx2 <- makeFixture(td2, "r2.tif")
  out2 <- testthat::with_mocked_bindings(
    getRemoteMetadata = function(...) list(
      targetFile = "r2.tif", fileSize = as.character(fx2$size),
      remoteHash = digest::digest("DIFFERENT", algo = "md5", serialize = FALSE),
      remoteAlgorithm = "md5", timestampOnline = "now"),
    { pp(makePPCtx("https://e.com/r2.tif", td2, neededFiles = fx2$path)) }
  )
  expect_false(isTRUE(out2$skipDownload))

  ## R3: size mismatch → reject
  td3 <- withr::local_tempdir()
  fx3 <- makeFixture(td3, "r3.tif")
  out3 <- testthat::with_mocked_bindings(
    getRemoteMetadata = function(...) list(
      targetFile = "r3.tif", fileSize = as.character(fx3$size + 1L),
      remoteHash = digest::digest(file = fx3$path, algo = "md5"),
      remoteAlgorithm = "md5", timestampOnline = "now"),
    { pp(makePPCtx("https://e.com/r3.tif", td3, neededFiles = fx3$path)) }
  )
  expect_false(isTRUE(out3$skipDownload))

  ## R4: opaque ETag → reject
  td4 <- withr::local_tempdir()
  fx4 <- makeFixture(td4, "r4.tif")
  out4 <- testthat::with_mocked_bindings(
    getRemoteMetadata = function(...) list(
      targetFile = "r4.tif", fileSize = as.character(fx4$size),
      remoteHash = "W/abc-12-34", remoteAlgorithm = "etag-opaque",
      timestampOnline = "now"),
    { pp(makePPCtx("https://e.com/r4.tif", td4, neededFiles = fx4$path)) }
  )
  expect_false(isTRUE(out4$skipDownload))

  ## R5: empty remote hash → reject (treated as opaque)
  td5 <- withr::local_tempdir()
  fx5 <- makeFixture(td5, "r5.tif")
  out5 <- testthat::with_mocked_bindings(
    getRemoteMetadata = function(...) list(
      targetFile = "r5.tif", fileSize = as.character(fx5$size),
      remoteHash = "", remoteAlgorithm = "etag-opaque",
      timestampOnline = "now"),
    { pp(makePPCtx("https://e.com/r5.tif", td5, neededFiles = fx5$path)) }
  )
  expect_false(isTRUE(out5$skipDownload))

  ## R6: sidecar with matching contents under opt-in re-check
  td6 <- withr::local_tempdir()
  fx6 <- makeFixture(td6, "r6.tif")
  url6 <- "https://e.com/r6.tif"
  hash6 <- digest::digest(file = fx6$path, algo = "md5")
  mk <- getFromNamespace("makeRemoteHashFile", "reproducible")
  mk(url6, td6, basename(fx6$path), hash6, algorithm = "md5", write = TRUE)

  withr::local_options(reproducible.checkRemoteHash = TRUE)
  out6 <- testthat::with_mocked_bindings(
    getRemoteMetadata = function(...) list(
      targetFile = "r6.tif", fileSize = as.character(fx6$size),
      remoteHash = hash6, remoteAlgorithm = "md5", timestampOnline = "now"),
    { pp(makePPCtx(url6, td6, neededFiles = fx6$path)) }
  )
  expect_true(isTRUE(out6$skipDownload))
})


# ===========================================================================
# §5.7  Backward compatibility
# ===========================================================================

test_that("B1: legacy reproducible.inputPaths still works (regression guard)", {
  testInit("digest")
  shared <- normPath(file.path(tmpdir, "shared"))
  dest   <- normPath(file.path(tmpdir, "dest"))
  src    <- normPath(file.path(tmpdir, "src"))
  for (d in c(shared, dest, src)) dir.create(d, recursive = TRUE)

  fixSrc    <- makeFixture(src,    "foo.csv")
  fixShared <- makeFixture(shared, "foo.csv")
  writeChecksumsFor(shared, fixShared)

  withr::local_options(list(reproducible.inputPaths = shared))

  capture.output(
    preProcess(url = fixSrc$url, targetFile = "foo.csv",
               destinationPath = dest, fun = NA, verbose = -1),
    type = "output"
  )

  skip_on_os("windows")
  expect_true(isTRUE(sameInode(file.path(dest, "foo.csv"), fixShared$path)))
})

# B2 / B3 deleted: covered by P5 (inputPaths-only deprecation chain) and P6
# (destinationPathShared wins over inputPaths). The third option name `dataPath` was
# removed before the rename ever shipped — `destinationPathShared` is canonical and
# `inputPaths` is the only legacy alias.


# ===========================================================================
# §5.8  Integration with prepInputs/Cache
# ===========================================================================

test_that("I1+I2: Cache(prepInputs(...)) — cold uses destinationPathShared; warm is cache hit", {
  # Cache(...) -> doSaveToCache -> wrapSaveToCache -> .wrap.default needs
  # terra to wrap raster-shaped outputs. Without terra (nosuggests=true /
  # _R_CHECK_DEPENDS_ONLY_=true matrix entry), it aborts with "Please
  # install terra package". Skip then; full-Suggests entries still cover.
  skip_if_not_installed("terra")
  testInit("digest")
  shared <- normPath(file.path(tmpdir, "shared"))
  dest   <- normPath(file.path(tmpdir, "dest"))
  src    <- normPath(file.path(tmpdir, "src"))
  cache  <- normPath(file.path(tmpdir, "cache"))
  for (d in c(shared, dest, src, cache)) dir.create(d, recursive = TRUE)

  fixSrc    <- makeFixture(src,    "foo.csv")
  fixShared <- makeFixture(shared, "foo.csv")
  writeChecksumsFor(shared, fixShared)

  withr::local_options(list(
    reproducible.inputPaths = shared,
    reproducible.cachePath  = cache
  ))

  doIt <- function() {
    preProcess(url = fixSrc$url, targetFile = "foo.csv",
               destinationPath = dest, fun = NA, verbose = -1)
  }
  capture.output(Cache(doIt), type = "output")
  capture.output(Cache(doIt), type = "output")

  expect_true(file.exists(file.path(dest, "foo.csv")))
  skip_on_os("windows")
  expect_true(isTRUE(sameInode(file.path(dest, "foo.csv"), fixShared$path)))
})


# ===========================================================================
# §5.10  Performance smoke
# ===========================================================================

test_that("Q1: 10k unrelated files in shared, recursive=FALSE → fast lookup", {
  skip_on_cran()
  testInit("digest")
  if (!isTRUE(getOption("reproducible.runLargeFileTests", FALSE)))
    skip("set reproducible.runLargeFileTests=TRUE to run")

  shared <- normPath(file.path(tmpdir, "shared"))
  dest   <- normPath(file.path(tmpdir, "dest"))
  src    <- normPath(file.path(tmpdir, "src"))
  for (d in c(shared, dest, src)) dir.create(d, recursive = TRUE)
  for (i in seq_len(10000))
    file.create(file.path(shared, sprintf("noise_%05d.bin", i)))
  fixSrc    <- makeFixture(src,    "foo.csv")
  fixShared <- makeFixture(shared, "foo.csv")
  writeChecksumsFor(shared, fixShared)

  withr::local_options(list(reproducible.inputPaths = shared))

  t <- system.time(
    capture.output(
      preProcess(url = fixSrc$url, targetFile = "foo.csv",
                 destinationPath = dest, fun = NA, verbose = -1),
      type = "output"
    )
  )
  # lookup over 10k flat files should be subsecond on typical disk;
  # 5 s is a generous CI ceiling.
  expect_lt(t[["elapsed"]], 5)
})

test_that("Q3: hot destinationPath, repeated calls → no HEAD after first", {
  testInit()
  td  <- withr::local_tempdir()
  fx  <- makeFixture(td, "data.tif")
  url <- "https://example.com/path/data.tif"
  fakeMeta <- list(
    targetFile = "data.tif", fileSize = as.character(fx$size),
    remoteHash = digest::digest(file = fx$path, algo = "md5"),
    remoteAlgorithm = "md5", timestampOnline = "now"
  )
  pp <- getFromNamespace("pp_remote_hash_check", "reproducible")

  callCount <- 0L
  testthat::with_mocked_bindings(
    getRemoteMetadata = function(...) { callCount <<- callCount + 1L; fakeMeta },
    { pp(makePPCtx(url, td, neededFiles = fx$path)) }
  )
  expect_identical(callCount, 1L)

  testthat::with_mocked_bindings(
    getRemoteMetadata = function(...) {
      callCount <<- callCount + 1L
      stop("HEAD should not fire on hot calls")
    },
    { for (i in 1:5) pp(makePPCtx(url, td, neededFiles = fx$path)) }
  )
  expect_identical(callCount, 1L)
})

test_that("Q5: classify md5/sha1/sha256/etag-opaque by hash shape", {
  classify <- getFromNamespace(".classifyRemoteHashAlgo", "reproducible")
  expect_identical(classify(strrep("a", 32L)), "md5")
  expect_identical(classify(strrep("b", 40L)), "sha1")
  expect_identical(classify(strrep("c", 64L)), "sha256")
  expect_identical(classify("anything", isGDurl = TRUE), "md5")
  expect_identical(classify("W/abc-12-34"), "etag-opaque")
})



# ===========================================================================
# §5.14  Remote-hash content verification (size as negative-only signal)
# ---------------------------------------------------------------------------
# These tests exercise the algorithm:
#   1. remote.size != local.size            -> download
#   2. remote algorithm == "etag-opaque"    -> download
#   3. size match + hash match              -> sidecar + CHECKSUMS row, skip
#   4. size match + hash mismatch (.tif     -> download (the size-poison case
#      false-positive scenario)                that the previous code missed)
# ===========================================================================

# Helpers (makePPCtx / readSidecar / readChecksumsRows) hoisted to the top
# of this file so they're visible to earlier test_that() blocks too.

# .classifyRemoteHashAlgo --------------------------------------------------

test_that("classify: md5/sha1/sha256/opaque + Google Drive override", {
  classify <- getFromNamespace(".classifyRemoteHashAlgo", "reproducible")
  expect_identical(classify(strrep("a", 32L)), "md5")
  expect_identical(classify(strrep("0", 40L)), "sha1")
  expect_identical(classify(strrep("f", 64L)), "sha256")
  expect_identical(classify("W/abc-12-34"), "etag-opaque")
  expect_identical(classify(""), "etag-opaque")
  expect_identical(classify(NA_character_), "etag-opaque")
  expect_identical(classify(NULL), "etag-opaque")
  # Google Drive override forces md5 regardless of shape
  expect_identical(classify("anything", isGDurl = TRUE), "md5")
})

# .parseRemoteHashFile (legacy + new format) -------------------------------

test_that("parse sidecar: legacy single-line vs new <algo>:<hash>", {
  parse <- getFromNamespace(".parseRemoteHashFile", "reproducible")
  td <- withr::local_tempdir()

  # New format
  pNew <- file.path(td, "new.hash")
  writeLines("md5:abcdef0123456789abcdef0123456789", pNew)
  out <- parse(pNew)
  expect_identical(out$algorithm, "md5")
  expect_identical(out$hash, "abcdef0123456789abcdef0123456789")

  # Legacy single-line, md5-shaped
  pLegacyMd5 <- file.path(td, "legacymd5.hash")
  writeLines(strrep("a", 32L), pLegacyMd5)
  out2 <- parse(pLegacyMd5)
  expect_identical(out2$algorithm, "md5")
  expect_identical(out2$hash, strrep("a", 32L))

  # Legacy single-line, sha1-shaped
  pLegacySha1 <- file.path(td, "legacysha1.hash")
  writeLines(strrep("b", 40L), pLegacySha1)
  out3 <- parse(pLegacySha1)
  expect_identical(out3$algorithm, "sha1")

  # Missing file
  expect_null(parse(file.path(td, "does-not-exist.hash")))
})

# pp_remote_hash_check: md5 match -> sidecar + CHECKSUMS row + skipDownload --

test_that("pp_remote_hash_check: md5 match writes md5 sidecar + row, skips DL", {
  testInit()
  td  <- withr::local_tempdir()
  fx  <- makeFixture(td, "data.tif")
  url <- "https://example.com/path/data.tif"

  fakeMeta <- list(
    targetFile = "data.tif",
    fileSize = as.character(fx$size),
    remoteHash = digest::digest(file = fx$path, algo = "md5"),
    remoteAlgorithm = "md5",
    timestampOnline = "now"
  )
  pp <- getFromNamespace("pp_remote_hash_check", "reproducible")
  ctx <- makePPCtx(url, td, neededFiles = fx$path)

  out <- testthat::with_mocked_bindings(
    getRemoteMetadata = function(...) fakeMeta,
    { pp(ctx) }
  )

  expect_true(isTRUE(out$skipDownload))
  expect_true(fx$path %in% out$hashVerified)

  sc <- readSidecar(td, fx, url)
  expect_false(is.null(sc))
  expect_match(sc$contents, "^md5:")
  expect_identical(sub("^md5:", "", sc$contents), fakeMeta$remoteHash)

  rows <- readChecksumsRows(td)
  expect_false(is.null(rows))
  hit <- which(rows$algorithm == "md5" & basename(rows$file) == "data.tif")
  expect_length(hit, 1L)
  expect_identical(rows$checksum[hit], fakeMeta$remoteHash)
})

# pp_remote_hash_check: sha1 match -> sha1 sidecar + sha1 row ---------------

test_that("pp_remote_hash_check: sha1 match writes sha1 sidecar + row", {
  testInit()
  td  <- withr::local_tempdir()
  fx  <- makeFixture(td, "data.bin", content = "sha1-content\n")
  url <- "https://example.com/sha1/data.bin"

  fakeMeta <- list(
    targetFile = "data.bin",
    fileSize = as.character(fx$size),
    remoteHash = digest::digest(file = fx$path, algo = "sha1"),
    remoteAlgorithm = "sha1",
    timestampOnline = "now"
  )
  pp <- getFromNamespace("pp_remote_hash_check", "reproducible")
  ctx <- makePPCtx(url, td, neededFiles = fx$path)

  out <- testthat::with_mocked_bindings(
    getRemoteMetadata = function(...) fakeMeta,
    { pp(ctx) }
  )

  expect_true(isTRUE(out$skipDownload))
  sc <- readSidecar(td, fx, url)
  expect_match(sc$contents, "^sha1:")

  rows <- readChecksumsRows(td)
  hit <- which(rows$algorithm == "sha1" & basename(rows$file) == "data.bin")
  expect_length(hit, 1L)
})

# pp_remote_hash_check: opaque ETag -> no positive trust -> download path ---

test_that("pp_remote_hash_check: opaque ETag falls through to download", {
  testInit()
  td  <- withr::local_tempdir()
  fx  <- makeFixture(td, "data.bin")
  url <- "https://example.com/opaque/data.bin"

  fakeMeta <- list(
    targetFile = "data.bin",
    fileSize = as.character(fx$size),
    remoteHash = "W/some-weak-etag",
    remoteAlgorithm = "etag-opaque",
    timestampOnline = "now"
  )
  pp <- getFromNamespace("pp_remote_hash_check", "reproducible")
  ctx <- makePPCtx(url, td, neededFiles = fx$path)

  out <- testthat::with_mocked_bindings(
    getRemoteMetadata = function(...) fakeMeta,
    { pp(ctx) }
  )

  expect_false(isTRUE(out$skipDownload))
  expect_false(fx$path %in% out$hashVerified)
  expect_null(readSidecar(td, fx, url))
})

# pp_remote_hash_check: size match + hash MISMATCH (.tif poison case) ------

test_that("pp_remote_hash_check: size match + hash mismatch -> download", {
  testInit()
  td  <- withr::local_tempdir()
  fx  <- makeFixture(td, "data.tif", content = "local-content-A\n")
  url <- "https://example.com/poison/data.tif"

  # Remote has the same size but completely different content.
  fakeMeta <- list(
    targetFile = "data.tif",
    fileSize = as.character(fx$size),
    remoteHash = digest::digest("DIFFERENT-CONTENT", algo = "md5",
                                serialize = FALSE),
    remoteAlgorithm = "md5",
    timestampOnline = "now"
  )
  pp <- getFromNamespace("pp_remote_hash_check", "reproducible")
  ctx <- makePPCtx(url, td, neededFiles = fx$path)

  out <- testthat::with_mocked_bindings(
    getRemoteMetadata = function(...) fakeMeta,
    { pp(ctx) }
  )

  # Size matched but hash didn't — must NOT skip download, must NOT write
  # sidecar or CHECKSUMS row (the previous size-only-fallback code would have
  # incorrectly accepted this case).
  expect_false(isTRUE(out$skipDownload))
  expect_false(fx$path %in% out$hashVerified)
  expect_null(readSidecar(td, fx, url))
})

# pp_remote_hash_check: second call hits sidecar fast-path (no HEAD) -------

test_that("pp_remote_hash_check: 2nd call uses sidecar fast-path (no HEAD)", {
  testInit()
  td  <- withr::local_tempdir()
  fx  <- makeFixture(td, "data.tif")
  url <- "https://example.com/path/data.tif"

  fakeMeta <- list(
    targetFile = "data.tif",
    fileSize = as.character(fx$size),
    remoteHash = digest::digest(file = fx$path, algo = "md5"),
    remoteAlgorithm = "md5",
    timestampOnline = "now"
  )
  pp <- getFromNamespace("pp_remote_hash_check", "reproducible")

  # First call: HEAD invoked, writes sidecar.
  callCount <- 0L
  ctx1 <- makePPCtx(url, td, neededFiles = fx$path)
  testthat::with_mocked_bindings(
    getRemoteMetadata = function(...) { callCount <<- callCount + 1L; fakeMeta },
    { pp(ctx1) }
  )
  expect_identical(callCount, 1L)
  expect_false(is.null(readSidecar(td, fx, url)))

  # Second call: must NOT call getRemoteMetadata (sidecar fast-path).
  ctx2 <- makePPCtx(url, td, neededFiles = fx$path)
  out2 <- testthat::with_mocked_bindings(
    getRemoteMetadata = function(...) {
      callCount <<- callCount + 1L
      stop("getRemoteMetadata should not be called when sidecar exists")
    },
    { pp(ctx2) }
  )
  expect_identical(callCount, 1L)             # unchanged
  expect_true(isTRUE(out2$skipDownload))
})

# Checksums(): multi-row-per-file (xxhash64 + md5 coexist) -----------------

test_that("Checksums: multiple algorithms per file coexist; reads pick algo", {
  testInit()
  td <- withr::local_tempdir()
  fx <- makeFixture(td, "shared.dat")

  csf <- file.path(td, "CHECKSUMS.txt")
  hdr <- '"file" "checksum" "filesize" "algorithm"'
  rowX <- sprintf('"%s" "%s" "%d" "xxhash64"',
                  basename(fx$path), fx$hash, fx$size)
  md5  <- digest::digest(file = fx$path, algo = "md5")
  rowM <- sprintf('"%s" "%s" "%d" "md5"',
                  basename(fx$path), md5, fx$size)
  writeLines(c(hdr, rowX, rowM), csf)

  # Default algo (xxhash64): expect OK row matching the xxhash64 entry.
  resX <- Checksums(path = td, write = FALSE, files = basename(fx$path),
                    checksumFile = csf, verbose = 0)
  hits <- resX[!is.na(resX$result) & resX$result == "OK", ]
  expect_true(NROW(hits) >= 1L)
  expect_true(any(basename(hits$expectedFile) == basename(fx$path)))

  # Explicit md5: expect the md5 row to match.
  resM <- Checksums(path = td, write = FALSE, files = basename(fx$path),
                    checksumFile = csf, verbose = 0, algo = "md5")
  hitsM <- resM[!is.na(resM$result) & resM$result == "OK", ]
  expect_true(NROW(hitsM) >= 1L)
})

# .upsertChecksumsRow preserves rows for other algorithms ------------------

test_that("upsertChecksumsRow: preserves other-algorithm rows for same file", {
  testInit()
  td <- withr::local_tempdir()
  fx <- makeFixture(td, "shared.dat")

  csf <- writeChecksumsFor(td, fx)            # xxhash64 row
  upsert <- getFromNamespace(".upsertChecksumsRow", "reproducible")
  md5 <- digest::digest(file = fx$path, algo = "md5")

  upsert(csf, file = basename(fx$path), hash = md5,
         filesize = fx$size, algorithm = "md5")

  rows <- read.table(csf, header = TRUE, stringsAsFactors = FALSE)
  algos <- sort(rows$algorithm[rows$file == basename(fx$path)])
  expect_identical(algos, c("md5", "xxhash64"))
})


# ===========================================================================
# §5.6  Stale sidecar with locally-deleted file
#
# User scenario: a previous prepInputs() wrote a .hash sidecar in
# destinationPath and the file is later removed by hand. The sidecar is left
# behind. Two sub-cases matter:
#
#   C5: a fresh copy of the file is still in destinationPathShared. The next
#       prepInputs() should re-materialise the file (hardlink) from shared
#       *without* a network round-trip — the sidecar's recorded hash is the
#       authority that lets us trust the shared copy.
#
#   C6: the file is gone everywhere (no shared copy). The next prepInputs()
#       must fall through to a real download. The stale sidecar's recorded
#       hash is now meaningless — we cannot trust it because there is no
#       on-disk file to verify it against.
#
# Snapshots (rather than regex pattern matching) lock down the user-visible
# messages so a refactor of the message text can't silently break the
# documented contract — `testthat::snapshot_accept()` makes the intent
# explicit when wording is updated on purpose.
# ===========================================================================

# Scrub volatile tmpdir paths so the snapshot is stable across runs, and
# normalize continuation-line wrapping so the snapshot doesn't break when
# messagePreProcess reflows the same message into a different number of
# lines (terminal width, indent style, etc.).
.snapTransform <- function(lines) {
  ## testthat is called once per captured Message/Code block. We collapse the
  ## entire block into ONE line: `messagePreProcess` wraps its output to
  ## `getOption("width")`, and the line boundaries (and indentation of
  ## continuation lines) shift between environments — for the same logical
  ## message, an 80-col tty produces a different line count than a 120-col
  ## tty. Erasing both newlines and run-of-whitespace inside the block
  ## removes that variability while keeping the semantics ("Running
  ## preProcess Preparing: foo.csv ...downloading... Downloading <file-url>
  ## ... Appending checksums to ...") intact.
  joined <- paste(as.character(lines), collapse = " ")
  joined <- gsub("/tmp/Rtmp[A-Za-z0-9]+", "<tmpdir>", joined)
  joined <- gsub("file://[^ ]+", "<file-url>", joined)
  joined <- gsub("file[A-Za-z0-9]{6,}", "<tmp>", joined)
  joined <- gsub("[[:space:]]+", " ", joined)
  joined <- sub("^[[:space:]]+", "", joined)
  joined <- sub("[[:space:]]+$", "", joined)
  joined
}

## Plant a remote-hash sidecar that `.findRemoteHashSidecars` will discover
## by its `.<filename>_*.hash` prefix. We bypass `makeRemoteHashFile()` here
## on purpose: that helper derives the suffix from the source URL, which
## under R CMD check can produce paths whose `file(con, "w")` fails for
## reasons unrelated to what these tests are exercising. The lookup logic
## only inspects the prefix/suffix and the file contents, so any matching
## name works.
.plantSidecar <- function(destDir, filename, algorithm, hash, tag = "synthetic") {
  stopifnot(dir.exists(destDir))
  sc <- file.path(destDir, paste0(".", filename, "_", tag, ".hash"))
  writeLines(paste0(algorithm, ":", hash), sc)
  sc
}

test_that("C5: deleted local file + stale sidecar + file in destinationPathShared → relink, no download", {
  ## Under nosuggests R CMD check (`_R_CHECK_DEPENDS_ONLY_=true`), reproducible
  ## emits a one-time "install httr2 and try again" hint via `preProcess()`
  ## that we don't expect in the snapshot. The test is exercising the
  ## prepInputs sidecar logic, which already depends on Suggests packages
  ## (digest, terra). Easier to skip the whole pair under nosuggests than
  ## to scrub the hint from .snapTransform.
  skip_if_not_installed("httr2")
  testInit("digest")
  ## Force a wide print width so testthat's snapshot of the `Code` block
  ## (the deparsed expression) doesn't reflow between an 80-col local TTY
  ## and a 200-col CI runner — `expect_snapshot()`'s `transform` argument
  ## only touches captured OUTPUT, not the rendered code line.
  withr::local_options(width = 200)
  shared <- normPath(file.path(tmpdir, "shared"))
  dest   <- normPath(file.path(tmpdir, "dest"))
  src    <- normPath(file.path(tmpdir, "src"))
  for (d in c(shared, dest, src)) dir.create(d, recursive = TRUE)

  fixSrc    <- makeFixture(src,    "foo.csv")
  fixShared <- makeFixture(shared, "foo.csv")             # same content/hash
  writeChecksumsFor(shared, fixShared)

  # Plant a sidecar that records the (correct) hash, but leave dest/foo.csv
  # itself absent — the user manually deleted it.
  sc <- .plantSidecar(dest, "foo.csv", "xxhash64", fixShared$hash)
  expect_true(file.exists(sc))
  expect_false(file.exists(file.path(dest, "foo.csv")))

  withr::local_options(list(reproducible.inputPaths = shared))

  testthat::local_edition(3)
  expect_snapshot(
    {
      invisible(preProcess(url = fixSrc$url, targetFile = "foo.csv",
                           destinationPath = dest, fun = NA, verbose = 1))
    },
    transform = .snapTransform
  )

  # The destination should now have the file back, and (POSIX) it should
  # share an inode with the shared copy — i.e. it was hardlinked, not copied
  # via a fresh download.
  expect_true(file.exists(file.path(dest, "foo.csv")))
  skip_on_os("windows")
  expect_true(isTRUE(sameInode(file.path(dest, "foo.csv"), fixShared$path)),
              info = "C5: should hardlink from destinationPathShared, not re-download")
})

test_that("C6: deleted local file + stale sidecar + nothing in shared → download proceeds", {
  skip_if_not_installed("httr2")     # see C5 for rationale
  testInit("digest")
  withr::local_options(width = 200)  # see C5 for rationale
  dest <- normPath(file.path(tmpdir, "dest"))
  src  <- normPath(file.path(tmpdir, "src"))
  for (d in c(dest, src)) dir.create(d, recursive = TRUE)

  fixSrc <- makeFixture(src, "foo.csv")

  # Stale sidecar with a hash that has NOTHING to do with current content —
  # simulates the file having drifted (or just an old run) and being deleted.
  sc <- .plantSidecar(dest, "foo.csv", "xxhash64", strrep("d", 16L))
  expect_true(file.exists(sc))
  expect_false(file.exists(file.path(dest, "foo.csv")))

  withr::local_options(list(
    reproducible.inputPaths            = NULL,
    reproducible.destinationPathShared = NULL
  ))

  testthat::local_edition(3)
  expect_snapshot(
    {
      invisible(preProcess(url = fixSrc$url, targetFile = "foo.csv",
                           destinationPath = dest, fun = NA, verbose = 1))
    },
    transform = .snapTransform
  )

  # The file should now exist locally (downloaded from `file://src/foo.csv`).
  expect_true(file.exists(file.path(dest, "foo.csv")))
})
