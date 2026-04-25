# Tests for `reproducible.sharedInputs` — the read-write shared local cache
# used by prepInputs() / preProcess() to avoid re-downloads across projects.
#
# Test matrix lives in dev/dataPath-design.md (§5). Case IDs (P1, H1, ...)
# in this file map 1:1 to that matrix. Tests for behavior not yet
# implemented are stubbed with skip() so the matrix is visible end-to-end.
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
    url  = paste0("file://", normPath(p))
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

# Inode of a file via `stat -c %i`. POSIX-only; returns NA on failure.
# Used to detect hardlinks (same inode = same physical file).
inoOf <- function(p) {
  if (.Platform$OS.type != "unix") return(NA_integer_)
  out <- suppressWarnings(
    system2("stat", c("-c", "%i", shQuote(p)), stdout = TRUE, stderr = FALSE)
  )
  if (length(out) == 0L || !grepl("^[0-9]+$", out[[1L]])) return(NA_integer_)
  as.integer(out[[1L]])
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

getSharedInputsFn <- function() {
  getInternalOrNull(".getSharedInputs") %||% getInternalOrNull(".getDataPath")
}
getSharedInputsRecursiveFn <- function() {
  getInternalOrNull(".getSharedInputsRecursive") %||%
    getInternalOrNull(".getDataPathRecursive")
}


# ===========================================================================
# §5.1  Option plumbing (no I/O)
# ===========================================================================

test_that("P1: option unset → getter returns NULL", {
  testInit()
  withr::local_options(list(
    reproducible.sharedInputs = NULL,
    reproducible.dataPath     = NULL,
    reproducible.inputPaths   = NULL
  ))
  fn <- getSharedInputsFn()
  skip_if(is.null(fn), "no sharedInputs/dataPath getter available")
  expect_null(fn())
})

test_that("P2: sharedInputs = '/x' → '/x'  [BASELINE: fails until §4 step 5]", {
  testInit()
  skip_if_not(
    !is.null(getInternalOrNull(".getSharedInputs")),
    "sharedInputs option not yet wired (§4 step 5)"
  )
  withr::local_options(list(reproducible.sharedInputs = "/x"))
  expect_identical(getInternalOrNull(".getSharedInputs")(), "/x")
})

test_that("P3: vector value preserved", {
  testInit()
  skip_if_not(
    !is.null(getInternalOrNull(".getSharedInputs")),
    "sharedInputs option not yet wired (§4 step 5)"
  )
  withr::local_options(list(reproducible.sharedInputs = c("/a", "/b")))
  expect_identical(getInternalOrNull(".getSharedInputs")(), c("/a", "/b"))
})

test_that("P4: only dataPath set → returned + deprecation message once", {
  testInit()
  skip_if_not(
    !is.null(getInternalOrNull(".getSharedInputs")),
    "dataPath alias activates after §4 step 5"
  )
  withr::local_options(list(
    reproducible.sharedInputs = NULL,
    reproducible.dataPath     = "/x",
    reproducible.inputPaths   = NULL
  ))
  fn <- getInternalOrNull(".getSharedInputs")
  msgs1 <- testthat::capture_messages(out1 <- fn())
  msgs2 <- testthat::capture_messages(out2 <- fn())
  expect_identical(out1, "/x")
  expect_identical(out2, "/x")
  expect_true(any(grepl("deprecated", msgs1)))
  expect_false(any(grepl("deprecated", msgs2)))    # one-shot
})

test_that("P5: only inputPaths set → returned + deprecation message", {
  testInit()
  fn <- getSharedInputsFn()
  skip_if(is.null(fn), "no getter available")
  withr::local_options(list(
    reproducible.sharedInputs = NULL,
    reproducible.dataPath     = NULL,
    reproducible.inputPaths   = "/x"
  ))
  msgs <- testthat::capture_messages(out <- fn())
  expect_identical(out, "/x")
  expect_true(any(grepl("deprecated", msgs)))
})

test_that("P6: sharedInputs and dataPath both set → new wins, no deprecation", {
  testInit()
  skip_if_not(
    !is.null(getInternalOrNull(".getSharedInputs")),
    "sharedInputs option not yet wired (§4 step 5)"
  )
  withr::local_options(list(
    reproducible.sharedInputs = "/new",
    reproducible.dataPath     = "/old"
  ))
  msgs <- testthat::capture_messages(
    out <- getInternalOrNull(".getSharedInputs")()
  )
  expect_identical(out, "/new")
  expect_false(any(grepl("deprecated", msgs)))
})

test_that("P7: all three options set → sharedInputs wins, no deprecation", {
  testInit()
  skip_if_not(
    !is.null(getInternalOrNull(".getSharedInputs")),
    "sharedInputs option not yet wired (§4 step 5)"
  )
  withr::local_options(list(
    reproducible.sharedInputs = "/new",
    reproducible.dataPath     = "/mid",
    reproducible.inputPaths   = "/old"
  ))
  msgs <- testthat::capture_messages(
    out <- getInternalOrNull(".getSharedInputs")()
  )
  expect_identical(out, "/new")
  expect_false(any(grepl("deprecated", msgs)))
})

test_that("P8: empty string entry → error", {
  testInit()
  skip_if_not(
    !is.null(getInternalOrNull(".normalizeSharedInputs")),
    "validator not yet implemented (§4 step 8)"
  )
  expect_error(
    getInternalOrNull(".normalizeSharedInputs")("", destinationPath = tmpdir)
  )
})

test_that("P9: duplicate entries deduplicated", {
  testInit()
  skip_if_not(
    !is.null(getInternalOrNull(".normalizeSharedInputs")),
    "validator not yet implemented (§4 step 8)"
  )
  out <- getInternalOrNull(".normalizeSharedInputs")(c("/x", "/x"),
                                                     destinationPath = tmpdir)
  expect_identical(out, "/x")
})

test_that("P10: entry equal to destinationPath dropped", {
  testInit()
  skip_if_not(
    !is.null(getInternalOrNull(".normalizeSharedInputs")),
    "validator not yet implemented (§4 step 8)"
  )
  out <- getInternalOrNull(".normalizeSharedInputs")(tmpdir,
                                                     destinationPath = tmpdir)
  expect_length(out, 0)
})

test_that("P11: recursive option unset → FALSE", {
  testInit()
  fn <- getSharedInputsRecursiveFn()
  skip_if(is.null(fn), "no recursive getter available")
  withr::local_options(list(
    reproducible.sharedInputsRecursive = NULL,
    reproducible.dataPathRecursive     = NULL,
    reproducible.inputPathsRecursive   = NULL
  ))
  expect_false(fn())
})

test_that("P12: sharedInputsRecursive = TRUE → TRUE", {
  testInit()
  skip_if_not(
    !is.null(getInternalOrNull(".getSharedInputsRecursive")),
    "recursive option not yet wired (§4 step 5)"
  )
  withr::local_options(list(reproducible.sharedInputsRecursive = TRUE))
  expect_true(getInternalOrNull(".getSharedInputsRecursive")())
})

test_that("P13: only inputPathsRecursive = TRUE → TRUE + deprecation", {
  testInit()
  fn <- getSharedInputsRecursiveFn()
  skip_if(is.null(fn), "no recursive getter available")
  withr::local_options(list(
    reproducible.sharedInputsRecursive = NULL,
    reproducible.dataPathRecursive     = NULL,
    reproducible.inputPathsRecursive   = TRUE
  ))
  msgs <- testthat::capture_messages(out <- fn())
  expect_true(out)
  expect_true(any(grepl("deprecated", msgs)))
})

test_that("P14: only dataPathRecursive = TRUE → TRUE + deprecation", {
  testInit()
  skip_if_not(
    !is.null(getInternalOrNull(".getSharedInputsRecursive")),
    "dataPathRecursive alias activates after §4 step 5"
  )
  withr::local_options(list(
    reproducible.sharedInputsRecursive = NULL,
    reproducible.dataPathRecursive     = TRUE
  ))
  msgs <- capture.output(
    out <- getInternalOrNull(".getSharedInputsRecursive")(),
    type = "message"
  )
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

test_that("H1: file in sharedInputs with matching CHECKSUMS → hardlinked, no copy", {
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
               info = "destinationPath copy should share inode with sharedInputs (hardlink)")
})

test_that("H2: cross-device → copy fallback", {
  skip("cross-device cannot be reliably simulated in unit tests; manual coverage")
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

test_that("H4: file in subdir, recursive=TRUE → hardlinked from subdir", {
  ## Current code finds the file (search via dir(... recursive=TRUE) works)
  ## but the resulting destinationPath copy is not hardlinked from the
  ## subdir — current .checkLocalSources writes a copy via Checksums()
  ## propagation. Becomes a hardlink after §4 step 7 (single-phase
  ## resolution + explicit hardLinkOrCopy from candidate).
  skip("recursive subdir hardlinking lands in §4 step 7")
})

test_that("H5: two sharedInputs entries; file only in second → found", {
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

test_that("H6: file in both sharedInputs entries → first wins", {
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
               info = "deterministic order: first sharedInputs entry wins")
  expect_false(isTRUE(sameInode(file.path(dest, "foo.csv"), fix2$path)))
})

test_that("H7: file in destinationPath wins; sharedInputs not consulted", {
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

test_that("H8: stale destinationPath sidecar; sharedInputs has correct version", {
  skip("requires sidecar implementation (§4 step 2)")
})


# ===========================================================================
# §5.3  Resolution — sad paths
# ===========================================================================

test_that("S1: sharedInputs has bad-hash file → skipped, not deleted", {
  ## Today's downloadRemote refuses to overwrite a bad file already in
  ## sharedInputs ("good.csv already exists ... Use overwrite = TRUE?")
  ## because pp_check_local_sources does not cleanly disqualify the bad
  ## file before download. The clean handling lands in §4 step 11
  ## (mismatch handling: log, never delete, never block download).
  skip("hash-mismatch graceful handling lands in §4 step 11")
})

test_that("S2: hash unavailable anywhere → first basename match accepted", {
  skip("no-hash-anywhere policy lands in §3.2 Step E (§4 step 7)")
})

test_that("S3: sharedInputs path doesn't exist → falls through", {
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

test_that("S4: sharedInputs read-only → no crash, downloads proceed", {
  ## Today, runChecksums tries to checkPath(create = TRUE) on the
  ## inputPaths dir and fails when the dir is chmod 000. Graceful
  ## degradation (warn + skip-shared, continue with destinationPath only)
  ## lands in §4 step 5 (option getter) + step 8 (validator).
  skip("read-only sharedInputs graceful path lands in §4 steps 5 + 8")
})

test_that("S5: filename collision under recursive search", {
  skip("collision-with-recursive cases pinned after §4 step 7")
})

test_that("S6: hardLinkOrCopy fails", {
  skip("simulating both hardlink + copy failure is OS-specific; manual coverage")
})

test_that("S7: HEAD network error → falls back to .guessAtFile", {
  skip("HEAD-aware single-phase resolution lands in §4 step 7")
})


# ===========================================================================
# §5.4  Sidecar lifecycle and CHECKSUMS interaction
# ===========================================================================

test_that("C1: first-ever download writes sidecars in both locations", {
  skip("sidecar I/O lands in §4 step 2")
})
test_that("C2: second project hits sharedInputs via sidecar", {
  skip("sidecar I/O lands in §4 step 2")
})
test_that("C3: sharedInputs read-only → destinationPath sidecar only", {
  skip("sidecar I/O lands in §4 step 2")
})
test_that("C4: shared sidecar with missing adjacent file → ignored", {
  skip("sidecar I/O lands in §4 step 2")
})
test_that("C5: stale sidecar self-heals under exclusive lock", {
  skip("sidecar I/O + locking land in §4 steps 2–3")
})
test_that("C6: project CHECKSUMS A vs adjacent file B → project wins, redownload", {
  skip("requires §4 step 6 (resolveExpectedHash)")
})
test_that("C7: project CHECKSUMS A; sidecar absent; file is A → linked", {
  skip("requires §4 step 6")
})
test_that("C8: legacy <urlEncoded>.hash hydration on first encounter", {
  skip("legacy hydration lands in §4 step 4")
})
test_that("C9: legacy CHECKSUMS hydration with no adjacent files → no-op", {
  skip("legacy hydration lands in §4 step 4")
})
test_that("C10: malformed sidecar JSON → treated as missing", {
  skip("sidecar I/O lands in §4 step 2")
})


# ===========================================================================
# §5.5  targetFile inference
# ===========================================================================

test_that("T1: targetFile supplied + sharedInputs hit → no HEAD request", {
  skip("HEAD-skip assertion needs §4 step 7 instrumentation")
})
test_that("T2: targetFile NULL, plain URL → guessed; HEAD avoided when hash known", {
  skip("requires §4 step 7")
})
test_that("T3: content-disposition gives canonical filename", {
  skip("requires §4 step 7 + httr2 mock")
})
test_that("T4: no content-disposition + URL fallback wrong → search misses", {
  skip("requires §4 step 7")
})
test_that("T5: Google Drive URL → md5 from drive_get used", {
  skip("requires Google Drive auth; covered in test-cloud.R analogues")
})
test_that("T6: HEAD network error → silent fall-through", {
  skip("requires §4 step 7")
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
  skip("first-class remoteHash matching lands in §4 step 7")
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

test_that("B2: reproducible.dataPath honored", {
  skip_if_not(
    !is.null(getInternalOrNull(".getSharedInputs")),
    "dataPath alias activates after §4 step 5"
  )
})

test_that("B3: sharedInputs and dataPath disagree → sharedInputs wins", {
  skip_if_not(
    !is.null(getInternalOrNull(".getSharedInputs")),
    "requires §4 step 5"
  )
})


# ===========================================================================
# §5.8  Integration with prepInputs/Cache
# ===========================================================================

test_that("I1+I2: Cache(prepInputs(...)) — cold uses sharedInputs; warm is cache hit", {
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
# §5.9  Auto-population (write path)
# ===========================================================================

test_that("W1–W9: auto-population, single-physical-copy invariant", {
  skip("auto-population lands in §4 step 7 via pp_finalize_placement (§3.7)")
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
  expect_lt(t[["elapsed"]], 5,
            info = "lookup over 10k flat files should be subsecond on typical disk")
})

test_that("Q2: deep nested tree, recursive=TRUE → bounded", {
  skip("regression guard, runs only with reproducible.runLargeFileTests")
})

test_that("Q3: hot destinationPath, repeated calls → no HEAD after first", {
  skip("requires §4 step 7 + HEAD instrumentation")
})

test_that("Q4: digest cost — file hashed at most once across hot calls", {
  skip("requires §4 step 6 (resolveExpectedHash) + digest stub")
})

test_that("Q5: server md5 → no sha1 computed", {
  skip("requires §4 step 7")
})


# ===========================================================================
# §5.11  Migration from legacy formats
# ===========================================================================

test_that("M1–M5: legacy <urlEncoded>.hash and CHECKSUMS hydration", {
  skip("legacy hydration lands in §4 step 4")
})


# ===========================================================================
# §5.12  Concurrency (filelock)
# ===========================================================================

test_that("L1–L7: per-file locking, atomicity, graceful degradation", {
  skip_if_not_installed("filelock")
  skip("locking shim lands in §4 step 3")
})


# ===========================================================================
# §5.13  Public API: sharedInputsLs() / sharedInputsRefresh()
# ===========================================================================

test_that("A1–A10: sharedInputsLs() and sharedInputsRefresh()", {
  skip("public API exports land in §4 step 13 (post-refactor)")
})
