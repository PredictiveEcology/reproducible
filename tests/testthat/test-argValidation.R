## Coverage for argument-validation branches -- the errors raised when a caller
## gets it wrong. These are cheap to reach (pass the wrong thing) and worth
## pinning: each one is a contract with the caller, and a silently-relaxed check
## is how bad input starts flowing downstream.
##
## No network, no Drive.

test_that("determineFilename rejects deprecated argument names", {
  testInit()

  ## Each of these was renamed; the error names both the old and the new form so
  ## the caller knows what to change.
  expect_error(
    determineFilename(filename2 = "a.tif", inputFilePath = "x"),
    "inputFilePath is being deprecated"
  )
  expect_error(
    determineFilename(filename2 = "a.tif", postProcessedFilename = "x"),
    "postProcessedFilename is being deprecated"
  )
  expect_error(
    determineFilename(filename2 = "a.tif", targetFilePath = "x"),
    "targetFilePath is being deprecated"
  )
})

test_that("determineFilename rejects an unusable filename2", {
  testInit()

  ## filename2 must be logical, character or NULL -- a number is none of these.
  expect_error(
    determineFilename(filename2 = 1L),
    "filename2 must be logical or character string or NULL"
  )

  ## Mixing relative and absolute paths is ambiguous: the relative ones would be
  ## resolved against destinationPath while the absolute ones would not, so the
  ## set would land in two different places.
  expect_error(
    determineFilename(filename2 = c("/abs/a.tif", "rel/b.tif"), destinationPath = tmpdir),
    "must be all relative or all absolute"
  )
})

test_that("determineFilename resolves the valid cases", {
  testInit()

  ## Relative paths are placed under destinationPath ...
  out <- determineFilename(filename2 = "a.tif", destinationPath = tmpdir, verbose = 0)
  expect_identical(basename(out), "a.tif")
  expect_identical(normPath(dirname(out)), normPath(tmpdir))

  ## ... absolute paths are taken as-is ...
  abs <- normPath(file.path(tmpdir, "b.tif"))
  expect_identical(determineFilename(filename2 = abs, verbose = 0), abs)

  ## ... and FALSE means "do not write a file", so nothing is built.
  expect_false(isTRUE(determineFilename(filename2 = FALSE, verbose = 0)))
})

test_that("isDirectory/isFile and asPath reject wrong types", {
  testInit()

  ## Path predicates take character; anything else is an error rather than a
  ## silent as.character() coercion, which would happily "work" on a number.
  expect_error(isDirectory(1), "must be character")
  expect_error(isFile(list("a")), "must be character")

  ## asPath has no method for a list, so passing one is an error naming the
  ## class -- useful, because the usual cause is forgetting to unlist().
  expect_error(asPath(list("a")), "asPath")

  ## NULL is explicitly supported and returns NULL, so callers can pass an
  ## optional path straight through. This requires the method to be named
  ## asPath.NULL: class(NULL) is the string "NULL" and S3 dispatch is
  ## case-sensitive, so the previous asPath.null was unreachable and this
  ## errored with "no applicable method".
  expect_null(asPath(NULL))

  ## The working cases are unaffected.
  expect_s3_class(asPath("a/b"), "Path")
})

test_that("wrapSpatRaster requires named dots", {
  skip_if_not_installed("terra")
  testInit("terra")

  ## Must be file-backed: wrapSpatRaster reads the filename slots, and an
  ## in-memory raster fails earlier with terra's "filename is empty".
  r <- terra::rast(nrows = 2, ncols = 2, vals = 1)
  r <- terra::writeRaster(r, file.path(tmpdir, "r.tif"), overwrite = TRUE)

  ## The dots carry path anchors as a NAMED list; an unnamed one cannot be
  ## matched to anything, so it is refused rather than guessed at.
  ## NB the list must be the 4th argument: the signature is
  ## (obj, cachePath, cacheId, ...), so positions 2 and 3 are consumed first.
  expect_error(
    wrapSpatRaster(r, cachePath = tmpdir, cacheId = NULL, list("unnamed")),
    "named list"
  )

  ## The same list, named, is accepted.
  expect_no_error(
    wrapSpatRaster(r, cachePath = tmpdir, cacheId = NULL, list(cachePath = tmpdir))
  )
})

test_that("Cache reports the multi-step message for braced code, with or without other args", {
  testInit()

  msg <- "does not yet support multi-step caching"

  ## Cache cannot attribute a multi-statement braced block to a cacheId, so it
  ## refuses rather than caching under a wrong key.
  expect_error(Cache({
    a <- 1
    a + 1
  }), msg)

  ## Same unsupported code, merely with another argument. This shape used to
  ## reach `[[-1]]` on the braced block before the length check and surfaced
  ## R's opaque "invalid negative subscript in get1index <real>" instead.
  expect_error(Cache({
    a <- 1
    a + 1
  }, cachePath = tmpCache), msg)

  ## A single-statement braced block is supported and must keep working.
  expect_no_error(Cache({
    rnorm(1)
  }, cachePath = tmpCache))
})

test_that(".file.move deprecation names the real replacement", {
  testInit()

  from <- file.path(tmpdir, "src.txt")
  to <- file.path(tmpdir, "dst.txt")
  writeLines("payload", from)

  ## The message must name a function that actually exists -- it previously said
  ## "hardLinkeOrCopy", which does not, so anyone following the advice failed.
  expect_warning(.file.move(from, to), "hardLinkOrCopy")
  expect_true(exists("hardLinkOrCopy", envir = asNamespace("reproducible")))

  ## And it really moves: the copy lands, the original goes.
  expect_true(file.exists(to))
  expect_false(file.exists(from))
  expect_identical(readLines(to, warn = FALSE), "payload")
})

test_that(".extractFunction names the offending argument when `fun` cannot be resolved", {
  testInit()

  ## `fun` is the loader for the target file, so it must be a function, the name
  ## of one, or a quoted call -- NOT a filename, which is the mistake that
  ## actually lands here. Each accepted form resolves through a different
  ## lookup, and each used to surface that lookup's own internal complaint
  ## ("object 'hello' not found", "there is no package called 'nopkg'", or for a
  ## symbol "use of NULL environment is defunct"), none of which say which
  ## argument was wrong.
  ## Matched against the .message entry rather than a copy of the string, so a
  ## reword updates the code and the test together (R/messages.R).
  msg <- .message$stopFunNotResolvedTxt

  ## Plain string naming nothing.
  expect_error(.extractFunction("hello"), msg, fixed = TRUE)
  ## The full string is quoted back, so the caller can see what was passed.
  expect_error(.extractFunction("hello"), "hello", fixed = TRUE)

  ## "pkg::name": missing package, and present package but missing object.
  expect_error(.extractFunction("nopkg::nofun"), msg, fixed = TRUE)
  expect_error(.extractFunction("stats::nofun"), msg, fixed = TRUE)
  ## Quoted whole, not just the half after "::".
  expect_error(.extractFunction("nopkg::nofun"), "nopkg::nofun", fixed = TRUE)

  ## An unresolvable symbol: .whereInStack() finds nothing and returns NULL.
  expect_error(.extractFunction(quote(hello), envir = new.env()), msg, fixed = TRUE)
})

test_that(".extractFunction still resolves every valid form of `fun`", {
  testInit()

  ## The guard must not have narrowed what is accepted.
  expect_true(is.function(.extractFunction("readRDS")))
  expect_true(is.function(.extractFunction("base::readRDS")))
  expect_true(is.function(.extractFunction(readRDS)))

  ## A quoted call is passed through untouched -- it is evaluated later, not now.
  expect_true(is.call(.extractFunction(quote(readRDS(x)))))

  ## NA means "do not load"; NULL means "nothing supplied". Neither is a lookup.
  expect_true(is.na(.extractFunction(NA)))
  expect_null(.extractFunction(NULL))
})
