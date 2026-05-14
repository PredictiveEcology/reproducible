utils::globalVariables(c(
  "reproducible.inputPaths", "successfulCheckSumFilePath", "successfulDir",
  "destinationPathUser"
))

#' @param n Number of non-null arguments passed to `preProcess`.
#' E.g., passing `n = 1` returns combinations with only a single non-NULL parameter.
#' If `NULL` (default), all parameter combinations are returned.
#'
#' @export
#' @rdname preProcess
preProcessParams <- function(n = NULL) {
  p1 <- data.frame(
    url = c("char", "NULL", "NULL", "NULL"),
    targetFile = c("NULL", "char", "NULL", "NULL"),
    archive = c("NULL", "NULL", "char", "NULL"),
    alsoExtract = c("NULL", "NULL", "NULL", "char"),
    Result = c(
      "Download, extract all files if an archive, guess at 'targetFile', load into R.",
      "Load 'targetFile' into R.",
      "Extract all files, guess at 'targetFile', load into R.",
      "Guess at 'targetFile' from files in 'alsoExtract', load into R."
    ),
    FirstChecksum = c(
      "Write or append all new files.",
      "Write or append 'targetFile'.",
      "Write or append all new files.",
      "Write or append all new files."
    ),
    SecondChecksum = c(
      "Same as first; no 'targetFile'.*",
      "No downloading, so no checksums used.",
      "No downloading, so no checksums used.",
      "No downloading, so no checksums used."
    ),
    stringsAsFactors = FALSE
  )
  p2 <- data.frame(
    url = c("char", "char", "char", "NULL", "NULL", "NULL"),
    targetFile = c("char", "NULL", "NULL", "char", "char", "NULL"),
    archive = c("NULL", "char", "NULL", "NULL", "char", "char"),
    alsoExtract = c("NULL", "NULL", "char", "char", "NULL", "char"),
    Result = c(
      "Download, extract all files if an archive, load 'targetFile' into R.",
      "Download, extract all files, guess at 'targetFile', load into R.",
      "Download, extract only named files in 'alsoExtract', guess at 'targetFile', load into R.",
      "Load 'targetFile' into R.",
      "Extract all files, load 'targetFile' into R.",
      " Extract only named files in 'alsoExtract', guess at 'targetFile', load into R."
    ),
    FirstChecksum = c(
      "Write or append all new files.",
      "Write or append all new files.",
      "Write or append all new files.",
      "Write or append all new files.",
      "Write or append all new files.",
      "Write or append all new files."
    ),
    SecondChecksum = c(
      "Use Checksums, skip downloading.",
      "Same as first; no 'targetFile'.*",
      "Same as first; no 'targetFile'.*",
      "No downloading, so no checksums used.",
      "No downloading, so no checksums used.",
      "No downloading, so no checksums used."
    ),
    stringsAsFactors = FALSE
  )
  p3 <- data.frame(
    url = c("char", "char", "char", "char", "char", "char", "NULL"),
    targetFile = c("char", "NULL", "NULL", "char", "char", "char", "char"),
    archive = c("char", "char", "char", "NULL", "NULL", "char", "char"),
    alsoExtract = c("NULL", "char", "'similar'", "char", "'similar", "NULL", "char"),
    Result = c(
      "Download, extract all files, load 'targetFile' into R.",
      "Download, extract files named in 'alsoExtract', guess at 'targetFile', load into R.",
      "Download, extract all files (can't understand \"similar\"), guess at 'targetFile', load into R.",
      "Download, if an archive, extract files named in 'targetFile' and 'alsoExtract', load 'targetFile' into R.",
      "Download, if an archive, extract files with same base as 'targetFile', load 'targetFile' into R.",
      "Download, extract all files from archive, load 'targetFile' into R.",
      "Extract  files named in 'alsoExtract' from archive, load 'targetFile' into R."
    ),
    FirstChecksum = c(
      "Write or append all new files.",
      "Write or append all new files.",
      "Write or append all new files.",
      "Write or append all new files.",
      "Write or append all new files.",
      "Write or append all new files.",
      "Write or append all new files."
    ),
    SecondChecksum = c(
      "Use Checksums, skip downloading.",
      "Use Checksums, skip downloading.",
      "Same as first; no 'targetFile'.",
      "Use Checksums, skip downloading.",
      "Use Checksums, skip downloading.",
      "Use Checksums, skip downloading.",
      "No downloading, so no checksums used."
    ),
    stringsAsFactors = FALSE
  )
  p4 <- data.frame(
    url = c("char", "char"),
    targetFile = c("char", "char"),
    archive = c("char", "char"),
    alsoExtract = c("char", "'similar'"),
    Result = c(
      "Download, extract files named in 'targetFile' and 'alsoExtract', load 'targetFile' into R.",
      "Download, extract all files with same basename as 'targetFile', load 'targetFile' into R."
    ),
    FirstChecksum = c(
      "Write or append all new files.",
      "Write or append all new files."
    ),
    SecondChecksum = c(
      "Use Checksums, skip downloading.",
      "Use Checksums, skip downloading."
    ),
    stringsAsFactors = FALSE
  )

  if (is.null(n)) {
    rbind(p1, p2, p3, p4)
  } else if (n == 1) {
    p1
  } else if (n == 2) {
    p2
  } else if (n == 3) {
    p3
  } else if (n == 4) {
    p4
  }
}

#' Download, checksum, extract files
#'
#' This does downloading (via `downloadFile`), checksumming (`Checksums`),
#' and extracting from archives (`extractFromArchive`), plus cleaning up of input
#' arguments (e.g., paths, function names).
#' This is the first stage of three used in `prepInputs`.
#'
#' @return
#' A list with 5 elements: `checkSums` (the result of a `Checksums`
#' after downloading), `dots` (cleaned up `...`, including deprecated argument checks),
#' `fun` (the function to be used to load the `preProcess`ed object from disk),
#' and `targetFilePath` (the fully qualified path to the `targetFile`).
#'
#' @param .callingEnv The environment where the function was called from. Used to find
#'   objects, if necessary.
#' @section Combinations of `targetFile`, `url`, `archive`, `alsoExtract`:
#'
#'   Use `preProcessParams()` for a table describing various parameter combinations and their
#'   outcomes.
#'
#'  `*` If the `url` is a file on Google Drive, checksumming will work
#'  even without a `targetFile` specified because there is an initial attempt
#'  to get the remove file information (e.g., file name). With that, the connection
#'  between the `url` and the filename used in the \file{CHECKSUMS.txt} file can be made.
#'
#' @inheritParams prepInputs
#' @inheritParams downloadFile
#'
#' @author Eliot McIntire
#' @export
#' @importFrom data.table fread setDT
preProcess <- function(targetFile = NULL, url = NULL, archive = NULL, alsoExtract = NULL,
                       destinationPath = getOption("reproducible.destinationPath", "."),
                       fun = NULL, dlFun = NULL,
                       quick = getOption("reproducible.quick"),
                       overwrite = getOption("reproducible.overwrite", FALSE),
                       purge = FALSE,
                       verbose = getOption("reproducible.verbose", 1),
                       .tempPath, .callingEnv = parent.frame(),
                       ...) {
  st <- Sys.time()
  messagePreProcess("Running `preProcess`", verbose = verbose, verboseLevel = 0)
  .message$IndentUpdate()
  on.exit(.message$IndentRevert(), add = TRUE)

  if (missing(.tempPath)) {
    .tempPath <- tempdir2(rndstr(1, 6))
    on.exit(unlink(.tempPath, recursive = TRUE), add = TRUE)
  }

  # Capture dlFun before any other evaluation (must be in caller frame)
  dlFunCaptured <- substitute(dlFun)
  prepInputsAssertions(environment())

  # Three cases for the captured expression:
  #   (a) quote(fn(...))            -> unwrap with eval to get the inner call
  #   (b) fn(...) or pkg::fn(...)   -> keep as a deferred call object (the
  #                                    canonical dlFun usage; evaluated later
  #                                    inside downloadFile in the caller env)
  #   (c) anything else (symbol,    -> force the lazy promise so ctx stores the
  #       NULL, control-flow like   -- actual value (function/NULL/string), not
  #       `if (x) fn else NULL`)    -- an expression referencing caller-frame
  #                                    variables that may be out of scope later
  dlFunCaptured <- .resolveDlFunCaptured(dlFunCaptured, dlFun)

  dots <- list(...)
  fun  <- .checkFunInDots(fun = fun, dots = dots)
  dots <- .checkDeprecated(dots, verbose = verbose)

  ctx <- .pp_make_ctx(
    targetFile = targetFile, url = url, archive = archive, alsoExtract = alsoExtract,
    destinationPath = destinationPath, fun = fun, dlFunCaptured = dlFunCaptured,
    dots = dots, quick = quick, overwrite = overwrite, purge = purge, verbose = verbose,
    .tempPath = .tempPath, .callingEnv = .callingEnv
  )

  ctx <- pp_resolve_files(ctx)
  ctx <- pp_checksums_init(ctx)
  ctx <- pp_purge(ctx)
  ctx <- pp_resolve_needed_files(ctx)
  ctx <- pp_check_local_sources(ctx)
  ctx <- pp_remote_hash_check(ctx)
  ctx <- pp_download(ctx)
  ctx <- pp_extract(ctx)
  ctx <- pp_link_to_destination(ctx)
  out <- pp_finalize(ctx)

  reportTime(st, mess = "`preProcess` done; took ", minSeconds = 10)
  out
}

# ---------------------------------------------------------------------------
# Context constructor
# ---------------------------------------------------------------------------
.pp_make_ctx <- function(targetFile, url, archive, alsoExtract, destinationPath, fun,
                         dlFunCaptured, dots, quick, overwrite, purge, verbose,
                         .tempPath, .callingEnv) {
  if (is.null(destinationPath))
    destinationPath <- getOption("reproducible.destinationPath", ".")
  destinationPath <- normPath(destinationPath)

  # Treat a zero-length character `url` (e.g. `character()`) as no URL — the
  # downstream guards check `is.null(ctx$url)`, and `grepl()` on length-0 input
  # returns `logical(0)`, which crashes `if`.
  if (is.character(url) && length(url) == 0L) url <- NULL

  if (!is.null(archive) && any(is.na(archive)) && all(!is.character(archive)))
    archive <- as.character(archive)

  if (is.logical(alsoExtract))
    alsoExtract <- c("none", "all")[alsoExtract + 1L]

  list(
    url = url, fun = fun, dlFunCaptured = dlFunCaptured, dots = dots,
    quick = quick, overwrite = overwrite, purge = purge, verbose = verbose,
    .tempPath = .tempPath, .callingEnv = .callingEnv,
    targetFile = targetFile, targetFilePath = NULL,
    archive = archive, alsoExtract = alsoExtract,
    destinationPath = destinationPath, destinationPathUser = NULL,
    checkSumFilePath = identifyCHECKSUMStxtFile(destinationPath),
    checkSums = .emptyChecksumsResult, needChecksums = 0L,
    neededFiles = NULL, filesToChecksum = NULL, filesExtr = NULL,
    reproducible.inputPaths = NULL, successfulDir = NULL,
    successfulCheckSumFilePath = NULL,
    downloadResult = NULL, funChar = NULL,
    skipDownload = FALSE, remoteMetadata = NULL,
    hashVerified = character(),
    verboseCFS = verbose
  )
}

# ---------------------------------------------------------------------------
# Phase 1: Resolve targetFile / archive / alsoExtract from url
# ---------------------------------------------------------------------------
pp_resolve_files <- function(ctx) {
  targetFileGuess <- NULL
  if (is.null(ctx[["targetFile"]]) || is.null(ctx$archive)) {
    targetFileGuess <- .guessAtFile(
      url = ctx$url, archive = ctx$archive, targetFile = ctx[["targetFile"]],
      destinationPath = ctx$destinationPath, verbose = ctx$verbose,
      team_drive = getTeamDrive(ctx$dots)
    )
    if (is.null(ctx$archive))
      ctx$archive <- .isArchive(targetFileGuess)
  }

  ctx$targetFilePath <- getTargetFilePath(
    ctx[["targetFile"]], ctx$archive, targetFileGuess, ctx$verbose,
    ctx$destinationPath, ctx$alsoExtract, ctx$checkSumFilePath
  )
  ctx[["targetFile"]]  <- makeRelative(ctx$targetFilePath, ctx$destinationPath)
  ctx$alsoExtract <- guessAlsoExtract(ctx[["targetFile"]], ctx$alsoExtract, ctx$checkSumFilePath)

  if (!dir.exists(ctx$destinationPath)) {
    if (isFile(ctx$destinationPath)) stop("destinationPath must be a directory")
    checkPath(ctx$destinationPath, create = TRUE)
  }

  if (isTRUE(!is.na(ctx[["targetFile"]])))
    messagePreProcess("Preparing: ", ctx[["targetFile"]], verbose = ctx$verbose)

  ctx$archive <- setupArchive(ctx$archive, ctx$destinationPath)
  ctx
}

# ---------------------------------------------------------------------------
# Phase 2: Load CHECKSUMS.txt; handle reproducible.inputPaths redirect
# ---------------------------------------------------------------------------
pp_checksums_init <- function(ctx) {
  filesToCheck <- na.omit(unique(c(ctx$archive, ctx$targetFilePath, ctx$alsoExtract)))

  # Pre-verify via remote-hash sidecar files. A `<basename>_<urlEncoded>.hash`
  # file in destinationPath (or any reproducible.inputPaths dir) is written
  # only by a successful pp_remote_hash_check, so its presence implies the
  # file has been validated against the remote source. Skip the local
  # CHECKSUMS.txt-driven re-digest for those files — re-hashing a multi-GB
  # archive in this phase otherwise dominates wall time of an idempotent
  # preProcess() call (4 invocations: destinationPath + inputPaths × 2
  # CHECKSUMS.txt locations). User can `rm` the .hash file to force a full
  # local re-verification.
  filesPreVerified <- character()
  if (length(filesToCheck)) {
    sidecarDirs <- unique(c(ctx$destinationPath, .getDestinationPathShared()))
    hasSidecar <- vapply(filesToCheck, function(f) {
      length(.findRemoteHashSidecars(basename2(f), sidecarDirs)) > 0L &&
        file.exists(f)
    }, FUN.VALUE = logical(1L))
    if (any(hasSidecar)) {
      filesPreVerified <- filesToCheck[hasSidecar]
      filesToCheck     <- filesToCheck[!hasSidecar]
      ctx$hashVerified <- unique(c(ctx$hashVerified, filesPreVerified))
    }
  }

  inputPaths <- runChecksums(ctx$destinationPath, ctx$checkSumFilePath, filesToCheck, ctx$verbose)

  ctx$reproducible.inputPaths <- inputPaths$reproducible.inputPaths
  ctx$destinationPathUser     <- inputPaths$destinationPathUser
  ctx$destinationPath         <- inputPaths$destinationPath
  ctx$checkSums               <- inputPaths$checkSums
  ctx$checkSumFilePath        <- identifyCHECKSUMStxtFile(ctx$destinationPath)

  if (!is.null(inputPaths$destinationPathUser)) {
    ctx$targetFilePath <- makeAbsolute(
      makeRelative(ctx$targetFilePath, inputPaths$destinationPathUser),
      ctx$destinationPath
    )
    if (!is.null(ctx$archive)) {
      ctx$archive <- makeAbsolute(
        makeRelative(ctx$archive, inputPaths$destinationPathUser),
        ctx$destinationPath
      )
    }
  }

  if (is.null(ctx$checkSums) || is(ctx$checkSums, "try-error")) {
    ctx$needChecksums <- 1L
    ctx$checkSums     <- .emptyChecksumsResult
  }

  # Synthesise OK rows for pre-verified files so downstream phases
  # (.compareChecksumsAndFilesAddDirs, .checkLocalSources) see them as
  # already-OK without a digest pass. If a row with the same expectedFile
  # already exists (e.g. left over from a stale CHECKSUMS.txt comparison),
  # drop it — the synthetic row's "OK" is authoritative for downstream joins.
  if (length(filesPreVerified)) {
    rel <- makeRelative(filesPreVerified, ctx$destinationPath)
    sz  <- as.character(file.size(filesPreVerified))
    okRows <- data.table::data.table(
      result       = "OK",
      expectedFile = rel,
      actualFile   = rel,
      checksum.x   = NA_character_,
      checksum.y   = NA_character_,
      algorithm.x  = NA_character_,
      algorithm.y  = NA_character_,
      filesize.x   = sz,
      filesize.y   = sz
    )
    if (NROW(ctx$checkSums)) {
      ctx$checkSums <- ctx$checkSums[!ctx$checkSums$expectedFile %in% rel, ]
    }
    ctx$checkSums <- data.table::rbindlist(
      list(ctx$checkSums, okRows), fill = TRUE
    )
  }

  ctx
}

# ---------------------------------------------------------------------------
# Phase 3: Apply purge strategy
# ---------------------------------------------------------------------------
pp_purge <- function(ctx) {
  purge <- ctx$purge
  if (is.logical(purge)) purge <- as.integer(purge)

  if (purge == 1L) {
    unlink(ctx$checkSumFilePath)
    ctx$needChecksums <- 1L
  } else if (purge > 1L) {
    ctx$checkSums     <- .purge(
      checkSums = ctx$checkSums, purge = purge,
      targetFile = ctx[["targetFile"]], archive = ctx$archive,
      url = ctx$url, alsoExtract = ctx$alsoExtract,
      destinationPath = ctx$destinationPath
    )
    ctx$needChecksums <- 2L
  }
  ctx$purge <- purge
  ctx
}

# ---------------------------------------------------------------------------
# Phase 4: Build neededFiles; pre-download extraction attempt if archive local
# ---------------------------------------------------------------------------
pp_resolve_needed_files <- function(ctx) {
  archiveOut    <- dealWithArchive(
    ctx$archive, ctx$url, ctx[["targetFile"]], ctx$checkSums, ctx$alsoExtract,
    ctx$destinationPath, getTeamDrive(ctx$dots), ctx$verbose
  )
  ctx$checkSums <- archiveOut$checkSums
  ctx$archive   <- archiveOut$archive

  # Expand any regex patterns in alsoExtract while the archive is already on
  # disk.  Doing this BEFORE makeAbsolute() prevents fake absolute paths like
  # "/dest/CMD_sm|CMD_sp" from entering neededFiles.
  if (!isNULLorNA(ctx$alsoExtract) &&
      !isNULLorNA(ctx$archive) && any(file.exists(ctx$archive))) {
    fia <- makeRelative(.listFilesInArchive(ctx$archive), ctx$destinationPath)
    if (length(fia))
      ctx$alsoExtract <- .expandAlsoExtractPatterns(ctx$alsoExtract, fia)
  }

  neededFiles <- c(ctx[["targetFile"]], makeAbsolute(ctx$alsoExtract, ctx$destinationPath))
  if (is.null(neededFiles)) neededFiles <- makeAbsolute(ctx$archive)
  if (any(is.na(neededFiles)))  neededFiles <- na.omit(neededFiles)
  neededFiles <- grep("similar$", neededFiles, value = TRUE, invert = TRUE)
  neededFiles <- grep("none$",    neededFiles, value = TRUE, invert = TRUE)

  neededFilesOrig <- NULL
  if (is.null(ctx$alsoExtract)) {
    filesInsideArchive <- .listFilesInArchive(ctx$archive)
    neededFiles        <- checkRelative(neededFiles, ctx$destinationPath, filesInsideArchive)
    if (length(filesInsideArchive) > 0L)
      ctx$checkSums <- .checkSumsUpdate(
        ctx$destinationPath,
        makeAbsolute(filesInsideArchive, ctx$destinationPath),
        checkSums = ctx$checkSums
      )
    neededFiles <- unique(c(neededFiles, filesInsideArchive))
  } else {
    outFromSimilar  <- .checkForSimilar(
      neededFiles, ctx$alsoExtract, ctx$archive, ctx[["targetFile"]],
      destinationPath = ctx$destinationPath, ctx$checkSums,
      checkSumFilePath = ctx$checkSumFilePath, ctx$url, verbose = ctx$verboseCFS
    )
    ctx$verboseCFS  <- ctx$verbose - 1L
    neededFilesOrig <- unique(makeAbsolute(neededFiles, ctx$destinationPath))
    ctx$checkSums   <- outFromSimilar$checkSums
    neededFiles     <- outFromSimilar$neededFiles
  }

  neededFiles <- unique(makeAbsolute(neededFiles, ctx$destinationPath))
  neededFiles <- unique(c(neededFilesOrig, neededFiles))
  neededFiles <- grep("none$", neededFiles, value = TRUE, invert = TRUE)

  filesToChecksum <- if (is.null(ctx$archive) || isTRUE(is.na(ctx$archive))) NULL else ctx$archive
  filesToChecksum <- unique(c(filesToChecksum, neededFiles))

  isOK <- .compareChecksumsAndFilesAddDirs(ctx$checkSums, filesToChecksum, ctx$destinationPath)
  if (isTRUE(!all(isOK))) {
    results <- .tryExtractFromArchive(
      archive = if (isTRUE(is.na(ctx$archive))) NULL else ctx$archive,
      neededFiles = neededFiles, alsoExtract = ctx$alsoExtract,
      destinationPath = ctx$destinationPath, checkSums = ctx$checkSums,
      needChecksums = ctx$needChecksums, checkSumFilePath = ctx$checkSumFilePath,
      filesToChecksum = filesToChecksum, targetFilePath = ctx$targetFilePath,
      quick = ctx$quick, verbose = ctx$verbose, .tempPath = ctx$.tempPath
    )
    ctx$neededFiles    <- results$neededFiles
    ctx$checkSums      <- results$checkSums
    ctx$filesExtr      <- results$filesExtr
    ctx$targetFilePath <- results$targetFilePath
    filesToChecksum    <- results$filesToChecksum
    ctx$needChecksums  <- results$needChecksums

    if (ctx$needChecksums > 0L) {
      ctx$checkSums     <- appendChecksumsTable(
        checkSumFilePath = ctx$checkSumFilePath,
        filesToChecksum  = unique(filesToChecksum),
        destinationPath  = ctx$destinationPath,
        append           = results$needChecksums >= 2L
      )
      ctx$needChecksums <- 0L
    }
  } else {
    targetFilePoss <- makeRelative(ctx$targetFilePath, ctx$destinationPath)
    if (isTRUE(!targetFilePoss %in% names(isOK))) {
      wh <- grep(targetFilePoss, names(isOK))
      if (length(wh)) ctx$targetFilePath <- names(isOK)[wh]
    }
    ctx$neededFiles <- neededFiles
    ctx$filesExtr   <- setdiff(
      unique(c(filesToChecksum, neededFiles)),
      .isArchive(c(filesToChecksum, neededFiles))
    )
  }

  ctx$filesToChecksum <- filesToChecksum
  ctx
}

# ---------------------------------------------------------------------------
# Phase 5: Check reproducible.inputPaths for existing local copies
# ---------------------------------------------------------------------------
pp_check_local_sources <- function(ctx) {
  localChecks <- .checkLocalSources(
    ctx$neededFiles, checkSums = ctx$checkSums,
    checkSumFilePath = ctx$checkSumFilePath,
    otherPaths = ctx$reproducible.inputPaths,
    destinationPath = ctx$destinationPath,
    needChecksums = ctx$needChecksums, verbose = ctx$verbose
  )
  ctx$neededFiles                <- localChecks$neededFiles
  ctx$checkSums                  <- localChecks$checkSums
  ctx$needChecksums              <- localChecks$needChecksums
  ctx$successfulDir              <- localChecks$successfulDir
  ctx$successfulCheckSumFilePath <- localChecks$successfulCheckSumFilePath

  if (!is.null(ctx$reproducible.inputPaths)) {
    outs <- copyFromDPtoReproducibleIPs(
      ctx$targetFilePath, ctx$destinationPathUser, ctx$destinationPath,
      ctx$reproducible.inputPaths, ctx$neededFiles, ctx$archive,
      ctx$checkSums, checkSumFilePath = ctx$checkSumFilePath, ctx$verbose
    )
    ctx$neededFiles         <- outs$neededFiles
    ctx$targetFilePath      <- outs$targetFilePath
    ctx$destinationPath     <- outs$destinationPath
    ctx$destinationPathUser <- outs$destinationPathUser
    ctx$archive             <- outs$archive
  }
  ctx
}

# ---------------------------------------------------------------------------
# Phase 6: Remote hash check
# ---------------------------------------------------------------------------
# When a local copy already exists, decide whether it can stand in for a fresh
# download by consulting the remote source. The semantic rule:
#
#   filesize is a NEGATIVE-only signal. remote.size != local.size proves
#   the bytes differ → download. remote.size == local.size proves nothing
#   on its own (two GeoTIFFs of identical pixel count have identical byte
#   size with arbitrary content), so a positive trust decision REQUIRES a
#   content-hash comparison in the remote's algorithm.
#
# Algorithm:
#   1. If a `.hash` sidecar exists and `reproducible.checkRemoteHash = FALSE`
#      (the default), trust it and skip the network round-trip entirely.
#   2. Otherwise HEAD the remote (`getRemoteMetadata`) for size, hash, and
#      hash algorithm.
#   3. If `remote.size != local.size` → return; let `pp_download` proceed.
#   4. If `remote.algorithm == "etag-opaque"` (HTTP ETag we cannot reproduce
#      locally — no positive trust possible) → return; `pp_download` proceeds.
#   5. Digest local file with the remote's algorithm; if hashes differ →
#      return; `pp_download` proceeds.
#   6. On match: write `.hash` sidecar (`<algo>:<hash>`), upsert the
#      `(file, hash, size, algorithm)` row in CHECKSUMS.txt, mark
#      `skipDownload` and `hashVerified`.
#
# Any network/package error causes a silent fall-through to the normal
# download (best-effort optimisation, not a hard precondition).
pp_remote_hash_check <- function(ctx) {
  if (is.null(ctx$url)) return(ctx)
  # Skip file:// URLs only: they are local (no remote to compare against),
  # and makeRemoteHashFile's URL-to-filename mapping only strips ^https?://,
  # so file:// leaves a colon in the path and fails on Windows. For any
  # other scheme, attempt the metadata fetch — getRemoteMetadata is wrapped
  # in tryCatch below, so unsupported schemes (s3://, gs://, ...) silently
  # fall through to the normal download.
  if (grepl("^file://", ctx$url)) return(ctx)

  # Identify the local file that would be the download target
  localFile <- if (!is.null(ctx$archive) && !isTRUE(is.na(ctx$archive)) &&
                    file.exists(ctx$archive)) {
    ctx$archive
  } else {
    nf <- ctx$neededFiles
    if (!is.null(nf) && length(nf) > 0L) {
      nf <- nf[file.exists(nf)]
      if (length(nf)) nf[[1L]] else NULL
    }
  }
  if (is.null(localFile)) return(ctx)   # nothing local yet — let download proceed

  # ------------------------------------------------------------------
  # Step 1: sidecar fast-path (no network)
  # ------------------------------------------------------------------
  # A `.hash` sidecar from a previous successful match means the file has
  # already been validated against the remote source. By default we trust it
  # and skip the network round-trip entirely. To force a remote re-check on
  # every call (e.g. when the upstream file may change), set
  # `options(reproducible.checkRemoteHash = TRUE)`. Sidecar removal forces
  # re-verification.
  remoteHashFileLocal <- makeRemoteHashFile(
    ctx$url, ctx$destinationPath, basename(localFile), ""
  )
  haveSidecar <- file.exists(remoteHashFileLocal) &&
    isTRUE(suppressWarnings(file.size(remoteHashFileLocal)) > 0L)
  if (haveSidecar &&
      !isTRUE(getOption("reproducible.checkRemoteHash", FALSE))) {
    messagePreProcess(
      "Skipping download; local file matches remote version (cached): ",
      .messageFunctionFn(basename(localFile)), verbose = ctx$verbose
    )
    ctx$skipDownload <- TRUE
    ctx$hashVerified <- unique(c(ctx$hashVerified, localFile))
    if (is.null(ctx$archive) && !is.null(.isArchive(localFile)))
      ctx$archive <- localFile
    return(ctx)
  }

  # ------------------------------------------------------------------
  # Step 2: HEAD remote
  # ------------------------------------------------------------------
  remoteMetadata <- tryCatch(
    getRemoteMetadata(url = ctx$url),
    error = function(e) NULL
  )
  if (is.null(remoteMetadata)) return(ctx)

  # ------------------------------------------------------------------
  # Step 3: size fail-fast (negative-only)
  # ------------------------------------------------------------------
  remoteSize <- suppressWarnings(as.numeric(remoteMetadata$fileSize))
  localSize  <- file.size(localFile)
  if (!is.na(remoteSize) && !is.na(localSize) && remoteSize != localSize) {
    return(ctx)  # bytes definitely differ → let pp_download fetch
  }

  # ------------------------------------------------------------------
  # Step 4: opaque ETag → no positive trust possible → download
  # ------------------------------------------------------------------
  remoteAlgo <- remoteMetadata$remoteAlgorithm
  if (is.null(remoteAlgo) || identical(remoteAlgo, "etag-opaque") ||
      !nzchar(remoteAlgo)) {
    return(ctx)
  }

  # ------------------------------------------------------------------
  # Step 4b: existing sidecar with checkRemoteHash = TRUE — quick check.
  # If the sidecar's recorded hash already matches what the server now
  # advertises, skip re-digesting the local file.
  # ------------------------------------------------------------------
  if (haveSidecar) {
    parsedSidecar <- .parseRemoteHashFile(remoteHashFileLocal)
    if (!is.null(parsedSidecar) &&
        identical(parsedSidecar$hash, remoteMetadata$remoteHash)) {
      messagePreProcess(
        "Skipping download; local file matches remote version: ",
        .messageFunctionFn(basename(localFile)), verbose = ctx$verbose
      )
      ctx$skipDownload   <- TRUE
      ctx$remoteMetadata <- remoteMetadata
      ctx$hashVerified   <- unique(c(ctx$hashVerified, localFile))
      if (is.null(ctx$archive) && !is.null(.isArchive(localFile)))
        ctx$archive <- localFile
      return(ctx)
    }
  }

  # ------------------------------------------------------------------
  # Step 5: digest local file with remote algorithm and compare
  # ------------------------------------------------------------------
  localHash <- tryCatch(
    .digest(localFile, algo = remoteAlgo, quickCheck = FALSE),
    error = function(e) NULL
  )
  if (is.null(localHash) || !length(localHash) ||
      !identical(tolower(localHash[[1L]]),
                 tolower(as.character(remoteMetadata$remoteHash)))) {
    return(ctx)  # hash mismatch (or compute failed) → download
  }

  # ------------------------------------------------------------------
  # Step 6: confirmed match — persist sidecar + CHECKSUMS row, skip download
  # ------------------------------------------------------------------
  makeRemoteHashFile(
    ctx$url, ctx$destinationPath,
    remoteMetadata$targetFile, remoteMetadata$remoteHash,
    algorithm = remoteAlgo, write = TRUE
  )

  # Upsert (file, algorithm) row in CHECKSUMS.txt without re-digesting via
  # Checksums(write=TRUE) — we already have the hash. Preserves any other
  # algorithm rows recorded for this file (e.g. xxhash64 from a prior
  # pp_finalize).
  csfp <- ctx$checkSumFilePath
  if (is.null(csfp) || !nzchar(csfp))
    csfp <- identifyCHECKSUMStxtFile(ctx$destinationPath)
  tryCatch(
    .upsertChecksumsRow(
      checkSumFilePath = csfp,
      file      = makeRelative(localFile, ctx$destinationPath),
      hash      = remoteMetadata$remoteHash,
      filesize  = localSize,
      algorithm = remoteAlgo
    ),
    error = function(e) NULL
  )

  messagePreProcess(
    "Skipping download; local file matches remote version: ",
    .messageFunctionFn(basename(localFile)), verbose = ctx$verbose
  )
  ctx$skipDownload   <- TRUE
  ctx$remoteMetadata <- remoteMetadata
  ctx$hashVerified   <- unique(c(ctx$hashVerified, localFile))
  if (is.null(ctx$archive) && !is.null(.isArchive(localFile)))
    ctx$archive <- localFile

  ctx
}

# ---------------------------------------------------------------------------
# Phase 7: Download
# ---------------------------------------------------------------------------
pp_download <- function(ctx) {
  # ------------------------------------------------------------------
  # Fast path: pp_remote_hash_check confirmed local file == remote.
  # Skip downloadFile() entirely; synthesise a minimal result so the
  # .checkForSimilar / filesToChecksum logic below works unchanged.
  # ------------------------------------------------------------------
  if (isTRUE(ctx$skipDownload)) {
    downloadFileResult <- list(
      downloaded    = ctx$archive,
      archive       = ctx$archive,
      neededFiles   = ctx$neededFiles,
      checkSums     = ctx$checkSums,
      needChecksums = ctx$needChecksums,
      targetFilePath = NULL,
      out           = NULL
    )
  } else {
  # ------------------------------------------------------------------
  # Normal path: call downloadFile()
  # dlFun may be a call/language object (e.g. quote(getDataFn(...))).
  # do.call() EVALUATES language objects in its args list by calling eval() on
  # each element — so passing a call object through do.call executes it
  # immediately. Fix: capture dlFunCaptured as a local variable and call
  # downloadFile via a closure, so dlFun is passed as a lazy promise (variable
  # lookup) rather than being evaluated by do.call.
  dlFunCaptured <- ctx$dlFunCaptured
  dlArgs <- c(
    list(
      archive        = if (isTRUE(is.na(ctx$archive))) NULL else ctx$archive,
      targetFile     = ctx[["targetFile"]],
      neededFiles    = ctx$neededFiles,
      destinationPath = ctx$destinationPath,
      quick          = ctx$quick,
      checkSums      = ctx$checkSums,
      url            = ctx$url,
      checksumFile   = asPath(ctx$checkSumFilePath),
      needChecksums  = ctx$needChecksums,
      overwrite      = ctx$overwrite,
      purge          = ctx$purge,
      alsoExtract    = ctx$alsoExtract,
      verbose        = ctx$verbose,
      .tempPath      = ctx$.tempPath,
      .callingEnv    = ctx$.callingEnv
    ),
    ctx$dots
  )
  # The closure captures dlFunCaptured; do.call only sees non-language args.
  downloadFile_wrapper <- function(...) downloadFile(dlFun = dlFunCaptured, ...)
  downloadFileResult <- do.call(downloadFile_wrapper, dlArgs)
  } # end normal path

  if (!isTRUE(ctx$skipDownload))
    downloadFileResult <- .fixNoFileExtension(
      downloadFileResult = downloadFileResult,
      targetFile = ctx[["targetFile"]], archive = ctx$archive,
      destinationPath = ctx$destinationPath, verbose = ctx$verbose
    )

  if (!is.null(downloadFileResult$targetFilePath))
    ctx$targetFilePath <- makeAbsolute(downloadFileResult$neededFiles, ctx$destinationPath)
  ctx$checkSums     <- downloadFileResult$checkSums
  ctx$needChecksums <- downloadFileResult$needChecksums
  ctx$neededFiles   <- downloadFileResult$neededFiles

  if (!isTRUE(is.na(ctx$archive))) {
    if (identical(downloadFileResult$downloaded, downloadFileResult$archive))
      ctx$archive <- downloadFileResult$downloaded
    if (is.null(ctx$archive))
      ctx$archive <- downloadFileResult$archive
  }

  outFromSimilar  <- .checkForSimilar(
    neededFiles = ctx$neededFiles, alsoExtract = ctx$alsoExtract,
    archive = ctx$archive, targetFile = ctx[["targetFile"]],
    destinationPath = ctx$destinationPath, checkSums = ctx$checkSums,
    checkSumFilePath = ctx$checkSumFilePath, url = ctx$url,
    verbose = ctx$verboseCFS
  )
  ctx$verboseCFS  <- ctx$verbose - 1L
  ctx$neededFiles <- outFromSimilar$neededFiles
  ctx$checkSums   <- outFromSimilar$checkSums

  if (length(ctx$neededFiles) > 1L)
    ctx$alsoExtract <- setdiff(ctx$neededFiles, ctx[["targetFile"]])

  ctx$filesToChecksum <- if (isTRUE(is.na(ctx$archive)) || is.null(ctx$archive)) {
    downloadFileResult$downloaded
  } else {
    ctx$archive
  }
  ctx$downloadResult <- downloadFileResult
  ctx
}

# ---------------------------------------------------------------------------
# Phase 8: Extract from archive (single consolidated call)
# ---------------------------------------------------------------------------
pp_extract <- function(ctx) {
  # Expand any regex patterns in alsoExtract now that the archive is on disk
  # (covers the download path where expansion in pp_resolve_needed_files was
  # skipped because the archive didn't exist yet).  If the expansion changes
  # alsoExtract we also patch ctx$neededFiles: remove the fake absolute paths
  # that makeAbsolute() created from the unresolved pattern strings and replace
  # them with the real expanded paths.
  if (!isNULLorNA(ctx$alsoExtract) &&
      !isNULLorNA(ctx$archive) && any(file.exists(ctx$archive))) {
    fia <- makeRelative(.listFilesInArchive(ctx$archive), ctx$destinationPath)
    if (length(fia)) {
      oldAbs <- makeAbsolute(ctx$alsoExtract, ctx$destinationPath)
      ctx$alsoExtract <- .expandAlsoExtractPatterns(ctx$alsoExtract, fia)
      newAbs <- makeAbsolute(ctx$alsoExtract, ctx$destinationPath)
      if (!identical(oldAbs, newAbs))
        ctx$neededFiles <- unique(c(setdiff(ctx$neededFiles, oldAbs), newAbs))
    }
  }

  filesToChecksum <- unique(c(ctx$filesToChecksum, ctx$neededFiles))
  isOK <- .compareChecksumsAndFilesAddDirs(ctx$checkSums, filesToChecksum, ctx$destinationPath)

  if (isTRUE(!all(isOK))) {
    extracted <- .tryExtractFromArchive(
      archive = ctx$archive, neededFiles = ctx$neededFiles,
      alsoExtract = ctx$alsoExtract, destinationPath = ctx$destinationPath,
      checkSums = ctx$checkSums, needChecksums = ctx$needChecksums,
      checkSumFilePath = ctx$checkSumFilePath, filesToChecksum = filesToChecksum,
      targetFilePath = ctx$targetFilePath, quick = ctx$quick,
      verbose = ctx$verbose, .tempPath = ctx$.tempPath
    )
    ctx$neededFiles    <- extracted$neededFiles
    ctx$checkSums      <- extracted$checkSums
    ctx$filesExtr      <- extracted$filesExtr
    ctx$targetFilePath <- extracted$targetFilePath
    filesToChecksum    <- extracted$filesToChecksum
    ctx$needChecksums  <- extracted$needChecksums
  } else {
    if (!is.null(.isArchive(ctx$archive)))
      messagePreProcess("Skipping extractFromArchive attempt: no files missing",
                        verbose = ctx$verbose)
    if (!is.null(ctx$targetFilePath) && isTRUE(!is.na(ctx$targetFilePath))) {
      if (any(!makeAbsolute(ctx$targetFilePath, ctx$destinationPath) %in%
              makeAbsolute(ctx$neededFiles, ctx$destinationPath))) {
        if (!basename2(ctx$targetFilePath) %in%
            makeRelative(ctx$neededFiles, ctx$destinationPath)) {
          poss <- grep(basename2(ctx$targetFilePath), ctx$neededFiles, value = TRUE)
          if (length(ctx$targetFilePath) > 1L) ctx$targetFilePath <- poss
        }
      }
    }
    ctx$filesExtr <- setdiff(
      unique(c(filesToChecksum, ctx$neededFiles)),
      .isArchive(c(filesToChecksum, ctx$neededFiles))
    )
  }

  ctx$filesExtr       <- unique(c(ctx$filesExtr, filesToChecksum))
  ctx$filesToChecksum <- filesToChecksum

  # Handle nested archives
  if (any(fileExt(ctx$neededFiles) %in% c("zip", "tar", "rar")) &&
      !isTRUE(is.na(ctx$archive))) {
    nestedArchive <- makeAbsolute(
      ctx$neededFiles[fileExt(ctx$neededFiles) %in% c("zip", "tar", "rar")][1L],
      ctx$destinationPath
    )
    messagePreProcess(
      "There are still archives in the extracted files.",
      " preProcess will try to extract the files from ", basename2(nestedArchive), ".",
      " If this is incorrect, please supply archive.",
      verbose = ctx$verbose
    )
    nestedTargetFiles  <- .listFilesInArchive(archive = nestedArchive)
    outFromSimilar     <- .checkForSimilar(
      alsoExtract = ctx$alsoExtract, archive = nestedArchive,
      neededFiles = nestedTargetFiles, destinationPath = ctx$destinationPath,
      checkSums = ctx$checkSums, checkSumFilePath = ctx$checkSumFilePath,
      targetFile = ctx[["targetFile"]], verbose = ctx$verboseCFS
    )
    neededFilesNested  <- outFromSimilar$neededFiles
    ctx$checkSums      <- outFromSimilar$checkSums
    alsoExtractNested  <- if (length(neededFilesNested) > 1L) {
      setdiff(neededFilesNested, ctx[["targetFile"]])
    } else {
      ctx$alsoExtract
    }
    ftcNested <- if (is.null(ctx$archive)) {
      ctx$downloadResult$downloaded
    } else {
      basename2(ctx$archive)
    }
    extractedNested    <- .tryExtractFromArchive(
      archive = nestedArchive, neededFiles = neededFilesNested,
      alsoExtract = alsoExtractNested, destinationPath = ctx$destinationPath,
      checkSums = ctx$checkSums, needChecksums = ctx$needChecksums,
      checkSumFilePath = ctx$checkSumFilePath, filesToChecksum = ftcNested,
      targetFilePath = ctx$targetFilePath, quick = ctx$quick,
      verbose = ctx$verbose, .tempPath = ctx$.tempPath
    )
    feOrig   <- fs::path_rel(ctx$filesExtr, fs::path_common(ctx$filesExtr))
    absFiles <- fs::is_absolute_path(extractedNested$filesExtr)
    feNew    <- c(
      extractedNested$filesExtr[!absFiles],
      fs::path_rel(
        extractedNested$filesExtr[absFiles],
        fs::path_common(extractedNested$filesExtr[absFiles])
      )
    )
    ctx$filesExtr      <- c(setdiff(ctx$filesExtr[!feOrig %in% feNew], ctx$archive),
                            extractedNested$filesExtr)
    ctx$needChecksums  <- extractedNested$needChecksums
    ctx$checkSums      <- extractedNested$checkSums
    ctx$targetFilePath <- extractedNested$targetFilePath
  }
  ctx
}

# ---------------------------------------------------------------------------
# Phase 9: Link files back to user's destinationPath if inputPaths was used
# ---------------------------------------------------------------------------
pp_link_to_destination <- function(ctx) {
  if (is.null(ctx$reproducible.inputPaths)) return(ctx)

  if (!is.null(ctx$destinationPathUser)) {
    foundInInputPaths <- grepl(normPath(ctx$destinationPath), normPath(ctx$filesExtr))
    to <- ctx$targetFilePath
    if (isTRUE(any(foundInInputPaths))) {
      wh <- which(file.exists(ctx$filesExtr[foundInInputPaths]))
      if (length(wh)) {
        from <- ctx$filesExtr[wh]
        to   <- makeAbsolute(makeRelative(from, ctx$destinationPath), ctx$destinationPathUser)
        if (!isTRUE(all(from %in% to)))
          messagePreProcess("...using file(s) in getOption('reproducible.inputPaths')...",
                            verbose = ctx$verbose)
        hardLinkOrCopy(from, to, verbose = ctx$verbose)
        ctx$filesExtr[foundInInputPaths] <- to
      }
    }
    if (!is.null(ctx$targetFilePath) && !identical(to, ctx$targetFilePath)) {
      tmp <- to[basename(to) %in% basename(ctx$targetFilePath)]
      if (any(file.exists(tmp))) {
        ctx$targetFilePath <- tmp
      } else {
        ctx$targetFilePath <- makeAbsolute(
          makeRelative(ctx$targetFilePath, ctx$destinationPath),
          ctx$destinationPathUser
        )
      }
    }
    ctx$destinationPath <- ctx$destinationPathUser
  } else {
    foundInLocalPaths <- grepl(normPath(ctx$destinationPath), normPath(ctx$filesExtr))
    if (isTRUE(any(foundInLocalPaths))) {
      wh <- which(file.exists(ctx$filesExtr[foundInLocalPaths]))
      if (length(wh)) {
        from <- ctx$filesExtr[wh]
        to   <- from
        for (riP in ctx$reproducible.inputPaths) {
          to <- makeAbsolute(makeRelative(from, ctx$destinationPath), riP)
          if (all(from %in% to)) break
        }
        a            <- fread(ctx$checkSumFilePath)
        common       <- ctx$checkSums[ctx$checkSums$expectedFile %in% a$file]
        missingFiles <- common[!a, on = c("expectedFile" = "file", "checksum.x" = "checksum")]
        if (NROW(missingFiles)) {
          messagePreProcess("... linking to getOption('reproducible.destinationPathShared')...",
                            verbose = ctx$verbose)
          hardLinkOrCopy(from, to, verbose = ctx$verbose)
        } else {
          messagePreProcess("Skipping copy from destinationPathShared; all files present",
                            verbose = ctx$verbose)
        }
      }
    }
  }
  ctx
}

# ---------------------------------------------------------------------------
# Phase 10: Finalize — guess target/fun, write checksums, validate, return
# ---------------------------------------------------------------------------
pp_finalize <- function(ctx) {
  targetParams       <- .guessAtTargetAndFun(
    ctx$targetFilePath, ctx$destinationPath,
    filesExtracted = ctx$filesExtr, ctx$fun, verbose = ctx$verbose
  )
  ctx[["targetFile"]]     <- makeRelative(targetParams$targetFilePath, ctx$destinationPath)
  ctx$targetFilePath <- targetParams$targetFilePath
  ctx$funChar        <- targetParams$fun

  if (is.null(ctx$targetFilePath)) {
    ctx$targetFilePath <- if (!is.null(ctx$filesExtr)) ctx$filesExtr else ctx$downloadResult$downloaded
  }
  if (is.null(ctx[["targetFile"]]) && !is.null(ctx$targetFilePath))
    ctx[["targetFile"]] <- makeRelative(ctx$targetFilePath, ctx$destinationPath)

  fun <- .extractFunction(ctx$funChar)

  # Write checksums (replaces 3 scattered on.exit blocks)
  needChecksums <- ctx$needChecksums
  # Drop files that pp_remote_hash_check already validated against the remote
  # sidecar (.hash file) — re-hashing a multi-GB archive locally to populate
  # CHECKSUMS.txt is wasted work; the .hash file is already the authoritative
  # record for this run and any future run. Also drop NAs (which can land in
  # filesToChecksum from the skipDownload path where downloaded == archive == NA)
  # so they don't keep the "write" branch alive on otherwise-empty input.
  filesToChecksum <- ctx$filesToChecksum
  filesToChecksum <- filesToChecksum[!is.na(filesToChecksum)]
  if (length(ctx$hashVerified) && length(filesToChecksum)) {
    fileBases     <- basename2(filesToChecksum)
    verifiedBases <- basename2(ctx$hashVerified)
    filesToChecksum <- filesToChecksum[!fileBases %in% verifiedBases]
  }
  if (needChecksums > 0L && length(filesToChecksum) > 0L) {
    if (needChecksums == 3L) {
      if (identical(ctx$checkSumFilePath, ctx$successfulCheckSumFilePath))
        ctx$checkSumFilePath <- identifyCHECKSUMStxtFile(ctx$successfulDir)
    }
    csps <- ctx$destinationPath
    if (!is.null(ctx$reproducible.inputPaths))
      csps <- c(csps, ctx$reproducible.inputPaths)
    for (csp in csps) {
      csfp <- identifyCHECKSUMStxtFile(csp)
      ctx$checkSums <- appendChecksumsTable(
        checkSumFilePath = csfp,
        filesToChecksum  = basename2(unique(filesToChecksum)),
        destinationPath  = csp,
        append           = needChecksums >= 2L
      )
    }
    if (!is.null(ctx$reproducible.inputPaths) && needChecksums != 3L) {
      ctx$checkSums <- appendChecksumsTable(
        checkSumFilePath = identifyCHECKSUMStxtFile(ctx$reproducible.inputPaths[[1L]]),
        filesToChecksum  = unique(filesToChecksum),
        destinationPath  = ctx$destinationPath,
        append           = needChecksums == 2L,
        verbose          = ctx$verbose - 1L
      )
    }
  }

  if (isTRUE(isDirectory(ctx$url, mustExist = FALSE)) && is.null(ctx[["targetFile"]])) {
    messagePrepInputs(
      "url pointed to a directory, but no `targetFile` specified; using targetFilePath:\n",
      paste0(ctx$downloadResult$downloaded, collapse = "\n")
    )
    ctx$targetFilePath <- ctx$downloadResult$downloaded
  }

  failStop <- FALSE
  if (is.null(ctx$targetFilePath)) {
    failStop <- TRUE
  } else if (isTRUE(all(is.na(ctx$targetFilePath)))) {
    if (length(ctx$targetFilePath) > 1L) ctx$targetFilePath <- NA
  } else if (!isTRUE(all(file.exists(ctx$targetFilePath)))) {
    failStop <- TRUE
  }
  if (isTRUE(failStop))
    stop("targetFile appears to be misspecified at: ", ctx$targetFilePath, ". ",
         "Possibly, it does not exist in the specified archive, ",
         "or the file doesn't exist in destinationPath")

  # Only mark rows as "ArchiveOK" when there is actually an archive. With
  # archive = NA, makeRelative(NA, ...) is NA and `%in% NA` matches any
  # NA actualFile rows, producing a spurious assignment.
  if (!isNULLorNA(ctx$archive)) {
    archiveInChecksums <- ctx$checkSums$actualFile %in%
      makeRelative(ctx$archive, ctx$destinationPath)
    if (any(archiveInChecksums))
      ctx$checkSums[which(archiveInChecksums), result := "ArchiveOK"]
  }

  list(
    checkSums       = ctx$checkSums,
    dots            = ctx$dots,
    fun             = fun,
    funChar         = ctx$funChar,
    targetFilePath  = ctx$targetFilePath,
    destinationPath = ctx$destinationPath,
    object          = ctx$downloadResult$object
  )
}

#' Purge individual line items from checksums file
#'
#' @inheritParams downloadFile
#' @keywords internal
#' @rdname purge
.purge <- function(checkSums, purge, targetFile, archive, alsoExtract, url, destinationPath) {
  purgeChar <- as.character(purge)
  checkSums <- tryCatch(
    switch(purgeChar,
           "2" = checkSums[!(checkSums$expectedFile %in% makeRelative(targetFile, destinationPath)), ],
           "3" = checkSums[!(checkSums$expectedFile %in% makeRelative(archive, destinationPath)), ],
           "4" = checkSums[!(checkSums$expectedFile %in% makeRelative(alsoExtract, destinationPath)), ],
           "5" = checkSums[!(checkSums$expectedFile %in% makeRelative(unique(c(targetFile, alsoExtract)), destinationPath)), ], # nolint
           "6" = checkSums[!(checkSums$expectedFile %in%
                               makeRelative(unique(c(targetFile, alsoExtract, archive)), destinationPath)), ], # nolint
           "7" = checkSums[!(checkSums$expectedFile %in%
                               makeRelative(unique(c(targetFile, alsoExtract, archive, url)), destinationPath)), ] # nolint
    ),
    error = function(x) checkSums
  )
  checkSums
}

#' @keywords internal
.emptyChecksumsResult <- data.table::data.table(
  expectedFile = character(),
  actualFile = character(),
  result = character()
)

#' @keywords internal
.emptyChecksumsFileContent <- data.frame(
  file = character(),
  checksum = character(),
  filesize = character(),
  algorithm = character()
)

#' Decide how to materialize a captured `dlFun` expression
#'
#' `captured` is the result of `substitute(dlFun)`; `lazy` is the (still-promise)
#' formal `dlFun` argument. See preProcess() for the three cases this handles.
#' @keywords internal
#' @noRd
.resolveDlFunCaptured <- function(captured, lazy) {
  if (is.call(captured)) {
    head <- captured[[1L]]
    # quote(...) / bquote(...) — unwrap to the inner call
    if (is.symbol(head) && as.character(head) %in% c("quote", "bquote"))
      return(eval(captured))
    # pkg::fn(...) or pkg:::fn(...) — a namespaced function call to defer
    if (is.call(head) && is.symbol(head[[1L]]) &&
        as.character(head[[1L]]) %in% c("::", ":::"))
      return(captured)
    # fn(...) — a plain function call to defer, unless `fn` is a control-flow
    # / special-form name (in which case the expression must be evaluated now)
    controlFlow <- c("if", "for", "while", "repeat", "{", "(",
                     "&&", "||", "<-", "<<-", "=", "function", "~", "@",
                     "$", "[", "[[", "::", ":::")
    if (is.symbol(head) && !(as.character(head) %in% controlFlow))
      return(captured)
  }
  lazy  # symbol, literal, or control-flow call: force the promise
}

#' @keywords internal
#' @importFrom utils getFromNamespace
.extractFunction <- function(fun, envir = parent.frame()) {
  if (!is.null(fun)) {
    if (is.name(fun)) {
      possFun <- get0(fun, envir = envir)
      if (is.null(possFun)) {
        env <- .whereInStack(fun)
        fun <- get(fun, envir = env)
      }
    }
    if (is.call(fun)) {
      fun
    } else {
      suppressWarnings(isNAFun <- is.na(fun))
      if (!any(isNAFun)) {
        if (!is.function(fun)) {
          if (any(grepl("::", fun))) {
            fun2 <- strsplit(fun, "::")[[1]]
            pkg <- fun2[1]
            fun <- fun2[2]
            fun <- getFromNamespace(fun, pkg)
          } else {
            fun <- get(fun, envir)
          }
        }
      }
    }
  }
  fun
}

#' @keywords internal
.guessAtFile <- function(url, archive, targetFile, destinationPath,
                         verbose = getOption("reproducible.verbose", 1), team_drive = NULL) {
  # if (is.null(targetFile)) {
  guessedFile <- if (!is.null(url)) {
    if (isTRUE(isDirectory(url, FALSE))) {
      gf <- NULL
    } else {
      gf <- file.path(destinationPath, basename2(url))
    }

    # Test for just Google ID supplied
    isGID <- isGoogleID(url)
    # isGID <- all(grepl("^[A-Za-z0-9_-]{33}$", url), # Has 33 characters as letters, numbers or - or _
    #              !grepl("\\.[^\\.]+$", url))
    # doesn't have an extension
    if (any(isGoogleDriveURL(url), isGID)) {
      if (isGID) messagePreProcess("url seems to be a Google Drive ID", verbose = verbose, verboseLevel = 2)

      # if (grepl("drive.google.com", url)) {
      # ie <- isTRUE(internetExists())
      # if (ie) {
      gf <- assessGoogle(
        url = url, archive = archive, targetFile = targetFile,
        destinationPath = destinationPath, verbose = verbose, team_drive = NULL
      )
      gf <- makeAbsolute(gf, destinationPath)
      # }
    }
    gf
  } else {
    NULL
  }
  if (!is.null(guessedFile))
    guessedFile <- normPath(guessedFile)
  guessedFile
}


isGoogleID <- function(url) {
  all(grepl("^[A-Za-z0-9_-]{33}$", url), # Has 33 characters as letters, numbers or - or _
      !grepl("\\.[^\\.]+$", url)) # ||
    # grepl("drive.google.com", url)
}

isGoogleDriveURL <- function(url) {
  grepl("drive.google.com", url)
}
# COPIED FROM REQUIRE
# urlExists <- function(url) {
#   con <- url(url)
#   on.exit(try(close(con), silent = TRUE), add = TRUE)
#   for (i in 1:5) {
#     a <- try(suppressWarnings(readLines(con, n = 1)), silent = TRUE)
#     try(close(con), silent = TRUE)
#     ret <- if (is(a, "try-error")) FALSE else TRUE
#     if (isTRUE(ret)) {
#       break
#     } else {
#       Sys.sleep(0.1)
#     }
#   }
#   ret
# }


#' @keywords internal
.checkSumsUpdate <- function(destinationPath, newFilesToCheck, checkSums,
                             checkSumFilePath = NULL, verbose = getOption("reproducible.verbose", 1)) {
  if (!is.null(newFilesToCheck)) {
    if (is.null(checkSumFilePath) || length(checkSumFilePath) == 0) {
      checkSumFilePath <- identifyCHECKSUMStxtFile(destinationPath)
    }
    if (!file.exists(checkSumFilePath)) {
      checkSums
    } else {
      checkSums2 <- try(Checksums(
        path = destinationPath, write = FALSE,
        files = newFilesToCheck,
        checksumFile = checkSumFilePath,
        verbose = verbose - 1
      ), silent = TRUE)
      if (!is(checkSums2, "try-error")) {
        checkSums <- rbindlist(list(checkSums, checkSums2))
        data.table::setkey(checkSums, result)
        checkSums <- unique(checkSums, fromLast = TRUE, by = "expectedFile")
        checkSums <- rbindlist(list(
          checkSums[compareNA("OK", result)],
          checkSums[compareNA("FAIL", result)],
          checkSums[is.na(result)]
        ))
      } else {
        stop("checkSumFilePath is not a CHECKSUMS.txt file")
      }
    }
  }
  checkSums
}

#' @keywords internal
.similarFilesInCheckSums <- function(file, checkSums, alsoExtract) {
  if (NROW(checkSums)) {
    if (!missing(alsoExtract)) {
      file <- unique(c(file, alsoExtract))
    }
    anySimilarInCS <- vapply(filePathSansExt(file), function(fi) {
      res <- checkSums[grepl(paste0(fi, "\\."), checkSums$expectedFile), ]$result
      res <- na.omit(res)
      if (length(res) == 0) res <- "NotOK"
      isTRUE(all(compareNA("OK", res)))
    }, FUN.VALUE = logical(1))

    all(anySimilarInCS)
  } else {
    FALSE
  }
}

#' Expand regex patterns in `alsoExtract` against a known list of archive files.
#'
#' For each element of `alsoExtract` that is not a literal match in
#' `filesInArchive` (by relative path or basename), the element is treated as a
#' regular expression and `grep()`-ed against `filesInArchive`.  Matched names
#' replace the pattern; unmatched patterns are kept as-is so that downstream
#' code can emit a useful "file not found" error.  Special sentinel values
#' (`"similar"`, `"none"`, `NA`) are passed through unchanged.
#'
#' This lets users write e.g.
#' `alsoExtract = "CMD_sm|CMD_sp"` to select all archive members whose name
#' contains `CMD_sm` or `CMD_sp`.
#'
#' @param alsoExtract Character vector (or `NULL`).
#' @param filesInArchive Character vector of files inside the archive, as
#'   returned by `makeRelative(.listFilesInArchive(archive), destinationPath)`.
#' @return A character vector with pattern elements replaced by their matches.
#' @keywords internal
.expandAlsoExtractPatterns <- function(alsoExtract, filesInArchive) {
  if (is.null(alsoExtract) || length(alsoExtract) == 0L ||
      length(filesInArchive) == 0L) return(alsoExtract)
  out <- character(0L)
  for (pat in alsoExtract) {
    if (is.na(pat) || pat %in% c("similar", "none")) {
      out <- c(out, pat)
      next
    }
    # Literal match by relative path or basename → keep as-is
    if (pat %in% filesInArchive || pat %in% basename2(filesInArchive)) {
      out <- c(out, pat)
    } else {
      # Treat as regex pattern; grep against archive member names
      matched <- grep(pat, filesInArchive, value = TRUE)
      out <- c(out, if (length(matched)) matched else pat)
    }
  }
  out
}

#' @keywords internal
.checkForSimilar <- function(neededFiles, alsoExtract, archive, targetFile,
                             destinationPath, checkSums, checkSumFilePath,
                             url, verbose = getOption("reproducible.verbose", 1)) {
  # No archive => the alsoExtract / "files in archive" messaging below is not
  # meaningful; return inputs unchanged so callers (e.g. archive = NA) don't
  # see spurious "alsoExtract is unspecified" / "Checksumming all files in
  # archive" output.
  if (isNULLorNA(archive))
    return(list(neededFiles = neededFiles, checkSums = checkSums))
  lookForSimilar <- FALSE
  if (is.null(alsoExtract) || length(alsoExtract) == 0) {
    messagePreProcess("alsoExtract is unspecified; assuming that all files must be extracted",
                      verbose = verbose)
    lookForSimilar <- "all"
  } else {
    if (!all(is.na(alsoExtract))) {
      if ("similar" %in% basename2(alsoExtract)) {
        lookForSimilar <- TRUE
      }
    }
  }

  if (isTRUE(lookForSimilar) || ("all" %in% lookForSimilar && !is.null(archive))) {
    allFiles <- .listFilesInArchive(archive)
    archiveFilesInCS <- allFiles %in% checkSums$expectedFile
    rerunChecksums <- TRUE
    if (any(archiveFilesInCS)) {
      if (all(archiveFilesInCS)) {
        isOK <- checkSums[expectedFile %in% allFiles][, isOK := result %in% "OK"]#$result %in% "OK"
        if (isTRUE(all(isOK$isOK))) {
          rerunChecksums <- FALSE
        } else { # some not OK, but present
          allFiles <- isOK$expectedFile[isOK$isOK %in% FALSE]
        }
      } else { # some files in the archive are not yet in checkSums -- rerunChecksums on these
        allFiles <- allFiles[!archiveFilesInCS]
      }
    }
    if (rerunChecksums) {
      neededFiles <- checkRelative(neededFiles, destinationPath, allFiles)
      if (is.null(targetFile) || isTRUE(all(is.na(targetFile)))) {
        messagePreProcess("No targetFile supplied. ", messageChecksummingAllFiles,
                          verbose = verbose)
        neededFiles <- allFiles
      } else if ("all" %in% lookForSimilar) {
        messagePreProcess(messageChecksummingAllFiles, verbose = verbose)
        neededFiles <- allFiles
      } else {
        allOK <- .similarFilesInCheckSums(targetFile, checkSums, alsoExtract)
        if (!allOK) {
          filePatternToKeep <- gsub(basename2(targetFile),
                                    pattern = fileExt(basename2(targetFile)), replacement = ""
          )
          filesToGet <- grep(allFiles, pattern = filePatternToKeep, value = TRUE)
          neededFiles <- c(neededFiles, filesToGet)
        }
      }
      if (exists("filesToGet", inherits = FALSE)) {
        if (length(filesToGet) == 0) {
          rerunChecksums <- FALSE
        }
      }
      neededFiles <- unique(makeAbsolute(neededFiles, destinationPath))

      if (!is.null(neededFiles) && rerunChecksums) {
        checkSums <- .checkSumsUpdate(
          destinationPath = destinationPath, newFilesToCheck = neededFiles,
          checkSums = checkSums,
          checkSumFilePath = checkSumFilePath, verbose = verbose
        )
      }
    }
  }
  list(neededFiles = neededFiles, checkSums = checkSums)
}

#' @keywords internal
.checkLocalSources <- function(neededFiles, checkSumFilePath, checkSums, otherPaths, needChecksums,
                               destinationPath, verbose = getOption("reproducible.verbose", 1)) {
  foundInInputPaths <- character()
  successfulCheckSumFilePath <- character()
  successfulDir <- character()
  if (!is.null(neededFiles)) {
    filesInHand <- checkSums[compareNA(checkSums$result, "OK"), ]$expectedFile
    neededFilesRel <- makeRelative(neededFiles, destinationPath)
    if (!all(neededFilesRel %in% filesInHand)) {
      for (op in otherPaths) {
        recursively <- .getDestinationPathSharedRecursive()
        opFiles <- dir(op, recursive = recursively, full.names = TRUE)
        if (any(neededFilesRel %in% basename2(opFiles))) {
          isNeeded <- basename2(opFiles) %in% neededFilesRel
          dirNameOPFiles <- dirname(opFiles[isNeeded])

          # foundRecursively <- dirNameOPFiles != dirname(opFiles[isNeeded])
          # foundRecursively <- basename2(opFiles[isNeeded][foundRecursively])

          uniqueDirsOPFiles <- rev(unique(dirNameOPFiles))

          checkSumFilePathTry <- checkSumFilePath
          # check CHECKSUMS.txt files, first the one in destinationPath, then ones in inputPaths
          for (dirOPFiles in uniqueDirsOPFiles) {
            # for (i in seq(1 + length(uniqueDirsOPFiles))) {
            checkSumFilePathTry <- identifyCHECKSUMStxtFile(dirOPFiles)
            checkSumsInputPath <- suppressMessages(
              Checksums(
                path = dirOPFiles, write = FALSE,
                files = neededFilesRel,
                checksumFile = checkSumFilePathTry,
                verbose = verbose
              )
            )
            isOK <- checkSumsInputPath[checkSumsInputPath$expectedFile %in% neededFilesRel, ]$result
            if (length(isOK)) {
              if (all(compareNA(isOK, "OK"))) {
                needChecksums <- 3 # Basically this means that we *may* have to update
                #   checksums file in either destinationPath or
                #   options("reproducible.inputPaths")
                successfulCheckSumFilePath <- checkSumFilePathTry
                successfulDir <- unique(dirOPFiles)
                break
              }
            }

            checkSumFilePathTry <- identifyCHECKSUMStxtFile(dirOPFiles)
          }
          checkSumsIPOnlyNeeded <- checkSumsInputPath[compareNA(checkSumsInputPath$result, "OK"), ]
          filesInHandIP <- checkSumsIPOnlyNeeded$expectedFile
          filesInHandIPLogical <- neededFilesRel %in% filesInHandIP
          if (any(filesInHandIPLogical)) {
            outHLC <- hardLinkOrCopy(
              from = makeAbsolute(filesInHandIP, dirOPFiles),
              to = neededFiles[filesInHandIPLogical], verbose = verbose
            )
            checkSums <- rbindlist(list(checkSumsIPOnlyNeeded, checkSums))
            checkSums <- unique(checkSums, by = "expectedFile")
          }
          foundInInputPaths <- c(foundInInputPaths, filesInHandIP)
          if (isTRUE(all(filesInHandIPLogical))) {
            break
          }
        }
      }
    }
    # do a check here that destinationPath is already the destinationPathShared
    #   need to emulate the above behaviour
    reproducible.inputPaths <- .getDestinationPathShared()
    if (!is.null(reproducible.inputPaths)) {
      reproducible.inputPaths <- normPath(reproducible.inputPaths)
    }

    if (identical(destinationPath, reproducible.inputPaths)) {
      foundInInputPaths <- filesInHand
      successfulDir <- destinationPath
      successfulCheckSumFilePath <- checkSumFilePath
    }
  }
  list(
    checkSums = checkSums, needChecksums = needChecksums,
    # foundRecursively = foundRecursively,
    neededFiles = neededFiles,
    successfulCheckSumFilePath = successfulCheckSumFilePath,
    successfulDir = successfulDir,
    foundInInputPaths = makeAbsolute(foundInInputPaths, destinationPath)
  )
}

#' Hardlink, symlink, or copy a file
#'
#' Attempt first to make a hardlink. If that fails, try to make
#' a symlink (on non-windows systems and `symlink = TRUE`).
#' If that fails, copy the file.
#'
#' @note Use caution with files-backed objects (e.g., rasters). See examples.
#'
#' @param from,to  Character vectors, containing file names or paths.
#'                 `to` can alternatively be the path to a single existing directory.
#' @param symlink  Logical indicating whether to use symlink (instead of hardlink).
#'                 Default `FALSE`.
#' @inheritParams prepInputs
#' @seealso [file.link()], [file.symlink()], [file.copy()].
#'
#' @author Alex Chubaty and Eliot McIntire
#' @export
#' @return
#' This function is called for its side effects, which will be a `file.link` is that
#' is available or `file.copy` if not (e.g., the two directories are not on the
#' same physical disk).
#'
#' @examples
#'
#' tmpDir <- file.path(tempdir(), "symlink-test")
#' tmpDir <- normalizePath(tmpDir, winslash = "/", mustWork = FALSE)
#' dir.create(tmpDir)
#'
#' f0 <- file.path(tmpDir, "file0.csv")
#' write.csv(iris, f0)
#'
#' d1 <- file.path(tmpDir, "dir1")
#' dir.create(d1)
#' write.csv(iris, file.path(d1, "file1.csv"))
#'
#' d2 <- file.path(tmpDir, "dir2")
#' dir.create(d2)
#' f2 <- file.path(tmpDir, "file2.csv")
#'
#' ## create link to a file
#' linkOrCopy(f0, f2)
#' file.exists(f2) ## TRUE
#' identical(read.table(f0), read.table(f2)) ## TRUE
#'
#' ## deleting the link shouldn't delete the original file
#' unlink(f0)
#' file.exists(f0) ## FALSE
#' file.exists(f2) ## TRUE
#'
#' if (requireNamespace("terra", quietly = TRUE)) {
#'   ## using spatRasters and other file-backed objects
#'   f3a <- system.file("ex/test.grd", package = "terra")
#'   f3b <- system.file("ex/test.gri", package = "terra")
#'   r3a <- terra::rast(f3a)
#'   f4a <- file.path(tmpDir, "raster4.grd")
#'   f4b <- file.path(tmpDir, "raster4.gri")
#'   linkOrCopy(f3a, f4a) ## hardlink
#'   linkOrCopy(f3b, f4b) ## hardlink
#'   r4a <- terra::rast(f4a)
#'
#'   isTRUE(all.equal(r3a, r4a)) # TRUE
#'
#'   ## cleanup
#'   unlink(tmpDir, recursive = TRUE)
#' }
linkOrCopy <- function(from, to, symlink = TRUE, overwrite = TRUE,
                       verbose = getOption("reproducible.verbose", 1)) {
  existsLogical <- file.exists(from)
  existsTo <- file.exists(to)

  toCollapsed <- paste(to, collapse = "\n")
  fromCollapsed <- paste(from, collapse = "\n")
  result <- TRUE

  ## if the filename is the same, you can't copy from self to self
  if (!all(normPath(to) %in% normPath(from))) {
    if (any(existsLogical)) {
      toDirs1 <- unique(dirname(to))
      dirDoesntExist1 <- !dir.exists(toDirs1)

      ## some directories in `from` won't look like directories in the prev `to`
      ##  e.g., test/folder1/folder2
      ##     folder2 could be a file or a dir, and "dir.exists(to)" won't know which
      ##     because test/folder1 didn't exist
      ## So, identify the dirs in the `from`, and those ones will also be dirs in `to`
      fromDirs <- dir.exists(from)
      toDirs2 <- to[fromDirs]
      dirDoesntExist2 <- rep(TRUE, length(toDirs2))

      if (any(dirDoesntExist1) || any(dirDoesntExist2)) {
        needCreate <- unique(c(toDirs1[dirDoesntExist1], toDirs2[dirDoesntExist2]))
        if (any(is.na(needCreate))) {
          needCreate <- na.omit(needCreate)
        }
        lapply(needCreate, dir.create, recursive = TRUE)
      }
      isDir <- dir.exists(to)
      dups <- duplicated(from)

      if (any(existsTo)) {
        unlink(to[existsTo])
      }
      # Try hard link first -- the only type that R deeply recognizes
      result <- captureWarningsToAttr(
        file.link(from[!dups & !isDir], to[!dups & !isDir])
      )
      warns <- attr(result, "warning")
      attr(result, "warning") <- NULL

      if (isTRUE(all(result))) {
        messagePreProcess("Hardlinked ", hardlinkOrSymlinkMessagePrefix, ":", verbose = verbose)
        messagePreProcess("\n", toCollapsed, "\n",
                          whPointsToMess, "\n",
                          fromCollapsed,
                          verbose = verbose - 1
        )
        messagePreProcess(messageNoCopyMade, verbose = verbose)
      } else {
        if ((
          grepl("cannot link.+different disk drive", warns) ||
          grepl("Invalid cross-device link", warns)) && !isTRUE(symlink)) {
          messageColoured("An attempt was made to use hard links to make a quick pointer ",
                          "from one (set of) file(s) to another; \nthis is not possible because ",
                          "the files would be on different drives. Consider changing the paths\n",
                          "so that they will be on the same physical drive", colour = "red",
                          verbose = verbose)
          if (verbose > 0)
            message(warns)
        }
      }

      if (any(grepl("file already exists", warns))) {
        messagePreProcess("File named ", toCollapsed, " already exists; will try to use it/them", verbose = verbose)
        result <- TRUE
      }

      # On *nix types -- try symlink
      if (isFALSE(all(result)) && isTRUE(symlink)) {
        if (!isWindows()) {
          result <- suppressWarnings(file.symlink(from[!result], to[!result]))
          if (isTRUE(all(result))) {
            messagePreProcess("Symlinked", hardlinkOrSymlinkMessagePrefix, verbose = verbose)
            messagePreProcess("\n", toCollapsed, "\n",
                              whPointsToMess, "\n",
                              fromCollapsed,
                              verbose = verbose - 1
            )
            messagePreProcess(messageNoCopyMade, verbose = verbose)
          }
        }
      }

      if (isFALSE(all(result))) {
        len <- length(from[!result])
        # if (len < 50) fromCollapsed[!result] else c(head(fromCollapsed[!result]), tail(fromCollapsed[!result]))
        if (len < 50) {
          fromMess <- from[!result]
          toMess <- to[!result]
        } else {
          fromMess <- c(head(fromCollapsed[!result]), tail(fromCollapsed[!result]))
          toMess <- c(head(toCollapsed[!result], 24), "... (omitting many)", tail(toCollapsed[!result], 24))
        }
        result2 <- try(file.copy(from[!result], to[!result], overwrite = overwrite))
        if (is(result2, "try-error")) browser()

        toMessCollapsed <- paste(toMess, collapse = "\n")
        fromMessCollapsed <- paste(fromMess, collapse = "\n")

        messagePreProcess(singularPlural(c("Copy", "Copies"), l = fromMess), " of ", singularPlural(c("file: ", "files: "), l = fromMess),
                          "\n", fromMessCollapsed, ",\n",
                          singularPlural(c("was", "were"), l = fromMess)," created at:\n",
                          toMessCollapsed, verbose = verbose)
      }
    } else {
      messagePreProcess("File ", fromCollapsed, " does not exist. Not copying.", verbose = verbose)
      result <- FALSE
    }
  }
  return(result)
}

#' @keywords internal
.tryExtractFromArchive <- function(archive,
                                   neededFiles,
                                   filesToChecksum,
                                   alsoExtract,
                                   destinationPath,
                                   checkSums,
                                   needChecksums,
                                   checkSumFilePath,
                                   targetFilePath,
                                   quick, verbose = getOption("reproducible.verbose", 1),
                                   .tempPath) {
  if (missing(.tempPath)) {
    .tempPath <- tempdir2(rndstr(1, 6))
    on.exit(
      {
        unlink(.tempPath, recursive = TRUE)
      },
      add = TRUE
    )
  }
  alsoExtract <- grep("none$", alsoExtract, value = TRUE, invert = TRUE) # remove "none" from neededFiles; for extracting
  neededFiles <- c(neededFiles, if (!isNULLorNA(alsoExtract)) alsoExtract)
  neededFiles <- setdiff(neededFiles, "similar") # remove "similar" from neededFiles; for extracting
  neededFiles <- unique(makeAbsolute(neededFiles, destinationPath)) # unique is b/c neededFiles was absolute and alsoExtract was rel
  alsoExtract <- makeAbsolute(alsoExtract, destinationPath)

  filesExtr <- NULL
  if (!isNULLorNA(archive)) {
    # if (!is.na(archive)) {
    if (any(file.exists(archive))) {
      filesExtracted <- extractFromArchive(
        archive = archive,
        destinationPath = destinationPath,
        neededFiles = neededFiles,
        checkSums = checkSums,
        needChecksums = needChecksums,
        checkSumFilePath = checkSumFilePath,
        quick = quick,
        verbose = verbose,
        .tempPath = .tempPath
      )
      neededFiles <- filesExtracted$neededFiles # will have been potentially corrected if user supplied incorrect relative paths

      targetFilePath <- checkRelative(targetFilePath, destinationPath, neededFiles, verbose = verbose - 1)
      filesToChecksum <- checkRelative(filesToChecksum, destinationPath, neededFiles, verbose = verbose - 1)

      checkSums <- .checkSumsUpdate(
        destinationPath = destinationPath,
        newFilesToCheck = filesExtracted$filesExtracted,
        checkSums = filesExtracted$checkSums, verbose = verbose
      )

      # filesToChecksum may be wrong because of relative path without subfolder
      filesToChecksum <- unique(c(
        filesToChecksum, targetFilePath, # alsoExtract, # alsoExtract will be in filesExtracted$filesExtracted
        filesExtracted$filesExtracted
      ))

      needChecksums <- filesExtracted$needChecksums
      data.table::setDT(filesExtracted$checkSums)
      dontNeedChecksums <- if (NROW(filesExtracted$checkSums) > 0) {
        filesExtracted$checkSums[
          filesExtracted$checkSums$expectedFile %in%
            filesToChecksum & compareNA(result, "OK"),
          expectedFile
        ]
      } else {
        dontNeedChecksums <- character()
      }
      filesToChecksum <- setdiff(filesToChecksum, dontNeedChecksums)

      if (needChecksums > 0) {
        checkSums <- appendChecksumsTable(
          checkSumFilePath = checkSumFilePath,
          filesToChecksum = unique(filesToChecksum),
          destinationPath = destinationPath,
          append = needChecksums >= 2
        )
        needChecksums <- 0
      }

      ## targetFilePath might still be NULL, need destinationPath too
      filesExtr <- unique(c(
        filesToChecksum,
        if (is.null(filesExtracted$filesExtracted) ||
            length(filesExtracted$filesExtracted) == 0) {
          character()
        } else {
          filesExtracted$filesExtracted
        }
      ))
    }
    # }
  }
  if (!is.null(filesExtr)) {
    filesExtr <- unique(filesExtr)
  }
  list(
    filesToChecksum = filesToChecksum, filesExtr = filesExtr,
    needChecksums = needChecksums,
    targetFilePath = targetFilePath,
    neededFiles = neededFiles, checkSums = checkSums
  )
}

#' @keywords internal
.decodeMagicNumber <- function(magicNumberString) {
  fileExt <- if (any(grepl(pattern = "Zip", x = magicNumberString))) {
    ".zip"
  } else if (any(grepl(pattern = "RAR", x = magicNumberString))) {
    ".rar"
  } else if (any(grepl(pattern = "tar", x = magicNumberString))) {
    ".tar"
  } else if (any(grepl(pattern = "TIFF", x = magicNumberString))) {
    ".tif"
  } else if (any(grepl(pattern = "Shapefile", x = magicNumberString))) {
    ".shp"
  } else {
    NULL
  }
  return(fileExt)
}

#' @keywords internal
.guessFileExtension <- function(file) {
  if (isWindows()) {
    tryCatch(
      {
        possLocs <- c(
          "C:/cygwin/bin/file.exe",
          "C:\\cygwin64/bin/file.exe"
        )
        findFile <- file.exists(possLocs)
        if (any(findFile)) {
          fileLoc <- possLocs[findFile][1]
        }

        magicNumber <- captureWarningsToAttr(
          system(paste(fileLoc, file), intern = TRUE)
        )
        warn <- attr(magicNumber, "warning")
        attr(magicNumber, "warning") <- NULL

        if (length(warn) > 0) {
          splitted <- unlist(strsplit(x = file, split = ":/"))
          fileAdapted <- file.path(paste0("/mnt/", tolower(splitted[1])), splitted[2])
          magicNumber <- captureWarningsToAttr(
            shell(paste0("'file ", fileAdapted, "'"), "bash",
                  intern = TRUE,
                  wait = TRUE, translate = FALSE, mustWork = TRUE
            )
          )
          warn <- attr(magicNumber, "warning")
          attr(magicNumber, "warning") <- NULL
        }
        fileExt <- if (length(warn) == 0) {
          .decodeMagicNumber(magicNumberString = magicNumber)
        } else {
          NULL
        }
        return(fileExt)
      },
      error = function(e) {
        fileExt <- NULL
        return(fileExt)
      }
    )
  } else {
    magicNumber <- system(paste0("file ", file), wait = TRUE, intern = TRUE)
    fileExt <- .decodeMagicNumber(magicNumberString = magicNumber)
    return(fileExt)
  }
}

#' @keywords internal
.fixNoFileExtension <- function(downloadFileResult, targetFile, archive,
                                destinationPath, verbose = getOption("reproducible.verbose", 1)) {
  needFinalCopy <- TRUE
  if (!is.null(downloadFileResult$downloaded) &&
      identical(fileExt(downloadFileResult$downloaded), "")) {
    if (!is.null(targetFile) && !identical(fileExt(normPath(basename2(downloadFileResult$neededFiles))), "")) {
      if (is.null(archive)) {
        messagePreProcess(
          "Downloaded file has no extension: targetFile is provided, but archive is not.\n",
          " Downloaded file will be considered as the targetFile. If the downloaded file is an archive\n",
          " that contains the targetFile, please specify both archive and targetFile.",
          verbose = verbose
        )
        newFileWithExtension <- downloadFileResult$neededFiles
      } else {
        messagePreProcess(
          "Downloaded file has no extension: both targetFile and archive are provided.\n",
          " Downloaded file will be considered as the archive.",
          verbose = verbose
        )
        newFileWithExtension <- downloadFileResult$archive
      }
    } else {
      if (!is.null(archive)) {
        messagePreProcess(
          "Downloaded file has no extension: archive is provided. \n",
          " downloaded file will be considered as the archive.",
          verbose = verbose
        )
        downloadFileResult$neededFiles <- archive
        newFileWithExtension <- downloadFileResult$neededFiles
      } else {
        messagePreProcess(
          "Downloaded file has no extension: neither archive nor targetFile are provided. \n",
          "prepInputs will try accessing the file type.",
          verbose = verbose
        )
        fileExt <- .guessFileExtension(file = normPath(downloadFileResult$downloaded))
        if (is.null(fileExt)) {
          messagePreProcess("The file was not recognized by prepInputs. ",
                            "Will assume the file is an archive and add '.zip' extension. ",
                            "If this is incorrect or return error, please supply archive or targetFile",
                            verbose = verbose
          )
          fileExt <- ".zip"
        }
        newFileWithExtension <- paste0(downloadFileResult$neededFiles, fileExt)
        hardLinkOrCopy(downloadFileResult$neededFiles, newFileWithExtension, verbose = 0)
        needFinalCopy <- FALSE
        downloadFileResult$neededFiles <- makeAbsolute(.listFilesInArchive(newFileWithExtension), destinationPath)
        downloadFileResult$archive <- newFileWithExtension
        downloadFileResult$targetFilePath <- normPath(downloadFileResult$neededFiles)
      }
    }
    if (isTRUE(needFinalCopy)) {
      hardLinkOrCopy(
        verbose = 0,
        from = normPath(downloadFileResult$downloaded),
        to = normPath(newFileWithExtension)
      )
    }
    downloadFileResult$downloaded <- newFileWithExtension
  }
  downloadFileResult
}

moveAttributes <- function(source, receiving, attrs = NULL) {
  if (!is.null(receiving)) {
    sourceAttributes <- attributes(source)
    if (length(sourceAttributes) > 0) {
      if (!is.null(attrs)) {
        sourceAttributes <- attrs
      }

      for (i in length(sourceAttributes)) {
        attr(receiving, names(sourceAttributes)[i]) <- sourceAttributes[[i]]
      }
    }
  }
  receiving
}

.checkDeprecated <- function(dots, verbose = getOption("reproducible.verbose", 1)) {
  if (!is.null(dots$cacheTags)) {
    messagePreProcess("cacheTags is being deprecated;",
                      " use userTags which will pass directly to Cache.",
                      verbose = verbose
    )
    dots$userTags <- dots$cacheTags
    dots$cacheTags <- NULL
  }
  if (!is.null(dots$postProcessedFilename)) {
    messagePreProcess("postProcessedFilename is being deprecated;",
                      " use filename2, used in determineFilename.",
                      verbose = verbose
    )
    dots$filename2 <- dots$postProcessedFilename
    dots$postProcessedFilename <- NULL
  }
  if (!is.null(dots$writeCropped)) {
    messagePreProcess("writeCropped is being deprecated;",
                      " use filename2, used in determineFilename.",
                      verbose = verbose
    )
    dots$filename2 <- dots$writeCropped
    dots$writeCropped <- NULL
  }
  if (!is.null(dots$rasterInterpMethod)) {
    messagePreProcess("rasterInterpMethod is being deprecated;",
                      " use method which will pass directly to projectRaster.",
                      verbose = verbose
    )
    dots$method <- dots$rasterInterpMethod
    dots$rasterInterpMethod <- NULL
  }
  if (!is.null(dots$rasterDatatype)) {
    messagePreProcess("rasterDatatype is being deprecated;",
                      " use datatype which will pass directly to writeRaster.",
                      verbose = verbose
    )
    dots$datatype <- dots$rasterDatatype
    dots$rasterDatatype <- NULL
  }
  if (!is.null(dots$pkg)) {
    messagePreProcess("pkg is being deprecated;",
                      "name the package and function directly, if needed,\n",
                      "  e.g., 'pkg::fun'.",
                      verbose = verbose
    )
    dots$pkg <- NULL
  }

  dots
}

.checkFunInDots <- function(fun = NULL, dots) {
  if (is.null(fun)) {
    if (!is.null(dots$pkg)) {
      fun <- paste0(dots$pkg, "::", fun)
    }
  }
  fun
}

hardLinkOrCopy <- function(from, to, overwrite = FALSE, verbose = TRUE) {
  linkOrCopy(from, to, symlink = FALSE, verbose = verbose)
}

escapeRegexChars <- function(str, repl = c("(", ")")) {
  for (r in repl) {
    str <- gsub(paste0("\\", r, ""), paste0("\\\\", r), str)
  }
  str
}

hardlinkOrSymlinkMessagePrefix <- "version of file(s) created"
hardlinkOrSymlinkMessagePrefixForGrep <- escapeRegexChars(hardlinkOrSymlinkMessagePrefix)

messageNoCopyMade <- "... no copy/copies made."

whPointsToMess <- "which point(s) to"
whPointsToMessForGrep <- escapeRegexChars(whPointsToMess)

getTeamDrive <- function(dots) {
  if (requireNamespace("googledrive", quietly = TRUE)) {
    teamDrive <- if (packageVersion("googledrive") < "2.0.0") {
      dots[["team_drive"]]
    } else {
      dots[["shared_drive"]]
    }
  } else {
    teamDrive <- NULL
  }
}

getTargetFilePath <- function(targetFile, archive, fileGuess, verbose,
                              destinationPath, alsoExtract, checkSumFilePath) {
  if (is.null(targetFile)) {
    if ((is.null(archive) || is.na(archive)) && !is.null(fileGuess)) {
      messagePreProcess("targetFile was not supplied; guessed and will try ", fileGuess,
                        ". If this is incorrect, please supply targetFile",
                        verbose = verbose
      )
      targetFile <- makeRelative(fileGuess, destinationPath)
      targetFilePath <- makeAbsolute(targetFile, destinationPath)
    } else {

      # Case when archive is passed, and fileGuess exists
      # if ((!is.null(archive) || !is.na(archive)) && !is.null(fileGuess)) {
      #   messagePreProcess("archieve was supplied, but targetFile not; guessed and will try ", fileGuess,
      #                     ". If this is incorrect, please supply targetFile",
      #                     verbose = verbose
      #   )
      #   targetFile <- makeRelative(fileGuess, destinationPath)
      #   targetFilePath <- makeAbsolute(targetFile, destinationPath)
      # } else {
      targetFilePath <- NULL
      # }

    }
  } else {
    if (length(targetFile) > 1) {
      stop("targetFile should be only 1 file")
    }

    targetFilePath <- normPath(makeAbsolute(targetFile, destinationPath))
  }
  targetFilePath
}

guessAlsoExtract <- function(targetFile, alsoExtract, checkSumFilePath) {
  if (is.null(alsoExtract)) {
    if (file.exists(checkSumFilePath)) {
      if (file.size(checkSumFilePath) > 0) {
        # if alsoExtract is not specified, then try to find all files in CHECKSUMS.txt with
        # same base name, without extension
        if (!is.null(targetFile)) {
          checksumsTmp <- read.table(checkSumFilePath)
          alsoExtract <- grep(paste(collapse = "|", paste0(filePathSansExt(targetFile), "\\.")),
                              checksumsTmp$file,
                              value = TRUE
          )
          rm(checksumsTmp) # clean up
        }
      }
    }
  }
  if (any(is.na(alsoExtract))) {
    alsoExtract <- NA
  } else if (!is.null(alsoExtract)) { # must keep relative because user may not know what path is in archive
    if (isTRUE(all(is.na(alsoExtract)))) {
      alsoExtract <- character()
    }
  }

  alsoExtract
}

updateArchiveWithGuess <- function(archive, guess) {
  if (!is.null(guess)) {
    if (is.null(archive)) {
      archive <- .isArchive(guess)
    }
    if (isTRUE(!is.na(archive))) {
      archive <- moveAttributes(guess, archive)
    }
  }
  archive
}

setupArchive <- function(archive, destinationPath) {
  if (!is.null(archive)) {
    if (!is.na(archive)) {
      tmpArchive <- archive
      archive <- makeAbsolute(archive, destinationPath)
      # archive <- file.path(destinationPath, basename2(archive))
      # filesToCheck <- unique(c(filesToCheck, archive))
      archive <- moveAttributes(tmpArchive, archive)
    }
  }
  archive
}

runChecksums <- function(destinationPath, checkSumFilePath, filesToCheck, verbose) {
  reproducible.inputPaths <- .getDestinationPathShared()
  if (!is.null(reproducible.inputPaths)) {
    reproducible.inputPaths <- checkPath(reproducible.inputPaths, create = TRUE)
  }
  if (!is.null(reproducible.inputPaths)) {
    reproducible.inputPaths <- path.expand(reproducible.inputPaths)
  }

  destinationPathUser <- NULL
  # When filesToCheck is empty (e.g., dlFun-only call with no url/targetFile/
  # archive), do not consult reproducible.inputPaths: there is no canonical
  # filename to look up there, and Checksums() with files = NULL would match
  # arbitrary unrelated files in the stash.
  possDirs <- if (length(filesToCheck) == 0L) {
    destinationPath
  } else {
    unique(c(destinationPath, reproducible.inputPaths))
  }
  csfps <- vapply(possDirs, function(dp) identifyCHECKSUMStxtFile(dp), character(1))
  allDone <- FALSE
  for (dp in possDirs) {
    for (csfp in csfps) { # there can be a mismatch between checksums and file location
      # csfp <- identifyCHECKSUMStxtFile(dp)
      checkSumsTmp1 <- try(Checksums(
        path = dp, write = FALSE, checksumFile = csfp,
        files = makeRelative(filesToCheck, absoluteBase = destinationPath),
        verbose = verbose - 1
      ), silent = TRUE)
      checkSums <- NULL
      if (!is(checkSumsTmp1, "try-error")) {
        checkSums <- checkSumsTmp1
        if (!all(is.na(checkSums$result))) { # found something
          if (isTRUE(any(dp %in% reproducible.inputPaths))) {
            destinationPathUser <- destinationPath
            destinationPath <- dp
            on.exit(
              {
                destinationPath <- destinationPathUser
              },
              add = TRUE
            )
          }
          allDone <- TRUE
          break
        }
      }
    }
    if (isTRUE(allDone))
      break
  }
  list(
    reproducible.inputPaths = reproducible.inputPaths,
    destinationPathUser = destinationPathUser,
    destinationPath = destinationPath,
    checkSums = checkSums
  )
}

dealWithArchive <- function(archive, url, targetFile, checkSums, alsoExtract, destinationPath, teamDrive, verbose) {
  fileGuess <- NULL
  if (is.null(archive)) {
    if (!is.null(url)) {
      allOK <- .similarFilesInCheckSums(targetFile, checkSums, alsoExtract)

      if (!allOK) { # skip identification of archive if we have all files with same basename as targetFile
        # BUT if we don't have all files with identical root name (basename sans ext), then assess for
        #   an archive, either remotely, in the case of google or from the basename of url
        fileGuess <- .guessAtFile(
          url = url, archive = archive,
          targetFile = targetFile, destinationPath = destinationPath,
          verbose = verbose, team_drive = teamDrive
        )
        archive <- .isArchive(fileGuess)
        # The fileGuess MAY have a fileSize attribute, which can be attached to "archive"
        archive <- moveAttributes(fileGuess, receiving = archive)
        sourceAttributes <- attributes(fileGuess)
        if (length(sourceAttributes) > 0 && !is.null(archive)) {
          for (i in length(sourceAttributes)) {
            attr(archive, names(sourceAttributes)[i]) <- sourceAttributes[[i]]
          }
        }

        checkSums <- .checkSumsUpdate(
          destinationPath = destinationPath,
          newFilesToCheck = archive,
          checkSums = checkSums,
          verbose = verbose
        )
      }
    }
  }
  list(
    checkSums = checkSums,
    archive = archive,
    fileGuess = fileGuess
  )
}

#' Find any remote-hash sidecar files for a given basename across one or more
#' directories. Returns absolute paths of non-empty sidecars.
#' @keywords internal
#' @noRd
.findRemoteHashSidecars <- function(filename, dirs) {
  if (is.null(filename) || length(filename) != 1L ||
      is.na(filename) || !nzchar(filename)) return(character())
  # Sidecars are written as `.<basename>_<urlencoded>.hash` (hidden); use
  # all.files = TRUE so the dotfile form is discoverable.
  prefix <- paste0(".", filename, "_")
  out <- character()
  for (d in dirs) {
    if (!is.null(d) && nzchar(d) && dir.exists(d)) {
      all <- list.files(d, full.names = FALSE, all.files = TRUE)
      isSidecar <- startsWith(all, prefix) & endsWith(all, ".hash")
      sidecars <- file.path(d, all[isSidecar])
      if (length(sidecars))
        sidecars <- sidecars[file.size(sidecars) > 0L]
      out <- c(out, sidecars)
    }
  }
  out
}

isNULLorNA <- function(x) {
  out <- TRUE
  if (!is.null(x)) {
    if (!isTRUE(is.na(x))) {
      out <- FALSE
    }
  }
  out
}

identifyCHECKSUMStxtFile <- function(path) {
  file.path(path, "CHECKSUMS.txt")
}

linkOrCopyUpdateOnly <- function(from, to, verbose) {
  fifrom <- file.info(from)
  fito <- file.info(to)
  whNotSame <- fifrom$ctime != fito$ctime
  needCopy <- !whNotSame %in% FALSE
  if (any(needCopy)) {
    linkOrCopy(from[needCopy], to[needCopy],
               verbose = verbose - 1
    )
  }
}

messageChecksummingAllFiles <- "Checksumming all files in archive"



# may already have been changed above
copyFromDPtoReproducibleIPs <- function(targetFilePath, destinationPathUser, destinationPath,
                                        reproducible.inputPaths, neededFiles, archive, checkSums,
                                        checkSumFilePath, verbose) {
  outCheck <- if (!is.null(targetFilePath)) {
    !file.exists(targetFilePath)
  } else {
    TRUE
  } ## if NULL, it doesn't exist and we want to proceed
  if (isTRUE(outCheck)) { # skip if it already existed locally
    if (is.null(destinationPathUser)) {
      destinationPathUser <- destinationPath
    }
    on.exit(
      {
        destinationPath <- destinationPathUser
      },
      add = TRUE
    )

    if (!identical(destinationPath, reproducible.inputPaths)) {
      # CHANGE destinationPath FOR REMAINDER OF THIS FUNCTION
      neededFilesNew <- makeRelative(neededFiles, destinationPath)
      targetFilePathNew <- makeRelative(targetFilePath, destinationPath)
      destinationPathNew <- reproducible.inputPaths[1]
      archiveExistInDestDir <- if (!isNULLorNA(archive)) {
        file.exists(archive)
      } else {
        FALSE
      }
      existInDestDir <- if (!isNULLorNA(neededFiles)) {
        file.exists(neededFiles)
      } else {
        FALSE
      }
      filesToAppendToChecksums <- character()
      if (any(existInDestDir)) {
        from <- neededFiles[existInDestDir]
        to <- makeAbsolute(neededFilesNew[existInDestDir],
                           absoluteBase = destinationPathNew
        )
        linkOrCopyUpdateOnly(from, to, verbose = verbose)
        filesToAppendToChecksums <- c(filesToAppendToChecksums, to)
      }
      if (any(archiveExistInDestDir)) {
        from <- archive[archiveExistInDestDir]
        to <- makeAbsolute(makeRelative(archive[archiveExistInDestDir], destinationPath),
                           absoluteBase = destinationPathNew
        )
        linkOrCopyUpdateOnly(from, to, verbose = verbose)
        filesToAppendToChecksums <- c(filesToAppendToChecksums, to)
      }
      if (any(nzchar(filesToAppendToChecksums) %in% TRUE)) {
        appendChecksumsTableWithCS(append = TRUE,
                                   checkSumFilePath = file.path(destinationPathNew, basename(checkSumFilePath)),
                                   destinationPathNew,
                                   filesToChecksum = filesToAppendToChecksums, currentFiles = checkSums,
                                   verbose)

      }
      targetPath <- targetFilePathNew
      destinationPath <- destinationPathNew
      neededFiles <- neededFilesNew
    }

    if (isTRUE(any(grepl(archive, pattern = destinationPathUser)))) {
      # might have a "." as destinationPath -- messes with grepl
      patt <- if (grepl("^\\.", destinationPathUser)) {
        gsub("^\\.", "^\\\\.", destinationPathUser)
      } else {
        destinationPathUser
      }
      archive <- gsub(archive, pattern = patt, replacement = destinationPath)
    }
    targetFilePath <- makeAbsolute(targetFilePath, destinationPath)
    neededFiles <- makeAbsolute(neededFiles, destinationPath)
  }
  list(neededFiles = neededFiles, targetFilePath = targetFilePath,
       destinationPathUser = destinationPathUser, destinationPath = destinationPath,
       archive = archive)
}
