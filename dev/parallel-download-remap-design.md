# Parallel ranged downloads + URL remap — integration design

Two separable features in the `preProcess()` download path. Pure performance / extensibility;
**no public API change, no checksum change.** Existing tests must pass unchanged.

- **Feature A** — parallel ranged downloads (capability-detected, transparent).
- **Feature B** — user-supplied URL remap hook + `makeUrlRemap()` helper.

All code lives in `R/download.R`. New options registered in `R/options.R` (both the
`reproducibleOptions()` list ~L320 and the roxygen block ~L14). Logging uses the existing
`messagePreProcess()` / `messagePrepInputs()` helpers (not `messageVerbose()`). All network
namespaces (`curl`, `httr`, `httr2`, `googledrive`) are in **Suggests** → every new call is
guarded with `.requireNamespace()`, matching surrounding code. Confirmed locally:
`curl` 7.1.0, `curl::multi_download()` present.

---

## Dispatch recap (verified line refs, branch `development`)

```
downloadFile (download.R:32)              ← post-download Checksums() verification (UNCHANGED)
  └ downloadRemote (download.R:589)        ← regex dispatch switch (L721–846)
      ├ Google Drive → dlGoogle (L351)
      │     ├ assessGoogle (L923)          ← Drive ID → filename via drive_get  [B hook]
      │     └ download_resumable_httr2 (L1213)  ← GD byte fetch (httr2 / curl)
      └ generic HTTPS → dlGeneric (L518)   ← single-stream HTTPS fetch          [A hook]
```

URL is a plain `character` throughout — no wrapper class. `getRemoteFileSize(isGD, url)`
(L1386) already does `httr::HEAD` + `content-length` for the non-GD case — extend it (or a
sibling) to also read `Accept-Ranges`.

---

## Feature B — URL remap hook

### Option
```r
reproducible.urlRemap = NULL   # NULL = disabled (default). Else function(url, filename) -> url|NULL
```
Contract: returns a replacement URL string, or `NULL`/the original to mean "no change".
A remap that **errors** must NOT break the download — wrap in `tryCatch`, emit a
`warning()`, fall back to the original URL.

### Where it fires
The hook needs the *resolved filename*. Two entry conditions:

- **Generic URL** (`dlGeneric`, L523): basename known immediately → remap at top of `dlGeneric`.
- **Drive URL** (`dlGoogle`, L360): filename only known *after* `assessGoogle()` returns
  `fileAttr$name` (L991). So fire the hook in `dlGoogle` right after the `assessGoogle()` call:

```
dlGoogle():
  downloadFilename <- assessGoogle(url, ...)        # L360 — filename resolved here
  filename <- basename2(downloadFilename)
  newUrl <- .applyUrlRemap(url, filename)            # NEW: tryCatch-guarded hook
  if (!is.null(newUrl) && !identical(newUrl, url))
     return(dlGeneric(url = newUrl, destinationPath, verbose))   # delegate → generic path (+ Feature A)
  ... existing Google download path unchanged ...
```

Design note (confirmed with user): a remapped Drive→Arbutus URL leaves the Drive auth path
entirely and is fetched as **public unauthenticated HTTPS**. Intended — Arbutus bucket is public.

### Internal helper
```r
.applyUrlRemap(url, filename) -> character|NULL
  fn <- getOption("reproducible.urlRemap")
  if (is.null(fn)) return(NULL)
  tryCatch(fn(url, filename), error = \(e) { warning(...); NULL })
```

### Exported helper
```r
makeUrlRemap(manifest)   # manifest: data.frame with cols `filename`, `url`
  -> function(url, filename) { if filename in manifest$filename -> manifest$url[match]; else NULL }
```
Validates columns; matches on basename(filename). Roxygen-documented & exported.

---

## Feature A — parallel ranged downloads

### Options — STRICTLY OPT-IN
```r
reproducible.parallel.streams   = 1L             # N concurrent ranged GETs; 1L = OFF (default)
reproducible.parallel.threshold = 100 * 1024^2   # 100 MiB; below this → single-stream
```
**The feature is off unless the user explicitly sets `reproducible.parallel.streams > 1L`.**
With the default `1L`, capability detection never even runs and behaviour is byte-for-byte the
current single-stream path. Threshold is only a secondary guard once the user has opted in.
(Per user: nothing parallel happens without an explicit option set — capability detection alone
is NOT sufficient to engage it.)

### Where it fires
Inside `dlGeneric()` (L518), wrapping the single-stream fetch at L552–566. Strategy is gated on
the explicit opt-in option first, then capability detection; never a user-visible argument:

```
dlGeneric(url, destinationPath, verbose):
  destFile <- ...                                   # unchanged
  # remap hook (Feature B) runs here for generic urls
  streams <- getOption("reproducible.parallel.streams", 1L)
  useParallel <- streams > 1L &&                     # <-- explicit opt-in gate FIRST
                 .requireNamespace("curl")
  if (useParallel) {
    info <- .probeRange(url)                          # HEAD: size + Accept-Ranges (extends getRemoteFileSize)
    useParallel <- isTRUE(info$acceptRanges) &&
                   !is.na(info$size) &&
                   info$size > getOption("reproducible.parallel.threshold", 100*1024^2)
  }
  if (useParallel) {
     ok <- tryCatch(.parallelRangedDownload(url, destFile, info$size,
                       n = streams, verbose),
                    error = \(e) FALSE)
     if (isTRUE(ok)) return(list(destFile = destFile))   # else fall through
  }
  ... existing httr2 / download.file single-stream path (L535–572, UNCHANGED) ...
```

### `.parallelRangedDownload(url, destFile, size, n, verbose)`
- Split `[0, size)` into `n` contiguous byte ranges.
- `curl::multi_download(urls = rep(url, n), destfiles = partFiles, ...)` with per-part
  `Range: bytes=lo-hi` headers (via `curl::new_handle()` / `handle_setheaders`, one handle per part).
- On success: concatenate parts **in order** into `destFile`, `unlink` parts, verify
  `file.size(destFile) == size`. Mismatch → error (caller falls back to single-stream).
- Returns `TRUE` on verified success, `FALSE`/throws otherwise.

### Fallback matrix (all → single-stream, no error surfaced to user)
| Condition | Detected by |
|---|---|
| **Not opted in** (`streams <= 1L`, the default) | option check — feature never engages |
| No `Accept-Ranges: bytes` | `.probeRange` |
| HEAD fails | `.probeRange` returns NA size |
| size ≤ threshold | comparison |
| `curl` unavailable | `.requireNamespace` |
| any part fails / size mismatch mid-assembly | `tryCatch` around `.parallelRangedDownload` |

**Checksums untouched:** assembled `destFile` is byte-identical to single-stream output, so the
downstream `Checksums()` in `downloadFile()` (L183+) is unaffected.

---

## Build order (per brief)
1. ~~Map dispatch~~ ✓  2. ~~This summary~~ ✓  3. Feature B  4. Feature A
5. Arbutus manifest script (standalone, outside pkg)  6. Tests  7. NEWS + roxygen.

## Tests planned
- **A:** mock Range server → parallel path + correct reassembly; no-Range → fallback;
  below-threshold → single-stream; mid-download part failure → clean fallback.
- **B:** remap→new URL used; remap→NULL → original; remap errors → original + warning;
  `makeUrlRemap()` filename matching.
