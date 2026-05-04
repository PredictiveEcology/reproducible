# `reproducible.destinationPathShared` — Design Spec

Status: DRAFT v4 — pre-implementation
Owner: prepInputs / preProcess subsystem
Goal: a small, documented, fully tested mechanism for sharing one local data
cache across many projects without re-downloading.

This document is the source of truth for behavior. Code, docs, and tests must
match it; if any of them disagree with this file, this file wins (or this file
is wrong and should be edited first, then code follows). Not shipped on CRAN
(`dev/` is in `.Rbuildignore`).

**v4 changes:**
- `destinationPathShared` is **read-write**. When a download is needed and
  `destinationPathShared` is configured + writable, the file is downloaded into the
  first writable shared dir, then hardlinked into `destinationPath`. One
  physical copy on disk. (§3.7, §7 closed)
- Hash policy clarified: digest **once** with whichever algo the upstream
  source provides. No recompute across algos. (§7 closed)
- Public `destinationPathSharedLs()` exposed now. (§7 closed)
- Legacy `<urlEncoded>.hash` files in `destinationPathShared` are **deleted** after
  hydration to the sidecar format (matches destinationPath behavior). (§3.6)

**v3 changes:**
- `<destinationPathShared>/CHECKSUMS.txt` is **dropped**. Shared dirs use file-keyed
  JSON sidecars at `<dir>/.repro/<filename>.json` instead. Project CHECKSUMS
  in `destinationPath` is unchanged. (§3.3, §3.6)
- The existing `<urlEncoded>.hash` files are migrated to the same JSON
  sidecar format (file-keyed, not URL-keyed). One format everywhere.
- Concurrency / locking is **in scope this pass** via `filelock` (Suggests).
  (§3.5)
- New §5.4 (sidecar lifecycle), new §5.11 (concurrency). Old CHECKSUMS-
  divergence cases (C5–C9) collapsed to 2.

**v2 changes (still current):**
- Rename: `reproducible.dataPath` → `reproducible.destinationPathShared` (see §0).
- Resolution order revised so the destinationPathShared search uses authoritative
  remote-derived filenames and the remoteHash, not URL guesses (§3.2).
- §5 case matrix expanded for targetFile inference, remoteHash matching, and
  CHECKSUMS divergence.

## 0. Naming  *(decided)*

The canonical option is **`reproducible.destinationPathShared`**. Two words, both
load-bearing:
- "shared" — the directory is meant to be reused across projects (and on
  research clusters, across users via NFS).
- "inputs" — preserves continuity with the deprecated `inputPaths` and
  matches the conceptual category in `prepInputs` / `preProcess`.

Rejected alternatives:
- `reproducible.dataPath` — vague; briefly used in branch but never released.
- `reproducible.dataMirror` — accurate ("mirror" of remote downloads) but the
  word implies remote sync semantics we are not implementing.
- `reproducible.inputCache` — clear, but mentally collides with `Cache()`.
- `reproducible.preProcessCache` — verbose; same `Cache()` collision.
- `reproducible.dataLibrary` — "library" is overloaded in R.

**Migration (final):** the deprecation chain is
`inputPaths` (legacy alias) → `destinationPathShared` (canonical). The intermediate
`dataPath` name was dropped before release. `inputPaths` emits a one-shot
deprecation message and is honored if `destinationPathShared` is unset.

---

## 1. What it is

A user-set option, `reproducible.destinationPathShared`, that names one or more
directories on the local filesystem treated as a *shared read-through cache*
for files fetched by `prepInputs()` / `preProcess()`.

**Single sentence:** before downloading, look in `reproducible.destinationPathShared`
for each needed file; if a hash-matching copy is found, hardlink it into
`destinationPath` instead of downloading.

It is **not**:
- A replacement for `destinationPath`. Project files still live in
  `destinationPath`.
- A `Cache()` backend. `Cache()` has its own cachePath; the two are
  independent.
- A remote/cloud store. Local filesystem only (NFS-mounted dirs are fine — they
  look local).

## 2. Current state (2026-04-24)

What works today (verified by reading code):
- Option is read in `R/helpers.R:.getDataPath()` (handles deprecated alias
  `reproducible.inputPaths`).
- `pp_check_local_sources()` (`R/preProcess.R:455`) plumbs it as `otherPaths`
  into `.checkLocalSources()` (`R/preProcess.R:1262`), which:
  - iterates each path, lists files (recursive per `.getDataPathRecursive()`),
  - finds checksum-matching candidates,
  - hardlinks them into `destinationPath` via `hardLinkOrCopy()`.
- `runChecksums()` (`R/preProcess.R:1971`) writes a CHECKSUMS.txt copy into
  the shared dir so the next project sees them.
- `download.R:252` references it in error messages.
- `pp_remote_hash_check()` (`R/preProcess.R:499`) does etag/md5 lookup via
  `getRemoteMetadata()` (`R/downloadTileAndUpload.R:845`) and writes a
  `<url>.hash` sidecar in `destinationPath`. **Independent of the dataPath
  search.**

What is broken / unclear:
- **No tests.** Zero coverage in `tests/testthat/`. Any change risks regression.
- **Phase ordering means dataPath search runs before remote metadata is
  fetched.** When `targetFile` is not supplied, `.guessAtFile()`
  (`R/preProcess.R:1034`) guesses from the URL. For redirect URLs, content-
  disposition responses, and Google Drive shares, the authoritative filename
  comes from the HEAD response in `getRemoteMetadata()` — but that runs in
  `pp_remote_hash_check()` (preProcess.R:217), *after* `pp_check_local_sources`
  (preProcess.R:216). So the dataPath search may use a wrong-but-plausible
  basename and miss valid matches.
- **`pp_check_local_sources` ignores the remoteHash entirely.** Even when
  `getRemoteMetadata()` could give us an etag/md5 to compare against, the
  search uses CHECKSUMS-style hashes only.
- **Two duplicated read sites** outside the `otherPaths` ctx flow:
  `preProcess.R:1330` and `preProcess.R:1972`. They re-derive what
  `pp_check_local_sources` already computed.
- **Internal variable still named `reproducible.inputPaths`** in many places
  (preProcess.R:2, 253, 303, 459, 469, 472, 792, 803, 828, 836, 877, 878, 888,
  890, 1301, 1330, 1972, 2015). Reader confusion.
- **No documented semantics** for: vector of paths, missing CHECKSUMS in
  shared dir, filesystem-incompatible hardlinks (cross-device, Windows),
  filename collision, recursive search performance, project-CHECKSUMS vs.
  shared-CHECKSUMS divergence.
- **`reproducible.dataPathRecursive`** is honored in `.checkLocalSources` but
  not in `runChecksums` — recursion is partial.
- **Error path on checksum mismatch**: the existing message
  (`prepInputs.R:1222`) tells the user to *manually* delete bad copies. We can
  do better.

## 3. Specification

### 3.1 Option contract

| Option                                | Type             | Default | Meaning |
| ------------------------------------- | ---------------- | ------- | ------- |
| `reproducible.destinationPathShared`           | `NULL` \| `character(>=1)` | `NULL` | Directories to search before downloading. Searched in order; first hash-match wins. `NULL` disables the mechanism. |
| `reproducible.destinationPathSharedRecursive`  | `logical(1)`     | `FALSE` | If `TRUE`, search each path recursively. Applies to **all** elements of `reproducible.destinationPathShared`. |
| `reproducible.dataPath`               | (deprecated)     | —       | Alias for `destinationPathShared`. Emits a one-shot deprecation message per session. |
| `reproducible.dataPathRecursive`      | (deprecated)     | —       | Alias for `destinationPathSharedRecursive`. |
| `reproducible.inputPaths`             | (deprecated)     | —       | Older alias for `destinationPathShared`. |
| `reproducible.inputPathsRecursive`    | (deprecated)     | —       | Older alias for `destinationPathSharedRecursive`. |

Validation (fail loud at first use):
- Each entry must be `character(1)`, non-empty, and either an existing dir or
  creatable. We **create** missing directories with `checkPath(create = TRUE)`
  on first hit (matches today's `runChecksums` behavior).
- Duplicate entries: silently de-duplicated.
- An entry equal to `destinationPath`: silently dropped from search (avoids
  hardlink-to-self).

### 3.2 Resolution algorithm — single phase, single check

The two existing phases (`pp_check_local_sources`, `pp_remote_hash_check`) are
merged into one new phase, `pp_resolve_inputs`. It runs **once per call** and
makes **one** filesystem search of `destinationPathShared`. Pseudocode:

```
pp_resolve_inputs(ctx):

  # Step A: figure out the canonical filename(s) we are looking for.
  # We need this BEFORE searching destinationPathShared, otherwise basename
  # comparison is unreliable (URL guesses can be wrong).
  if ctx.targetFile is not None:
    canonicalFile = ctx.targetFile
    remoteMeta    = None        # may be filled in step B if needed
  else:
    remoteMeta    = getRemoteMetadata(ctx.url)   # one HEAD request
    canonicalFile = remoteMeta.targetFile        # authoritative
    # if HEAD fails or url is None, fall back to .guessAtFile (current behavior)

  # Step B: pull the expected hash. Order of authority:
  #   1. remoteMeta.remoteHash       (etag / md5 from server, fetched in A)
  #   2. <destinationPath>/.repro/<canonicalFile>.json (project sidecar)
  #   3. CHECKSUMS.txt entry in destinationPath (project CHECKSUMS — contract)
  #   4. <destinationPathShared>/<dp>/.repro/<canonicalFile>.json (shared sidecar)
  #   5. None — we accept the first hash-less basename match (with a warning)
  expectedHash, hashAlgo = resolveExpectedHash(canonicalFile, remoteMeta, ctx)

  # Step C: short-circuit if destinationPath already has a good copy.
  if file exists in destinationPath AND matches expectedHash:
    return ctx (already done; skip download AND skip destinationPathShared search)

  # Step D: search destinationPathShared ONCE. Build the candidate list and pick
  # the best match. Scan order: each dp in option order; within a dp,
  # recursive iff reproducible.destinationPathSharedRecursive.
  candidates = []
  for dp in reproducible.destinationPathShared:
    for f in list_files(dp, recursive = reproducible.destinationPathSharedRecursive):
      if basename(f) == canonicalFile:
        candidates.append(f)

  # Step E: pick a winner.
  for candidate in candidates:
    # Cheap path: if candidate has a sidecar with a matching hash, trust it.
    sidecar = readSidecar(<candidateDir>/.repro/<basename(candidate)>.json)
    if sidecar and sidecar.hash == expectedHash:
      h = expectedHash      # no recompute
    elif expectedHash is not None:
      h = computeHash(candidate, algo = hashAlgo)
    else:
      h = None              # accept on basename only

    if expectedHash is None or h == expectedHash:
      with sharedLock(<candidateDir>/.repro/locks/<basename>.lock):
        hardLinkOrCopy(candidate -> destinationPath/canonicalFile)
      writeSidecar(<destinationPath>/.repro/<canonicalFile>.json,
                   hash = h or computeHash(candidate),
                   sourceUrl = ctx.url, savedAt = now())
      # Refresh shared sidecar if missing or stale, under exclusive lock.
      if sharedSidecarMissingOrStale(candidateDir, basename(candidate), h):
        with exclusiveLock(<candidateDir>/.repro/locks/<basename>.lock,
                           timeout = 30s):
          writeSidecar(<candidateDir>/.repro/<basename>.json, ...)
      ctx.skipDownload = True
      return ctx
    else:
      log "hash mismatch in destinationPathShared candidate; not using" (no delete)

  # Step F: nothing matched in destinationPathShared. Fall through to pp_download.
  return ctx

# After pp_download (or after Step C/E success) — pp_finalize handles placement:
pp_finalize_placement(downloadedFile, ctx):
  # Goal: exactly ONE physical copy on disk; destinationPath always
  # presents a usable file (hardlink or original).
  writableShared = first(dp in reproducible.destinationPathShared where writable(dp))
  if writableShared is not None and downloadedFile not already in shared:
    move(downloadedFile -> <writableShared>/<canonicalFile>)   # rename, atomic
    hardLinkOrCopy(<writableShared>/<canonicalFile> -> <destinationPath>/<canonicalFile>)
    writeSidecar(<writableShared>/.repro/<canonicalFile>.json, ...)
    writeSidecar(<destinationPath>/.repro/<canonicalFile>.json, ...)
  else:
    # Either no destinationPathShared configured, or none writable.
    leave downloadedFile in <destinationPath>/<canonicalFile>
    writeSidecar(<destinationPath>/.repro/<canonicalFile>.json, ...)
```

Key changes from today:
- **One filesystem traversal of `destinationPathShared` per call.** Today the same dirs
  are walked in `.checkLocalSources` and again in `runChecksums`.
- **One HEAD request, conditional.** Skipped when `targetFile` and
  `expectedHash` are both available locally. Issued when we genuinely don't
  know the filename or any hash.
- **`remoteHash` is first-class.** When the server gave us an etag/md5, we
  use that to validate destinationPathShared candidates directly — no CHECKSUMS round
  trip required.
- **JSON sidecars replace `<url>.hash` files.** File-keyed at
  `<dir>/.repro/<filename>.json` (see §3.6). One format, two locations
  (destinationPath and destinationPathShared). Existing `<urlEncoded>.hash` files are
  migrated transparently on first encounter.
- **No CHECKSUMS.txt in `destinationPathShared`.** Sidecars are the only metadata.
  Project CHECKSUMS in `destinationPath` is unchanged.

### 3.3 CHECKSUMS and sidecars — division of labor

Two metadata mechanisms with different roles:

- **`<destinationPath>/CHECKSUMS.txt` — the project CHECKSUMS.** Unchanged
  from today. Hand-editable. Often committed to git. The *contract* a
  project signs about which file versions it intends to consume.
- **`<dir>/.repro/<filename>.json` — file-keyed sidecars.** Machine-managed.
  Written in both `destinationPath` and `destinationPathShared`. The *cache of
  observations* — what hash this file most recently had, where it came from,
  when it was saved.

**No CHECKSUMS.txt is written to or read from `destinationPathShared`.** Removed
because:
- Hand-curation / git-commit affordances don't apply to the shared dir.
- One file becomes a write hot-spot under concurrency.
- Stale entries accumulate when files are deleted (today's case C4).
- Positional rows can't be extended with size / etag / sourceUrl.

**Legacy reading (one-time migration):** if a destinationPathShared dir has a
`CHECKSUMS.txt` and no `.repro/` subdir, on first access we *read* the
legacy file to populate sidecars (one-time hydration). We never *write* back
to the legacy file. After hydration, destinationPathShared is sidecar-only. Emit a
one-shot info message: `"Hydrated N sidecars from legacy CHECKSUMS.txt; the
shared CHECKSUMS file is no longer maintained."`

**Lookup rules (in `resolveExpectedHash`):**

1. Prefer `remoteHash` from this call's `getRemoteMetadata()` — freshest.
2. Else read `<destinationPath>/.repro/<canonicalFile>.json`.
3. Else read **project CHECKSUMS** in destinationPath (the contract).
4. Else read `<destinationPathShared>/<dp>/.repro/<canonicalFile>.json` (shared
   sidecar, in `destinationPathShared` order).
5. Else: no expected hash; see step E above.

**Divergence (project CHECKSUMS says X, shared sidecar says Y):**
- Project CHECKSUMS wins for the *expected* value (it is the contract).
- We still scan `destinationPathShared` for files whose computed hash matches the
  project value, regardless of what the shared sidecar claims.
- A wrong shared sidecar is *self-healing*: on a miss/mismatch we recompute
  and overwrite it.

**Write rules (in `pp_finalize`):**
- After a successful resolution, append the entry to project CHECKSUMS
  (today's behavior preserved).
- Write/update `<destinationPath>/.repro/<canonicalFile>.json` always.
- Write/update `<destinationPathShared>/<dp>/.repro/<canonicalFile>.json` for the
  `dp` from which the file came (or first writable `dp` if it was just
  downloaded into the shared dir directly). One sidecar per file per dir.
- Read-only target → log once and continue.

### 3.4 Hardlink fallback

`hardLinkOrCopy()` already implements: try `file.link()`, fall back to
`file.copy()` on failure (cross-device, Windows quirks, permission). Document
this. On copy fallback, emit a one-shot per-session message so users know
they're not getting the space-saving benefit.

### 3.5 Concurrency

Multiple R sessions writing to the same `destinationPathShared` dir is the **expected**
case (cluster NFS), so locking is in scope this pass.

**Library:** `filelock` from CRAN. Listed as `Suggests`, not `Imports` —
absence is graceful (one-shot warning, fall through to no-locking behavior
which matches today). Cluster users should install it.

**Lock locations:** `<dir>/.repro/locks/<basename>.lock`. One lock file per
data file; no global lock. The locks dir is created on demand.

**Lock taxonomy:**
- **Exclusive (write) lock** around: copying or hardlinking a file *into*
  destinationPathShared; writing or updating its sidecar; running the legacy
  CHECKSUMS hydration.
- **Shared (read) lock** around: reading the sidecar, hardlinking the file
  *out* (i.e., from destinationPathShared into destinationPath).

**Atomicity belt-and-suspenders:** even with the lock, all writes use the
write-rename pattern. Write to `<file>.tmp.<pid>`, fsync, then `file.rename()`.
POSIX rename is atomic; this protects readers from ever seeing a partial
file even if `filelock` is absent or a writer crashes mid-operation.

**Timeout:** `filelock::lock(timeout = 30000)` (30s). On timeout, log and
fall through as if the candidate didn't exist — better to download than to
block a long simulation on a stuck lock.

**Stale lock recovery:** none — `filelock` releases on file-descriptor close,
which the OS guarantees on process exit. We do not implement a staleness
timer.

**Lock dir not creatable** (read-only destinationPathShared): one-shot warning;
proceed without locking. Reads from such a dir are still safe; writes are
disabled implicitly because the rename target is unwritable.

### 3.6 Sidecar format

Path: `<dir>/.repro/<filename>.json`, where `<dir>` is either a
`destinationPath` or one of `destinationPathShared`. The `.repro/` subdir is hidden
on POSIX, ignored by `dir(..., all.files = FALSE)` (R's default), and skipped
by `prepInputs` itself when listing candidate files (so sidecars never look
like data).

Format: a flat JSON object. Required and optional fields:

```json
{
  "schemaVersion": 1,
  "hash": "ab12cd34…",
  "hashAlgo": "sha1",
  "size": 12345678,
  "sourceUrl": "https://example/foo.tif",
  "etag": "\"abc123\"",
  "savedAt": "2026-04-24T12:34:56Z",
  "savedByPkg": "reproducible 3.0.0.9042"
}
```

- `schemaVersion`: integer; bump if we change required fields.
- `hash`, `hashAlgo`: required. The hash of the *adjacent file*, computed
  with `hashAlgo` (`sha1` default; `md5` when remote provided one).
- `size`: byte size of the adjacent file. Cheap fast-fail check before
  recomputing the hash.
- `sourceUrl`: optional. Recorded for traceability and for the
  prepInputs-tracking work in plan item #2.
- `etag`: optional. The remote validator that produced the hash, for
  debugging and weak-validator handling.
- `savedAt`: ISO-8601 UTC.
- `savedByPkg`: which version wrote the sidecar. Useful when migrating
  schemas later.

**Reader policy:** unknown fields are ignored. Missing optional fields are
fine. A sidecar whose `hash` does not match the adjacent file's actual hash
is treated as stale (recompute, overwrite under exclusive lock).

**Migration from `<urlEncoded>.hash`:** on first encounter of a directory
that has any `<urlEncoded>.hash` files, we:
1. For each `<urlEncoded>.hash`, find the adjacent data file by reading the
   URL → filename mapping (`makeRemoteHashFile()` writes
   `<urlWithUnderscores>.hash` next to a file named via `targetFile` or URL
   basename — we match by reading the hash string and finding the file with
   that hash, or by URL→basename guess and confirm).
2. Write a JSON sidecar with `hash`, `hashAlgo` (sha1 unless we can detect
   md5 by length), `sourceUrl` reconstructed from the underscored name.
3. Delete the legacy `<urlEncoded>.hash` file.
4. Emit a single info message naming the count migrated.

If migration cannot find a confident filename match, leave the legacy file
alone and emit a warning naming the file. Rare and recoverable manually.

### 3.7 Auto-population and the single-physical-copy invariant

When a download is required and `destinationPathShared` is configured with at least
one writable directory:

1. Download to a tmp file.
2. `file.rename()` (atomic POSIX) the tmp file to the canonical path inside
   the **first writable** `destinationPathShared` entry, under exclusive lock.
3. `hardLinkOrCopy()` from there into `destinationPath`.
4. Write sidecars in both locations.

When `destinationPathShared` is unset or has no writable entry, the download lands in
`destinationPath` directly (today's behavior). Sidecar written only there.

**The invariant:** for any data file accessed via `prepInputs` /
`preProcess`, there is **at most one physical copy on disk per filesystem**.
All other apparent copies are hardlinks. On filesystems where hardlinks fail
(cross-device, Windows specifics, permission), `hardLinkOrCopy` falls back
to copy with a one-shot warning — the invariant is best-effort, not absolute.

**Why this matters:** users with `destinationPathShared` set on cluster NFS get
deduplication for free. A 50 GB raster downloaded by Project A is one
physical 50 GB blob; Project B's `prepInputs` call is a `file.link()` not a
50 GB read. Without auto-population, that benefit only happens if users
manually pre-populate the shared dir.

**Hash algorithm policy:** digest each file at most **once** per encounter,
using whichever algo the upstream source provides:
- Server returned md5 → store hash with `hashAlgo: "md5"`.
- Server returned sha1 etag → store with `hashAlgo: "sha1"`.
- No remote → use `digest::digest(file = ..., algo = "sha1")` (today's
  default for CHECKSUMS).

The sidecar's `hashAlgo` field records which algo was used. Future
comparisons:
- If a candidate sidecar's algo matches the expected hash's algo → direct
  comparison, no recompute.
- If they differ → recompute the candidate hash in the expected algo (this
  is the only "second digest" case, and it only happens when the project
  CHECKSUMS forces a different algo than the sidecar; rare).

Large files (multi-GB rasters) are the dominant cost driver, and this rule
guarantees exactly one full read of the bytes per file per encounter.

### 3.8 Public surface

Two new exported functions in this iteration:

- `destinationPathSharedLs(path = NULL, hash = TRUE)` — inspect what is in the
  shared dir(s). Returns a `data.frame` with one row per data file:
  `dir`, `filename`, `size`, `hash`, `hashAlgo`, `sourceUrl`, `savedAt`,
  `hasSidecar` (logical). If `path` is `NULL`, uses `getOption(
  "reproducible.destinationPathShared")`. If `hash = FALSE`, sidecar fields are
  filled but no hash recompute is triggered for files lacking a sidecar.
- `destinationPathSharedRefresh(path = NULL, deleteOrphans = FALSE)` — rebuild
  sidecars from disk contents. `deleteOrphans = TRUE` removes sidecars
  whose adjacent file is missing. Useful for users who manage the shared
  dir externally (rsync, Globus, etc.).

Internal helpers stay `.`-prefixed (see §6).

## 4. Refactor plan

> **Status note (May 2026):** the implementation took a different, smaller
> shape than this 13-step plan envisaged. The status of each item is shown
> below. Items marked **DONE-DIFFERENT** were achieved with different
> function/option names than the plan specified; **DONE** matches; **PARTIAL**
> means some functionality exists; **DEFERRED** means not built yet.

1. **DONE.** Tests-first scaffold: `tests/testthat/test-destinationPathShared.R` with
   the case matrix in §5. (Many cases are still skipped because they target
   helpers that were never built; see notes below.)
2. **DONE-DIFFERENT.** Sidecar I/O is provided via the existing
   `makeRemoteHashFile(url, dir, basename, hash, algorithm = …, write = TRUE)`
   for write and the new `.parseRemoteHashFile(path)` for read. The `.hash`
   file format is now one line `<algo>:<hash>` (with a length-heuristic
   legacy fallback for plain-hash files). The originally-proposed
   `.readSidecar / .writeSidecar / .sidecarPath` triplet was not introduced.
3. **DEFERRED.** No dedicated `.withFileLock` shim around sidecar/CHECKSUMS
   writes. Locking still happens at the Cache layer (`R/GPT2.R::lockFile`),
   not around per-file sidecar writes. Per-file locking is a real gap; in
   practice the no-overwrite property of `makeRemoteHashFile` mitigates it.
4. **PARTIAL.** Legacy hydration is on-the-fly: `.parseRemoteHashFile`
   accepts plain single-line hashes (legacy format) and the multi-row
   CHECKSUMS.txt support means an old `xxhash64`-only row coexists with a
   new `md5` row written by `pp_remote_hash_check`. There is no batch
   `.hydrateLegacyChecksums(dir)` helper.
5. **DONE-DIFFERENT.** Option getter is `.getDestinationPathShared()` /
   `.getDestinationPathSharedRecursive()`. The deprecation chain is two names, not
   three: `reproducible.destinationPathShared` (canonical) ←
   `reproducible.inputPaths` (legacy alias). The intermediate
   `reproducible.dataPath` name was dropped before release.
6. **DEFERRED.** No named `resolveExpectedHash()` helper. The 4-step lookup
   logic (remote → destinationPath sidecar → project CHECKSUMS → shared
   sidecar) is distributed across `pp_checksums_init`,
   `pp_check_local_sources`, and `pp_remote_hash_check`.
7. **PARTIAL.** No unified `pp_resolve_inputs()` phase. `pp_check_local_sources`
   and `pp_remote_hash_check` remain separate. The latter was rewritten
   (May 2026) to use a content-hash-with-fail-fast-size algorithm:
   - size mismatch ⇒ download
   - opaque ETag ⇒ download (no positive trust possible)
   - size match + hash match (in remote's algorithm) ⇒ write sidecar +
     CHECKSUMS row, skip download
   - size match + hash mismatch ⇒ download
8. **DEFERRED.** No `.normalizeDestinationPathShared(x, destinationPath)` helper for
   dedup / drop-self / create-if-missing. Vector handling is informal.
9. **DEFERRED.** `runChecksums()` still writes back to `<destinationPathShared>/CHECKSUMS.txt`
   in some paths.
10. **DEFERRED.** `ctx$reproducible.inputPaths` (the variable) is still
    named for the old option in `R/preProcess.R`. The option name is the
    new one with the legacy alias; the internal variable rename was not
    done.
11. **PARTIAL.** On hash mismatch we silently fall through to download (no
    sidecar/CHECKSUMS row is written), but we don't emit an informative
    "candidate file in shared dir didn't match" message.
12. **DONE-DIFFERENT.** `filelock` is in `DESCRIPTION:Imports` (not
    `Suggests`), pinned to `>= 1.0.3` (CRAN). The PredictiveEcology fork
    fix for the fd leak is not pinned because it isn't on CRAN; an R-level
    `EMFILE` retry workaround in `R/GPT2.R::lockFile` covers most cases
    (with a known >42-min worst-case cliff documented in NEWS.md).
13. **PARTIAL.** Options docs in `R/options.R` updated; NEWS describes the
    sidecar format and the rename. The "Downloading Data" vignette has not
    been rewritten with a worked example.

### Outstanding work (summary)

The honest remaining gap, in priority order:

- **Step 7** (unified `pp_resolve_inputs` + first-class `resolveExpectedHash`
  helper) — would clean up the most code. Steps 6 and 7 collapse together.
- **Step 3** (locking shim around sidecar/CHECKSUMS writes) — required for
  correctness under heavy concurrency, especially on NFS.
- **Step 10** (rename internal `reproducible.inputPaths` variable) —
  cosmetic but keeps things confusing until done.
- **Step 11** (informative mismatch message) — quality-of-life only.
- **Step 13** (vignette example) — documentation only.

After those land, every behavior in §3 is true and every case in §5 is green.

## 5. Test matrix (the part that matters)

Test file: `tests/testthat/test-destinationPathShared.R`. Each row → one `test_that()`
block.

### 5.1 Option plumbing (no I/O)

| # | Setup | Assertion |
|---|---|---|
| P1 | option unset | `.getDestinationPathShared()` returns `NULL` |
| P2 | `reproducible.destinationPathShared = "/x"` | returns `"/x"` |
| P3 | `reproducible.destinationPathShared = c("/a","/b")` | returns `c("/a","/b")` |
| P4 | only `reproducible.dataPath = "/x"` | returns `"/x"` + deprecation message exactly once |
| P5 | only `reproducible.inputPaths = "/x"` | returns `"/x"` + deprecation message exactly once |
| P6 | both `destinationPathShared` and `dataPath` set | new wins; deprecation message NOT emitted |
| P7 | all three set (`destinationPathShared`, `dataPath`, `inputPaths`) | `destinationPathShared` wins; no deprecation message |
| P8 | `reproducible.destinationPathShared = ""` | error: empty path |
| P9 | `reproducible.destinationPathShared = c("/x","/x")` | de-duplicated to `"/x"` |
| P10 | `reproducible.destinationPathShared = "/x"`, destinationPath = `"/x"` | search disabled (drop-self) |
| P11 | `reproducible.destinationPathSharedRecursive` unset | returns `FALSE` |
| P12 | `reproducible.destinationPathSharedRecursive = TRUE` | returns `TRUE` |
| P13 | only `reproducible.inputPathsRecursive = TRUE` | returns `TRUE` + deprecation message |
| P14 | only `reproducible.dataPathRecursive = TRUE` | returns `TRUE` + deprecation message |

### 5.2 Resolution — happy paths (require fixtures)

Fixtures: a tiny .rds (or .csv) with known sha1, hosted via `httptest2` mock or
a local `file://` URL so no network is needed. Do **not** use real downloads
in unit tests.

| # | Pre-state | Action | Expected |
|---|---|---|---|
| H1 | empty destinationPath; file present in `destinationPathShared` with matching hash | `prepInputs(url=..., destinationPath, targetFile=foo)` | no download attempted; file in destinationPath; both files share inode (hardlink) when same device |
| H2 | as H1, but cross-device | as H1 | no download; file present; copied (not hardlinked); message about copy fallback emitted once |
| H3 | as H1, recursive disabled, file is in subdir of `destinationPathShared` | as H1 | download happens (file not found at top level) |
| H4 | as H3 but `destinationPathSharedRecursive = TRUE` | as H1 | no download; file linked from subdir |
| H5 | two `destinationPathShared` entries, file only in second | as H1 | found in second path; no download |
| H6 | two `destinationPathShared` entries, file in both with different mtimes, both valid | as H1 | first wins (deterministic order); assert which one was linked by inode |
| H7 | file present in destinationPath AND destinationPathShared, both valid | as H1 | neither download nor link; existing destinationPath copy used; `destinationPathShared` not even traversed |
| H8 | file present in destinationPath but with stale `<url>.hash` matching old version, destinationPathShared has correct version | as H1 | sidecar mismatch → search proceeds; destinationPathShared match wins; sidecar refreshed |

### 5.3 Resolution — sad paths

| # | Pre-state | Action | Expected |
|---|---|---|---|
| S1 | file present in `destinationPathShared` but hash mismatch | `prepInputs(...)` | the bad file is NOT used, NOT deleted; informative message names the file and the dir; download proceeds normally |
| S2 | file present in `destinationPathShared` but no expected hash anywhere (no CHECKSUMS, no remote, no sidecar) | `prepInputs(...)` | one-shot warning; basename match accepted; sidecar created with computed hash for future runs |
| S3 | `destinationPathShared` set to a path that doesn't exist | `prepInputs(...)` | path is created; lookup is empty; download proceeds; no error |
| S4 | `destinationPathShared` set to a path with no read permission | `prepInputs(...)` | warning; downloads proceed; no crash |
| S5 | filename collision: two files with same basename in recursive destinationPathShared, only one matches expected hash | `prepInputs(...)` | matching one is used; non-matching one is ignored without error |
| S6 | hardlink fails AND copy fails (e.g., disk full) | `prepInputs(...)` | error surfaces from `hardLinkOrCopy`; not a silent download |
| S7 | network down during the conditional HEAD (Step A in §3.2) | `prepInputs(url=..., targetFile=NULL)` | falls back to `.guessAtFile`; if guess yields a name, search proceeds; otherwise error matches today's no-network behavior |

### 5.4 Sidecar lifecycle and CHECKSUMS interaction

| # | Pre-state | Action | Expected |
|---|---|---|---|
| C1 | first-ever download, destinationPathShared set, no sidecars anywhere | `prepInputs(...)` | project CHECKSUMS in destinationPath gains entry; sidecars written in both `<dest>/.repro/<file>.json` and `<dp>/.repro/<file>.json` |
| C2 | second project, same destinationPathShared, sidecar present in shared | `prepInputs(...)` from a fresh destinationPath | hits destinationPathShared via shared sidecar; project CHECKSUMS in destinationPath gains entry; destinationPath sidecar created; shared sidecar unchanged |
| C3 | destinationPathShared read-only (cannot create `.repro/`) | `prepInputs(...)` | destinationPath gets project CHECKSUMS + sidecar; shared sidecar skipped with one-shot warning |
| C4 | shared sidecar exists, but the adjacent file in shared has been deleted | `prepInputs(...)` | sidecar is ignored (no adjacent file); not deleted (we don't garbage-collect); falls through to download |
| C5 | shared sidecar says hash B; adjacent shared file actually hashes to C (sidecar stale/wrong) | `prepInputs(...)` with project CHECKSUMS = C | sidecar's claimed hash is rechecked, mismatch with adjacent file detected; shared sidecar overwritten with C under exclusive lock; file linked; no error |
| C6 | **divergence**: project CHECKSUMS says hash A; shared sidecar says hash B; adjacent shared file is actually B | `prepInputs(...)` | project CHECKSUMS wins as expected; shared file fails A check (S1); download proceeds; destinationPath gets fresh A-hash file + sidecar |
| C7 | project CHECKSUMS empty; shared sidecar says hash A; adjacent file is A | `prepInputs(...)` | shared sidecar used as expected hash; linked; project CHECKSUMS gains entry; destinationPath sidecar created |
| C8 | destinationPathShared dir contains legacy `<urlEncoded>.hash` files but no `.repro/` | first `prepInputs(...)` | one-shot info message about hydration; sidecars written; legacy `.hash` files **deleted** after successful migration; subsequent calls use sidecars only |
| C9 | destinationPathShared dir contains legacy `CHECKSUMS.txt` but no `.repro/` and no data files | first `prepInputs(...)` | hydration runs, no sidecars written (no adjacent files), no error; subsequent calls behave as empty shared |
| C10 | destinationPath sidecar is malformed JSON | `prepInputs(...)` | sidecar treated as missing; recomputed and overwritten; one-shot warning |

### 5.5 targetFile inference

These cases pin Step A in §3.2: when can we get away with no HEAD request,
when do we need one, what happens when it fails.

| # | Pre-state | Action | Expected |
|---|---|---|---|
| T1 | `targetFile = "foo.tif"` supplied; destinationPathShared has `foo.tif` matching CHECKSUMS | `prepInputs(url, targetFile="foo.tif")` | NO HEAD request issued; destinationPathShared match wins |
| T2 | `targetFile = NULL`, URL is plain `https://x/foo.tif` | `prepInputs(url)` | `.guessAtFile` returns `"foo.tif"`; HEAD avoided if we already have a hash from any CHECKSUMS source |
| T3 | `targetFile = NULL`, URL ends in random hash (no extension); server returns `Content-Disposition: filename="foo.tif"` | `prepInputs(url)` | one HEAD; canonicalFile = `"foo.tif"`; destinationPathShared searched with the right name |
| T4 | as T3 but content-disposition is missing; URL fallback gives a wrong name | `prepInputs(url)` | destinationPathShared miss (correct: we genuinely can't tell); download proceeds |
| T5 | Google Drive URL, `targetFile = NULL` | `prepInputs(url)` | `getRemoteMetadata` uses drive_get; canonicalFile from `file$name`; remoteHash = md5; destinationPathShared match by hash even if basename differs |
| T6 | HEAD request raises a network error | `prepInputs(url)` | silent fall-through to `.guessAtFile` (today's behavior preserved); no crash |
| T7 | `targetFile = NULL`, no URL provided (local-only `prepInputs`) | `prepInputs(targetFile=NULL, archive=...)` | no HEAD; existing local-only path used; no regression |

### 5.6 remoteHash matching

These cases pin the Step B authority order and Step E hash comparison.

| # | Pre-state | Action | Expected |
|---|---|---|---|
| R1 | server returns etag `"abc"`; destinationPathShared has `foo.tif` whose computed sha matches `"abc"` (after algo normalization) | `prepInputs(url, targetFile="foo.tif")` | linked from destinationPathShared; no full download |
| R2 | server returns etag `"abc"`; destinationPathShared has `foo.tif` whose hash is `"def"` | `prepInputs(...)` | hash mismatch logged (S1 path); download proceeds |
| R3 | no remote available (offline); `<destinationPath>/.repro/<file>.json` from prior run says `"abc"`; destinationPathShared has matching file | `prepInputs(...)` | linked from destinationPathShared using destinationPath sidecar's hash (Step B item 2) |
| R4 | destinationPathShared has matching JSON sidecar; no other hash source | `prepInputs(...)` | linked; sidecar copied to destinationPath under exclusive lock |
| R5 | server hash present AND project CHECKSUMS entry present; they disagree | `prepInputs(...)` | project CHECKSUMS wins (Step B order); if file was just downloaded, post-download verification flags the disagreement |
| R6 | etag is a weak validator (`W/"abc"`) | `prepInputs(...)` | weak-validator handling: prefix stripped; comparison proceeds |
| R7 | server returns no etag and no md5 (some CDNs) | `prepInputs(...)` | falls back to file-size proxy (today's behavior at preProcess.R:538); document explicitly |

### 5.7 Backward compatibility

| # | Pre-state | Action | Expected |
|---|---|---|---|
| B1 | code from before this refactor that sets `options(reproducible.inputPaths = ...)` | `prepInputs(...)` | works identically to setting `reproducible.destinationPathShared`; one deprecation message per session |
| B2 | code that sets `options(reproducible.dataPath = ...)` (intermediate name) | `prepInputs(...)` | works; one deprecation message per session |
| B3 | `destinationPathShared` and `dataPath` both set with different values | `prepInputs(...)` | `destinationPathShared` wins; deprecated option ignored silently |

### 5.8 Integration with prepInputs/Cache

| # | Pre-state | Action | Expected |
|---|---|---|---|
| I1 | `Cache(prepInputs(...))` with cold cache, destinationPathShared populated | first call | cache miss; prepInputs runs; uses destinationPathShared; cache filled |
| I2 | as I1, second call | second call | cache hit; destinationPathShared not consulted (no I/O) |
| I3 | `Cache(prepInputs(...))` cold cache, destinationPathShared empty + writable | first call | downloads; file lands in destinationPathShared; hardlinked into destinationPath; cache filled |

### 5.9 Auto-population (write path)

These cases pin §3.7 and the `pp_finalize_placement` logic.

| # | Pre-state | Action | Expected |
|---|---|---|---|
| W1 | destinationPathShared set + writable; file not in either dir; no Cache | `prepInputs(url, destinationPath)` | downloaded; physical file lives in destinationPathShared; destinationPath has a hardlink (same inode); sidecars in both |
| W2 | destinationPathShared set + writable; same call from a fresh second project | `prepInputs(...)` for the same url to a different destinationPath | NO download (W1's file picked up via destinationPathShared); two destinationPaths now share one inode in destinationPathShared; sidecar count = 3 (1 shared + 2 dest) |
| W3 | destinationPathShared unset | `prepInputs(...)` | physical file in destinationPath; no shared activity (today's behavior preserved exactly) |
| W4 | destinationPathShared set but read-only | `prepInputs(...)` | physical file in destinationPath; one-shot warning; sidecar in destinationPath only |
| W5 | destinationPathShared has multiple entries; first writable, second read-only | `prepInputs(...)` | physical file in first; second untouched |
| W6 | destinationPathShared has multiple entries; first read-only, second writable | `prepInputs(...)` | physical file in second; first read-checked but not written |
| W7 | cross-device destinationPathShared (file.link fails) | `prepInputs(...)` | physical file in destinationPathShared; copy (not hardlink) into destinationPath; one-shot warning naming the cost; **invariant relaxed** (now two physical copies — document explicitly) |
| W8 | destinationPathShared same filesystem as destinationPath; sufficient free space | `prepInputs(...)` for a 1 GB fixture | exactly one inode used; assert `file.info(...)$nlinks == 2` |
| W9 | rename from tmp into destinationPathShared fails (full disk in shared, dest on different FS) | `prepInputs(...)` | fall back to leaving file in destinationPath; one-shot warning; sidecar in destinationPath; no zero-byte file in shared |

### 5.10 Performance smoke

| # | Pre-state | Action | Expected |
|---|---|---|---|
| Q1 | destinationPathShared with 10,000 unrelated files, recursive=FALSE | `prepInputs(...)` for one needed file | finishes in < 1s on typical disk; assert via `system.time` ceiling |
| Q2 | destinationPathShared with deep nested tree, recursive=TRUE | `prepInputs(...)` | finishes in < 5s; primarily a regression guard |
| Q3 | repeated `prepInputs()` for the same file, hot destinationPath | 10 calls | no HEAD requests after the first; sidecar short-circuits |
| Q4 | digest cost: 100 MB fixture, 5 sequential `prepInputs(...)` calls (cold start, then 4 hot) | 5 calls | hash computed at most once across the 5 calls; assert via stub on `digest::digest` call count == 1 |
| Q5 | digest cost: server provides md5 etag; project CHECKSUMS empty | `prepInputs(...)` | md5 used; **no** sha1 computed; sidecar `hashAlgo == "md5"` |

### 5.11 Migration from legacy formats

Tests that exercise step 4 of §4 (legacy hydration).

| # | Pre-state | Action | Expected |
|---|---|---|---|
| M1 | destinationPath has `<urlEncoded>.hash` files (one per URL) and adjacent data files | `prepInputs(...)` for one of those URLs | hydration runs; sidecars created at `<dest>/.repro/<file>.json` with reconstructed `sourceUrl`; legacy `.hash` files **deleted from destinationPath** after migration; one-shot info message |
| M2 | destinationPathShared has `<urlEncoded>.hash` files | first `prepInputs(...)` reading from this dir | sidecars created; legacy `.hash` files **deleted from destinationPathShared** under exclusive lock; message names count |
| M3 | destinationPathShared has both `CHECKSUMS.txt` and `<urlEncoded>.hash`; entries don't fully overlap | first `prepInputs(...)` | hydration reads both, prefers `<urlEncoded>.hash` for entries that exist in both (it has the more authoritative remote hash); union written to sidecars |
| M4 | destinationPath sidecar exists with `schemaVersion: 0` (hypothetical future migration) | `prepInputs(...)` | reader treats as legacy: rehashes file, writes new sidecar at current schemaVersion |
| M5 | hydration encounters a `<urlEncoded>.hash` file with no resolvable adjacent data file | `prepInputs(...)` | warning naming the orphan; legacy file left alone; no sidecar written; other migrations succeed |

### 5.12 Concurrency

Requires `filelock`. If absent, tests in this section are skipped (with an
explicit `skip()` message), and a separate sub-test confirms the no-lock
graceful-degradation path.

| # | Pre-state | Action | Expected |
|---|---|---|---|
| L1 | two R sessions call `prepInputs(...)` for the same file simultaneously, both finding the file in destinationPathShared | both call `prepInputs(...)` (use `callr::r_bg` × 2 or `parallel::mclapply`) | both succeed; destinationPaths each contain a valid file; sidecars consistent; no partial files |
| L2 | one writer copying a file *into* destinationPathShared (slow `file.copy`, simulated with a hook), one reader trying to link the same file out | concurrent | reader either waits until writer finishes (acquires shared lock after exclusive released) or sees no file (writer hasn't started rename yet); never sees a partial file |
| L3 | writer holds an exclusive lock past the 30s timeout; another session tries to acquire | timeout fires | second session logs timeout warning, falls through to download; no crash, no partial state |
| L4 | sidecar file deleted mid-read by another process | concurrent | reader treats as missing sidecar; recomputes; writes new sidecar under exclusive lock; no crash |
| L5 | `filelock` not installed | `prepInputs(...)` to a destinationPathShared dir | one-shot warning at first lock attempt; operation proceeds without locking; rename-based atomicity still protects readers |
| L6 | `<destinationPathShared>/.repro/locks/` not creatable (read-only mount) | `prepInputs(...)` | one-shot warning; reads proceed without lock; writes to shared sidecar are skipped; destinationPath sidecar still written |
| L7 | a writer crashes mid-update (simulate via `Sys.kill` or process abort in a child) | next session | OS releases the lock on FD close; rename-based write means the partial tmp file may exist but the canonical sidecar/file is unchanged; tmp file is cleaned up on next access by the same writer (best-effort `unlink` of `<file>.tmp.*`) |

### 5.13 Public API: `destinationPathSharedLs()` and `destinationPathSharedRefresh()`

| # | Pre-state | Action | Expected |
|---|---|---|---|
| A1 | destinationPathShared unset | `destinationPathSharedLs()` | returns 0-row data.frame with the expected columns; no error |
| A2 | destinationPathShared set, dir empty | `destinationPathSharedLs()` | 0-row data.frame |
| A3 | destinationPathShared has 3 files with sidecars | `destinationPathSharedLs()` | 3 rows; columns populated from sidecars; no hash recompute |
| A4 | destinationPathShared has 3 files, 2 with sidecars, 1 without; `hash = TRUE` | `destinationPathSharedLs()` | 3 rows; missing sidecar's hash computed and *also* written to disk under exclusive lock |
| A5 | as A4 but `hash = FALSE` | `destinationPathSharedLs(hash = FALSE)` | 3 rows; missing-sidecar row has `NA` hash; no recompute, no disk writes |
| A6 | sidecar exists but adjacent data file is missing (orphan) | `destinationPathSharedRefresh(deleteOrphans = FALSE)` | sidecar left in place; 0 rows reported as removed |
| A7 | as A6 with `deleteOrphans = TRUE` | `destinationPathSharedRefresh(deleteOrphans = TRUE)` | orphan sidecar deleted; 1 row reported |
| A8 | data file exists but no sidecar | `destinationPathSharedRefresh()` | sidecar created; row reported as added |
| A9 | data file exists with sidecar but adjacent file's hash no longer matches sidecar (manual replacement) | `destinationPathSharedRefresh()` | sidecar updated with new hash; row reported as repaired |
| A10 | `path` arg overrides option | `destinationPathSharedLs("/explicit/path")` | option ignored; explicit path scanned |

### 5.14 What we are NOT testing (out of scope, document explicitly)

- Real network downloads (use mocked URLs).
- Cloud / S3 / GDrive paths (those go through `useCloud`, separate subsystem).
- NFSv3-specific lock semantics (`filelock` documents NFSv4+ as supported).
  We document this caveat in the option help.
- Cross-host concurrency on truly broken NFS setups (no quorum, etc.).
- Performance under thousands of concurrent writers — not a real workload.

## 6. Migration & rollout

- All changes are backward compatible: deprecated option aliases keep working,
  legacy `<urlEncoded>.hash` files and shared `CHECKSUMS.txt` are auto-
  hydrated to sidecars on first encounter.
- New `Suggests: filelock` in DESCRIPTION. Not a hard dep — graceful
  degradation if absent (with a one-shot warning).
- Two new exported functions: `destinationPathSharedLs()`, `destinationPathSharedRefresh()`
  (§3.8).
- Internal helpers stay `.`-prefixed: `.readSidecar`, `.writeSidecar`,
  `.sidecarPath`, `.withFileLock`, `.hydrateLegacyChecksums`,
  `.normalizeDestinationPathShared`, `.resolveExpectedHash`, `.getDestinationPathShared`,
  `.getDestinationPathSharedRecursive`, `.placeDownloadedFile`.
- NEWS.md entry highlights:
  - rename `reproducible.dataPath` → `reproducible.destinationPathShared`;
  - shared CHECKSUMS.txt no longer maintained (sidecars instead);
  - destinationPathShared now auto-populates on download (single-physical-copy
    invariant, §3.7);
  - locking via `filelock` (Suggests);
  - new exports `destinationPathSharedLs()` and `destinationPathSharedRefresh()`;
  - mismatched files in destinationPathShared now logged, not silently skipped.
- Bump to `3.0.0.9042` (next patch in the current dev cycle).

## 7. Open questions

All decided. Recorded for traceability:

- ~~**Naming.**~~ Adopted `reproducible.destinationPathShared`. (§0)
- ~~**Concurrency.**~~ In scope this pass via `filelock` (Suggests). (§3.5)
- ~~**Shared CHECKSUMS.**~~ Dropped. Sidecars only in shared. (§3.3, §3.6)
- ~~**Sidecar key.**~~ File-keyed, JSON. URL stored as a field. (§3.6)
- ~~**Legacy `<urlEncoded>.hash`.**~~ Migrated to sidecar format on first
  encounter; deleted in both destinationPath and destinationPathShared after
  successful migration. (§3.6, §5.11)
- ~~**Auto-population.**~~ destinationPathShared is read-write. On download, place
  the physical file in the first writable shared dir; hardlink into
  destinationPath. One physical copy per filesystem. (§3.7, §5.9)
- ~~**destinationPath hash mismatch.**~~ Re-download. (Today's behavior.)
- ~~**Public `destinationPathSharedLs()`.**~~ Yes, this iteration. Plus
  `destinationPathSharedRefresh()` for managing externally-populated dirs. (§3.8,
  §5.13)
- ~~**Hash algorithm.**~~ Digest once, using whichever algo upstream
  provided. Sidecar's `hashAlgo` records the choice. Cross-algo recompute
  only when project CHECKSUMS forces it (rare). (§3.7)

---

End of spec. Next action: write the test file in §5 against current code as
a baseline (most should fail), then execute the refactor steps in §4.
