## Purpose

This is a patch release fixing the check ERROR CRAN reported for 3.2.0 on
`r-devel-linux-x86_64-fedora-clang` and `r-devel-linux-x86_64-fedora-gcc`.

`prepInputs()` determined whether the system's 7-Zip supported RAR archives by
shelling out to `apt`, which exists only on Debian derivatives. Because
`system(..., intern = TRUE)` signals an R error when the command is absent
(rather than returning a non-zero status), this failed on Fedora and any other
non-Debian Linux. It now asks 7-Zip itself (`7z i`), which answers the actual
question on every platform and needs no package manager. Two nearby calls to
external binaries (`ps`, `unrar`) are likewise guarded with `Sys.which()`.

**Days since last update: 0.** 3.2.0 was published today; this release exists
solely to clear that ERROR. I am happy to hold it if you would prefer to batch
it with a later update.

## Test environments

* Local: Ubuntu 24.04, R 4.5.3 — `R CMD check --as-cran`
* GitHub Actions: Ubuntu (R-devel, R-release, R-oldrel-1), Windows (R-devel,
  R-release, R-oldrel-1, R-oldrel-2), macOS (R-release), all with
  `--as-cran --run-dontrun --run-donttest`
* GitHub Actions: Ubuntu R-release with `_R_CHECK_DEPENDS_ONLY_=true`
* Fedora 41 container, R 4.5.3, hard dependencies only — added for this
  release specifically to reproduce the flavour that caught the bug, and
  verified to fail on the old code and pass on the new

win-builder was not reachable for this submission: the upload form accepted the
tarball but returned no HTTP response on repeated attempts.

## R CMD check results

No errors or warnings. One NOTE, from the incoming feasibility check:

* `Days since last update: 0` — see above.
* One possibly-invalid URL (`https://stackoverflow.com/a/44445010`, cited in a
  2020 `NEWS.md` entry as the source of a suggestion); the page loads in a
  browser, but Stack Overflow returns HTTP 403 to automated requests.

## Downstream dependencies

None currently. SpaDES, SpaDES.core and SpaDES.tools were archived on
2026-07-13 as a consequence of this package's archival; they share this
maintainer and will be resubmitted now that this package is back on CRAN.
