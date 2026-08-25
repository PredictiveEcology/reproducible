## Resubmission

`reproducible` was archived on 2026-07-13 because tests using Internet
resources did not fail gracefully. Every test and example that reaches the
network is now either gated off CRAN, or skips with an informative message when
a download fails or a checksum no longer matches, so a remote outage cannot
produce a check failure. Third-party data used by the test suite has been
replaced with small fixtures hosted in this project's own GitHub releases.

See `NEWS.md` for the full list of changes in 3.2.0.

## Test environments

* win-builder: R-oldrelease, R-release, R-devel
* GitHub Actions: Ubuntu, Windows, macOS (R-release, R-devel, R-oldrel)
* Local: Linux, R 4.5.3

## R CMD check results

No errors or warnings. One NOTE, from the incoming feasibility check:

* `New submission` and `Package was archived on CRAN` — expected for this
  resubmission.
* `Possibly misspelled words in DESCRIPTION: checksums`. This is spelled
  correctly; it is the standard term for the file digests the package compares.

## Downstream dependencies

None currently. SpaDES, SpaDES.core and SpaDES.tools were archived on
2026-07-13 as a consequence of this archival; they share this maintainer and
will be resubmitted once this package is accepted.
