## Release information

This is a resubmission of `reproducible`, which was archived from CRAN on
2026-07-13 for repeated policy violation. Version 3.2.0 is a minor release: it
fixes the policy problem that led to the archival, and includes the accumulated
development work since 3.1.1.

See `NEWS.md` for the full list of changes.

## Why the package was archived, and what has changed

The package was archived because tests that use Internet resources did not fail
gracefully. When a remote resource used by a test became unavailable, the test
errored rather than skipping, producing check failures on CRAN. This violates
the policy that packages using Internet resources "should fail gracefully with
an informative message ... and not give a check warning nor error."

We have addressed this in two independent ways, so that neither a transient
outage nor the permanent loss of a remote resource can produce a check failure
again.

**1. Downloads that fail now skip, rather than error, when running under CRAN.**

When a download terminally fails, or a downloaded file no longer matches its
recorded checksum, and the code is executing inside a test on CRAN
(`TESTTHAT == "true"` and `NOT_CRAN != "true"`), the remainder of that test is
skipped with an informative message instead of raising an error. `skip()`
unwinds before any downstream code can fail on the missing file, and it requires
no pre-flight connectivity check.

Outside of tests -- that is, in normal use of the package -- behaviour is
unchanged: a failed download still raises an informative error, as users expect.
Where `NOT_CRAN == "true"` (our own machines and continuous integration) failures
also still raise errors, so we do not mask real regressions from ourselves.

**2. Tests no longer depend on third-party servers.**

The failures originated with data hosted by third parties over which we have no
control. Every such resource used by the test suite has now been replaced by a
small fixture hosted in this project's own GitHub releases:

* test fixtures previously downloaded from a personal third-party repository
  (the source whose intermittent unavailability triggered the original check
  failures);
* a shapefile previously downloaded from a federal government server
  (1.4 MB, reduced to a 15 kB simplified copy);
* a 215 MB cloud-optimized GeoTIFF previously read from a federal government
  server (reduced to a 6 kB fixture that retains the tiling and overview
  structure the test requires).

The remaining tests that reach the network are all gated with `skip_on_cran()`.

We are grateful for the CRAN team's patience, and we are sorry that this took
more than one attempt to get right.

## Test environments

### win-builder
* Windows (win-builder), R-oldrelease
* Windows (win-builder), R-release
* Windows (win-builder), R-devel

### GitHub Actions
* Ubuntu 24.04 (GitHub), R-release, R-devel, R-oldrel
* Windows      (GitHub), R-release, R-devel, R-oldrel
* macOS        (GitHub), R-release

### Local
* Linux (local), R 4.5.3

## R CMD check results

There are no errors, warnings, or notes.

## Downstream dependencies

There are currently none. The three reverse dependencies (SpaDES, SpaDES.core,
SpaDES.tools) were archived on 2026-07-13 as a consequence of this package's
archival. They are maintained by the same maintainer and will be resubmitted
once this package is accepted.

`revdepcheck` was therefore not run: no package on CRAN currently depends on
`reproducible`.
