## Release information

This is a minor feature release (3.0.0 -> 3.1.0).

Highlights:

* `prepInputsCOG`: a new fast-path inside `prepInputs` that fetches only the
  spatial window of interest from remote tiled/Cloud-Optimized GeoTiffs via
  GDAL's `/vsicurl/`, avoiding full-file downloads.
* The `reproducible.inputPaths` / `reproducible.inputPathsRecursive` options
  have been renamed to `reproducible.destinationPathShared` /
  `reproducible.destinationPathSharedRecursive`; the old names remain fully
  functional as backwards-compatible aliases.
* `alsoExtract` now also accepts regular-expression patterns.
* `preProcess` skips re-downloading when a matching local copy already exists.

See `NEWS.md` for the full list of changes. No user-visible changes are
expected to break existing code.

## Test environments

### Previous R versions
* Ubuntu 24.04                 (GitHub), R 4.4.x
* Windows                      (GitHub), R 4.4.x
* Windows                 (win-builder), R-oldrelease

### Current R versions
* macOS 14                     (GitHub), R 4.5.2
* Ubuntu 24.04                 (GitHub), R 4.5.2
* Windows                      (GitHub), R 4.5.2
* Windows                       (local), R 4.5.2
* Windows                 (win-builder), R-release

### Development R version
* Ubuntu 24.04                 (GitHub), R-devel
* Windows                      (GitHub), R-devel
* Windows                 (win-builder), R-devel

### R-hub v2
* Flavours              Linux, Mac, Windows (via GitHub Actions, r-hub/actions)

## R CMD check results

There are no errors or warnings. There is one NOTE related to the author,
Eliot McIntire.

## Resubmission

This is a resubmission. The previous submission produced a NOTE for the
`postProcess` example exceeding the 5s elapsed-time limit; the timing was
dominated by a remote download, which has been moved into `\donttest{}`.

## Downstream dependencies

The three reverse dependencies on CRAN (SpaDES, SpaDES.core, SpaDES.tools)
are all maintained by the same maintainer.

## revdepcheck results

We checked 3 reverse dependencies, comparing R CMD check results across the
CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages
