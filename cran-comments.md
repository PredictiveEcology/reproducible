## Release information

This is an update of internals of `postProcess()` and family to deal with a wholesale replacement with `terra` and `sf` instead of `raster` and `sp`. Suggestions on the title of the package and removals of backticks and single quotes in DESCRIPTION paragraph also implemented.

See `NEWS.md` for a full list of changes.

## Test environments

### Previous R versions
* Ubuntu 20.04                 (GitHub), R 4.0.5
* Ubuntu 20.04                 (GitHub), R 4.1.3
* Windows                      (GitHub), R 4.0.5
* Windows                      (GitHub), R 4.1.3
* Windows                 (win-builder), R 4.1.3

### Current R versions
* macOS 11.7 Big Sur           (GitHub), R 4.2.2
* macOS 11.7 Big Sur            (local), R 4.2.2
* macOs (m1) Big Sur             (rhub), R 4.2.2
* Ubuntu 20.04                 (GitHub), R 4.2.2
* Ubuntu 20.04                  (local), R 4.2.2 Patched (2022-11-10 r83330)
* Windows                      (GitHub), R 4.2.2
* Windows                       (local), R 4.2.2
* Windows                 (win-builder), R 4.2.2

### Development R version
* Ubuntu 20.04                 (GitHub), R-devel (2022-12-18 r83472)
* Ubuntu 20.04                  (local), R-devel (2022-12-19 r83478)
* Windows                      (GitHub), R-devel (2022-12-19 r83478 ucrt)
* Windows                 (win-builder), R-devel (2022-12-19 r83478 ucrt)

## R CMD check results

There were no ERRORs nor WARNINGs or NOTEs.

## Downstream dependencies

Currently none, but we are working to resubmit our other packages that depend on this one, and they are passing.
