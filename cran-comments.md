## Release information

This is a new submission to restore the package to CRAN following archival due to removal of dependency package `Require`.
This version now includes changes to documentation as requested on Nov 28, 2022 upon a previous submission to CRAN.
We have also simplified and removed tests and examples that were causing some systems on CRAN to take >10 minutes to complete package checking.

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
