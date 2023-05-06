## Release information

This is a major update that brings all spatial components of the package to use `terra` and `sf`. `Cache` components are now done with or without a `DBI` backend, allowing reduction in recursive dependencies from 27 to 11 packages. The internals of `postProcess()` and family to deal with a wholesale replacement with `terra` and `sf` instead of `raster` and `sp`. 

See `NEWS.md` for a full list of changes.

## Test environments

### Previous R versions
* macOS (m2) Ventura 13.2.1     (local), R 4.2.2
* Ubuntu 20.04                 (GitHub), R 4.1.3
* Ubuntu 20.04                 (GitHub), R 4.2.3
* Windows                      (GitHub), R 4.1.3
* Windows                      (GitHub), R 4.2.3
* Windows                 (win-builder), R 4.2.3

### Current R versions
* macOS Monterey 12.6.5        (GitHub), R 4.3.0
* macOS (m2) Ventura 13.2.1     (local), R 4.3.0
* macOs (m1) Big Sur             (rhub), R 4.3.0
* Ubuntu 20.04                 (GitHub), R 4.3.0
* Ubuntu 20.04                  (local), R 4.3.0
* Windows                      (GitHub), R 4.3.0
* Windows                       (local), R 4.3.0
* Windows                 (win-builder), R 4.3.0

### Development R version
* Ubuntu 20.04 LTS             (GitHub), R-devel (2023-04-25 r84327)
* Ubuntu 20.04 LTS              (local), R-devel (2023-04-25 r84327)
* Windows                      (GitHub), R-devel (2023-04-25 r84327 ucrt)
* Windows                 (win-builder), R-devel (2023-04-25 r84327 ucrt)

## R CMD check results

No NOTEs, WARNINGs, or ERRORs

## Downstream dependencies

Currently none, but we are working to resubmit our other packages that depend on this one, and they are passing.
