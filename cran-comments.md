## Release information

This is minor package update that addresses a new failure due to a package dependency. 

See `NEWS.md` for a full list of changes.

## Test environments

  
### Previous R versions
* Ubuntu 20.04                 (GitHub), R 4.1.3, 4.2.3, 4.3.3
* Windows                      (GitHub), R 4.1.3, 4.2.3, 4.3.3
* Windows                 (win-builder), R 4.3.3

### Current R versions
* macOS 12.6.3                 (GitHub), R 4.4.2
* macOS 13.3.1            (mac-builder), R 4.4.2
* macOS 14.4.1                  (local), R 4.4.2
* Ubuntu 20.04                 (GitHub), R 4.4.2
* Ubuntu 20.04                  (local), R 4.4.2
* Windows                      (GitHub), R 4.4.2
* Windows                       (local), R 4.4.2
* Windows                 (win-builder), R 4.4.2

### Development R version
* Ubuntu 20.04                 (GitHub), R-devel (2024-12-10 r87437)
* Ubuntu 20.04                  (local), R-devel (2024-12-10 r87437)
* Windows                      (GitHub), R-devel (2024-12-10 r87437 ucrt)
* Windows                 (win-builder), R-devel (2024-12-10 r87437 ucrt)

### R-hub v2
* Flavours              Linux, Mac, Windows


## R CMD check results

There are no errors, or warnings in any of the above.

There are some NOTEs:

## Downstream dependencies

We checked all reverse dependency from CRAN, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 1 new problem with package `SpaDES.core`, which we will submit an updated version of as soon as `reproducible` is updated on CRAN.

 * We failed to check 0 packages
