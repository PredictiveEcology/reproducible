## Release information

This is major package update that includes a rewrite of the internals of a main function of the 
package, Cache. This change improves maintainability going forward, and has many enhancements.

See `NEWS.md` for a full list of changes.

## Test environments

### Previous R versions
* Ubuntu 24.04                 (GitHub), R 4.4.3
* Windows                      (GitHub), R 4.4.3
* Windows                 (win-builder), R 4.4.3

### Current R versions
* macOS 14.7.6                 (GitHub), R 4.5.2
* Ubuntu 24.04                 (GitHub), R 4.5.2
* Windows                      (GitHub), R 4.5.2
* Windows                       (local), R 4.5.2
* Windows                 (win-builder), R 4.5.2

### Development R version
* Ubuntu 24.04                 (GitHub), R-devel (2026-01-06 r89281)
* Windows                      (GitHub), R-devel (2026-01-06 r89281 ucrt)
* Windows                 (win-builder), R-devel (2026-01-06 r89281 ucrt)

### R-hub v2
* Flavours              Linux, Mac, Windows

## R CMD check results

There are no errors, or warnings. There is one NOTE related to the author, Eliot McIntire.

## Downstream dependencies

## revdepcheck results

We checked 3 reverse dependencies on CRAN), comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 2 packages
 
We will submit updated versions of SpaDES.core and SpaDES.tools immediately upon acceptance of this package.
