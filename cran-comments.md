## resubmission

This is a major update to our package.

## Test environments

### Previous R versions
* Ubuntu 14.04.5      (travis-ci), R 3.3.3
* Windows              (appveyor), R 3.3.2
* Windows              (appveyor), R 3.3.3
* Windows 7               (local), R 3.3.3

### Current R versions
* macOS High Sierra    (local), R 3.4.3
* Ubuntu 16.0          (local), R 3.4.3
* Ubuntu 14.04.5   (travis-ci), R 3.4.2
* Debian 4.9.51        (local), R 3.4.3
* Windows           (appveyor), R 3.4.3
* Windows        (win-builder), R 3.4.3
* Windows 7            (local), R 3.4.3

### Development R version
* Ubuntu 14.04        (travis-ci), R 3.5.0 (2018-01-24 r74157)
* Windows                 (local), R 3.5.0 (2018-01-24 r74157)
* Windows           (win-builder), R 3.5.0 (2018-01-24 r74157)

## R CMD check results

There are no errors, warnings, or notes in any of the above.

## Downstream dependencies

There are several 3 downstream dependencies of this package. I am a co-developer of these 3 packages. These packages are about to be updated with CRAN. We checked against current CRAN versions of all 3 and current development versions of all 3. There were 2 failed tests on SpaDES.core on CRAN and 2 failed tests currently on the development version. These have been noted, are easy to change. Changes to those packages will be will be submitted soon (within 2 weeks) to CRAN.

- `SpaDES` (Imports)
- `SpaDES.addins` (Imports)
- `SpaDES.core` (Depends)
