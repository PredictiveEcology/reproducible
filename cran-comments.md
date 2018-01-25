## resubmission

This is a major update to our package.

## Test environments

### Previous R versions
* Ubuntu 14.04.5      (travis-ci), R 3.3.3 YES
* Windows              (appveyor), R 3.3.2 YES
* Windows              (appveyor), R 3.3.3 YES
* Windows 7               (local), R 3.3.3 YES

### Current R versions
* macOS Sierra         (local), R 3.4.3 WAITING for Alex
* OS X El Capitan  (travis-ci), R 3.4.3 WAITING for Travis
* Ubuntu 14.04.5   (travis-ci), R 3.4.2 YES
* Ubuntu 16.04         (local), R 3.4.3 WAITING for Alex
* Windows           (appveyor), R 3.4.3 YES
* Windows        (win-builder), R 3.4.3 WAITING for email
* Windows 7            (local), R 3.4.3 YES

### Development R version
* Ubuntu 14.04        (travis-ci), R 3.5.0 (2018-01-24 r74157) YES
* Windows              (appveyor), R 3.5.0 (2018-01-24 r74157) FAIL - bitops
* Windows           (win-builder), R 3.5.0 (2018-01-24 r74157) WAITING for email

## R CMD check results

There is currently 1 error on Windows R-devel about bitops package needing rebuilding due to new R internals

## Downstream dependencies

There are several  downstream dependencies of this package.

- `SpaDES` (Imports)
- `SpaDES.addins` (Imports)
- `SpaDES.core` (Depends)
