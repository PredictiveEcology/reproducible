## Updated release

This is a release which adds several new features and fixes some minor bugs. See `NEWS.md`.

## Test environments

### Previous R versions
* Ubuntu 16.04        (travis-ci), R 3.5.0
* Windows              (appveyor), R 3.5.0

### Current R versions
* macOS Mojave        (travis-ci), R 3.6.1
* macOS Mojave            (local), R 3.6.0
* Ubuntu 14.04        (travis-ci), R 3.6.1
* Ubuntu 18.04            (local), R 3.6.0
* Windows           (win-builder), R 3.6.1
* Windows              (appveyor), R 3.6.1
* Windows 7               (local), R 3.6.0

### Development R version
* Ubuntu 16.04       (travis-ci), R 3.7.0 (2019-03-15 r76244)
* Ubuntu 18.04           (local), R 3.7.0 (2019-03-18 r76245)
* Windows          (win-builder), R 3.7.0 (2019-03-15 r76244)

## R CMD check results

There were no ERRORs nor WARNINGs, nor NOTEs.

## Downstream dependencies

We have run R CMD check on downstream dependencies, and all have passed except those noted below.
Summary at https://github.com/PredictiveEcology/reproducible/blob/master/revdep/README.md.

* problems detected on the CRAN versions of `SpaDES` and `SpaDES.core` are fixed in the versions to be submitted shatly to CRAN.
