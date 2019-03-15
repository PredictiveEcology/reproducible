## Updated release

This is a maintenance release which fixes some minor bugs. See `NEWS.md`.

## Test environments

### Previous R versions
* Ubuntu 14.04        (travis-ci), R 3.3.3
* Ubuntu 14.04        (travis-ci), R 3.4.4
* Windows              (appveyor), R 3.3.3
* Windows              (appveyor), R 3.4.4

### Current R versions
* macOS Mojave        (travis-ci), R 3.5.3
* macOS Mojave            (local), R 3.5.3
* Ubuntu 14.04        (travis-ci), R 3.5.3
* Ubuntu 18.04            (local), R 3.5.3
* Windows              (appveyor), R 3.5.3
* Windows           (win-builder), R 3.5.3
* Windows 7               (local), R 3.5.2

### Development R version
* Ubuntu 14.04       (travis-ci), R 3.6.0 (2019-03-05 r76199)
* Ubuntu 18.04           (local), R 3.6.0 (2019-02-28 r76173)
* Windows             (appveyor), R 3.6.0 (2019-03-04 r76198)
* Windows          (win-builder), R 3.6.0 (2019-03-03 r76192)

## R CMD check results

There were no ERRORs nor WARNINGs.

There were 1 NOTEs:

1. spell-check reports false positives: 

    Possibly mis-spelled words in DESCRIPTION:
      Reproducibility (3:36)
      checksums (9:55)
 
## Downstream dependencies

We have run R CMD check on downstream dependencies, and all have passed except those noted below.
Summary at https://github.com/PredictiveEcology/reproducible/blob/master/revdep/README.md.

* problems detected on the CRAN versions of `SpaDES` and `SpaDES.core` are fixed in the versions to be submitted shatly to CRAN.
