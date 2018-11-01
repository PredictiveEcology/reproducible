## Updated release

This is an enhancement release. Several new functions and new options. 


## Test environments

### Previous R versions
* Ubuntu 14.04        (travis-ci), R 3.1.0
* Ubuntu 14.04        (travis-ci), R 3.2.0
* Ubuntu 14.04        (travis-ci), R 3.3.0
* Ubuntu 14.04        (travis-ci), R 3.4.0
* Windows              (appveyor), R 3.1.0
* Windows              (appveyor), R 3.2.0
* Windows              (appveyor), R 3.3.0
* Windows              (appveyor), R 3.4.0

### Current R versions
* macOS El Capitan     (r-hub), R 3.5.0
* macOS Sierra     (travis-ci), R 3.5.0
* macOS High Sierra    (local), R 3.5.1
* Ubuntu 14.04     (travis-ci), R 3.5.1
* Ubuntu 18.04         (local), R 3.5.1
* Windows           (appveyor), R 3.5.1
* Windows        (win-builder), R 3.5.1
* Windows 7            (local), R 3.5.1

### Development R version
* Ubuntu 14.04     (travis-ci), R 3.6.0 (2018-10-31 r75531)
* Ubuntu 18.04         (local), R 3.6.0 (2018-11-01 r75533)
* Windows           (appveyor), R 3.6.0 (2018-10-31 r75525)
* Windows        (win-builder), R 3.6.0 (2018-10-30 r75516)

## R CMD check results

There were no ERRORs nor WARNINGs.

There were 2 NOTEs:

1. As requested by CRAN, we have fixed problems enconutered during the Windows build process last week.

    Days since last update: 6

2. spell-check reports false positives: 

    Possibly mis-spelled words in DESCRIPTION:
      Reproducibility (3:36)
      checksums (9:55)
 
## Downstream dependencies

We have run R CMD check on downstream dependencies, and all have passed except those noted below.
Summary at https://github.com/PredictiveEcology/reproducible/blob/master/revdep/README.md.

* `SpaDES` shows a warning due to `DISPLAY` not being set in the headless session.

* `SpaDES.core` throws some ERRORs, which are fixed in the next version (will be submitted shortly).
  Also shows a warning due to `DISPLAY` not being set in the headless session.
