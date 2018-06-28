## Updated release

THis release fixes a major bug that is holding up the submission of `SpaDES.core` (see NEWS.md).

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
* macOS High Sierra    (local), R 3.5.0
* OS X El Capitan  (travis-ci), R 3.5.0
* Ubuntu 14.04     (travis-ci), R 3.5.0
* Ubuntu 18.04         (local), R 3.5.0
* Windows           (appveyor), R 3.5.0
* Windows        (win-builder), R 3.5.0
* Windows 7            (local), R 3.5.0

### Development R version
* Ubuntu 14.04     (travis-ci), R 3.6.0 (2018-06-20 r74923)
* Ubuntu 18.04         (local), R 3.6.0 (2018-06-28 r74935)
* Windows           (appveyor), R 3.6.0 (2018-06-26 r74934)
* Windows        (win-builder), R 3.6.0 (2018-06-26 r74934)

## R CMD check results

There were no ERRORs nor WARNINGs.

There was 1 NOTE:

1. spell-check reports false positives: 

    Possibly mis-spelled words in DESCRIPTION:
      Reproducibility (3:36)
      checksums (9:55)
 
## Downstream dependencies

We have run R CMD check on downstream dependencies, and all have passed except those noted below.
Summary at https://github.com/PredictiveEcology/reproducible/blob/master/revdep/README.md.

* `SpaDES` shows a warning due to `DISPLAY` not being set in the headless session.
  Three other warnings regarding replaced imports are caused by having depracated and moved these functions from `SpaDES.tools` to this package.

* `SpaDES.core` produce an error due to a non-CRAN package in Suggests, but passes once that dependency is installed.

* `SpaDES.tools` produces an error due to an unrelated error in that package. I am a developer of `SpaDES.tools` and have prepared a fixed version for CRAN which will be submitted shortly.
