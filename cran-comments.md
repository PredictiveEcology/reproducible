## Updated release

This release fixes an error revealed during CRAN checks.

## Test environments

Tested and passed using winbuilder all three versions, and rhub with no errors, warnings, or notes.

### Previous R versions
* Ubuntu 16.04              (travis-ci), R 3.6.3
* Windows                       (local), R 3.6.3
* Windows                 (win-builder), R 3.6.3

### Current R versions
* macOS 10.13.3 High Sierra (travis-ci), R 4.0.2
* Ubuntu 16.04              (travis-ci), R 4.0.2
* Ubuntu 18.04                  (local), R 4.0.2
* Windows                 (win-builder), R 4.0.2
* Windows                       (local), R 4.0.2

### Development R version
* Ubuntu 16.04              (travis-ci), R 4.1.0 (2020-07-27)
* Ubuntu 18.04                  (local), R 4.1.0 (2020-07-27)
* Windows                       (local), R 4.1.0 (2020-07-27)
* Windows                 (win-builder), R 4.1.0 (2020-07-27)

## R CMD check results

There were no ERRORs nor WARNINGs.

There is one NOTE. A possibly misspelled word: `reproducibleOptions` is spelled correctly.

## Downstream dependencies

We will be submitting an update to SpaDES.core when `reproducible` is accepted. 

