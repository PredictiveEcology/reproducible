## New submission

This is a spin off of an existing CRAN package (`SpaDES`), which we have split due to growing package size.
We have removed `\dontrun` from the examples so that they *do* run, and have added the examples to out tests.

## Test environments

### Previous R versions
* Ubuntu 14.04        (travis-ci), R 3.3.3
* Windows              (appveyor), R 3.3.3
* Windows 7               (local), R 3.3.3

### Current R versions
* macOS Sierra         (local), R 3.4.1
* OS X El Capitan  (travis-ci), R 3.4.1
* Ubuntu 14.04     (travis-ci), R 3.4.1
* Ubuntu 16.04         (local), R 3.4.1
* Windows           (appveyor), R 3.4.1
* Windows        (win-builder), R 3.4.1
* Windows 7            (local), R 3.4.1

### Development R version
* Debian:testing (rocker/r-devel), R 3.5.0 (2017-07-26 r72972)
* Ubuntu 14.04        (travis-ci), R 3.5.0 (2017-08-04 r73032)
* Ubuntu 16.04            (local), R 3.5.0 (2017-08-03 r73028)
* Windows              (appveyor), R 3.5.0 (2017-08-03 r73028)
* Windows           (win-builder), R 3.5.0 (2017-08-03 r73028)

## R CMD check results

There were no ERRORs or WARNINGs

There was 1 NOTE:

1. There are multiple parts to this note:

    a. This is a new package submission:
    
            Maintainer: 'Eliot McIntire <eliot.mcintire@canada.ca>'
            
            New submission

    b. Some words were flagged as possibly misspelled, but they are not. 
     
            Possibly mis-spelled words in DESCRIPTION: 
                GitHub (9:38)
                Reproducibility (3:36)

## Downstream dependencies

There are currently no downstream dependencies of this package.
However, as we submit further `SpaDES` spinoff packages, this package will become a dependency for the following packages:

- `SpaDES` (Imports)
- `SpaDES.addins` (Imports)
- `SpaDES.core` (Depends)
