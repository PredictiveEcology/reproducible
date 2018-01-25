## Resubmission

This is an update to our recent submission which addresses issues discovered during CRAN package checks.
We setup a Solaris VM to test the code we previously believe had been fixed, and have now confirmed that the issue has been resolved.
We apologize for our previous incomplete submission.

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
* Ubuntu 14.04        (travis-ci), R 3.5.0 (2017-08-10 r73083)
* Ubuntu 16.04            (local), R 3.5.0 (2017-08-08 r73067)
* Windows              (appveyor), R 3.5.0 (2017-08-09 r73082)
* Windows           (win-builder), R 3.5.0 (2017-08-09 r73082)

## R CMD check results

There were no ERRORs or WARNINGs

There was 1 NOTE:

1. There are multiple parts to this note:

    a. This submission fixes issues discovered in our recent package update.
    
            Maintainer: 'Eliot J B McIntire <eliot.mcintire@canada.ca>'
            
            Days since last update: 1

    b. Some words were flagged as possibly misspelled, but they are false positives. 
     
            Possibly mis-spelled words in DESCRIPTION: 
                GitHub (9:38)
                Reproducibility (3:36)

## Downstream dependencies

There are currently no downstream dependencies of this package.
However, as we submit further `SpaDES` spinoff packages, this package will become a dependency for the following packages:

- `SpaDES` (Imports)
- `SpaDES.addins` (Imports)
- `SpaDES.core` (Depends)
