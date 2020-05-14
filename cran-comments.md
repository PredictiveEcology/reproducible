## Updated release

This release fixes several errors revealed during CRAN checks.
We have also begun working through required changes to our use of spatial packages due to upstream CRS changes in PROJ and GDAL.
See `NEWS.md` for a complete list of changes.

## Test environments

Tested and passed using winbuilder all three versions, and rhub with no errors, warnings, or notes.

### Previous R versions
* Ubuntu 16.04              (travis-ci), R 3.6.3
* Windows                    (appveyor), R 3.6.3
* Windows                 (win-builder), R 3.6.3

### Current R versions
* macOS 10.13.3 High Sierra (travis-ci), R 4.0.0
* macOS 10.15.4 Catalina        (local), R 4.0.0
* Ubuntu 16.04              (travis-ci), R 4.0.0
* Ubuntu 18.04                  (local), R 4.0.0
* Windows                    (appveyor), R 4.0.0
* Windows                 (win-builder), R 4.0.0

### Development R version
* Ubuntu 16.04              (travis-ci), R 4.1.0 (2020-05-13 r78453)
* Ubuntu 18.04                  (local), R 4.1.0 (2020-05-13 r78456)
* Windows                    (appveyor), R 4.1.0 (2020-05-12 r78431)
* Windows                 (win-builder), R 4.1.0 (2020-05-11 r78411)

## R CMD check results

There were no ERRORs nor WARNINGs.

There is one NOTE:

```
> checking package dependencies ... NOTE
  Imports includes 25 non-default packages.
  Importing from so many packages makes the package vulnerable to any of
  them becoming unavailable.  Move as many as possible to Suggests and
  use conditionally.
```

We will pare down the number of dependencies in future releases, as we plan to split some functionality into another package.

## Downstream dependencies

We have run R CMD check on downstream dependencies.
`SpaDES.core` requires an update, which we will submit this package immediately upon `reproducible` being accepted.
Other reverse dependencies are unaffected.
