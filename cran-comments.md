## Updated release

This is a maintenance release which adds several new features and fixes some minor bugs.
See `NEWS.md`.

## Test environments

### Previous R versions
* Ubuntu 16.04              (travis-ci), R 3.5.3
* Windows                    (appveyor), R 3.5.3
* Windows                 (win-builder), R 3.5.3

### Current R versions
* macOS 10.13.3 High Sierra (travis-ci), R 3.6.1
* macOS 10.15.1 Catalina        (local), R 3.6.1
* Ubuntu 16.04              (travis-ci), R 3.6.1
* Ubuntu 18.04                  (local), R 3.6.1
* Windows                    (appveyor), R 3.6.1
* Windows                 (win-builder), R 3.6.1

### Development R version
* Ubuntu 16.04              (travis-ci), R 4.0.0 (2019-11-21 r77446)
* Ubuntu 18.04                  (local), R 4.0.0 (2019-11-21 r77446)
* Windows                    (appveyor), R 4.0.0 (2019-11-20 r77445)
* Windows                 (win-builder), R 4.0.0 (2019-11-21 r77446)

## R CMD check results

There were no ERRORs nor WARNINGs, nor NOTEs.

## Downstream dependencies

We have run R CMD check on downstream dependencies, and have no ERRORs and 1 WARNING:

```
* SpaDES.core
  checking S3 generic/method consistency ... WARNING
```

As a co-developer of the `SpaDES.core` package, we have already fixed this issue and will be submitting and updated version to CRAN shortly.

## revdepcheck results

We checked 4 reverse dependencies (0 from CRAN + 4 from BioConductor), comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages
