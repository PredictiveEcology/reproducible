## Updated release

This release fixes an error revealed during CRAN checks.

## Test environments

### Previous R versions
* Ubuntu 16.04              (travis-ci), R 3.6.3
* Windows                    (appveyor), R 3.6.3
* Windows                 (win-builder), R 3.6.3

### Current R versions
* macOS 10.13.3 High Sierra (travis-ci), R 4.0.2
* macOS 10.15.5 Catalina        (local), R 4.0.2
* Ubuntu 16.04              (travis-ci), R 4.0.2
* Ubuntu 20.04                  (local), R 4.0.2
* Windows                    (appveyor), R 4.0.2
* Windows                       (local), R 4.0.2
* Windows                 (win-builder), R 4.0.2

### Development R version
* Ubuntu 16.04              (travis-ci), R 4.1.0 (2020-08-03 r78963)
* Ubuntu 20.04                  (local), R 4.1.0 (2020-08-03 r78963)
* Windows                    (appveyor), R 4.1.0 (2020-08-02 r78957)
* Windows                 (win-builder), R 4.1.0 (2020-08-02 r78957)

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

We checked 4 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages
