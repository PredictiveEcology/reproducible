## Updated release

This is a release which adds several new features and fixes some minor bugs. See `NEWS.md`.

## Test environments

### Previous R versions
* Ubuntu 16.04        (travis-ci), R 3.5.3
* Windows           (win-builder), R 3.5.3
* Windows 7               (local), R 3.5.3

### Current R versions
* macOS Mojave        (travis-ci), R 3.6.1
* macOS Mojave            (local), R 3.6.1
* Ubuntu 16.04        (travis-ci), R 3.6.1
* Ubuntu 18.04            (local), R 3.6.1
* Windows           (win-builder), R 3.6.1

### Development R version
* Ubuntu 16.04       (travis-ci), R 3.7.0 (2019-09-05 r77152)
* Debian 18.04           (local), R 3.7.0 (2019-09-05 r77152)
* Windows          (win-builder), R 3.7.0 (2019-09-04 r77145)

## R CMD check results

There were no ERRORs nor WARNINGs, nor NOTEs.

## Downstream dependencies

We have run R CMD check on downstream dependencies, and have no ERRORs and 1 WARNING:

```
* SpaDES.core
  checking S3 generic/method consistency ... WARNING
```

As a co-developer of the `SpaDES.core` package, we have already fixed this issue and will be submitting and updated version to CRAN shortly.
