# reproducible

<!-- badges: start -->
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/reproducible)](https://cran.r-project.org/package=reproducible)
[![CRAN Downloads](https://cranlogs.r-pkg.org/badges/grand-total/reproducible)](https://cran.r-project.org/package=reproducible)
[![R build status](https://github.com/PredictiveEcology/reproducible/workflows/R-CMD-check/badge.svg)](https://github.com/PredictiveEcology/reproducible/actions)
[![Codecov test coverage](https://codecov.io/gh/PredictiveEcology/reproducible/branch/master/graph/badge.svg)](https://app.codecov.io/gh/PredictiveEcology/reproducible?branch=master)
<!-- badges: end -->

A set of tools for R that enhance reproducibility for data analytics and forecasting.
This package aims at making high-level, robust, machine and OS independent tools for making deeply reproducible and reusable content in R.
The suggested package `geodata` can be installed from the repository 
(<https://PredictiveEcology.r-universe.dev>).

## News

See updates from latest [CRAN](https://cran.r-project.org/package=reproducible) and [development](https://github.com/PredictiveEcology/reproducible/blob/development/NEWS.md) versions.
Note that versions 2.0.0 and later are not compatible with previous versions.
The current version can be much faster and creates smaller repository files (each with specific options set using `Suggests` packages) and allows for different (e.g., `RPostgres` backends for the database[^1] -- not the saved files, however; these are still saved locally).

[^1]: <https://github.com/PredictiveEcology/SpaDES/wiki/Using-alternate-database-backends-for-Cache>

# Reproducible workflows

A reproducible workflow is a series of code steps (e.g., in a script) that, when run, produce the same output from the same inputs every time.
The big challenge with such a workflow is that many steps are so time consuming that a scientist tends to _not_ re-run each step every time. After many months of work, it is often unclear if the code will actually function from the start.
Is the original dataset still there? Have the packages that were used been updated? Are some of the steps missing because there was some "point and clicking"? 

The best way to maintain reproducibility is to have all the code re-run _all the time_.
That way, errors are detected early and can be fixed.
The challenge is how to make all the steps fast enough that it becomes convenient to re-run everything from scratch each time.

## `Cache`

Caching is the principle tool to achieve this reproducible work-flow.
There are many existing tools that support some notion of _caching_.
The main tool here, `Cache`, can be nested hierarchically, becoming very powerful for the data science developer who is regularly working at many levels of an analysis.

```
rnorm(1) # give a random number
Cache(rnorm, 1) # generates a random number
Cache(rnorm, 1) # recovers the previous random number because call is identical
```

## `prepInputs`

A common data problem is starting from a raw (spatial) dataset and getting it into shape for an analysis.
Often, copies of a dataset are haphazardly placed in *ad hoc* local file systems.
This makes it particularly difficult to share the workflow. The solution to this is use a canonical location (e.g., cloud storage, permalink to original data provider, etc.) and use tools that are smart enough to download only once.

Get a geospatial dataset. It will be checksummed (locally), meaning if the file is already in place locally, it will not download it again. 

```
# Using dlFun -- a custom download function -- passed to preProcess
test1 <- prepInputs(targetFile = "GADM_2.8_LUX_adm0.rds", # must specify currently
                    dlFun = "raster::getData", name = "GADM", country = "LUX", level = 0,
                    path = dPath)
```

## `Cache` with `prepInputs`

Putting these tools together allows for very rich data flows.
For example, with `prepInputs` and using the `fun` argument or passing a `studyArea`, a raw dataset can be downloaded, loaded into R, and post processed -- all potentially very time consuming steps resulting in a clean, often much smaller dataset.
Wrapping all these with a `Cache` can make it very quick.

```
test1 <- Cache(prepInputs, targetFile = "GADM_2.8_LUX_adm0.rds", # must specify currently
                    dlFun = "raster::getData", name = "GADM", country = "LUX", level = 0,
                    path = dPath)
```                    

See vignettes and help files for many more real-world examples.

## Installation

### Current release (on CRAN)

[![R build status](https://github.com/PredictiveEcology/reproducible/workflows/R-CMD-check/badge.svg?branch=master)](https://github.com/PredictiveEcology/reproducible/actions)
[![Codecov test coverage](https://codecov.io/gh/PredictiveEcology/reproducible/branch/master/graph/badge.svg)](https://app.codecov.io/gh/PredictiveEcology/reproducible?branch=master)

**Install from CRAN:**

```r
install.packages("reproducible")
```

**Install from GitHub:**
    
```r
#install.packages("devtools")
library("devtools")
install_github("PredictiveEcology/reproducible", dependencies = TRUE) 
```

### Development version

[![R build status](https://github.com/PredictiveEcology/reproducible/workflows/R-CMD-check/badge.svg?branch=development)](https://github.com/PredictiveEcology/reproducible/actions)
[![Codecov test coverage](https://codecov.io/gh/PredictiveEcology/reproducible/branch/development/graph/badge.svg)](https://app.codecov.io/gh/PredictiveEcology/reproducible?branch=development)

**Install from GitHub:**

```r
#install.packages("devtools")
library("devtools")
install_github("PredictiveEcology/reproducible", ref = "development", dependencies = TRUE) 
```

## Contributions

Please see `CONTRIBUTING.md` for information on how to contribute to this project.
