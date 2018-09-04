# reproducible

[![Build Status](https://travis-ci.org/PredictiveEcology/reproducible.svg?branch=master)](https://travis-ci.org/PredictiveEcology/reproducible)
[![Appveyor Build status](https://ci.appveyor.com/api/projects/status/2fxqhgk6miv2fytd/branch/master?svg=true)](https://ci.appveyor.com/project/achubaty/reproducible/branch/master)
[![Coverage Status](https://coveralls.io/repos/github/PredictiveEcology/reproducible/badge.svg?branch=master)](https://coveralls.io/github/PredictiveEcology/reproducible?branch=master)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/reproducible)](https://cran.r-project.org/package=reproducible)
[![Downloads](http://cranlogs.r-pkg.org/badges/grand-total/reproducible)](https://cran.r-project.org/package=reproducible)
[![Downloads](http://cranlogs.r-pkg.org/badges/last-month/reproducible)](https://cran.r-project.org/package=reproducible)

A set of tools for R that enhance reproducibility beyond package management.
Built on top of `git2r` and `archivist`, this package aims at making high-level, robust, machine and OS independent tools for making deeply reproducible and reusable content in R.
This extends beyond the package management utilites of `packrat` and `checkpoint` by including tools for caching and accessing GitHub repositories.

## Installation

### Current stable release

**Install from CRAN:**

```r
install.packages("reproducible")
```

**Install from GitHub:**
    
```r
#install.packages("devtools")
library("devtools")
install_github("PredictiveEcology/reproducible", dependencies = TRUE) # stable
```

### Development version (unstable)

[![Build Status](https://travis-ci.org/PredictiveEcology/reproducible.svg?branch=development)](https://travis-ci.org/PredictiveEcology/reproducible)
[![Coverage Status](https://coveralls.io/repos/github/PredictiveEcology/reproducible/badge.svg?branch=development)](https://coveralls.io/github/PredictiveEcology/reproducible?branch=development)

**Install from GitHub:**

```r
#install.packages("devtools")
library("devtools")
install_github("PredictiveEcology/reproducible", ref = "development", dependencies = TRUE) # unstable
```

