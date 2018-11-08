# Setup

## Platform

|setting  |value                        |
|:--------|:----------------------------|
|version  |R version 3.5.1 (2018-07-02) |
|system   |x86_64, darwin15.6.0         |
|ui       |RStudio (1.1.419)            |
|language |(EN)                         |
|collate  |en_CA.UTF-8                  |
|tz       |America/Edmonton             |
|date     |2018-11-06                   |

## Packages

|package      |*  |version    |date       |source                                          |
|:------------|:--|:----------|:----------|:-----------------------------------------------|
|covr         |   |3.2.1      |2018-10-18 |cran (@3.2.1)                                   |
|digest       |   |0.6.18     |2018-10-10 |cran (@0.6.18)                                  |
|dplyr        |   |0.7.7      |2018-10-16 |cran (@0.7.7)                                   |
|future       |   |1.10.0     |2018-10-17 |cran (@1.10.0)                                  |
|googledrive  |   |0.1.2      |2018-10-06 |cran (@0.1.2)                                   |
|quickPlot    |   |0.1.5.9001 |2018-11-06 |Github (PredictiveEcology/quickPlot@1c701e1)    |
|raster       |   |2.8-4      |2018-11-03 |cran (@2.8-4)                                   |
|remotes      |   |2.0.2.9000 |2018-11-06 |Github (r-lib/remotes@05091df)                  |
|reproducible |   |0.2.5      |2018-11-06 |Github (PredictiveEcology/reproducible@b1adf13) |
|testthat     |   |2.0.1      |2018-10-13 |cran (@2.0.1)                                   |

# Check results

3 packages

|package      |version | errors| warnings| notes|
|:------------|:-------|------:|--------:|-----:|
|SpaDES.core  |0.2.2   |      1|        1|     0|
|SpaDES       |2.0.2   |      0|        0|     0|
|SpaDES.tools |0.3.0   |      0|        0|     0|

## SpaDES.core (0.2.2)
Maintainer: Alex M Chubaty <alex.chubaty@gmail.com>  
Bug reports: https://github.com/PredictiveEcology/SpaDES.core/issues

1 error  | 1 warning  | 0 notes

```
checking tests ... ERROR
  Running ‘test-all.R’ [99s/101s]
Running the tests in ‘tests/test-all.R’ failed.
Last 13 lines of output:
  
  [34m  Using cached copy of .inputObjects event in test module. Adding to memoised copy.
  [39m[34m  Using cached copy of .inputObjects event in test module. Adding to memoised copy.
  [39m[34m  Using memoised copy of .inputObjects event in test module
  [39m[34m  Using cached copy of .inputObjects event in test module. Adding to memoised copy.
  [39m[31m──[39m [31m1. Failure: test objSize (@test-cache.R#333) [39m [31m───────[39m
  length(os) == 4 isn't true.
  
  [34m  Using cached copy of .inputObjects event in child6 module. Adding to memoised copy.
  [39m══ testthat results  ═══════════════════════════════════
  OK: 310 SKIPPED: 35 FAILED: 1
  1. Failure: test objSize (@test-cache.R#333) 
  
  Error: testthat unit tests failed
  Execution halted

checking Rd cross-references ... WARNING
Missing link or links in documentation object 'moduleCoverage.Rd':
  ‘[covr]{shine}’

See section 'Cross-references' in the 'Writing R Extensions' manual.

```

## SpaDES (2.0.2)
Maintainer: Alex M Chubaty <alex.chubaty@gmail.com>  
Bug reports: https://github.com/PredictiveEcology/SpaDES/issues

0 errors | 0 warnings | 0 notes

## SpaDES.tools (0.3.0)
Maintainer: Alex M Chubaty <alex.chubaty@gmail.com>  
Bug reports: https://github.com/PredictiveEcology/SpaDES.tools/issues

0 errors | 0 warnings | 0 notes

