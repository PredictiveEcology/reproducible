# Setup

## Platform

|setting  |value                        |
|:--------|:----------------------------|
|version  |R version 3.4.4 (2018-03-15) |
|system   |x86_64, linux-gnu            |
|ui       |RStudio (1.1.453)            |
|language |en_CA:en                     |
|collate  |en_CA.UTF-8                  |
|tz       |America/Edmonton             |
|date     |2018-06-12                   |

## Packages

|package      |*  |version    |date       |source                                          |
|:------------|:--|:----------|:----------|:-----------------------------------------------|
|memoise      |   |1.1.0      |2018-06-12 |Github (hadley/memoise@06d16ec)                 |
|quickPlot    |   |0.1.3.9002 |2018-06-12 |Github (PredictiveEcology/quickPlot@d26bb6e)    |
|reproducible |   |0.2.0      |2018-06-12 |Github (PredictiveEcology/reproducible@fc661a6) |

# Check results

3 packages

|package      |version | errors| warnings| notes|
|:------------|:-------|------:|--------:|-----:|
|SpaDES.core  |0.1.1   |      1|        0|     0|
|SpaDES       |2.0.1   |      1|        0|     0|
|SpaDES.tools |0.1.1   |      1|        0|     0|

## SpaDES.core (0.1.1)
Maintainer: Alex M Chubaty <alexander.chubaty@canada.ca>  
Bug reports: https://github.com/PredictiveEcology/SpaDES.core/issues

1 error  | 0 warnings | 0 notes

```
checking whether package ‘SpaDES.core’ can be installed ... ERROR
Installation failed.
See ‘/home/achubaty/Documents/GitHub/SpaDES/reproducible/revdep/checks/SpaDES.core.Rcheck/00install.out’ for details.
```

## SpaDES (2.0.1)
Maintainer: Alex M Chubaty <alexander.chubaty@canada.ca>  
Bug reports: https://github.com/PredictiveEcology/SpaDES/issues

1 error  | 0 warnings | 0 notes

```
checking whether package ‘SpaDES’ can be installed ... ERROR
Installation failed.
See ‘/home/achubaty/Documents/GitHub/SpaDES/reproducible/revdep/checks/SpaDES.Rcheck/00install.out’ for details.
```

## SpaDES.tools (0.1.1)
Maintainer: Alex M Chubaty <alexander.chubaty@canada.ca>  
Bug reports: https://github.com/PredictiveEcology/SpaDES.tools/issues

1 error  | 0 warnings | 0 notes

```
checking tests ... ERROR
  Running ‘test-all.R’ [73s/72s]
Running the tests in ‘tests/test-all.R’ failed.
Last 13 lines of output:
  ══ testthat results  ══════════════════════════════════════════════════════
  OK: 1595 SKIPPED: 23 FAILED: 40
  1. Failure: spread2 tests -- asymmetry (@test-spread2.R#630) 
  2. Failure: spread2 tests -- asymmetry (@test-spread2.R#630) 
  3. Failure: spread2 tests -- asymmetry (@test-spread2.R#630) 
  4. Failure: spread2 tests -- asymmetry (@test-spread2.R#630) 
  5. Failure: spread2 tests -- asymmetry (@test-spread2.R#630) 
  6. Failure: spread2 tests -- asymmetry (@test-spread2.R#630) 
  7. Failure: spread2 tests -- asymmetry (@test-spread2.R#630) 
  8. Failure: spread2 tests -- asymmetry (@test-spread2.R#630) 
  9. Failure: spread2 tests -- asymmetry (@test-spread2.R#630) 
  1. ...
  
  Error: testthat unit tests failed
  Execution halted
```

