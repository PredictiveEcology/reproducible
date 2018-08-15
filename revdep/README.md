# Setup

## Platform

|setting  |value                        |
|:--------|:----------------------------|
|version  |R version 3.4.4 (2018-03-15) |
|system   |x86_64, linux-gnu            |
|ui       |X11                          |
|language |(EN)                         |
|collate  |en_CA.UTF-8                  |
|tz       |America/Edmonton             |
|date     |2018-08-07                   |

## Packages

|package      |*  |version |date       |source                                          |
|:------------|:--|:-------|:----------|:-----------------------------------------------|
|reproducible |   |0.2.3   |2018-08-07 |Github (PredictiveEcology/reproducible@8e0b0c1) |
|rgdal        |   |1.3-4   |2018-08-03 |cran (@1.3-4)                                   |

# Check results

3 packages

|package      |version | errors| warnings| notes|
|:------------|:-------|------:|--------:|-----:|
|SpaDES.core  |0.2.0   |      1|        1|     1|
|SpaDES       |2.0.2   |      0|        1|     0|
|SpaDES.tools |0.3.0   |      0|        0|     0|

## SpaDES.core (0.2.0)
Maintainer: Alex M Chubaty <alex.chubaty@gmail.com>  
Bug reports: https://github.com/PredictiveEcology/SpaDES.core/issues

1 error  | 1 warning  | 1 note 

```
checking tests ... ERROR
  Running ‘test-all.R’ [111s/107s]
Running the tests in ‘tests/test-all.R’ failed.
Last 13 lines of output:
    Using cached copy of .inputObjects event in test module. Adding to memoised copy.
  ── 3. Failure: test checkpointing with disk-backed raster (@test-checkpoint.R#10
  all.equal(simA, simB) isn't true.
  
    Using cached copy of .inputObjects event in child6 module. Adding to memoised copy.
  ══ testthat results  ═══════════════════════════════════════════════════════════
  OK: 361 SKIPPED: 35 FAILED: 3
  1. Failure: test cache (@test-cache.R#42) 
  2. Failure: test .prepareOutput (@test-cache.R#233) 
  3. Failure: test checkpointing with disk-backed raster (@test-checkpoint.R#106) 
  
  Error: testthat unit tests failed
  In addition: Warning message:
  no DISPLAY variable so Tk is not available 
  Execution halted

checking whether package ‘SpaDES.core’ can be installed ... WARNING
Found the following significant warnings:
  Warning: no DISPLAY variable so Tk is not available
See ‘/home/achubaty/Documents/GitHub/PredictiveEcology/reproducible/revdep/checks/SpaDES.core.Rcheck/00install.out’ for details.

checking installed package size ... NOTE
  installed size is 12.4Mb
  sub-directories of 1Mb or more:
    R    10.1Mb
    doc   1.4Mb
```

## SpaDES (2.0.2)
Maintainer: Alex M Chubaty <alex.chubaty@gmail.com>  
Bug reports: https://github.com/PredictiveEcology/SpaDES/issues

0 errors | 1 warning  | 0 notes

```
checking whether package ‘SpaDES’ can be installed ... WARNING
Found the following significant warnings:
  Warning: no DISPLAY variable so Tk is not available
See ‘/home/achubaty/Documents/GitHub/PredictiveEcology/reproducible/revdep/checks/SpaDES.Rcheck/00install.out’ for details.
```

## SpaDES.tools (0.3.0)
Maintainer: Alex M Chubaty <alex.chubaty@gmail.com>  
Bug reports: https://github.com/PredictiveEcology/SpaDES.tools/issues

0 errors | 0 warnings | 0 notes

