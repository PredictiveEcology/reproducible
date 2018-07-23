# Setup

## Platform

|setting  |value                        |
|:--------|:----------------------------|
|version  |R version 3.4.4 (2018-03-15) |
|system   |x86_64, linux-gnu            |
|ui       |X11                          |
|language |en_CA:en                     |
|collate  |en_CA.UTF-8                  |
|tz       |America/Edmonton             |
|date     |2018-07-23                   |

## Packages

|package      |*  |version |date       |source                                          |
|:------------|:--|:-------|:----------|:-----------------------------------------------|
|reproducible |   |0.2.2   |2018-07-23 |Github (PredictiveEcology/reproducible@597dc9d) |

# Check results

2 packages with problems

|package     |version | errors| warnings| notes|
|:-----------|:-------|------:|--------:|-----:|
|SpaDES.core |0.2.0   |      1|        1|     1|
|SpaDES      |2.0.2   |      0|        2|     0|

## SpaDES.core (0.2.0)
Maintainer: Alex M Chubaty <alex.chubaty@gmail.com>  
Bug reports: https://github.com/PredictiveEcology/SpaDES.core/issues

1 error  | 1 warning  | 1 note 

```
checking tests ... ERROR
  Running ‘test-all.R’ [154s/154s]
Running the tests in ‘tests/test-all.R’ failed.
Last 13 lines of output:
  all.equal(simA, simB) isn't true.
  
    Using cached copy of .inputObjects event in child6 module. Adding to memoised copy.
  ══ testthat results  ═══════════════════════════════════════════════════════════
  OK: 358 SKIPPED: 35 FAILED: 5
  1. Failure: test cache (@test-cache.R#42) 
  2. Error: test event-level cache (@test-cache.R#98) 
  3. Error: test module-level cache (@test-cache.R#162) 
  4. Failure: test .prepareOutput (@test-cache.R#233) 
  5. Failure: test checkpointing with disk-backed raster (@test-checkpoint.R#106) 
  
  Error: testthat unit tests failed
  In addition: Warning message:
  no DISPLAY variable so Tk is not available 
  Execution halted

checking whether package ‘SpaDES.core’ can be installed ... WARNING
Found the following significant warnings:
  Warning: no DISPLAY variable so Tk is not available
See ‘/home/achubaty/Documents/GitHub/SpaDES/reproducible/revdep/checks/SpaDES.core.Rcheck/00install.out’ for details.

checking installed package size ... NOTE
  installed size is 12.4Mb
  sub-directories of 1Mb or more:
    R    10.1Mb
    doc   1.4Mb
```

## SpaDES (2.0.2)
Maintainer: Alex M Chubaty <alex.chubaty@gmail.com>  
Bug reports: https://github.com/PredictiveEcology/SpaDES/issues

0 errors | 2 warnings | 0 notes

```
checking whether package ‘SpaDES’ can be installed ... WARNING
Found the following significant warnings:
  Warning: no DISPLAY variable so Tk is not available
See ‘/home/achubaty/Documents/GitHub/SpaDES/reproducible/revdep/checks/SpaDES.Rcheck/00install.out’ for details.

checking re-building of vignette outputs ... WARNING
Error in re-building vignettes:
  ...
loading reproducible     0.2.2
loading quickPlot        0.1.4
loading SpaDES.core      0.2.1
loading SpaDES.tools     0.3.0
loading SpaDES.addins    0.1.1

Default paths for SpaDES directories set to:
... 8 lines ...
/tmp/RtmphJLqUh/R-lib/SpaDES.core/sampleModules/randomLandscapes/randomLandscapes.R
randomLandscapes: defineParameter: '.useCache' is not of specified type 'logical'.
randomLandscapes: module code appears clean
/tmp/RtmphJLqUh/R-lib/SpaDES.core/sampleModules/fireSpread/fireSpread.R
fireSpread: module code: landscape, testStats are declared in inputObjects, but no default(s) are provided in .inputObjects
fireSpread: inputObjects: DEM, Fires are used from sim inside doEvent.fireSpread, but are not declared in inputObjects
###### Module Code Checking ########
Quitting from lines 133-141 (iii-cache.Rmd) 
Error: processing vignette 'iii-cache.Rmd' failed with diagnostics:
use of NULL environment is defunct
Execution halted
```

