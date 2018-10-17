# Setup

## Platform

|setting  |value                        |
|:--------|:----------------------------|
|version  |R version 3.5.1 (2018-07-02) |
|system   |x86_64, linux-gnu            |
|ui       |RStudio (1.1.456)            |
|language |(EN)                         |
|collate  |en_CA.UTF-8                  |
|tz       |America/Edmonton             |
|date     |2018-10-10                   |

## Packages

|package      |*  |version |date       |source                                          |
|:------------|:--|:-------|:----------|:-----------------------------------------------|
|reproducible |*  |0.2.4   |2018-10-10 |Github (PredictiveEcology/reproducible@5248022) |

# Check results

3 packages

|package      |version | errors| warnings| notes|
|:------------|:-------|------:|--------:|-----:|
|SpaDES.core  |0.2.2   |      1|        2|     0|
|SpaDES       |2.0.2   |      0|        1|     0|
|SpaDES.tools |0.3.0   |      0|        1|     0|

## SpaDES.core (0.2.2)
Maintainer: Alex M Chubaty <alex.chubaty@gmail.com>  
Bug reports: https://github.com/PredictiveEcology/SpaDES.core/issues

1 error  | 2 warnings | 0 notes

```
checking tests ... ERROR
  Running ‘test-all.R’ [68s/63s]
Running the tests in ‘tests/test-all.R’ failed.
Last 13 lines of output:
  [39m══ testthat results  ══════════════════════════════════════════════════════════════
  OK: 217 SKIPPED: 35 FAILED: 13
  1. Error: test cache (@test-cache.R#29) 
  2. Error: test event-level cache (@test-cache.R#80) 
  3. Error: test module-level cache (@test-cache.R#128) 
  4. Failure: test objSize (@test-cache.R#333) 
  5. Error: test checkpointing (@test-checkpoint.R#24) 
  6. Error: test checkpointing with disk-backed raster (@test-checkpoint.R#87) 
  7. Error: experiment does not work correctly (@test-experiment.R#33) 
  8. Error: test-load.R: loading inputs does not work correctly (@test-load.R#36) 
  9. Error: test-load.R: passing arguments to filelist in simInit does not work correctly (@test-load.R#164) 
  1. ...
  
  Error: testthat unit tests failed
  Execution halted

checking Rd cross-references ... WARNING
Missing link or links in documentation object 'moduleCoverage.Rd':
  ‘[covr]{shine}’

See section 'Cross-references' in the 'Writing R Extensions' manual.


checking re-building of vignette outputs ... WARNING
Error in re-building vignettes:
  ...
    union

Loading required package: quickPlot
Loading required package: reproducible

Attaching package: 'reproducible'

... 8 lines ...
The following objects are masked from 'package:stats':

    end, start

Loading required package: SpaDES.tools
Loading required namespace: RandomFields
Failed with error:  'there is no package called 'RandomFields''
Quitting from lines 334-363 (ii-modules.Rmd) 
Error: processing vignette 'ii-modules.Rmd' failed with diagnostics:
The 'RandomFields' package is required but not installed.
Execution halted
```

## SpaDES (2.0.2)
Maintainer: Alex M Chubaty <alex.chubaty@gmail.com>  
Bug reports: https://github.com/PredictiveEcology/SpaDES/issues

0 errors | 1 warning  | 0 notes

```
checking re-building of vignette outputs ... WARNING
Error in re-building vignettes:
  ...
loading reproducible     0.2.4
loading quickPlot        0.1.5.9000
loading SpaDES.core      0.2.3
loading SpaDES.tools     0.3.0.9001
loading SpaDES.addins    0.1.1

Default paths for SpaDES directories set to:
... 8 lines ...
randomLandscapes: defineParameter: '.useCache' is not of specified type 'logical'.
randomLandscapes: inputObjects: stackName is used from sim inside doEvent.randomLandscapes, but is not declared in inputObjects
/tmp/Rtmp0luEQj/R-lib/SpaDES.core/sampleModules/fireSpread/fireSpread.R
fireSpread: module code: landscape, testStats are declared in inputObjects, but no default(s) are provided in .inputObjects
fireSpread: inputObjects: stackName, DEM, Fires are used from sim inside doEvent.fireSpread, but are not declared in inputObjects
###### Module Code Checking ########
Failed with error:  'there is no package called 'RandomFields''
Quitting from lines 66-68 (iii-cache.Rmd) 
Error: processing vignette 'iii-cache.Rmd' failed with diagnostics:
The 'RandomFields' package is required but not installed.
Execution halted
```

## SpaDES.tools (0.3.0)
Maintainer: Alex M Chubaty <alex.chubaty@gmail.com>  
Bug reports: https://github.com/PredictiveEcology/SpaDES.tools/issues

0 errors | 1 warning  | 0 notes

```
checking compilation flags used ... WARNING
Compilation used the following non-portable flag(s):
  ‘-Wdate-time’ ‘-Werror=format-security’ ‘-Wformat’
```

