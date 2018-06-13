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
|date     |2018-06-13                   |

## Packages

|package      |*  |version    |date       |source                                          |
|:------------|:--|:----------|:----------|:-----------------------------------------------|
|memoise      |   |1.1.0      |2018-06-13 |Github (hadley/memoise@06d16ec)                 |
|quickPlot    |   |0.1.3.9002 |2018-06-13 |Github (PredictiveEcology/quickPlot@d26bb6e)    |
|reproducible |   |0.2.0      |2018-06-13 |Github (PredictiveEcology/reproducible@fc661a6) |

# Check results

3 packages

|package      |version | errors| warnings| notes|
|:------------|:-------|------:|--------:|-----:|
|SpaDES.core  |0.1.1   |      1|        0|     0|
|SpaDES       |2.0.1   |      0|        4|     4|
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

0 errors | 4 warnings | 4 notes

```
checking whether package ‘SpaDES’ can be installed ... WARNING
Found the following significant warnings:
  Warning: replacing previous import ‘SpaDES.tools::writeOutputs’ by ‘reproducible::writeOutputs’ when loading ‘SpaDES’
  Warning: replacing previous import ‘SpaDES.tools::prepInputs’ by ‘reproducible::prepInputs’ when loading ‘SpaDES’
  Warning: replacing previous import ‘SpaDES.tools::checkGDALVersion’ by ‘reproducible::checkGDALVersion’ when loading ‘SpaDES’
  Warning: replacing previous import ‘SpaDES.tools::cropInputs’ by ‘reproducible::cropInputs’ when loading ‘SpaDES’
  Warning: replacing previous import ‘SpaDES.tools::postProcess’ by ‘reproducible::postProcess’ when loading ‘SpaDES’
  Warning: replacing previous import ‘SpaDES.tools::maskInputs’ by ‘reproducible::maskInputs’ when loading ‘SpaDES’
  Warning: replacing previous import ‘SpaDES.tools::downloadFile’ by ‘reproducible::downloadFile’ when loading ‘SpaDES’
  Warning: replacing previous import ‘SpaDES.tools::.suffix’ by ‘reproducible::.suffix’ when loading ‘SpaDES’
  Warning: replacing previous import ‘SpaDES.tools::getGDALVersion’ by ‘reproducible::getGDALVersion’ when loading ‘SpaDES’
  Warning: replacing previous import ‘SpaDES.tools::fastMask’ by ‘reproducible::fastMask’ when loading ‘SpaDES’
  Warning: replacing previous import ‘SpaDES.tools::projectInputs’ by ‘reproducible::projectInputs’ when loading ‘SpaDES’
  Warning: replacing previous import ‘SpaDES.tools::.prefix’ by ‘reproducible::.prefix’ when loading ‘SpaDES’
  Warning: replacing previous import ‘SpaDES.tools::fixErrors’ by ‘reproducible::fixErrors’ when loading ‘SpaDES’
See ‘/home/achubaty/Documents/GitHub/SpaDES/reproducible/revdep/checks/SpaDES.Rcheck/00install.out’ for details.

checking S3 generic/method consistency ... WARNING
Invalid MIT-MAGIC-COOKIE-1 keyInvalid MIT-MAGIC-COOKIE-1 key
See section ‘Generic functions and methods’ in the ‘Writing R
Extensions’ manual.

checking replacement functions ... WARNING
Invalid MIT-MAGIC-COOKIE-1 keyInvalid MIT-MAGIC-COOKIE-1 key
The argument of a replacement function which corresponds to the right
hand side must be named ‘value’.

checking for missing documentation entries ... WARNING
Invalid MIT-MAGIC-COOKIE-1 keyInvalid MIT-MAGIC-COOKIE-1 key
All user-level objects in a package should have documentation entries.
See chapter ‘Writing R documentation files’ in the ‘Writing R
Extensions’ manual.

checking dependencies in R code ... NOTE
Invalid MIT-MAGIC-COOKIE-1 keyInvalid MIT-MAGIC-COOKIE-1 key

checking foreign function calls ... NOTE
Invalid MIT-MAGIC-COOKIE-1 keyInvalid MIT-MAGIC-COOKIE-1 key
See chapter ‘System and foreign language interfaces’ in the ‘Writing R
Extensions’ manual.

checking R code for possible problems ... NOTE
Invalid MIT-MAGIC-COOKIE-1 keyInvalid MIT-MAGIC-COOKIE-1 key
Invalid MIT-MAGIC-COOKIE-1 keyInvalid MIT-MAGIC-COOKIE-1 key

checking Rd \usage sections ... NOTE
Invalid MIT-MAGIC-COOKIE-1 keyInvalid MIT-MAGIC-COOKIE-1 key
The \usage entries for S3 methods should use the \method markup and not
their full name.
See chapter ‘Writing R documentation files’ in the ‘Writing R
Extensions’ manual.
```

## SpaDES.tools (0.1.1)
Maintainer: Alex M Chubaty <alexander.chubaty@canada.ca>  
Bug reports: https://github.com/PredictiveEcology/SpaDES.tools/issues

1 error  | 0 warnings | 0 notes

```
checking tests ... ERROR
  Running ‘test-all.R’ [75s/75s]
Running the tests in ‘tests/test-all.R’ failed.
Last 13 lines of output:
  ══ testthat results  ═══════════════════════════════════════════════════════════
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

