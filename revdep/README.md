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
|date     |2018-06-28                   |

## Packages

|package      |*  |version    |date       |source                                          |
|:------------|:--|:----------|:----------|:-----------------------------------------------|
|memoise      |   |1.1.0      |2018-06-28 |Github (hadley/memoise@06d16ec)                 |
|quickPlot    |   |0.1.4      |2018-06-28 |Github (PredictiveEcology/quickPlot@cba38b4)    |
|reproducible |   |0.2.0.9003 |2018-06-28 |Github (PredictiveEcology/reproducible@5378668) |

# Check results

3 packages

|package      |version | errors| warnings| notes|
|:------------|:-------|------:|--------:|-----:|
|SpaDES.core  |0.1.1   |      1|        0|     0|
|SpaDES       |2.0.2   |      0|        1|     0|
|SpaDES.tools |0.2.0   |      0|        0|     0|

## SpaDES.core (0.1.1)
Maintainer: Alex M Chubaty <alexander.chubaty@canada.ca>  
Bug reports: https://github.com/PredictiveEcology/SpaDES.core/issues

1 error  | 0 warnings | 0 notes

```
checking whether package ‘SpaDES.core’ can be installed ... ERROR
Installation failed.
See ‘/home/achubaty/Documents/GitHub/SpaDES/reproducible/revdep/checks/SpaDES.core.Rcheck/00install.out’ for details.
```

## SpaDES (2.0.2)
Maintainer: Alex M Chubaty <alex.chubaty@gmail.com>  
Bug reports: https://github.com/PredictiveEcology/SpaDES/issues

0 errors | 1 warning  | 0 notes

```
checking whether package ‘SpaDES’ can be installed ... WARNING
Found the following significant warnings:
  Warning: no DISPLAY variable so Tk is not available
  Warning: replacing previous import ‘SpaDES.tools::checkGDALVersion’ by ‘reproducible::checkGDALVersion’ when loading ‘SpaDES’
  Warning: replacing previous import ‘SpaDES.tools::getGDALVersion’ by ‘reproducible::getGDALVersion’ when loading ‘SpaDES’
  Warning: replacing previous import ‘SpaDES.tools::fastMask’ by ‘reproducible::fastMask’ when loading ‘SpaDES’
See ‘/home/achubaty/Documents/GitHub/SpaDES/reproducible/revdep/checks/SpaDES.Rcheck/00install.out’ for details.
```

## SpaDES.tools (0.2.0)
Maintainer: Alex M Chubaty <alex.chubaty@gmail.com>  
Bug reports: https://github.com/PredictiveEcology/SpaDES.tools/issues

0 errors | 0 warnings | 0 notes

