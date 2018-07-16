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
|date     |2018-07-13                   |

## Packages

|package      |*  |version    |date       |source                                          |
|:------------|:--|:----------|:----------|:-----------------------------------------------|
|reproducible |   |0.2.1.9000 |2018-07-14 |Github (PredictiveEcology/reproducible@66659a1) |

# Check results

2 packages with problems

|package     |version | errors| warnings| notes|
|:-----------|:-------|------:|--------:|-----:|
|SpaDES.core |0.2.0   |      0|        1|     1|
|SpaDES      |2.0.2   |      0|        1|     0|

## SpaDES.core (0.2.0)
Maintainer: Alex M Chubaty <alex.chubaty@gmail.com>  
Bug reports: https://github.com/PredictiveEcology/SpaDES.core/issues

0 errors | 1 warning  | 1 note 

```
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

