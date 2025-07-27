# CBMutils

<details>

* Version: 2.0.3.0010
* GitHub: https://github.com/PredictiveEcology/CBMutils
* Source code: https://github.com/cran/CBMutils
* Number of recursive dependencies: 205

Run `revdepcheck::revdep_details(, "CBMutils")` for more info

</details>

## In both

*   checking R code for possible problems ... NOTE
    ```
    NPPplot: no visible binding for global variable ‘totalNPP’
    calcRootC: no visible binding for global variable
      ‘..aboveGroundColumns’
    Undefined global functions or variables:
      ..aboveGroundColumns totalNPP
    ```

# fireSenseUtils

<details>

* Version: 0.0.5.9093
* GitHub: https://github.com/PredictiveEcology/fireSenseUtils
* Source code: https://github.com/cran/fireSenseUtils
* Number of recursive dependencies: 200

Run `revdepcheck::revdep_details(, "fireSenseUtils")` for more info

</details>

## In both

*   checking examples ... ERROR
    ```
    Running examples in ‘fireSenseUtils-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: readLightningData
    > ### Title: Read lightning data
    > ### Aliases: readLightningData
    > 
    > ### ** Examples
    > 
    > library(reproducible)
    > crsToUse <- "+proj=lcc +lat_0=0 +lon_0=-95 +lat_1=49 +lat_2=77 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"
    > ras <- rast(ext(-1115000, -376750, 7267000, 7874000), res = 250, vals = 1,
    +             crs = crsToUse)
    Error in rast(ext(-1115000, -376750, 7267000, 7874000), res = 250, vals = 1,  : 
      could not find function "rast"
    Execution halted
    ```

*   checking running R code from vignettes ...
    ```
      ‘Firesense_LCC_flammability.Rmd’ using ‘UTF-8’... failed
      ‘fireSense-tutorial.Rmd’ using ‘UTF-8’... failed
     ERROR
    Errors in running code in vignettes:
    when running code in ‘Firesense_LCC_flammability.Rmd’
      ...
    ...downloading...
      Downloading
    
    Download of ftp://ftp.ccrs.nrcan.gc.ca/ad/NLCCLandCover/LandcoverCanada2005_250m/LandCoverOfCanada2005_V1_4.zip failed. This may be a permissions issue. Please check the url and permissions are correct.
    ...
    +     ncol = raster_width))
    
    > set.seed(123)
    
    > landTypeOne <- gaussMap(r_template, scale = 300, var = 300)
    
      When sourcing ‘fireSense-tutorial.R’:
    Error: Random landscape generation functionality has been removed because the RandomFields packages is no longer maintained.
     See neutralLandscapeMap() or use the NLMR package for tools to generate various random/neutral landscapes.
    Execution halted
    ```

*   checking dependencies in R code ... WARNING
    ```
    '::' or ':::' import not declared from: ‘clusters’
    Unavailable namespace imported from by a ':::' call: ‘clusters’
      See the note in ?`:::` about the use of this operator.
    ```

*   checking Rd \usage sections ... WARNING
    ```
    Undocumented arguments in Rd file 'abbreviateSpNames.Rd'
      ‘df’
    
    Undocumented arguments in Rd file 'runDEoptim.Rd'
      ‘cachePath’
    
    Functions with \usage entries need to have the appropriate \alias
    entries, and all their arguments documented.
    The \usage entries must correspond to syntactically valid R code.
    See chapter ‘Writing R documentation files’ in the ‘Writing R
    Extensions’ manual.
    ```

*   checking R code for possible problems ... NOTE
    ```
    readLightningData: no visible binding for global variable ‘plotID’
    runDEoptim: no visible binding for '<<-' assignment to ‘cl’
    Undefined global functions or variables:
      plotID
    ```

# LandWebUtils

<details>

* Version: 1.0.3
* GitHub: https://github.com/PredictiveEcology/LandWebUtils
* Source code: https://github.com/cran/LandWebUtils
* Number of recursive dependencies: 155

Run `revdepcheck::revdep_details(, "LandWebUtils")` for more info

</details>

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘map’
    
    Package suggested but not available for checking: ‘SDMTools’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# mpbutils

<details>

* Version: 0.1.3.9002
* GitHub: https://github.com/PredictiveEcology/mpbutils
* Source code: https://github.com/cran/mpbutils
* Number of recursive dependencies: 78

Run `revdepcheck::revdep_details(, "mpbutils")` for more info

</details>

## In both

*   checking package dependencies ... ERROR
    ```
    VignetteBuilder packages not declared: 'knitr', 'rmarkdown'
    
    Vignette dependencies required without any vignettes:
      ‘knitr’ ‘rmarkdown’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# PSPclean

<details>

* Version: 0.1.5.9003
* GitHub: NA
* Source code: https://github.com/cran/PSPclean
* Number of recursive dependencies: 142

Run `revdepcheck::revdep_details(, "PSPclean")` for more info

</details>

## In both

*   checking tests ...
    ```
      Running ‘testthat.R’
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      https://drive.google.com/file/d/1LmOaEtCZ6EBeIlAm6ttfLqBqQnQu4Ca7/view?usp=sharing,
       ... then download it manually, give it this name: '/private/var/folders/l2/hy6b0sl977bcd8695nt6j7s80000gn/T/Rtmpk1yZdd/QC/randomized_LandR_speciesParameters_Inputs.zip', and place file here: /private/var/folders/l2/hy6b0sl977bcd8695nt6j7s80000gn/T/Rtmpk1yZdd/QC.
      -------------------
      If manual download was successful, you will likely also need to run Checksums manually after you download the file with this command: reproducible:::appendChecksumsTable(checkSumFilePath = '/private/var/folders/l2/hy6b0sl977bcd8695nt6j7s80000gn/T/Rtmpk1yZdd/QC/CHECKSUMS.txt', filesToChecksum = 'randomizedPSPmeasure.rds', destinationPath = '/private/var/folders/l2/hy6b0sl977bcd8695nt6j7s80000gn/T/Rtmpk1yZdd/QC', append = TRUE)
      Backtrace:
          ▆
       1. └─PSPclean::getPSP(destinationPath = dPath, PSPdataTypes = "dummy") at test-all.R:254:3
       2.   └─reproducible::prepInputs(...)
       3.     └─reproducible::preProcess(...)
       4.       └─reproducible::downloadFile(...)
       5.         └─reproducible:::dlErrorHandling(...)
      
      [ FAIL 8 | WARN 0 | SKIP 0 | PASS 0 ]
      Error: Test failures
      Execution halted
    ```

# SpaDES

<details>

* Version: 2.0.11.9000
* GitHub: https://github.com/PredictiveEcology/SpaDES
* Source code: https://github.com/cran/SpaDES
* Number of recursive dependencies: 75

Run `revdepcheck::revdep_details(, "SpaDES")` for more info

</details>

## In both

*   checking whether package ‘SpaDES’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘quickPlot’ was built under R version 4.5.1
      Warning: package ‘SpaDES.core’ was built under R version 4.5.1
      Warning: package ‘SpaDES.tools’ was built under R version 4.5.1
    See ‘/Users/achubaty/Documents/GitHub/PredictiveEcology/reproducible/revdep/checks.noindex/SpaDES/new/SpaDES.Rcheck/00install.out’ for details.
    ```

# SpaDES.config

<details>

* Version: 1.0.7
* GitHub: https://github.com/PredictiveEcology/SpaDES.config
* Source code: https://github.com/cran/SpaDES.config
* Number of recursive dependencies: 194

Run `revdepcheck::revdep_details(, "SpaDES.config")` for more info

</details>

## In both

*   checking whether package ‘SpaDES.config’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘SpaDES.project’ was built under R version 4.5.1
    See ‘/Users/achubaty/Documents/GitHub/PredictiveEcology/reproducible/revdep/checks.noindex/SpaDES.config/new/SpaDES.config.Rcheck/00install.out’ for details.
    ```

# SpaDES.core

<details>

* Version: 2.1.5.9003
* GitHub: https://github.com/PredictiveEcology/SpaDES.core
* Source code: https://github.com/cran/SpaDES.core
* Number of recursive dependencies: 157

Run `revdepcheck::revdep_details(, "SpaDES.core")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
      Running ‘test-all.R’
     ERROR
    Running the tests in ‘tests/test-all.R’ failed.
    Last 13 lines of output:
       12.   └─SpaDES.core::simInit(...)
       13.     ├─base::withCallingHandlers(...)
       14.     └─SpaDES.core:::.runModuleInputObjects(sim, m, objects, notOlderThan)
       15.       └─reproducible::Cache(...)
       16.         └─reproducible:::evalTheFunAndAddChanged(...)
       17.           ├─reproducible::.addChangedAttr(...)
       18.           └─SpaDES.core::.addChangedAttr(...)
       19.             ├─base::unlist(...)
       20.             └─base::lapply(...)
      
      [ FAIL 14 | WARN 0 | SKIP 31 | PASS 408 ]
      Error: Test failures
      In addition: Warning message:
      package 'quickPlot' was built under R version 4.5.1 
      Execution halted
    ```

*   checking running R code from vignettes ...
    ```
      ‘i-introduction.Rmd’ using ‘UTF-8’... OK
      ‘ii-modules.Rmd’ using ‘UTF-8’... OK
      ‘iii-cache.Rmd’ using ‘UTF-8’... failed
      ‘iv-advanced.Rmd’ using ‘UTF-8’... OK
      ‘v-automated-testing.Rmd’ using ‘UTF-8’... OK
     ERROR
    Errors in running code in vignettes:
    when running code in ‘iii-cache.Rmd’
      ...
    Jul11 14:09:39 frSprd:stats total elpsd: 1.1 secs | 3 fireSpread stats 5
    Jul11 14:09:39 frSprd:stats fireSpread
    simList saved in
    SpaDES.core:::savedSimEnv()$.sim
    It will be deleted at next spades() call.
    Timing stopped at: 1.146 0.054 1.228
    
      When sourcing ‘iii-cache.R’:
    Error: $ operator is invalid for atomic vectors
    Execution halted
    ```

## In both

*   checking whether package ‘SpaDES.core’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘quickPlot’ was built under R version 4.5.1
    See ‘/Users/achubaty/Documents/GitHub/PredictiveEcology/reproducible/revdep/checks.noindex/SpaDES.core/new/SpaDES.core.Rcheck/00install.out’ for details.
    ```

# SpaDES.experiment

<details>

* Version: 0.0.2.9005
* GitHub: https://github.com/PredictiveEcology/SpaDES.experiment
* Source code: https://github.com/cran/SpaDES.experiment
* Number of recursive dependencies: 126

Run `revdepcheck::revdep_details(, "SpaDES.experiment")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ...
    ```
      ‘v-experiments.Rmd’ using ‘UTF-8’... failed
     ERROR
    Errors in running code in vignettes:
    when running code in ‘v-experiments.Rmd’
      ...
    Jul11 14:09:25 save  :spades total elpsd: 1.9 secs | 2 save spades 10
    simList saved in
    SpaDES.core:::savedSimEnv()$.sim
    It will be deleted at next spades() call.
    shutting down parallel nodes
    Timing stopped at: 1.616 0.032 1.66
    
      When sourcing ‘v-experiments.R’:
    Error: $ operator is invalid for atomic vectors
    Execution halted
    ```

## In both

*   checking tests ...
    ```
      Running ‘testthat.R’
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       20. │                     ├─terra::sources(obj)
       21. │                     └─terra::sources(obj)
       22. │                       └─terra (local) .local(x, ...)
       23. │                         └─x@pntr$filenames()
       24. ├─base::stop(`<Rcpp::xc>`)
       25. └─SpaDES.core (local) `<fn>`(`<Rcpp::xc>`)
      ── Failure ('test-experiment2.R:301:3'): simLists tests 1 ──────────────────────
      identical("hello", setdiff(lsOrig, lsClear)) is not TRUE
      
      `actual`:   FALSE
      `expected`: TRUE 
      
      [ FAIL 5 | WARN 5 | SKIP 3 | PASS 32 ]
      Error: Test failures
      Execution halted
    ```

*   checking whether package ‘SpaDES.experiment’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘SpaDES.core’ was built under R version 4.5.1
      Warning: package ‘quickPlot’ was built under R version 4.5.1
    See ‘/Users/achubaty/Documents/GitHub/PredictiveEcology/reproducible/revdep/checks.noindex/SpaDES.experiment/new/SpaDES.experiment.Rcheck/00install.out’ for details.
    ```

*   checking S3 generic/method consistency ... WARNING
    ```
    as.data.table:
      function(x, keep.rownames, ...)
    as.data.table.simLists:
      function(x, vals, objectsFromSim, objectsFromOutputs, ...)
    See section ‘Generic functions and methods’ in the ‘Writing R
    Extensions’ manual.
    ```

