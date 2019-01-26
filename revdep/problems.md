# SpaDES

Version: 2.0.2

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    loading SpaDES.tools     0.3.0
    loading SpaDES.addins    0.1.1
    
    Default paths for SpaDES directories set to:
      cachePath:  /tmp/RtmpNM5B36/SpaDES/cache
      inputPath:  /tmp/RtmpNM5B36/SpaDES/inputs
      modulePath: /tmp/RtmpNM5B36/SpaDES/modules
      outputPath: /tmp/RtmpNM5B36/SpaDES/outputs
    These can be changed using 'setPaths()'. See '?setPaths'.
    ###### Module Code Checking - Still experimental - please report problems ######## 
    /home/achubaty/Documents/GitHub/PredictiveEcology/reproducible/revdep/library/SpaDES/SpaDES.core/sampleModules/randomLandscapes/randomLandscapes.R
    randomLandscapes: defineParameter: '.useCache' is not of specified type 'logical'.
    randomLandscapes: inputObjects: stackName is used from sim inside doEvent.randomLandscapes, but is not declared in inputObjects
    /home/achubaty/Documents/GitHub/PredictiveEcology/reproducible/revdep/library/SpaDES/SpaDES.core/sampleModules/fireSpread/fireSpread.R
    fireSpread: module code: landscape, testStats are declared in inputObjects, but no default(s) are provided in .inputObjects
    fireSpread: inputObjects: stackName, DEM, Fires are used from sim inside doEvent.fireSpread, but are not declared in inputObjects
    ###### Module Code Checking ########
    Quitting from lines 66-68 (iii-cache.Rmd) 
    Error: processing vignette 'iii-cache.Rmd' failed with diagnostics:
    could not find symbol "objects" in environment of the generic function
    Execution halted
    ```

# SpaDES.core

Version: 0.2.3

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/test-all.R’ failed.
    Last 13 lines of output:
      OK: 333 SKIPPED: 34 FAILED: 9
      1. Error: test cache (@test-cache.R#29) 
      2. Error: test event-level cache (@test-cache.R#80) 
      3. Error: test module-level cache (@test-cache.R#128) 
      4. Error: test .prepareOutput (@test-cache.R#187) 
      5. Error: test .robustDigest for simLists (@test-cache.R#222) 
      6. Error: Cache of sim objects via .Cache attr -- using preDigest and postDigest (@test-cache.R#385) 
      7. Error: spades calls with different signatures don't work (@test-simulation.R#124) 
      8. Error: timeunits with child and parent modules work correctly (@test-timeunits.R#238) 
      9. Error: test userSuppliedObj (@test-userSuppliedObjs.R#35) 
      
      Error: testthat unit tests failed
      In addition: Warning message:
      In fun(libname, pkgname) : couldn't connect to display ":99"
      Execution halted
    ```

*   checking for code/documentation mismatches ... WARNING
    ```
    Codoc mismatches from documentation object '.robustDigest,simList-method':
    \S4method{.robustDigest}{simList}
      Code: function(object, objects, length, algo, quick, classOptions)
      Docs: function(object, objects, length =
                     getOption("reproducible.length", Inf), algo =
                     "xxhash64", quick = getOption("reproducible.quick",
                     FALSE), classOptions = list())
      Mismatches in argument default values (first 3):
        Name: 'length' Code:  Docs: getOption("reproducible.length", Inf)
        Name: 'algo' Code:  Docs: "xxhash64"
        Name: 'quick' Code:  Docs: getOption("reproducible.quick", FALSE)
    ```

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    The following object is masked from 'package:RandomFieldsUtils':
    
        RFoptions
    
    The following objects are masked from 'package:base':
    
        abs, acosh, asin, asinh, atan, atan2, atanh, cos, cosh, exp,
        expm1, floor, gamma, lgamma, log, log1p, log2, logb, max, min,
        round, sin, sinh, sqrt, tan, tanh, trunc
    
    
    Attaching package: 'data.table'
    
    The following object is masked from 'package:raster':
    
        shift
    
    Quitting from lines 637-677 (ii-modules.Rmd) 
    Error: processing vignette 'ii-modules.Rmd' failed with diagnostics:
    Column 6 of by= (5) is type 'list', not yet supported
    Execution halted
    ```

