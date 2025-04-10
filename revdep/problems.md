# SpaDES.core

<details>

* Version: 2.1.5.9003
* GitHub: https://github.com/PredictiveEcology/SpaDES.core
* Source code: https://github.com/cran/SpaDES.core
* Number of recursive dependencies: 160

Run `revdepcheck::revdep_details(, "SpaDES.core")` for more info

</details>

## Newly broken

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
    Apr10 11:46:53 frSprd:stats total elpsd: 1.9 secs | 3 fireSpread stats 5
    Apr10 11:46:53 frSprd:stats fireSpread
    simList saved in
    SpaDES.core:::savedSimEnv()$.sim
    It will be deleted at next spades() call.
    Timing stopped at: 1.995 0.134 2.129
    
      When sourcing ‘iii-cache.R’:
    Error: $ operator is invalid for atomic vectors
    Execution halted
    ```

## In both

*   checking tests ...
    ```
      Running ‘test-all.R’
     ERROR
    Running the tests in ‘tests/test-all.R’ failed.
    Last 13 lines of output:
       10. └─SpaDES.core::simInit(...)
       11.   ├─SpaDES.core::simInit(...)
       12.   └─SpaDES.core::simInit(...)
       13.     ├─base::withCallingHandlers(...)
       14.     └─SpaDES.core:::.runModuleInputObjects(sim, m, objects, notOlderThan)
       15.       └─reproducible::Cache(...)
       16.         └─reproducible:::evalTheFunAndAddChanged(...)
       17.           ├─reproducible::.addChangedAttr(...)
       18.           └─SpaDES.core::.addChangedAttr(...)
       19.             ├─base::unlist(...)
       20.             └─base::lapply(...)
      
      [ FAIL 16 | WARN 3 | SKIP 32 | PASS 400 ]
      Error: Test failures
      Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  7.1Mb
      sub-directories of 1Mb or more:
        R   5.1Mb
    ```

# SpaDES.experiment

<details>

* Version: 0.0.2.9005
* GitHub: https://github.com/PredictiveEcology/SpaDES.experiment
* Source code: https://github.com/cran/SpaDES.experiment
* Number of recursive dependencies: 129

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
    Apr10 11:39:04 save  :spades total elpsd: 7.2 secs | 2 save spades 10
    simList saved in
    SpaDES.core:::savedSimEnv()$.sim
    It will be deleted at next spades() call.
    shutting down parallel nodes
    Timing stopped at: 2.622 0.029 2.649
    
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
      
      [ FAIL 5 | WARN 0 | SKIP 3 | PASS 32 ]
      Error: Test failures
      Execution halted
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

