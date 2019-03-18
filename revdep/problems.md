# SpaDES.addins

Version: 0.1.2

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘devtools’ ‘rstudioapi’
      All declared Imports should be used.
    ```

# SpaDES.core

Version: 0.2.4

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/test-all.R’ failed.
    Last 13 lines of output:
      [9]  -6.33 -  -8.17 ==  1.831
      ...
      
      [34m  Using cached copy of .inputObjects event in child6 module.   
      [39m[34m  Using memoised copy of .inputObjects event in child6 module
      [39m[34m  Using memoised copy of .inputObjects event in child6 module
      [39m══ testthat results  ═════════════════════════════════════════════
      OK: 458 SKIPPED: 32 FAILED: 2
      1. Failure: simulation runs with simInit and spades (@test-simulation.R#86) 
      2. Failure: simulation runs with simInit and spades (@test-simulation.R#87) 
      
      Error: testthat unit tests failed
      In addition: Warning message:
      In fun(libname, pkgname) : couldn't connect to display ":99"
      Execution halted
    ```

