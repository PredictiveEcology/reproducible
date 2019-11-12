# SpaDES.core

<details>

* Version: 0.2.6
* Source code: https://github.com/cran/SpaDES.core
* URL: http://spades-core.predictiveecology.org/, https://github.com/PredictiveEcology/SpaDES.core
* BugReports: https://github.com/PredictiveEcology/SpaDES.core/issues
* Date/Publication: 2019-09-13 14:20:02 UTC
* Number of recursive dependencies: 167

Run `revdep_details(,"SpaDES.core")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/test-all.R’ failed.
    Last 13 lines of output:
      [90m 1. [39mreproducible::showCache(sims[[1]])
      [90m 2. [39mreproducible::showCache(sims[[1]])
      [90m 6. [39marchivist::showLocalRepo(x)
      [90m 7. [39mbase::stopifnot(is.character(repoDir) & length(repoDir) == 1)
      
      [1] "<environment: 0x564af4d84d88>"
      [1] "<environment: 0x564af34a6b90>"
      ══ testthat results  ════════════════════════════════════
      [ OK: 476 | SKIPPED: 9 | WARNINGS: 0 | FAILED: 1 ]
      1. Error: test cache (@test-cache.R#34) 
      
      Error: testthat unit tests failed
      In addition: Warning message:
      In fun(libname, pkgname) : couldn't connect to display ":99"
      Execution halted
    ```

