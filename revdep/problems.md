# SpaDES.core

<details>

* Version: 1.0.0
* Source code: https://github.com/cran/SpaDES.core
* URL: https://spades-core.predictiveecology.org/, https://github.com/PredictiveEcology/SpaDES.core
* BugReports: https://github.com/PredictiveEcology/SpaDES.core/issues
* Date/Publication: 2020-02-21 13:30:09 UTC
* Number of recursive dependencies: 160

Run `revdep_details(,"SpaDES.core")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/test-all.R’ failed.
    Last 13 lines of output:
          end, start
      
      The following object is masked from 'package:utils':
      
          citation
      
      [31m──[39m [31m1. Failure: local mod object (@test-mod.R#134) [39m [31m─────────────────────────────[39m
      grepl("loading cached", mess) isn't true.
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 505 | SKIPPED: 11 | WARNINGS: 0 | FAILED: 1 ]
      1. Failure: local mod object (@test-mod.R#134) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

