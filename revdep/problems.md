# SpaDES.core

<details>

* Version: 1.0.8
* GitHub: https://github.com/PredictiveEcology/SpaDES.core
* Source code: https://github.com/cran/SpaDES.core
* Date/Publication: 2021-06-10 09:10:02 UTC
* Number of recursive dependencies: 135

Run `revdep_details(, "SpaDES.core")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in 'tests/test-all.R' failed.
    Last 13 lines of output:
      == Failed tests ================================================================
      -- Failure (test-cache.R:519:3): test showSimilar ------------------------------
      any(grepl("This call to cache differs", mess)) is not TRUE
      
      `actual`:   FALSE
      `expected`: TRUE 
      -- Failure (test-cache.R:524:3): test showSimilar ------------------------------
      any(grepl("This call to cache differs", mess)) is not TRUE
      
      `actual`:   FALSE
      `expected`: TRUE 
      
      [ FAIL 2 | WARN 0 | SKIP 20 | PASS 588 ]
      Error: Test failures
      Execution halted
    ```

