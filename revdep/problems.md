# LandR

<details>

* Version: 1.1.1.9001
* GitHub: https://github.com/PredictiveEcology/LandR
* Source code: https://github.com/cran/LandR
* Number of recursive dependencies: 195

Run `revdepcheck::revdep_details(, "LandR")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
      Running ‘testthat.R’
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      • empty test (1): 'test-wardDispersalFunction.R:215:1'
      • interactive() is not TRUE (3): 'test-loadkNNSpeciesLayers.R:16:3',
        'test-loadkNNSpeciesLayers.R:86:3', 'test-loadkNNSpeciesLayers.R:144:3'
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('test-wardDispersalFunction.R:198:5'): test Ward dispersal seeding algorithm ──
      sum(tests < 0.01)/length(tests) <= 0.1 is not TRUE
      
      `actual`:   FALSE
      `expected`: TRUE 
      
      [ FAIL 1 | WARN 0 | SKIP 6 | PASS 563 ]
      Error: Test failures
      Execution halted
      Ran 1/1 deferred expressions
    ```

## In both

*   checking dependencies in R code ... WARNING
    ```
    Missing or unexported object: ‘biomod2::BIOMOD_ModelingOptions’
    ```

