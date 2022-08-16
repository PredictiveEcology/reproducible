# SpaDES.core

<details>

* Version: 1.0.10
* GitHub: https://github.com/PredictiveEcology/SpaDES.core
* Source code: https://github.com/cran/SpaDES.core
* Date/Publication: 2022-01-19 16:22:46 UTC
* Number of recursive dependencies: 152

Run `revdep_details(, "SpaDES.core")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘SpaDES.core-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: objSize.simList
    > ### Title: Object size for 'simList'
    > ### Aliases: objSize.simList
    > 
    > ### ** Examples
    > 
    > a <- simInit(objects = list(d = 1:10, b = 2:20))
    ...
        rasterTmpDir = '/tmp/RtmpHY0Nte/raster'
        reproducible.cachePath = '/tmp/RtmpHY0Nte/myProject/cache'
        spades.inputPath = '/tmp/RtmpHY0Nte/myProject/inputs'
        spades.outputPath = '/tmp/RtmpHY0Nte/myProject/outputs'
        spades.modulePath = '/tmp/RtmpHY0Nte/myProject/modules'
      )
    > objSize(a)
    Error in compute_bytes(bytes) : is.numeric(bytes) is not TRUE
    Calls: <Anonymous> ... format.lobstr_bytes -> <Anonymous> -> style -> compute_bytes -> stopifnot
    Execution halted
    ```

*   checking tests ...
    ```
      Running ‘test-all.R’
     ERROR
    Running the tests in ‘tests/test-all.R’ failed.
    Last 13 lines of output:
      • benchmarking DES (1)
      • empty test (4)
      • restartR not possible in automated tests (1)
      • restartR with logging not possible in automated tests (1)
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure (test-cache.R:384:3): test objSize ──────────────────────────────────
      length(os) == 6 is not TRUE
      
      `actual`:   FALSE
      `expected`: TRUE 
      
      [ FAIL 1 | WARN 12 | SKIP 36 | PASS 414 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘RandomFields’
    ```

