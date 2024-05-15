# SpaDES.core

<details>

* Version: 2.0.5
* GitHub: https://github.com/PredictiveEcology/SpaDES.core
* Source code: https://github.com/cran/SpaDES.core
* Date/Publication: 2024-04-25 17:20:02 UTC
* Number of recursive dependencies: 153

Run `revdepcheck::revdep_details(, "SpaDES.core")` for more info

</details>

## Newly broken

*   checking whether package ‘SpaDES.core’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/home/achubaty/Documents/GitHub/PredictiveEcology/reproducible/revdep/checks/SpaDES.core/new/SpaDES.core.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking tests ...
    ```
      Running ‘test-all.R’
     ERROR
    Running the tests in ‘tests/test-all.R’ failed.
    Last 13 lines of output:
      25: test_code(test = NULL, code = exprs, env = env, default_reporter = StopReporter$new())
      26: source_file(path, env = env(env), desc = desc, error_call = error_call)
      27: FUN(X[[i]], ...)
      28: lapply(test_paths, test_one_file, env = env, desc = desc, error_call = error_call)
      29: doTryCatch(return(expr), name, parentenv, handler)
      30: tryCatchOne(expr, names, parentenv, handlers[[1L]])
      31: tryCatchList(expr, classes, parentenv, handlers)
      32: tryCatch(code, testthat_abort_reporter = function(cnd) {    cat(conditionMessage(cnd), "\n")    NULL})
      33: with_reporter(reporters$multi, lapply(test_paths, test_one_file,     env = env, desc = desc, error_call = error_call))
      34: test_files_serial(test_dir = test_dir, test_package = test_package,     test_paths = test_paths, load_helpers = load_helpers, reporter = reporter,     env = env, stop_on_failure = stop_on_failure, stop_on_warning = stop_on_warning,     desc = desc, load_package = load_package, error_call = error_call)
      35: test_files(test_dir = path, test_paths = test_paths, test_package = package,     reporter = reporter, load_helpers = load_helpers, env = env,     stop_on_failure = stop_on_failure, stop_on_warning = stop_on_warning,     load_package = load_package, parallel = parallel)
      36: test_dir("testthat", package = package, reporter = reporter,     ..., load_package = "installed")
      37: test_check("SpaDES.core")
      An irrecoverable exception occurred. R is aborting now ...
      Segmentation fault (core dumped)
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  6.4Mb
      sub-directories of 1Mb or more:
        R   4.7Mb
    ```

## Installation

### Devel

```
* installing *source* package ‘SpaDES.core’ ...
** package ‘SpaDES.core’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Creating a new generic function for ‘citation’ in package ‘SpaDES.core’
Error in get(x, envir = ns, inherits = FALSE) : 
  object '.addingToMemoisedMsg' not found
Error: unable to load R code in package ‘SpaDES.core’
Execution halted
ERROR: lazy loading failed for package ‘SpaDES.core’
* removing ‘/home/achubaty/Documents/GitHub/PredictiveEcology/reproducible/revdep/checks/SpaDES.core/new/SpaDES.core.Rcheck/SpaDES.core’


```
### CRAN

```
* installing *source* package ‘SpaDES.core’ ...
** package ‘SpaDES.core’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Creating a new generic function for ‘citation’ in package ‘SpaDES.core’
** help
*** installing help indices
*** copying figures
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (SpaDES.core)


```
