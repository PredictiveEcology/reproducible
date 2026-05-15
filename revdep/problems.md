# SpaDES.core (3.0.4)

* GitHub: <https://github.com/PredictiveEcology/SpaDES.core>
* Email: <mailto:eliot.mcintire@canada.ca>
* GitHub mirror: <https://github.com/cran/SpaDES.core>

Run `revdepcheck::revdep_details(, "SpaDES.core")` for more info

## In both

*   checking running R code from vignettes ...
     ```
     ...
     Errors in running code in vignettes:
     when running code in ‘ii-modules.Rmd’
       ...
     the metadata for each module. To recover, use: `restartSpades()`
     
     Because of an interrupted spades call, the sim object at the start of the interrupted event was saved in
     SpaDES.core:::savedSimEnv()$.sim
     It will be deleted on next call to spades().
     
       When sourcing ‘ii-modules.R’:
     Error: Package 'NLMR' not available. Please install it using:
       install.packages('NLMR', repos = 'https://predictiveecology.r-universe.dev')
     Execution halted
     when running code in ‘iii-cache.Rmd’
       ...
     
     Because of an interrupted spades call, the sim object at the start of the interrupted event was saved in
     SpaDES.core:::savedSimEnv()$.sim
     It will be deleted on next call to spades().
     Timing stopped at: 0.279 0.022 0.301
     
       When sourcing ‘iii-cache.R’:
     Error: Package 'NLMR' not available. Please install it using:
       install.packages('NLMR', repos = 'https://predictiveecology.r-universe.dev')
     Execution halted
     ```

