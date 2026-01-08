# SpaDES.core (2.1.8)

* GitHub: <https://github.com/PredictiveEcology/SpaDES.core>
* Email: <mailto:eliot.mcintire@canada.ca>
* GitHub mirror: <https://github.com/cran/SpaDES.core>

Run `revdepcheck::revdep_details(, "SpaDES.core")` for more info

## Newly broken

*   checking whether package 'SpaDES.core' can be installed ... ERROR
     ```
     Installation failed.
     See 'C:/Eliot/GitHub/reproducible/revdep/checks/SpaDES.core/new/SpaDES.core.Rcheck/00install.out' for details.
     ```

## Newly fixed

*   checking running R code from vignettes ...
     ```
     ...
     Errors in running code in vignettes:
     when running code in 'ii-modules.Rmd'
       ...
     `restartSpades()`
     
     Because of an interrupted spades call, the sim object at the start of the interrupted event was saved in
     SpaDES.core:::savedSimEnv()$.sim
     It will be deleted on next call to spades().
     
       When sourcing 'ii-modules.R':
     Error: Package 'NLMR' not available. Please install it using:
       install.packages('NLMR', repos = 'https://predictiveecology.r-universe.dev')
     Execution halted
     when running code in 'iii-cache.Rmd'
       ...
     
     Because of an interrupted spades call, the sim object at the start of the interrupted event was saved in
     SpaDES.core:::savedSimEnv()$.sim
     It will be deleted on next call to spades().
     Timing stopped at: 0.01 0 0.22
     
       When sourcing 'iii-cache.R':
     Error: Package 'NLMR' not available. Please install it using:
       install.packages('NLMR', repos = 'https://predictiveecology.r-universe.dev')
     Execution halted
     ```

## Installation

### Devel

```
* installing *source* package 'SpaDES.core' ...
** this is package 'SpaDES.core' version '2.1.8'
** package 'SpaDES.core' successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Creating a new generic function for 'citation' in package 'SpaDES.core'
Error in get(x, envir = ns, inherits = FALSE) : object 'isSpat' not found
Error: unable to load R code in package 'SpaDES.core'
Execution halted
ERROR: lazy loading failed for package 'SpaDES.core'
* removing 'C:/Eliot/GitHub/reproducible/revdep/checks/SpaDES.core/new/SpaDES.core.Rcheck/SpaDES.core'


```
### CRAN

```
* installing *source* package 'SpaDES.core' ...
** this is package 'SpaDES.core' version '2.1.8'
** package 'SpaDES.core' successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Creating a new generic function for 'citation' in package 'SpaDES.core'
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
# SpaDES.tools (2.0.9)

* GitHub: <https://github.com/PredictiveEcology/SpaDES.tools>
* Email: <mailto:achubaty@for-cast.ca>
* GitHub mirror: <https://github.com/cran/SpaDES.tools>

Run `revdepcheck::revdep_details(, "SpaDES.tools")` for more info

## Newly broken

*   checking whether package 'SpaDES.tools' can be installed ... ERROR
     ```
     Installation failed.
     See 'C:/Eliot/GitHub/reproducible/revdep/checks/SpaDES.tools/new/SpaDES.tools.Rcheck/00install.out' for details.
     ```

## Installation

### Devel

```
* installing *source* package 'SpaDES.tools' ...
** this is package 'SpaDES.tools' version '2.0.9'
** package 'SpaDES.tools' successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: 'G__~1.EXE (GCC) 14.2.0'
g++ -std=gnu++17  -I"C:/PROGRA~1/R/R-45~1.2/include" -DNDEBUG  -I'C:/Eliot/GitHub/reproducible/revdep/library/SpaDES.tools/Rcpp/include'   -I"C:/rtools45/x86_64-w64-mingw32.static.posix/include"      -O2 -Wall  -mfpmath=sse -msse2 -mstackrealign    -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++17  -I"C:/PROGRA~1/R/R-45~1.2/include" -DNDEBUG  -I'C:/Eliot/GitHub/reproducible/revdep/library/SpaDES.tools/Rcpp/include'   -I"C:/rtools45/x86_64-w64-mingw32.static.posix/include"      -O2 -Wall  -mfpmath=sse -msse2 -mstackrealign    -c duplicated.cpp -o duplicated.o
g++ -std=gnu++17  -I"C:/PROGRA~1/R/R-45~1.2/include" -DNDEBUG  -I'C:/Eliot/GitHub/reproducible/revdep/library/SpaDES.tools/Rcpp/include'   -I"C:/rtools45/x86_64-w64-mingw32.static.posix/include"      -O2 -Wall  -mfpmath=sse -msse2 -mstackrealign    -c runif.cpp -o runif.o
g++ -std=gnu++17 -shared -s -static-libgcc -o SpaDES.tools.dll tmp.def RcppExports.o duplicated.o runif.o -LC:/rtools45/x86_64-w64-mingw32.static.posix/lib/x64 -LC:/rtools45/x86_64-w64-mingw32.static.posix/lib -LC:/PROGRA~1/R/R-45~1.2/bin/x64 -lR
...
installing to C:/Eliot/GitHub/reproducible/revdep/checks/SpaDES.tools/new/SpaDES.tools.Rcheck/00LOCK-SPADES~1.TOO/00new/SpaDES.tools/libs/x64
** R
** inst
** byte-compile and prepare package for lazy loading
Error in get(x, envir = ns, inherits = FALSE) : 
  object 'isGridded' not found
Error: unable to load R code in package 'SpaDES.tools'
Execution halted
ERROR: lazy loading failed for package 'SpaDES.tools'
* removing 'C:/Eliot/GitHub/reproducible/revdep/checks/SpaDES.tools/new/SpaDES.tools.Rcheck/SpaDES.tools'


```
### CRAN

```
* installing *source* package 'SpaDES.tools' ...
** this is package 'SpaDES.tools' version '2.0.9'
** package 'SpaDES.tools' successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: 'G__~1.EXE (GCC) 14.2.0'
g++ -std=gnu++17  -I"C:/PROGRA~1/R/R-45~1.2/include" -DNDEBUG  -I'C:/Eliot/GitHub/reproducible/revdep/library/SpaDES.tools/Rcpp/include'   -I"C:/rtools45/x86_64-w64-mingw32.static.posix/include"      -O2 -Wall  -mfpmath=sse -msse2 -mstackrealign    -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++17  -I"C:/PROGRA~1/R/R-45~1.2/include" -DNDEBUG  -I'C:/Eliot/GitHub/reproducible/revdep/library/SpaDES.tools/Rcpp/include'   -I"C:/rtools45/x86_64-w64-mingw32.static.posix/include"      -O2 -Wall  -mfpmath=sse -msse2 -mstackrealign    -c duplicated.cpp -o duplicated.o
g++ -std=gnu++17  -I"C:/PROGRA~1/R/R-45~1.2/include" -DNDEBUG  -I'C:/Eliot/GitHub/reproducible/revdep/library/SpaDES.tools/Rcpp/include'   -I"C:/rtools45/x86_64-w64-mingw32.static.posix/include"      -O2 -Wall  -mfpmath=sse -msse2 -mstackrealign    -c runif.cpp -o runif.o
g++ -std=gnu++17 -shared -s -static-libgcc -o SpaDES.tools.dll tmp.def RcppExports.o duplicated.o runif.o -LC:/rtools45/x86_64-w64-mingw32.static.posix/lib/x64 -LC:/rtools45/x86_64-w64-mingw32.static.posix/lib -LC:/PROGRA~1/R/R-45~1.2/bin/x64 -lR
...
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
*** copying figures
** building package indices
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (SpaDES.tools)


```
