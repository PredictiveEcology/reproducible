## Updated release

This release addresses transient failures on the CRAN version of `reproducible` (currently there are none: https://cran.r-project.org/web/checks/check_results_reproducible.html), plus adds several new features.

## Test environments

### GitHub Actions
- os: macOS-latest,   r: 'release'
- os: windows-latest, r: 'release'
- os: windows-latest, r: '4.0'
- os: windows-latest, r: '3.6'
- os: ubuntu-18.04,   r: 'devel', 
- os: ubuntu-18.04,   r: 'release'
- os: ubuntu-18.04,   r: 'oldrel'
- os: ubuntu-18.04,   r: '3.6'
          
### Winbuilder -- all passed Sept 25, 2021
* Windows                 (win-builder), R 3.6.3
* Windows                 (win-builder), R 4.0.5
* Windows                 (win-builder), R-devel

## R CMD check results

There is only a NOTE about the email address, which is correct, and unchanged, for the maintainer.

## Downstream dependencies

Running `revdepcheck::revdep_check` revealed all downstream dependencies OK. 

 * We saw 1 new problem with SpaDES.core. We are the developers of this package and will update shortly.
 * We failed to check 0 packages
