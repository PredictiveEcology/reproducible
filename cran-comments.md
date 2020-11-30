## Updated release

This release fixes an error that now occurs with the new `magrittr` >2.0.0. 

## Test environments

### GitHub Actions
- os: macOS-latest,   r: 'release'
- os: windows-latest, r: 'release'
- os: windows-latest, r: '3.6'
- os: ubuntu-18.04,   r: 'devel', 
- os: ubuntu-18.04,   r: 'release'
- os: ubuntu-18.04,   r: 'oldrel'
- os: ubuntu-18.04,   r: '3.5'
          
### Winbuilder -- all passed Sept 8, 2020
* Windows                 (win-builder), R 3.6.3
* Windows                 (win-builder), R 4.0.2
* Windows                 (win-builder), R 4.1.0 (2020-11-30)

## R CMD check results

There were no ERRORs nor WARNINGs or NOTEs.

## Downstream dependencies

We checked 4 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages
