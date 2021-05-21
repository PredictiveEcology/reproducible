## Updated release

This release fixes a bug that was introduced by an updated RSQLite package. It also contains several bugfixes for edges cases.

## Test environments

### GitHub Actions
- os: macOS-latest,   r: 'release'
- os: windows-latest, r: 'release'
- os: windows-latest, r: '3.6'
- os: ubuntu-18.04,   r: 'devel', 
- os: ubuntu-18.04,   r: 'release'
- os: ubuntu-18.04,   r: 'oldrel'
- os: ubuntu-18.04,   r: '3.5'
          
### Winbuilder -- all passed Feb 11, 2021
* Windows                 (win-builder), R 3.6.3 (2021-02-11)
* Windows                 (win-builder), R 4.0.3 (2021-02-11)
* Windows                 (win-builder), R-devel (2021-05-18 r80323)

## R CMD check results

There were no ERRORs nor WARNINGs or NOTEs.

## Downstream dependencies

We checked 4 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages
