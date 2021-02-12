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
* Windows                 (win-builder), R 4.1.0 (2021-02-11)

## R CMD check results

There were no ERRORs nor WARNINGs or NOTEs.

## Downstream dependencies

Running `revdepcheck::revdep_check` revealed all downstream dependencies OK. 

