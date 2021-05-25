## Updated release

This release fixes the issue of an empty `~/tmp` directory being left behind after running examples.
See `NEWS.md` for other changes.

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

### Winbuilder -- all passed
* Windows                 (win-builder), R 3.6.3 (2021-02-11)
* Windows                 (win-builder), R 4.0.3 (2021-02-11)
* Windows                 (win-builder), R 4.1.0 (2021-02-11)
* Windows                 (win-builder), R-devel (2021-02-11)

## R CMD check results

There were no ERRORs nor WARNINGs or NOTEs.

## Downstream dependencies

Running `revdepcheck::revdep_check` revealed all downstream dependencies OK. 
