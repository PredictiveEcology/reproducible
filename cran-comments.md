## Updated release

This release fixes a minor url change.

## Test environments

### GitHub Actions
- os: macOS-latest,   r: 'release'
- os: windows-latest, r: 'release'
- os: windows-latest, r: '3.6'
- os: ubuntu-18.04,   r: 'devel', 
- os: ubuntu-18.04,   r: 'release'
- os: ubuntu-18.04,   r: 'oldrel'
- os: ubuntu-18.04,   r: '3.5'
          
### Winbuilder -- all passed Dec 2, 2020
* Windows                 (win-builder), R 3.6.3
* Windows                 (win-builder), R 4.0.2
* Windows                 (win-builder), R 4.1.0 (2020-12-02)

## R CMD check results

There were no ERRORs nor WARNINGs or NOTEs.

## Downstream dependencies

We will be submitting an update to SpaDES.core when `reproducible` is accepted. 

