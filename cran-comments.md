## Updated release

This release fixes a bug that was introduced by an updated RSQLite package. It also contains several bugfixes for edges cases. We have addressed the `rcnst` issue identified in the previous reproducible 1.2.4 https://github.com/kalibera/cran-checks/tree/master/rcnst/results/reproducible. We were able to reproduce the issue locally on that previous version, and have fixed it. We have tested the current version with:

```
# build the package
env R_COMPILE_PKGS=1 R_JIT_STRATEGY=4 R CMD build reproducible
# check the package
env R_CHECK_CONSTANTS=5 R CMD check reproducible_1.2.6.tar.gz
```

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

### Winbuilder -- all passed Feb 16, 2021
* Windows                 (win-builder), R 3.6.3 (2021-02-11)
* Windows                 (win-builder), R 4.0.3 (2021-02-11)
* Windows                 (win-builder), R 4.1.0 (2021-02-11)
* Windows                 (win-builder), R-devel (2021-02-11)

## R CMD check results

There were no ERRORs nor WARNINGs or NOTEs.

## Downstream dependencies

Running `revdepcheck::revdep_check` revealed all downstream dependencies OK. 

