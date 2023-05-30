## Release information

This is a minor update that addresses several issues that were fixed, including one that was identified by one of our dependencies due to a change on their package (data.table) that was causing a fail in reverse depends testing. This also addresses an issue identified as "Fatal error: compiler constants were modified" which appears to occur due to use of a pass-by-reference function `data.table::setattr` that sets attributes. These have all been changed to use `base::attr`. After this change, the modified compiler constants error disappeared.

See `NEWS.md` for a full list of changes.

## Test environments

### Previous R versions
* Ubuntu 20.04                 (GitHub), R 4.2.3
* Windows                      (GitHub), R 4.2.3
* Windows                 (win-builder), R 4.2.3 (2023-03-15 ucrt)

### Current R versions
* macOS Monterey 12.6.5        (GitHub), R 4.3.0
* macOs (m1) Big Sur    (macOS-builder), R 4.3.0
* Ubuntu 20.04                 (GitHub), R 4.3.0
* Ubuntu 20.04                  (local), R 4.3.0
* Windows                      (GitHub), R 4.3.0
* Windows                       (local), R 4.3.0
* Windows                 (win-builder), R 4.3.0 (2023-04-21 ucrt)

### Development R version
* Ubuntu 20.04 LTS             (GitHub), R-devel (2023-05-27 r84465 ucrt)
* Windows                      (GitHub), R-devel (2023-05-27 r84465 ucrt)
* Windows                 (win-builder), R-devel (2023-05-27 r84465 ucrt)

## R CMD check results

No NOTEs, WARNINGs, or ERRORs

## Downstream dependencies

Currently `SpaDES.tools`, No conflicts were detected.
