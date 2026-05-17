## Release information

This is a patch release (3.1.0 -> 3.1.1) addressing a CRAN-flagged
check problem (see "CRAN-flagged issue" below).

See `NEWS.md` for the full list of changes. There are no user-visible
changes; this release modifies only a package test.

## Test environments

### Previous R versions
* Ubuntu 24.04                 (GitHub), R 4.4.x
* Windows                      (GitHub), R 4.4.x
* Windows                 (win-builder), R-oldrelease

### Current R versions
* macOS 14                     (GitHub), R 4.5.2
* Ubuntu 24.04                 (GitHub), R 4.5.2
* Windows                      (GitHub), R 4.5.2
* Windows                       (local), R 4.5.2
* Windows                 (win-builder), R-release

### Development R version
* Ubuntu 24.04                 (GitHub), R-devel
* Windows                      (GitHub), R-devel
* Windows                 (win-builder), R-devel

### R-hub v2
* Flavours              Linux, Mac, Windows (via GitHub Actions, r-hub/actions)

## R CMD check results

There are no errors or warnings. There is one NOTE related to the author,
Eliot McIntire.

## CRAN-flagged issue

Thank you very much for flagging the check problem and for the helpful
detail in your message.

We have found the cause and can reproduce the issue, and we have
implemented a fix.

One of our tests checks whether two files are the same physical file by
comparing their file-system identifiers. On the systems we had tested,
those identifiers were small enough to be stored as whole numbers, but
on file systems with very large numbers (such as the one used by the
CRAN check machine, as you kindly pointed out) the value was too large
for that storage and was lost, which made the test fail.

We have changed the test to compare these identifiers as text instead,
which works correctly for values of any size. No functionality in the
package itself was affected; the change is limited to this single test.

We are grateful for your patience and for the time the CRAN team spent
on this.

## Downstream dependencies

The three reverse dependencies on CRAN (SpaDES, SpaDES.core, SpaDES.tools)
are all maintained by the same maintainer.

## revdepcheck results

We checked 3 reverse dependencies, comparing R CMD check results across the
CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages
