## Release information

This is a patch release (3.1.0 -> 3.1.1) addressing a CRAN-flagged
check problem (see "CRAN-flagged issue" below).

See `NEWS.md` for the full list of changes. There are no user-visible
changes; this release modifies only a package test.

## Test environments

### win-builder (this 3.1.1 build)
* Windows                 (win-builder), R-oldrelease (R 4.6.0)
* Windows                 (win-builder), R-release    (R 4.5.3)
* Windows                 (win-builder), R-devel       (r90061)

### GitHub Actions (development branch)
* Ubuntu 24.04                 (GitHub), R-release
* Windows                      (GitHub), R-release
* Ubuntu 24.04                 (GitHub), R-devel
* Windows                      (GitHub), R-devel

### Local
* Linux                         (local), R 4.5.x

## R CMD check results

There are no errors or warnings.

All three win-builder builds (oldrelease, release, devel) returned a
single NOTE:

    Days since last update: 1

This is expected: version 3.1.0 was accepted one day ago. This 3.1.1
release contains only the fix that the CRAN team requested (see
"CRAN-flagged issue" below), submitted within the requested timeframe.

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

We checked 3 reverse dependencies, comparing R CMD check results across
CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages
