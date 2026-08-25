## Why this update

This fixes the errors reported on the CRAN check servers running Fedora
(r-devel-linux-x86_64-fedora-clang and r-devel-linux-x86_64-fedora-gcc) for
version 3.2.0.

When `prepInputs()` needed to determine whether the copy of 7-Zip on a computer
could handle RAR files, `apt` was consulted, which is the software installer
used by Debian and Ubuntu. Fedora does not have `apt`, and neither do several
other versions of Linux. On those machines the query did not simply come back
empty; it stopped with an error, and that is what caused the check to fail.
7-Zip is now asked directly, which behaves the same way everywhere and does not
depend on how software is installed on the machine. Two other places where
outside programs (`ps` and `unrar`) were called without first confirming they
were present have been corrected as well.

The fix was verified against Fedora directly: the previous code fails there and
the current code passes.

This release arrives the same day as 3.2.0. Another version would not normally
be submitted so soon; it is being sent now only because the current version
fails on Fedora. If it would be preferable to hold this release and take it
later alongside other changes, that is entirely acceptable.

## Where it was tested

|                | Platform     | R                                  |
|----------------|--------------|------------------------------------|
| Our machines   | Ubuntu 24.04 | 4.5.3                              |
| GitHub Actions | Ubuntu       | devel, release, oldrel-1           |
|                | Windows      | devel, release, oldrel-1, oldrel-2 |
|                | macOS        | release                            |
|                | Fedora 41    | 4.4.3                              |

## Results

No errors or warnings. One note, with two parts:

* "Days since last update: 0", explained above.
* A link to a Stack Overflow answer, quoted in a 2020 entry in `NEWS.md`, is
  reported as possibly invalid. The page opens normally in a browser. Automated
  requests are turned away by Stack Overflow, which is what produces the
  warning.

## Downstream dependencies

None at present. SpaDES, SpaDES.core and SpaDES.tools were archived on
2026-07-13 when this package was. They share this maintainer and will be
resubmitted now that this package is back.
