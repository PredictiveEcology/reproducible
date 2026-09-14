## The first lines of a script run by a separate R process, loading the reproducible under test.
## Separate processes, not forked children: a fork shares too much of its parent to exercise a race
## between processes, and it inherits filelock's record of the parent's locks.
## Under devtools/pkgload the installed reproducible is not the one being tested, and a fresh child
## process would silently load the installed one -- so the child loads the same source tree the
## parent did. An installed package has R/reproducible.rdb where a source tree has R/*.R; that is
## the difference, and it needs no extra dependency to ask (pkgload is not one of reproducible's).
childProcessPreamble <- function() {
  pkgPath <- normalizePath(getNamespaceInfo("reproducible", "path"), mustWork = FALSE)
  fromSource <- !file.exists(file.path(pkgPath, "R", "reproducible.rdb")) &&
    file.exists(file.path(pkgPath, "DESCRIPTION"))
  c(sprintf('.libPaths(%s)', paste0("c(", paste0('"', .libPaths(), '"', collapse = ", "), ")")),
    if (fromSource) sprintf('library(pkgload); load_all("%s", quiet = TRUE)', pkgPath) else
      'suppressMessages(library(reproducible))')
}
