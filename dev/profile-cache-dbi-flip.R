## Reproduce + profile the slow Cache() after a reproducible.useDBI flip.
## Hypothesis: switching useDBI invalidates / rebuilds cache state, and the
## next Cache() call re-imports the existing rds-based cache index into SQLite
## (or vice versa). Sub-second target turning into multi-minute reality.

suppressPackageStartupMessages({
  devtools::load_all(quiet = TRUE)
})

td <- file.path(tempdir(), "cache-bench")
unlink(td, recursive = TRUE); dir.create(td)
options(reproducible.cachePath = td,
        reproducible.ask = FALSE,
        reproducible.useMemoise = FALSE)

cat("== warmup: 3 Cache calls with useDBI=FALSE ==\n")
useDBI(FALSE)
t0 <- Sys.time()
for (i in 1:3) Cache(rnorm(i))
cat(sprintf("  elapsed: %.2fs\n", as.numeric(difftime(Sys.time(), t0, units = "secs"))))

cat("\n== flip to useDBI=TRUE, then 1 Cache call ==\n")
useDBI(TRUE)
Rprof("dev/Rprof-flip1.out", interval = 0.02)
t0 <- Sys.time()
x <- Cache(rnorm(4))
elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
Rprof(NULL)
cat(sprintf("  ONE Cache(rnorm(4)) after useDBI(TRUE) flip: %.2fs\n", elapsed))

cat("\n== another Cache call without flipping (no toggle) ==\n")
Rprof("dev/Rprof-noflip.out", interval = 0.02)
t0 <- Sys.time()
x <- Cache(rnorm(5))
elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
Rprof(NULL)
cat(sprintf("  ONE Cache(rnorm(5)) without flip: %.2fs\n", elapsed))

cat("\n== flip back to useDBI=FALSE, then 1 Cache call ==\n")
useDBI(FALSE)
Rprof("dev/Rprof-flip2.out", interval = 0.02)
t0 <- Sys.time()
x <- Cache(rnorm(6))
elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
Rprof(NULL)
cat(sprintf("  ONE Cache(rnorm(6)) after useDBI(FALSE) flip: %.2fs\n", elapsed))

cat("\n== Rprof summaries ==\n")
for (f in c("dev/Rprof-flip1.out", "dev/Rprof-noflip.out", "dev/Rprof-flip2.out")) {
  cat("\n---", f, "---\n")
  s <- summaryRprof(f)
  print(head(s$by.self, 15))
}
