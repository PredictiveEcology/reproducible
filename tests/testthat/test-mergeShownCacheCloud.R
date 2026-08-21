## Coverage for mergeShownCacheCloud() in R/GPT2.R.
##
## showSimilar() explains a cache miss by comparing this call against the
## closest prior entry. With useCloud, the interesting prior entry may have been
## computed on ANOTHER machine and never landed in this local cache -- so
## showSimilar folds the cloud's per-cacheId metadata into the local showCache()
## result before comparing. mergeShownCacheCloud is that fold.
##
## It is worth pinning separately from the Drive round-trip because its
## behaviour is pure data.table logic on two tables: no network needed, so
## these run everywhere, including CRAN and the nosuggests leg.
##
## No network, no Drive.

## A minimal shownCache-shaped table: the four columns in .dtFileMainCols.
mkShown <- function(cacheId, fname = "myFun") {
  data.table::rbindlist(lapply(cacheId, function(i) {
    data.table::data.table(
      cacheId = i,
      tagKey = c("function", "a"),
      tagValue = c(fname, paste0("v", i)),
      createdDate = as.character(Sys.time())
    )
  }))
}

test_that("mergeShownCacheCloud returns the local table untouched when the cloud has nothing", {
  testInit()

  local <- mkShown("L1")

  ## Both "no cloud metadata at all" and "cloud listed, but empty" must be
  ## no-ops rather than errors: showSimilar calls this unconditionally whenever
  ## useCloud is on, including on a cloudFolderID that has never been written.
  expect_identical(mergeShownCacheCloud(local, NULL, "myFun"), local)
  expect_identical(mergeShownCacheCloud(local, .emptyCacheTable, "myFun"), local)
})

test_that("mergeShownCacheCloud folds cloud-only cacheIds in alongside local ones", {
  testInit()

  local <- mkShown("L1")
  cloud <- mkShown("C1")

  out <- mergeShownCacheCloud(local, cloud, "myFun")

  ## Both survive. Dropping either would defeat the point: the local entry is
  ## the usual comparison, the cloud entry is the one this machine never
  ## computed.
  expect_setequal(unique(out$cacheId), c("L1", "C1"))
  expect_identical(NROW(out), NROW(local) + NROW(cloud))
})

test_that("mergeShownCacheCloud surfaces a cloud entry when the local cache is empty", {
  testInit()

  ## The cross-machine case that motivates the whole block: nothing computed
  ## here yet, so showCache() gives nothing and, without this merge, showSimilar
  ## would report "no similar item" despite the cloud holding a near match.
  out <- mergeShownCacheCloud(.emptyCacheTable, mkShown("C1"), "myFun")

  expect_identical(unique(out$cacheId), "C1")
})

test_that("mergeShownCacheCloud keeps only cloud entries from the same function", {
  testInit()

  local <- mkShown("L1", fname = "myFun")
  otherFun <- mkShown("C2", fname = "otherFun")

  ## An unrelated function's cacheId is not a "similar call" -- folding it in
  ## would have showSimilar diff this call against something with no relation
  ## to it.
  expect_identical(mergeShownCacheCloud(local, otherFun, "myFun"), local)

  ## With no function name to filter on, no filtering happens: the caller has
  ## not narrowed the comparison, so nothing is dropped.
  expect_setequal(unique(mergeShownCacheCloud(local, mkShown("C1"), NULL)$cacheId),
                  c("L1", "C1"))
})

test_that("mergeShownCacheCloud does not duplicate a cacheId present both places", {
  testInit()

  ## An artifact this machine computed AND uploaded appears in both tables.
  ## It must be counted once, or showSimilar would report the same cacheId
  ## twice as two separate near-matches.
  local <- mkShown("L1")

  expect_identical(NROW(mergeShownCacheCloud(local, local, "myFun")), NROW(local))
})
