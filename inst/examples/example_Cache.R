tmpDir <- file.path(tempdir())

# Basic use -- All same
Cache(rnorm, 10, 16, cachePath = tmpDir) # recovers cached copy
Cache(quote(rnorm(n = 10, 16)), cachePath = tmpDir) # recovers cached copy
# as function call
Cache(rnorm(10, 16), cachePath = tmpDir)
# with base pipe -- this is put in "" because R version 4.0 can't understand this
#  if you are using R >= 4.1, then you can just use pipe normally
pipeWay <- "rnorm(10, 16) |>
  Cache(cachePath = tmpDir)"
if (getRversion() >= 4.1) {
  eval(parse(text = pipeWay))
}

###############################################
# devMode -- enables cache database to stay
#            small even when developing code
###############################################
opt <- options("reproducible.useCache" = "devMode")
clearCache(tmpDir, ask = FALSE)
centralTendency <- function(x)
  mean(x)
funnyData <- c(1, 1, 1, 1, 10)
uniqueUserTags <- c("thisIsUnique", "reallyUnique")
ranNumsB <- Cache(centralTendency, funnyData, cachePath = tmpDir,
                  userTags = uniqueUserTags) # sets new value to Cache
showCache(tmpDir) # 1 unique cacheId -- cacheId is 71cd24ec3b0d0cac

# During development, we often redefine function internals
centralTendency <- function(x)
  median(x)
# When we rerun, we don't want to keep the "old" cache because the function will
#   never again be defined that way. Here, because of userTags being the same,
#   it will replace the entry in the Cache, effetively overwriting it, even though
#   it has a different cacheId
ranNumsD <- Cache(centralTendency, funnyData, cachePath = tmpDir, userTags = uniqueUserTags)
showCache(tmpDir) # 1 unique artifact -- cacheId is 632cd06f30e111be

# If it finds it by cacheID, doesn't matter what the userTags are
ranNumsD <- Cache(centralTendency, funnyData, cachePath = tmpDir, userTags = "thisIsUnique")
options(opt)

#########################################
# For more in depth uses, see vignette
if (interactive())
  browseVignettes(package = "reproducible")
