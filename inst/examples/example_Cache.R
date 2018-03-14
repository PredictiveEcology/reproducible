library(raster)

tmpDir <- file.path(tempdir(), "reproducible_examples", "Cache")
try(clearCache(tmpDir), silent = TRUE) # just to make sure it is clear

# Basic use
ranNumsA <- Cache(rnorm, 10, 16, cacheRepo = tmpDir)

# All same
ranNumsB <- Cache(rnorm, 10, 16, cacheRepo = tmpDir) # recovers cached copy
ranNumsC <- rnorm(10, 16) %>% Cache(cacheRepo = tmpDir) # recovers cached copy
ranNumsD <- Cache(quote(rnorm(n = 10, 16)), cacheRepo = tmpDir) # recovers cached copy

# Any minor change makes it different
ranNumsE <- rnorm(10, 6) %>% Cache(cacheRepo = tmpDir) # different

## Example 1: basic cache use with tags
ranNumsA <- Cache(rnorm, 4, cacheRepo = tmpDir, userTags = "objectName:a")
ranNumsB <- Cache(runif, 4, cacheRepo = tmpDir, userTags = "objectName:b")

showCache(tmpDir, userTags = c("objectName"))
showCache(tmpDir, userTags = c("^a$")) # regular expression ... "a" exactly
showCache(tmpDir, userTags = c("runif")) # show only cached objects made during runif call

clearCache(tmpDir, userTags = c("runif")) # remove only cached objects made during runif call
showCache(tmpDir) # only those made during rnorm call

clearCache(tmpDir)

## Example 2: using the "accessed" tag
ranNumsA <- Cache(rnorm, 4, cacheRepo = tmpDir, userTags = "objectName:a")
ranNumsB <- Cache(runif, 4, cacheRepo = tmpDir, userTags = "objectName:b")

# access it again, from Cache
ranNumsA <- Cache(rnorm, 4, cacheRepo = tmpDir, userTags = "objectName:a")
wholeCache <- showCache(tmpDir)

# keep only items accessed "recently" (i.e., only objectName:a)
onlyRecentlyAccessed <- showCache(tmpDir, userTags = max(wholeCache[tagKey == "accessed"]$tagValue))

# inverse join with 2 data.tables ... using: a[!b]
# i.e., return all of wholeCache that was not recently accessed
toRemove <- unique(wholeCache[!onlyRecentlyAccessed], by = "artifact")$artifact
clearCache(tmpDir, toRemove) # remove ones not recently accessed
showCache(tmpDir) # still has more recently accessed

clearCache(tmpDir)

## Example 3: using keepCache
ranNumsA <- Cache(rnorm, 4, cacheRepo = tmpDir, userTags = "objectName:a")
ranNumsB <- Cache(runif, 4, cacheRepo = tmpDir, userTags = "objectName:b")

# keep only those cached items from the last 24 hours
oneDay <- 60 * 60 * 24
keepCache(tmpDir, after = Sys.time() - oneDay)

# Keep all Cache items created with an rnorm() call
keepCache(tmpDir, userTags = "rnorm")

# Remove all Cache items that happened within a rnorm() call
clearCache(tmpDir, userTags = "rnorm")

showCache(tmpDir) ## empty

## Example 4: searching for multiple objects in the cache

# default userTags is "and" matching; for "or" matching use |
ranNumsA <- Cache(runif, 4, cacheRepo = tmpDir, userTags = "objectName:a")
ranNumsB <- Cache(rnorm, 4, cacheRepo = tmpDir, userTags = "objectName:b")

# show all objects (runif and rnorm in this case)
showCache(tmpDir)

# show objects that are both runif and rnorm
# (i.e., none in this case, because objecs are either or, not both)
showCache(tmpDir, userTags = c("runif", "rnorm")) ## empty

# show objects that are either runif or rnorm ("or" search)
showCache(tmpDir, userTags = "runif|rnorm")

# keep only objects that are either runif or rnorm ("or" search)
keepCache(tmpDir, userTags = "runif|rnorm")

clearCache(tmpDir)

## Example 5: using caching to speed up rerunning expensive computations
ras <- raster(extent(0, 10, 0, 10), res = 1,
              vals = sample(1:5, replace = TRUE, size = 1e2),
              crs = "+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100 +ellps=WGS84")

# A slow operation, like GIS operation
notCached <- suppressWarnings(
  # project raster generates warnings when run non-interactively
  projectRaster(ras, crs = crs(ras), res = 5, cacheRepo = tmpDir)
)

cached <- suppressWarnings(
  # project raster generates warnings when run non-interactively
  Cache(projectRaster, ras, crs = crs(ras), res = 5, cacheRepo = tmpDir)
)

# second time is much faster
reRun <- suppressWarnings(
  # project raster generates warnings when run non-interactively
  Cache(projectRaster, ras, crs = crs(ras), res = 5, cacheRepo = tmpDir)
)

# recovered cached version is same as non-cached version
all.equal(notCached, reRun) ## TRUE

## Example 6: working with file paths

# if passing a character string, it will take 2 complete passes to before
#  a cached copy is used when it is a save event (read or load is different)
obj <- 1:10
fname <- tempfile(fileext = ".RData")
Cache(saveRDS, obj, file = fname, cacheRepo = tmpDir)
Cache(saveRDS, obj, file = fname, cacheRepo = tmpDir)
Cache(saveRDS, obj, file = fname, cacheRepo = tmpDir) # cached copy is loaded

# however, using asPath(), cached version retrieved after being run once
fname1 <- tempfile(fileext = ".RData")
Cache(saveRDS, obj, file = asPath(fname1), cacheRepo = tmpDir)
Cache(saveRDS, obj, file = asPath(fname1), cacheRepo = tmpDir) # cached copy is loaded

clearCache(tmpDir)


##########################
## Nested Caching
# Make 2 functions
inner <- function(mean) {
  d <- 1
  Cache(rnorm, n = 3, mean = mean)
}
outer <- function(n) {
  Cache(inner, 0.1, cacheRepo = tmpdir2)
}

# make 2 different cache paths
tmpdir1 <- file.path(tempdir(), "first")
tmpdir2 <- file.path(tempdir(), "second")

# Run the Cache ... notOlderThan propagates to all 3 Cache calls,
#   but cacheRepo is tmpdir in top level Cache and all nested
#   Cache calls, unless individually overridden ... here inner
#   uses tmpdir2 repository
Cache(outer, n = 2, cacheRepo = tmpdir1, notOlderThan = Sys.time())

showCache(tmpdir1) # 2 function calls
showCache(tmpdir2) # 1 function call

# userTags get appended
# all items have the outer tag propagate, plus inner ones only have inner ones
clearCache(tmpdir1)
outerTag <- "outerTag"
innerTag <- "innerTag"
inner <- function(mean) {
  d <- 1
  Cache(rnorm, n = 3, mean = mean, notOlderThan = Sys.time() - 1e5, userTags = innerTag)
}
outer <- function(n) {
  Cache(inner, 0.1)
}
aa <- Cache(outer, n = 2, cacheRepo = tmpdir1, userTags = outerTag)
showCache(tmpdir1) # rnorm function has outerTag and innerTag, inner and outer only have outerTag

## cleanup
unlink(c("filename.rda", "filename1.rda"))
unlink(dirname(tmpDir), recursive = TRUE)

