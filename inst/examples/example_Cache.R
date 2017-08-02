library(raster)

tmpDir <- file.path(tempdir(), "reproducible_examples", "Cache")

## Example 1: basic cache use
ranNums <- Cache(rnorm, 4, cacheRepo = tmpDir, userTags = "objectName:a")
ranNums <- Cache(runif, 4, cacheRepo = tmpDir, userTags = "objectName:b")

showCache(tmpDir, userTags = c("objectName"))
showCache(tmpDir, userTags = c("^a$")) # regular expression ... "a" exactly
showCache(tmpDir, userTags = c("runif")) # show only cached objects made during runif call

clearCache(tmpDir, userTags = c("runif")) # remove only cached objects made during runif call
showCache(tmpDir) # only those made during rnorm call

clearCache(tmpDir)

## Example 2: using the "accessed" tag
ranNums <- Cache(rnorm, 4, cacheRepo = tmpDir, userTags = "objectName:a")
ranNums <- Cache(runif, 4, cacheRepo = tmpDir, userTags = "objectName:b")

# access it again, but "later"
Sys.sleep(1)
ranNums <- Cache(rnorm, 4, cacheRepo = tmpDir, userTags = "objectName:a")
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
ranNums <- Cache(rnorm, 4, cacheRepo = tmpDir, userTags = "objectName:a")
ranNums <- Cache(runif, 4, cacheRepo = tmpDir, userTags = "objectName:b")

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
ranNums <- Cache(runif, 4, cacheRepo = tmpDir, userTags = "objectName:a")
ranNums <- Cache(rnorm, 4, cacheRepo = tmpDir, userTags = "objectName:b")

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
ras <- raster(extent(0, 100, 0, 100), res = 1,
              vals = sample(1:5, replace = TRUE, size = 1e4),
              crs = "+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100 +ellps=WGS84")

# A slow operation, like GIS operation
notCached <- suppressWarnings(projectRaster(ras, crs = crs(ras), res = 5,
                                            cacheRepo = tmpDir))
cached <- Cache(suppressWarnings(projectRaster), ras, crs = crs(ras), res = 5,
                cacheRepo = tmpDir)

# second time is much faster
reRun <- Cache(suppressWarnings(projectRaster), ras, crs = crs(ras), res = 5,
               cacheRepo = tmpDir)

# recovered cached version is same as non-cached version
all.equal(notCached, reRun) ## TRUE

## Example 6: working with file paths

# if passing a character string, it will take 2 complete passes to before
#  a cached copy is used when it is a save event (read or load is different)
obj <- 1:10
Cache(saveRDS, obj, file = "filename.RData", cacheRepo = tmpDir)
Cache(saveRDS, obj, file = "filename.RData", cacheRepo = tmpDir)
Cache(saveRDS, obj, file = "filename.RData", cacheRepo = tmpDir) # cached copy is loaded

# however, using asPath(), cached version retrieved after being run once
Cache(saveRDS, obj, file = asPath("filename1.RData"), cacheRepo = tmpDir)
Cache(saveRDS, obj, file = asPath("filename1.RData"), cacheRepo = tmpDir) # cached copy is loaded

clearCache(tmpDir)

## cleanup
unlink(c("filename.rda", "filename1.rda"))
unlink(dirname(tmpDir), recursive = TRUE)
