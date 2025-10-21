data.table::setDTthreads(2)
tmpDir <- tempdir()
opts <- options(reproducible.cachePath = tmpDir)

# Usage -- All below are equivalent; even where args are missing or provided,
#   Cache evaluates using default values, if these are specified in formals(FUN)
a <- list()
b <- list(fun = rnorm)
bbb <- 1
ee <- new.env(parent = emptyenv())
ee$qq <- bbb

a[[1]] <- Cache(rnorm(1)) # no evaluation prior to Cache
a[[2]] <- Cache(rnorm, 1) # no evaluation prior to Cache
a[[3]] <- Cache(do.call, rnorm, list(1))
a[[4]] <- Cache(do.call(rnorm, list(1)))
a[[5]] <- Cache(do.call(b$fun, list(1)))
a[[6]] <- Cache(do.call, b$fun, list(1))
a[[7]] <- Cache(b$fun, 1)
a[[8]] <- Cache(b$fun(1))
a[[10]] <- Cache(quote(rnorm(1)))
a[[11]] <- Cache(stats::rnorm(1))
a[[12]] <- Cache(stats::rnorm, 1)
a[[13]] <- Cache(rnorm(1, 0, get("bbb", inherits = FALSE)))
a[[14]] <- Cache(rnorm(1, 0, get("qq", inherits = FALSE, envir = ee)))
a[[15]] <- Cache(rnorm(1, bbb - bbb, get("bbb", inherits = FALSE)))
a[[16]] <- Cache(rnorm(sd = 1, 0, n = get("bbb", inherits = FALSE))) # change order
a[[17]] <- Cache(rnorm(1, sd = get("ee", inherits = FALSE)$qq), mean = 0)

# with base pipe -- this is put in quotes ('') because R version 4.0 can't understand this
#  if you are using R >= 4.1 or R >= 4.2 if using the _ placeholder,
#  then you can just use pipe normally
usingPipe1 <- "b$fun(1) |> Cache()"  # base pipe

# For long pipe, need to wrap sequence in { }, or else only last step is cached
usingPipe2 <-
  '{"bbb" |>
      parse(text = _) |>
      eval() |>
      rnorm()} |>
    Cache()'
if (getRversion() >= "4.1") {
  a[[9]] <- eval(parse(text = usingPipe1)) # recovers cached copy
}
if (getRversion() >= "4.2") { # uses the _ placeholder; only available in R >= 4.2
  a[[18]] <- eval(parse(text = usingPipe2)) # recovers cached copy
}

length(unique(a)) == 1 #  all same

### Pipe -- have to use { } or else only final function is Cached
if (getRversion() >= "4.1") {
  b1a <- 'sample(1e5, 1) |> rnorm() |> Cache()'
  b1b <- 'sample(1e5, 1) |> rnorm() |> Cache()'
  b2a <- '{sample(1e5, 1) |> rnorm()} |> Cache()'
  b2b <- '{sample(1e5, 1) |> rnorm()} |> Cache()'
  b1a <- eval(parse(text = b1a))
  b1b <- eval(parse(text = b1b))
  b2a <- eval(parse(text = b2a))
  b2b <- eval(parse(text = b2b))
  all.equal(b1a, b1b) # Not TRUE because the sample is run first
  all.equal(b2a, b2b) # TRUE because of {  }, sample is not run
}

#########################
# Advanced examples
#########################

# .cacheExtra -- add something to digest
Cache(rnorm(1), .cacheExtra = "sfessee11") # adds something other than fn args
Cache(rnorm(1), .cacheExtra = "nothing") # even though fn is same, the extra is different

# omitArgs -- remove something from digest (kind of the opposite of .cacheExtra)
Cache(rnorm(2, sd = 1), omitArgs = "sd") # removes one or more args from cache digest
Cache(rnorm(2, sd = 2), omitArgs = "sd") # b/c sd is not used, this is same as previous

# cacheId -- force the use of a digest -- can give undesired consequences
Cache(rnorm(3), cacheId = "k323431232") # sets the cacheId for this call
Cache(runif(14), cacheId = "k323431232") # recovers same as above, i.e, rnorm(3)

# Turn off Caching session-wide
opts <- options(reproducible.useCache = FALSE)
Cache(rnorm(3)) # doesn't cache
options(opts)

# showSimilar can help with debugging why a Cache call isn't picking up a cached copy
Cache(rnorm(4), showSimilar = TRUE) # shows that the argument `n` is different

###############################################
# devMode -- enables cache database to stay
#            small even when developing code
###############################################
opt <- options("reproducible.useCache" = "devMode")
clearCache(tmpDir, ask = FALSE)
centralTendency <- function(x) {
  mean(x)
}
funnyData <- c(1, 1, 1, 1, 10)
uniqueUserTags <- c("thisIsUnique", "reallyUnique")
ranNumsB <- Cache(centralTendency, funnyData, cachePath = tmpDir,
                  userTags = uniqueUserTags) # sets new value to Cache
showCache(tmpDir) # 1 unique cacheId -- cacheId is 71cd24ec3b0d0cac

# During development, we often redefine function internals
centralTendency <- function(x) {
  median(x)
}
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
