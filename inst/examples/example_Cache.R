tmpDir <- file.path(tempdir())

# Basic use
ranNumsA <- Cache(rnorm, 10, 16, cacheRepo = tmpDir)

# All same
ranNumsB <- Cache(rnorm, 10, 16, cacheRepo = tmpDir) # recovers cached copy
ranNumsD <- Cache(quote(rnorm(n = 10, 16)), cacheRepo = tmpDir) # recovers cached copy

###############################################
# experimental devMode
###############################################
opt <- options("reproducible.useCache" = "devMode")
clearCache(tmpDir, ask = FALSE)
centralTendency <- function(x)
  mean(x)
funnyData <- c(1, 1, 1, 1, 10)
uniqueUserTags <- c("thisIsUnique", "reallyUnique")
ranNumsB <- Cache(centralTendency, funnyData, cacheRepo = tmpDir,
                  userTags = uniqueUserTags) # sets new value to Cache
showCache(tmpDir) # 1 unique artifact -- cacheId is 71cd24ec3b0d0cac

# During development, we often redefine function internals
centralTendency <- function(x)
  median(x)
# When we rerun, we don't want to keep the "old" cache because the function will
#   never again be defined that way. Here, because of userTags being the same,
#   it will replace the entry in the Cache, effetively overwriting it, even though
#   it has a different cacheId
ranNumsD <- Cache(centralTendency, funnyData, cacheRepo = tmpDir, userTags = uniqueUserTags)
showCache(tmpDir) # 1 unique artifact -- cacheId is bb1195b40c8d37a60fd6004e5d526e6b

# If it finds it by cacheID, doesn't matter what the userTags are
ranNumsD <- Cache(centralTendency, funnyData, cacheRepo = tmpDir, userTags = "thisIsUnique")

# Can use base pipe (does not with magrittr pipe!), and mix and match ways of calling
a <- rnorm(22) |> Cache()
b <- Cache(rnorm(22))
d <- Cache(rnorm, 22)
all.equal(a, b, check.attributes = F) # these random calls are same
all.equal(a, d, check.attributes = F) # these random calls are same
attr(a, ".Cache") # new item
attr(b, ".Cache") # not new --> recovered
attr(d, ".Cache") # not new --> recovered

# Can chain many using base pipe --> note Cache is as if it is "first", so
#    randomness is captured, even if it is called first in the chain
a <- rnorm(42) |> mean() |> Cache()
b <- Cache(mean(rnorm(42)))
all.equal(a, b, check.attributes = F) # these random calls are same

# longer works, even with _ placeholder
a <- rnorm(42, sd = 20) |> sd() |> rnorm(1, mean = _) |> Cache()
b <- Cache(rnorm(1, sd(rnorm(42, sd = 20))))
all.equal(a, b, check.attributes = F) # these random calls are same

options(opt)

# For more in depth uses, see vignette
\dontrun{
  # To use Postgres, set environment variables with the required credentials
  if (requireNamespace("RPostgres")) {
    Sys.setenv(PGHOST = "server.url")
    Sys.setenv(PGPORT = 5432)
    Sys.setenv(PGDATABASE = "mydatabase")
    Sys.setenv(PGUSER = "mydbuser")
    Sys.setenv(PGPASSWORD = "mysecurepassword")

    conn <- DBI::dbConnect(RPostgres::Postgres())
    options("reproducible.conn" = conn)

    # Will use postgres for cache data table, and tempdir() for saved R objects
    Cache(rnorm, 1, cacheRepo = tempdir())
  }

  browseVignettes(package = "reproducible")
}
