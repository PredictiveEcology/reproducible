## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(#cache = TRUE,
                      message = FALSE, warning = FALSE,
                      echo = TRUE, eval = FALSE)

## -----------------------------------------------------------------------------
#  cachedb <- config::get("cachedb")
#  
#  conn <- DBI::dbConnect(drv = RPostgres::Postgres(),
#                        host = cachedb$server,
#                        port = cachedb$port,
#                        dbname = cachedb$database,
#                        user = cachedb$user,
#                        password = cachedb$password)
#  
#  options("reproducible.conn" = conn) # sets the default connection globally

## -----------------------------------------------------------------------------
#  readRenviron(".Renviron") ## alternatively, use global ~/.Renviron
#  
#  conn <-   DBI::dbConnect(drv = RPostgres::Postgres(),
#                           host = Sys.getenv("PGHOST"),
#                           port = Sys.getenv("PGPORT"),
#                           dbname = Sys.getenv("PGDATABASE"),
#                           user = Sys.getenv("PGUSER"),
#                           password = Sys.getenv("PGPASSWORD"))
#  
#  options("reproducible.conn" = conn) # sets the default connection globally

