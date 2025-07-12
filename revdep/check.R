## install separately; keep this order to avoid conflicts
# pak::pkg_install(c("fs", "rprojroot"))
# pak::pkg_install("HenrikBengtsson/revdepcheck.extras")
# pak::pkg_install("r-lib/revdepcheck")
# pak::pkg_install("achubaty/crancache@r-universe")

CRAN_ONLY <- TRUE

if (isTRUE(CRAN_ONLY)) {
  options(
    repos = c(
      ## note this is counter to the "Canonical CRAN.r-project.org" for CRAN packages,
      ## but revdep/ directory is .Rbuildignored and this is used for revdep checks only.
      CRAN = paste0("https://", "cloud.", "r-project.", "org")
    ),
    revdepcheck.num_workers = getOption("Ncpus", 2L)
  )

  revdeps <- rprojroot::find_package_root_file() |>
    fs::path_file() |>
    tools::package_dependencies(which = "most", recursive = FALSE, reverse = TRUE) |>
    unlist() |>
    unname()
} else {
  options(
    repos = c(
      PE = "https://predictiveecology.r-universe.dev",

      ## note this is counter to the "Canonical CRAN.r-project.org" for CRAN packages,
      ## but revdep/ directory is .Rbuildignored and this is used for revdep checks only.
      CRAN = paste0("https://", "cloud.", "r-project.", "org")
    ),
    revdepcheck.num_workers = getOption("Ncpus", 2L)
  )

  omit_pkgs <- c(
    "SpaDES.project" ## omit b/c it circularly Suggests SpaDES.config
  )

  revdeps <- c(
    revdepcheck.extras::revdep_children(),
    revdepcheck.extras::revdep_grandchildren()
  ) |>
    setdiff(omit_pkgs)
}

## reset from any previous runs
revdepcheck.extras::revdep_reset() ## clears revdep/{cache,checks,library} directories

## manual pre-installation (uses crancache)
# crancache::update_packages(lib.loc = .libPaths()[1], ask = FALSE)
# revdepcheck.extras::revdep_precache(pkgs = revdeps)

## sequentially install deps of each revdeps package in parallel;
## NOTE: PE pkgs aren't crancached, and will be installed each time.
if (Sys.info()[["sysname"]] == "Darwin") {
  fs::path("revdep", "library.noindex", revdeps) |> fs::dir_create()
} else {
  fs::path("revdep", "library", revdeps) |> fs::dir_create()
}
try(lapply(revdeps, revdepcheck:::deps_install, pkgdir = "."))

## setup and run the checks
revdepcheck.extras::revdep_init()
revdepcheck::revdep_add(packages = revdeps)
revdepcheck::revdep_todo()

revdepcheck::revdep_check(
  num_workers = getOption("revdepcheck.num_workers", 2L),
  quiet = FALSE,
  timeout = as.difftime(60, units = "mins")
)

# revdepcheck::revdep_details(".", "SpaDES.core")
revdepcheck::revdep_report(all = TRUE)
revdepcheck::revdep_report_cran() ## update cran-comments with this output

### email maintainers of revdep packages (need to edit: `revdep/email.yml`)
# revdepcheck::revdep_email(type = "broken") ## will send via gmail
# revdepcheck::revdep_email(type = "failed") ## will send via gmail
