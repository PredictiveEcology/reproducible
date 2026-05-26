#!/usr/bin/env Rscript
#
# benchmark-postProcessTo-memory.R
# ---------------------------------
# Profile how terra's memory settings affect postProcessTo() on a large raster.
#
# The question this answers: on a high-RAM machine, is it faster to let terra
# pull whole rasters into RAM (todisk=FALSE, big memmax) or to force chunked /
# on-disk streaming (todisk=TRUE, or a small memmax)? It also records peak RSS
# and the result datatype, so you can see (a) the in-memory vs disk wall-time
# gap -- that gap IS your I/O cost -- and (b) whether any step silently promotes
# a 1-byte categorical raster to an 8-byte float.
#
# Design notes (important for a fair test on a 1TB box):
#   * Each config runs in a FRESH Rscript subprocess so /proc/self/status:VmHWM
#     reflects the peak RSS of that run alone (VmHWM is a per-process high-water
#     mark that can't be reset within a process).
#   * The source is read once to warm the OS page cache before timing, and each
#     config runs `reps` times, because on a 1TB box terra's "on-disk" temp
#     files mostly live in page cache -- so cold vs warm reads, not the terra
#     setting, can dominate if you don't control for it.
#   * "disk" wall time on this machine is therefore really "page-cache" time;
#     that's fine -- wall time is what you care about either way -- just don't
#     read it as platter I/O.
#
# Usage:
#   Rscript dev/benchmark-postProcessTo-memory.R                # full sweep
#   PPTO_SRC=/path/to/rstLCC.tif Rscript dev/benchmark-...R     # use real file
#   PPTO_RES=250 Rscript dev/benchmark-...R                     # coarser = quick
#
# Tunables (env vars, all optional):
#   PPTO_SRC   path to source raster. If unset/missing, a categorical raster of
#              the same dims as the example is synthesized from `cls` below.
#   PPTO_RES   output resolution in target CRS units (default: native 30).
#   PPTO_REPS  replicates per config (default: 2).
#   PPTO_TCRS  target CRS for projectTo (default: EPSG:3978, Canada Atlas LCC).
#   PPTO_SIZE  random study area size in m^2 (default 1e10 = 100x100 km).
#   PPTO_SEED  seed for the random study area + its center (default 42).
#
# Vary the geometry to map the response surface, e.g.:
#   PPTO_TCRS=EPSG:4326 PPTO_RES=0.0025 Rscript dev/benchmark-...R   # to longlat
#   PPTO_SIZE=1e11 PPTO_SEED=7          Rscript dev/benchmark-...R   # bigger area

suppressWarnings(suppressMessages({
  ok <- requireNamespace("terra", quietly = TRUE)
}))
if (!ok) stop("terra is required")

## ----- the categorical scheme from the example raster -----------------------
cls <- data.frame(
  value = c(20, 31, 32, 33, 40, 50, 80, 81, 100, 210, 220, 230, 240),
  label = c("water", "snow_ice", "rock_rubble", "exposed_barren_land",
            "bryoids", "shrubs", "wetland", "wetland_treed", "herbs",
            "coniferous", "broadleaf", "mixedwood", "disturbed")
)

## ----- shared config ---------------------------------------------------------
SRC    <- Sys.getenv("PPTO_SRC", "")
RES    <- as.numeric(Sys.getenv("PPTO_RES", "30"))
REPS   <- as.integer(Sys.getenv("PPTO_REPS", "2"))
TCRS   <- Sys.getenv("PPTO_TCRS", "EPSG:3978")
SIZE   <- as.numeric(Sys.getenv("PPTO_SIZE", "1e12"))   # study area size, m^2 (~700x700km, large enough to stress memory)
SEED   <- as.integer(Sys.getenv("PPTO_SEED", "42"))     # reproducible random SA + center

# Example raster geometry (from the user's rstLCC), used when synthesizing.
EX_NROW <- 49000L; EX_NCOL <- 29167L
EX_EXT  <- c(-2138131, -1263121, 1089918, 2559918)   # xmin xmax ymin ymax
EX_CRS  <- "ESRI:102002"  # Lambert Conformal Conic 2SP (Canada), close to source

peakRSS_GB <- function() {
  # Linux: VmHWM = peak resident set size of THIS process.
  f <- "/proc/self/status"
  if (!file.exists(f)) return(NA_real_)
  l <- grep("^VmHWM:", readLines(f), value = TRUE)
  if (!length(l)) return(NA_real_)
  kb <- as.numeric(gsub("[^0-9]", "", l))
  round(kb / 1024 / 1024, 3)
}

terraTmpUsageGB <- function() {
  # Bytes of terra temp files created in THIS session (proxy for spill volume).
  tf <- tryCatch(terra::tmpFiles(current = TRUE, remove = FALSE),
                 error = function(e) character(0))
  tf <- tf[file.exists(tf)]
  if (!length(tf)) return(0)
  round(sum(file.size(tf)) / 1024^3, 3)
}

makeSource <- function() {
  if (nzchar(SRC) && file.exists(SRC)) {
    r <- terra::rast(SRC)
    # Restore categorical levels so we test whether postProcessTo PRESERVES them
    # (terra temp tifs drop the RAT). Only applies if values match the scheme.
    if (!terra::is.factor(r)[1]) {
      vals <- terra::unique(r)[, 1]
      if (all(vals %in% c(0, cls$value))) levels(r) <- cls
    }
    return(r)
  }
  message("PPTO_SRC not found; synthesizing a categorical raster matching the example.")
  r <- terra::rast(nrows = EX_NROW, ncols = EX_NCOL,
                   xmin = EX_EXT[1], xmax = EX_EXT[2],
                   ymin = EX_EXT[3], ymax = EX_EXT[4],
                   crs = EX_CRS)
  vals <- sample(cls$value, size = terra::ncell(r), replace = TRUE)
  terra::values(r) <- vals
  levels(r) <- cls
  # write to a temp tif so reads behave like the real workload (not in-memory)
  tf <- file.path(tempdir(), "synth_rstLCC.tif")
  terra::writeRaster(r, tf, overwrite = TRUE, datatype = "INT1U")
  terra::rast(tf)
}

# Build a realistic crop/project/mask workload: a SpaDES.tools random study area
# dropped at a random point in the middle of `from`, then reprojected to the
# target CRS, plus a template raster at RES. CRS, res, size, seed all tunable.
makeTargets <- function(from) {
  if (!requireNamespace("SpaDES.tools", quietly = TRUE))
    stop("SpaDES.tools is required for randomStudyArea (set PPTO_SIZE/SEED)")
  set.seed(SEED)
  e  <- as.vector(terra::ext(from))             # xmin xmax ymin ymax
  dx <- (e[2] - e[1]); dy <- (e[4] - e[3])
  # random center within the central 50% of the extent ("somewhere in the middle")
  cx <- runif(1, e[1] + 0.25 * dx, e[2] - 0.25 * dx)
  cy <- runif(1, e[3] + 0.25 * dy, e[4] - 0.25 * dy)
  center <- terra::vect(cbind(cx, cy), crs = terra::crs(from))
  sa <- SpaDES.tools::randomStudyArea(center = center, size = SIZE, seed = SEED)
  if (!inherits(sa, "SpatVector")) sa <- terra::vect(sa)
  if (is.na(terra::crs(sa)) || !nzchar(terra::crs(sa)))
    terra::crs(sa) <- terra::crs(from)
  saT <- terra::project(sa, TCRS)               # study area in target CRS
  templ <- terra::rast(saT, resolution = RES)   # template controls res + grid
  list(studyArea = saT, template = templ)
}

## ----- single-config worker --------------------------------------------------
runOne <- function(cfg) {
  # cfg: named list todisk(logical) memfrac(num) memmax(num/NA) datatype(chr/NA)
  if (!is.na(cfg$memmax)) terra::terraOptions(memmax = cfg$memmax) else
    terra::terraOptions(memmax = NA)
  # Shared machines: never let terra grab more than 10% of RAM.
  cfg$memfrac <- min(cfg$memfrac, 0.1)
  terra::terraOptions(todisk = cfg$todisk, memfrac = cfg$memfrac, progress = 0)
  terra::tmpFiles(current = TRUE, remove = TRUE)  # clear our own temp baseline

  from <- makeSource()
  tg   <- makeTargets(from)

  if (requireNamespace("pkgload", quietly = TRUE) &&
      file.exists("DESCRIPTION")) {
    suppressMessages(pkgload::load_all(quiet = TRUE))
  } else {
    suppressMessages(library(reproducible))
  }

  gc(reset = TRUE)
  t <- system.time({
    out <- reproducible::postProcessTo(
      from      = from,
      cropTo    = tg$studyArea,
      maskTo    = tg$studyArea,
      projectTo = tg$template,
      verbose   = FALSE
    )
  })

  # datatype() is "" for in-memory rasters, so write once (post-timing) to learn
  # the would-be on-disk datatype -- this is where INT1U->FLT8S promotion shows.
  dtype <- tryCatch({
    dt <- terra::datatype(out)[1]
    if (is.na(dt) || !nzchar(dt)) {
      ck <- file.path(tempdir(), "ppto_dtype_check.tif")
      terra::writeRaster(out, ck, overwrite = TRUE); dt <- terra::datatype(terra::rast(ck))[1]
    }
    dt
  }, error = function(e) NA)

  list(
    elapsed_s = round(unname(t["elapsed"]), 2),
    user_s    = round(unname(t["user.self"]), 2),
    peakRSS_GB = peakRSS_GB(),
    terraTmp_GB = terraTmpUsageGB(),
    out_datatype = dtype,
    out_isFactor = tryCatch(terra::is.factor(out)[1], error = function(e) NA),
    out_inMemory = tryCatch(terra::inMemory(out)[1], error = function(e) NA),
    out_ncell = tryCatch(format(terra::ncell(out), big.mark = ","),
                         error = function(e) NA)
  )
}

## ----- worker entry (invoked as a subprocess) --------------------------------
if (nzchar(Sys.getenv("PPTO_WORKER"))) {
  cfg <- list(
    todisk  = as.logical(Sys.getenv("PPTO_TODISK")),
    memfrac = as.numeric(Sys.getenv("PPTO_MEMFRAC")),
    memmax  = suppressWarnings(as.numeric(Sys.getenv("PPTO_MEMMAX"))) # NA if ""
  )
  res <- runOne(cfg)
  cat("##RESULT##",
      paste(names(res), vapply(res, as.character, ""), sep = "=", collapse = ";"),
      "\n", sep = "")
  quit(save = "no")
}

## ----- driver: warm cache, sweep configs in fresh processes ------------------
message(sprintf("source=%s  res=%s  reps=%d  targetCRS=%s",
                if (nzchar(SRC)) SRC else "(synth)", RES, REPS, TCRS))

# Warm the OS page cache for the source so config order doesn't bias reads.
invisible(makeSource())

# Config grid. memmax in GB; NA => use memfrac. Edit freely.
# NOTE: these are SHARED machines -- memfrac must never exceed 0.1. The
# in-memory vs disk contrast is therefore driven by todisk and memmax, not
# memfrac (0.1 of 1TB = 100GB, still ample to hold these rasters in RAM).
MEMFRAC <- 0.1
grid <- list(
  list(name = "mem_inRAM",   todisk = FALSE, memfrac = MEMFRAC, memmax = NA),
  list(name = "disk_todisk", todisk = TRUE,  memfrac = MEMFRAC, memmax = NA),
  list(name = "memmax_4GB",  todisk = FALSE, memfrac = MEMFRAC, memmax = 4),
  list(name = "memmax_16GB", todisk = FALSE, memfrac = MEMFRAC, memmax = 16),
  list(name = "memmax_64GB", todisk = FALSE, memfrac = MEMFRAC, memmax = 64)
)

self <- normalizePath(sub("^--file=", "",
          grep("^--file=", commandArgs(FALSE), value = TRUE)[1]))

rows <- list()
for (cfg in grid) {
  for (rep in seq_len(REPS)) {
    env <- c(
      sprintf("PPTO_WORKER=1"),
      sprintf("PPTO_TODISK=%s", cfg$todisk),
      sprintf("PPTO_MEMFRAC=%s", cfg$memfrac),
      sprintf("PPTO_MEMMAX=%s", ifelse(is.na(cfg$memmax), "", cfg$memmax)),
      sprintf("PPTO_SRC=%s", SRC),
      sprintf("PPTO_RES=%s", RES),
      sprintf("PPTO_TCRS=%s", TCRS),
      sprintf("PPTO_SIZE=%s", SIZE),
      sprintf("PPTO_SEED=%s", SEED)
    )
    out <- system2("Rscript", self, env = env, stdout = TRUE, stderr = "")
    line <- grep("##RESULT##", out, value = TRUE)   # marker may be mid-line
    if (!length(line)) {
      message(sprintf("  [%s rep%d] FAILED; worker output:", cfg$name, rep))
      message(paste(tail(out, 15), collapse = "\n"))
      next
    }
    kv <- strsplit(sub("^.*##RESULT##", "", line[1]), ";")[[1]]
    rec <- as.list(sub("^[^=]+=", "", kv))
    names(rec) <- sub("=.*$", "", kv)
    rec$config <- cfg$name; rec$rep <- rep
    rows[[length(rows) + 1]] <- rec
    message(sprintf("  [%s rep%d] %ss  peakRSS=%sGB  tmp=%sGB  dtype=%s",
                    cfg$name, rep, rec$elapsed_s, rec$peakRSS_GB,
                    rec$terraTmp_GB, rec$out_datatype))
  }
}

df <- do.call(rbind, lapply(rows, function(r)
  as.data.frame(r, stringsAsFactors = FALSE)))
num <- c("elapsed_s", "user_s", "peakRSS_GB", "terraTmp_GB")
df[num] <- lapply(df[num], as.numeric)

cat("\n=== raw ===\n"); print(df, row.names = FALSE)

agg <- aggregate(cbind(elapsed_s, peakRSS_GB, terraTmp_GB) ~ config,
                 data = df, FUN = median)
agg <- agg[order(agg$elapsed_s), ]
cat("\n=== median per config (sorted by wall time) ===\n")
print(agg, row.names = FALSE)

outCsv <- "dev/benchmark-postProcessTo-memory-results.csv"
write.csv(df, outCsv, row.names = FALSE)
cat(sprintf("\nWrote %s\n", outCsv))
cat("\nRead: the elapsed gap between the fastest in-memory config and the\n",
    "todisk config is your I/O cost. If 'disk' wins or ties, terra's chunked\n",
    "path beats RAM-loading here, and pipeline-fusion work is not worth it.\n",
    "Watch out_datatype: anything other than INT1U means a step promoted the\n",
    "1-byte categorical to a wider type -- fix that before tuning memory.\n", sep = "")
