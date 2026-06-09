test_that("getRelativePaths works as expected", {
  testInit("fs")

  ## ensure multiple e.g. module paths work;
  ## based on usage in SpaDES.core::saveSimList()
  path <- list(cachePath = structure("/mnt/scratch/myProject/cache",
                                     class = c("fs_path", "character")),
               inputPath = structure("/mnt/projects/myProject/inputs",
                                     class = c("fs_path", "character")),
               modulePath = structure(c("/home/testUser/myProject/modules",
                                        "/home/testUser/myProject/modules/scfm/modules"),
                                      class = c("fs_path", "character")),
               outputPath = structure("/mnt/projects/myProject/outputs/runName_for_rep01",
                                      class = c("fs_path", "character")),
               rasterPath = structure("/mnt/scratch/myProject/raster",
                                      class = c("fs_path", "character")),
               scratchPath = structure("/mnt/scratch/myProject",
                                       class = c("fs_path", "character")),
               terraPath = structure("/mnt/scratch/myProject/terra",
                                     class = c("fs_path", "character")))

  relativeToPath <- "/home/testUser/myProject"

  corePaths <- c("modulePath", "cachePath", "inputPath", "outputPath")

  path[corePaths] <- getRelative(path[corePaths], relativeToPath)

  expect_identical(path$cachePath, "cache")
  expect_identical(path$inputPath, "inputs")
  expect_identical(path$outputPath, file.path("outputs", "runName_for_rep01"))
  expect_identical(path$modulePath, c("modules", file.path("modules", "scfm", "modules")))
})

test_that("relativeToWhat can handle multiple paths", {
  expect_no_error({
    res <- relativeToWhat(
      file = "/mnt/projects/HRV/BC_HRV/outputs/NRD_Quesnel_scfm_hrv_FRT_res125/rep01/speciesLayers_2011_NRD_Quesnel.tif",
      cachePath = NULL,
      paths = list(
        cachePath = "/mnt/scratch/achubaty/BC_HRV/cache",
        inputPath = "/mnt/projects/HRV/BC_HRV/inputs",
        modulePath = c("/home/achubaty/GitHub/BC_HRV/modules", "/home/achubaty/GitHub/BC_HRV/modules/scfm/modules"),
        outputPath = "/mnt/projects/HRV/BC_HRV/outputs/NRD_Quesnel_scfm_hrv_FRT_res125/rep01",
        rasterPath = "/mnt/scratch/achubaty/BC_HRV/raster",
        scratchPath = "/mnt/scratch/achubaty/BC_HRV",
        terraPath = "/mnt/scratch/achubaty/BC_HRV/terra"
      )
    )
  })
  # relativeToWhat() returns a named character vector: name = the (most specific)
  #   anchor the file lives under, value = the file's directory relative to it.
  expect_identical(res, c(outputPath = "."))
})

test_that("relativeToWhat picks the most specific anchor and a true relative path", {
  paths <- list(
    cachePath = "/proj/cache",
    projectPath = "/proj",
    inputPath = "/proj/inputs"
  )
  # file several levels under inputPath -> relative to inputPath (most specific),
  #   NOT projectPath; and certainly not stored as an absolute path.
  res <- relativeToWhat("/proj/inputs/sub/dir/r.tif", cachePath = NULL, paths)
  expect_identical(names(res), "inputPath")
  expect_identical(unname(res), "sub/dir")

  # a file under no anchor -> empty anchor name + absolute dir (an "orphan")
  res2 <- relativeToWhat("/elsewhere/x/r.tif", cachePath = NULL, paths)
  expect_identical(names(res2), "")
  expect_true(fs::is_absolute_path(unname(res2)))
})
