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
