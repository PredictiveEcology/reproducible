test_that("prepInputs doesn't work", {
  testthat::skip("prepInputs is still experimental")
  tmpdir <- file.path(tempdir(), paste(collapse = "", sample(LETTERS,5)))
  checkPath(tmpdir, create = TRUE)
  cwd <- getwd()
  setwd(tmpdir)

  on.exit({
    setwd(cwd)
    unlink(tmpdir, recursive = TRUE)
  }, add = TRUE)


  setwd(tempdir())
  test <- prepInputs(targetFile = "FMA_Boundary_Updated.shp",
                     url = "https://drive.google.com/file/d/1nTFOcrdMf1hIsxd_yNCSTr8RrYNHHwuc/view?usp=sharing",
                     destinationPath = "data/FMA")

  # m <- "child4"
  # newModule(m, tmpdir, open = FALSE)
  # fileName <- file.path(m, paste0(m, ".R"))#child4/child4.R"
  # xxx <- readLines(fileName)
  # lineWithInit <- grep(xxx, pattern = "^Init")
  #
  #
  # xxx1 <- gsub(xxx, pattern = 'plotFun', replacement = 'Plot') # nolint
  # cat(xxx1, file = fileName, sep = "\n")
  # expect_message(simInit(paths = list(modulePath = tmpdir), modules = m),
  #                "Plot is defined")
  #
  # # do functions like raster::levels
  # cat(xxx[1:lineWithInit], "
  #     library(raster)
  #     poiuoiu <- raster(extent(0,10,0,10), vals = rep(1:2, length.out = 100))
  #     poiuoiu <- poiuoiu
  #     poiuoiu <- scale(poiuoiu)
  #     poiuoiu <- ratify(poiuoiu)
  #     rat <- raster::levels(poiuoiu)[[1]]
  #
  #     levels(poiuoiu) <- rat
  #     ",
  #     xxx[(lineWithInit+1):length(xxx)], sep = "\n", fill = FALSE, file = fileName)
  #
  # mm <- capture_messages(simInit(paths = list(modulePath = tmpdir), modules = m))
  #
  # fullMessage <- c("the following function\\(s\\) is used that",
  #                  "raster::scale", "scale")
  # expect_true(all(unlist(lapply(fullMessage, function(x) any(grepl(mm, pattern = x))))))
  # nonMessage <- c("raster::levels", "levels")
  # expect_false(all(unlist(lapply(nonMessage, function(x) any(grepl(mm, pattern = x))))))
  #
  # cat(xxx[1:lineWithInit], "
  #     library(raster)
  #     poiuoiu <- raster(extent(0,10,0,10), vals = rep(1:2, length.out = 100))
  #     poiuoiu <- scale(poiuoiu)
  #     ",
  #     xxx[(lineWithInit+1):length(xxx)], sep = "\n", fill = FALSE, file = fileName)
  #
  # expect_message(simInit(paths = list(modulePath = tmpdir), modules = m),
  #                "raster::scale")
  #
  # ###
  # cat(xxx[1:lineWithInit], "
  #     library(raster)
  #     poiuoiu <- raster(extent(0,10,0,10), vals = rep(1:2, length.out = 100))
  #     poiuoiu <- raster::scale(poiuoiu)
  #     sim$poiuoiu <- poiuoiu
  #     ",
  #     xxx[(lineWithInit+1):length(xxx)], sep = "\n", fill = FALSE, file = fileName)
  #
  # expect_message(simInit(paths = list(modulePath = tmpdir), modules = m),
  #                "poiuoiu is assigned")
  #
  # cat(xxx[1:(lineWithInit - 1)], "
  #     a <- function(x) {
  #     b <- b + 1
  #     }
  #     ",
  #     xxx[(lineWithInit):length(xxx)], sep = "\n", fill = FALSE, file = fileName)
  #
  # expect_message(simInit(paths = list(modulePath = tmpdir), modules = m),
  #                "a: parameter")
  #
  # xxx1 <- gsub(xxx, pattern = "\\.plotInitialTime", replacement = "value")
  # xxx1 <- gsub(xxx1, pattern = "NA, NA, NA", replacement = "'hi', NA, NA")
  #
  # cat(xxx1[1:lineWithInit], "
  #     a <- sim$b
  #     d <- sim$d
  #     f <- sim[['f']]
  #     f <- sim[[P(sim)$value]]
  #     poiuoiu <- sim@.envir$d1
  #     qwerqwer <- sim@.envir[['test']]
  #     sim$g <- f
  #     sim@.envir$g1 <- f
  #     return(list(a, d, f, sim))
  #     ",
  #     xxx1[(lineWithInit+1):length(xxx1)], sep = "\n", fill = FALSE, file = fileName)
  #
  # mm <- capture_messages(simInit(paths = list(modulePath = tmpdir), modules = m))
  #
  # fullMessage <- c("defineParameter: 'value' is not of specified type 'numeric'",
  #                  "defineParameter: 'plotInterval' is not of specified type 'numeric'",
  #                  "defineParameter: 'saveInitialTime' is not of specified type 'numeric'",
  #                  "defineParameter: 'saveInterval' is not of specified type 'numeric'",
  #                  "child4: module code: Init: local variable.*qwerqwer.*assigned but may not be used",
  #                  "Running inputObjects for child4", "child4: module code: Init: local variable.*poiuoiu.*assigned but may not be used",
  #                  "child4: outputObjects: g, g1 are assigned to sim inside Init, but are not declared in outputObjects",
  #                  "child4: inputObjects: b, d, f, hi, d1, test are used from sim inside Init, but are not declared in inputObjects"
  # )
  #
  # mm <- cleanMessage(mm)
  # expect_true(all(unlist(lapply(fullMessage, function(x) any(grepl(mm, pattern = x))))))
  # # cat(paste("################################################"), file = tempfile(), append = FALSE)
  # # for (x in seq(fullMessage)) {
  # #   lineNum <- "444"
  # #   theGrepEach <- grepl(mm, pattern = fullMessage[x])
  # #   theGrep <- any(theGrepEach)
  # #   if (!theGrep) {
  # #     cat(paste("\nline ", lineNum, theGrep, fullMessage[x], "\n              ", paste(mm, collapse = "\n               "), collapse = ""), file = tempfile(), append = TRUE)
  # #   }
  # #   expect_true(theGrep)
  # # }
  #
  # cat(xxx[1:lineWithInit], "
  #     sim$child4 <- 1
  #     ",
  #     xxx[(lineWithInit+1):length(xxx)], sep = "\n", fill = FALSE, file = fileName)
  #
  # expect_error(simInit(paths = list(modulePath = tmpdir), modules = m),
  #              c(paste0(m, ": You have created an object")))
  #
  # # declared in inputObjects
  # lineWithInputObjects <- grep(xxx, pattern = " expectsInput")
  # cat(xxx[1:(lineWithInputObjects-1)], "
  #     expectsInput('a', 'numeric', '', '')
  #     ",
  #     xxx[(lineWithInputObjects+1):length(xxx)], sep = "\n", fill = FALSE, file = fileName)
  #
  # expect_message(simInit(paths = list(modulePath = tmpdir), modules = m),
  #                c(paste0(m, ": module code: a is declared in inputObjects")))
  #
  # # declared in outputObjects
  # lineWithOutputObjects <- grep(xxx, pattern = " createsOutput")
  # cat(xxx[1:(lineWithOutputObjects-1)], "
  #     createsOutput('b', 'numeric', '')
  #     ",
  #     xxx[(lineWithOutputObjects+1):length(xxx)], sep = "\n", fill = FALSE, file = fileName)
  #
  # expect_message(simInit(paths = list(modulePath = tmpdir), modules = m),
  #                c(paste0(m, ": module code: b is declared in outputObjects")))
  #
  # cat(xxx[1:(lineWithInputObjects-1)], "
  #     expectsInput('a', 'numeric', '', '')
  #     ",
  #     xxx[(lineWithInputObjects+1):(lineWithOutputObjects-1)],
  #     "
  #     createsOutput('b', 'numeric', '')
  #     ",
  #     xxx[(lineWithOutputObjects+1):length(xxx)], sep = "\n", fill = FALSE, file = fileName)
  #
  # mm <- capture_messages(simInit(paths = list(modulePath = tmpdir), modules = m))
  # expect_true(all(grepl(mm,
  #                       pattern = c(paste0(m, ": module code: b is declared in outputObjects|child4: module code: a is declared in inputObjects|Running .input")))))
  #
  # # assign to sim for functions like scheduleEvent
  # lineWithScheduleEvent <- grep(xxx, pattern = "scheduleEvent")[1]
  # xxx1 <- xxx
  # xxx1[lineWithScheduleEvent] <- sub(xxx[lineWithScheduleEvent], pattern = "sim <- scheduleEvent", replacement = "scheduleEvent")
  # cat(xxx1, sep = "\n", fill = FALSE, file = fileName)
  #
  # expect_message(simInit(paths = list(modulePath = tmpdir), modules = m),
  #                c(paste0(m, ": module code: scheduleEvent inside doEvent.child4 must")))
  #
  # # Return sim in doEvent
  # patt <- "return\\(invisible\\(sim\\)\\)"
  # lineWithReturnSim <- grep(xxx, pattern = patt)[1]
  # xxx1 <- xxx
  # xxx1[lineWithReturnSim] <- sub(xxx[lineWithReturnSim], pattern = patt,
  #                                replacement = "return(invisible())")
  # cat(xxx1, sep = "\n", fill = FALSE, file = fileName)
  #
  # expect_message(simInit(paths = list(modulePath = tmpdir), modules = m),
  #                c(paste0(m, ": module code: doEvent.",m," must return")))
  #
  #
  # lineWithInputObjects <- grep(xxx, pattern = " expectsInput")
  # lineWithOutputObjects <- grep(xxx, pattern = " createsOutput")
  # lineWithDotInputObjects <- grep(xxx, pattern = "\\.inputObjects")
  # cat(xxx[1:(lineWithInputObjects-1)], "
  #     expectsInput('ei1', 'numeric', '', ''),
  #     expectsInput('ei2', 'numeric', '', ''),
  #     expectsInput('ei3', 'numeric', '', ''),
  #     expectsInput('ei4', 'numeric', '', '')
  #     ",
  #     xxx[(lineWithInputObjects+1):(lineWithOutputObjects-1)], "
  #     createsOutput('co1', 'numeric', ''),
  #     createsOutput('co2', 'numeric', ''),
  #     createsOutput('co3', 'numeric', ''),
  #     createsOutput('co4', 'numeric', '')
  #     ",
  #     xxx[(lineWithOutputObjects+1):lineWithInit], "
  #     a <- sim$b
  #     sim$g <- f
  #     holy(sim$co4) <- f
  #     moly(sim$aaa) <- f
  #     fff <- sim$ei2
  #     fff <- sim$co3
  #     sim$co1 <- 123
  #     xx <- c(1,2)
  #     xx[sim$ei4] <- NA
  #     ",
  #     xxx[(lineWithInit+1):lineWithDotInputObjects], "
  #     a <- sim$b
  #     sim$g <- 1
  #     sim$ei1 <- 4
  #     fff <- sim$ei1
  #     fff <- sim$co3
  #     sim$co1 <- 123
  #     aaa <- sim$.userSuppliedObjNames # in the ignoreObjects
  #     ",
  #     xxx[(lineWithDotInputObjects+1):length(xxx)],
  #     sep = "\n", fill = FALSE, file = fileName)
  #
  # fullMessage <- c("Running inputObjects for child4", "child4: module code: co2, co3 are declared in outputObjects, but are not assigned in the module",
  #                  "child4: module code: ei2, ei3, ei4 are declared in inputObjects, but no default\\(s\\) are provided in inputObjects",
  #                  "child4: module code: ei3 is declared in inputObjects, but is not used in the module",
  #                  "child4: module code: inputObjects: local variable.*a.*assigned but may not be used",
  #                  "child4: module code: inputObjects: local variable.*fff.*assigned but may not be used",
  #                  "child4: module code: Init: local variable.*a.*assigned but may not be used",
  #                  "child4: module code: Init: local variable.*fff.*assigned but may not be used",
  #                  "child4: outputObjects: g, aaa are assigned to sim inside Init, but are not declared in outputObjects",
  #                  "child4: inputObjects: g, co1 are assigned to sim inside inputObjects, but are not declared in inputObjects",
  #                  "child4: inputObjects: b, aaa are used from sim inside Init, but are not declared in inputObjects",
  #                  "child4: inputObjects: b, co3 are used from sim inside inputObjects, but are not declared in inputObjects"
  # )
  #
  # mm <- capture_messages(simInit(paths = list(modulePath = tmpdir), modules = m))
  # mm <- cleanMessage(mm)
  # expect_true(all(unlist(lapply(fullMessage, function(x) any(grepl(mm, pattern = x))))))

})
