################################################################################
#' The `spatialClasses` class
#'
#' This class is the union of several spatial objects from \pkg{raster} and
#' \pkg{sp} packages.
#'
#' Members:
#'
#' \itemize{
#'   \item `RasterLayer`, `RasterLayerSparse`, `RasterStack`;
#'   \item `SpatialLines`, `SpatialLinesDataFrame`;
#'   \item `SpatialPixels`, `SpatialPixelsDataFrame`;
#'   \item `SpatialPoints`, `SpatialPointsDataFrame`;
#'   \item `SpatialPolygons`, `SpatialPolygonsDataFrame`.
#' }
#'
#' Notably missing is `RasterBrick`, for now.
#'
#' @aliases spatialClasses
#' @importClassesFrom sp SpatialLines
#' @importClassesFrom sp SpatialLinesDataFrame
#' @importClassesFrom sp SpatialPixels
#' @importClassesFrom sp SpatialPixelsDataFrame
#' @importClassesFrom sp SpatialPoints
#' @importClassesFrom sp SpatialPointsDataFrame
#' @importClassesFrom sp SpatialPolygons
#' @importClassesFrom sp SpatialPolygonsDataFrame
#' @name spatialClasses-class
#' @rdname spatialClasses-class
#' @author Eliot McIntire
#' @exportClass spatialClasses
setClassUnion(name = "spatialClasses",
              members = c("RasterLayer", "RasterLayerSparse", "RasterStack", "RasterBrick",
                          "SpatialLines", "SpatialLinesDataFrame",
                          "SpatialPixels", "SpatialPixelsDataFrame",
                          "SpatialPoints", "SpatialPointsDataFrame",
                          "SpatialPolygons", "SpatialPolygonsDataFrame")
)
