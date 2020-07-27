################################################################################
#' The \code{spatialClasses} class
#'
#' This class is the union of several spatial objects from \pkg{raster} and
#' \pkg{sp} packages.
#'
#' Members:
#'
#' \itemize{
#'   \item \code{RasterLayer}, \code{RasterLayerSparse}, \code{RasterStack};
#'   \item \code{SpatialLines}, \code{SpatialLinesDataFrame};
#'   \item \code{SpatialPixels}, \code{SpatialPixelsDataFrame};
#'   \item \code{SpatialPoints}, \code{SpatialPointsDataFrame};
#'   \item \code{SpatialPolygons}, \code{SpatialPolygonsDataFrame}.
#' }
#'
#' Notably missing is \code{RasterBrick}, for now.
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
              members = c("RasterLayer", "RasterLayerSparse", "RasterStack",
                          "SpatialLines", "SpatialLinesDataFrame",
                          "SpatialPixels", "SpatialPixelsDataFrame",
                          "SpatialPoints", "SpatialPointsDataFrame",
                          "SpatialPolygons", "SpatialPolygonsDataFrame")
)
