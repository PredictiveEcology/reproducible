
rebuildColors <- function(x, origColors) {
  if (isTRUE(all(origColors$origMinValue != minFn(x)) || all(origColors$origMaxValue != maxFn(x)) ||
    !identical(.getColors(x)[[1]], origColors$origColors))) {
    colorSequences <- unlist(lapply(seq_along(origColors$origMinValue), function(ind) {
      origColors$origMinValue[ind]:origColors$origMaxValue[ind]
    }))
    if (isTRUE(length(origColors$origColors) == length(colorSequences))) {
      newSeq <- minFn(x):maxFn(x)
      oldSeq <- origColors$origMinValue:origColors$origMaxValue
      whFromOld <- match(newSeq, oldSeq)
      x@legend@colortable <- origColors$origColors[whFromOld]
    }
  }
  x
}

.getColors <- function(object) {
  if (.isSpatRaster(object)) {
    cols <- terra::coltab(object)
  } else {
    nams <- names(object)
    cols <- lapply(nams, function(x) {
      as.character(object[[x]]@legend@colortable)
    })
    names(cols) <- nams
  }
  return(cols)
}
