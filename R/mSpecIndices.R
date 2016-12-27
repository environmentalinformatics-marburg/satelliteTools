#' Compute spectral remote sensing pixel-based indices
#'
#' @param sat a satellite::satellite class data set
#'
#' @return a raster::stack of the indices
#' @name mSpecIndices
#' @export mSpecIndices
#'
#' @examples
#' \dontrun{
#' }
mSpecIndices <- function(blue, green, red, nir){
  vblue <- getValues(blue)
  vgreen <- getValues(green)
  vred <- getValues(red)
  vnir <- getValues(nir)
  
  indices <- mSpecIndicesCPP(vblue, vgreen, vred, vnir)
  ndvi <- setValues(blue, indices[[1]])
  names(ndvi) <- "ndvi"
  
  gndvi <- setValues(blue, indices[[2]])
  names(gndvi) <- "gndvi"

  sr <- setValues(blue, indices[[3]])
  names(sr) <- "sr"

  return(raster::stack(ndvi, gndvi, sr))
}
