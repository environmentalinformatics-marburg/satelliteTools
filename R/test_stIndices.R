#' Compute spectral remote sensing pixel-based indices
#'
#' @param sat a satellite::satellite class data set
#'
#' @return a raster::stack of the indices
#' @export stIndices
#'
#' @examples
#' \dontrun{
#' }
stIndices <- function(sat, ...){
  
  cppFunction('List be_re_indices_cpp(NumericVector blue, NumericVector green, NumericVector red, NumericVector re, NumericVector nir){
              
              NumericVector ndvi = (nir - red) / (nir + red);
              NumericVector gndvi = (nir - green) / (nir + green);
              NumericVector sr = nir/red;
              NumericVector ndvire = (nir - re)/(nir + re);
              NumericVector srre = nir/re;
              NumericVector rtvicore = 100 * (nir - re) - 10 * (nir - green);
              
              return List::create(ndvi, gndvi, sr, srre, rtvicore);
}')
  
  blue <- raster::getValues(satellite::getSatDataLayers(sat, "B001n")[[1]])
  green <- raster::getValues(satellite::getSatDataLayers(sat, "B002n")[[1]])
  red <- raster::getValues(satellite::getSatDataLayers(sat, "B003n")[[1]])
  re <- raster::getValues(satellite::getSatDataLayers(sat, "B004n")[[1]])
  nir <- raster::getValues(satellite::getSatDataLayers(sat, "B005n")[[1]])
  
  indices <- be_re_indices_cpp(blue, green, red, re, nir)
  ndvi <- raster::raster(satellite::getSatDataLayers(sat, "B001n")[[1]])
  ndvi[] <- indices[[1]]
  names(ndvi) <- "ndvi"
  
  gndvi <- raster::raster(satellite::getSatDataLayers(sat, "B001n")[[1]])
  gndvi[] <- indices[[2]]
  names(gndvi) <- "gndvi"
  
  sr <- raster::raster(satellite::getSatDataLayers(sat, "B001n")[[1]])
  sr[] <- indices[[3]]
  names(sr) <- "sr"
  
  srre <- raster::raster(satellite::getSatDataLayers(sat, "B001n")[[1]])
  srre[] <- indices[[4]]
  names(srre) <- "srre"
  
  rtvicore <- raster::raster(satellite::getSatDataLayers(sat, "B001n")[[1]])
  rtvicore[] <- indices[[5]]
  names(rtvicore) <- "rtvicore"
  
  return(raster::stack(ndvi, gndvi, sr, srre, rtvicore))
  }
