#' Compute principal component analysis
#'
#' @param sat a satellite::satellite class data set
#'
#' @return a raster::stack of the indices
#' @export stPCA
#'
#' @examples
#' \dontrun{
#' }
stPCA <- function(sat, ...){
  
  mtrx <- as.matrix(stack(satellite::getSatDataLayers(sat)))
  
  mtrx <- matrix(c(4.0,4.2,3.9,4.3,4.1,2.0,2.1,2.0,2.1,2.2,0.60,0.59,0.58,0.62,0.63),
                 5, 3)
  
  sos <- apply(mtrx, 2, function(x) sum(x))
  sosm <- sos / nrow(mtrx)
  
  mtrxm <- t(t(mtrx) - sosm)
  
  cp <- crossprod(mtrxm)
  cpm <- cp / nrow(mtrxm)
  
  pca <- princomp(covmat = cpm)
  predict(pca, mtrx)
  
  # mtrx <- as.matrix(stack(satellite::getSatDataLayers(sat)))
  
  # pca <- rcpp_stPCA(mtrx)

  return(pca)
  }
