#' Tile \code{Raster*} object based on \code{Spatial*} object
#'
#' @description
#' Tile \code{Raster*} object based on \code{Spatial*} object using an optional 
#' buffer.
#'
#' @param raster \code{Raster*} object
#' @param spatial \code{Spatial*} object
#' @param buffer Buffer size (default NULL does not use buffer)
#' @param byid Parameter of \code{gBuffer} which is used in case a buffer should
#' be computed.
#'
#' @return
#' A list of \code{Raster*} objects with one object per vector within variable
#' spatial.
#'
#' @author
#' Thomas Nauss
#'
#' @references
#' NONE
#'
#' @examples
#' \notrun{
#' }
#'
#' @export snipRaster
#' @name snipRaster
#'
snipRaster <- function(raster, spatial, buffer = NULL, byid = TRUE)
  plots <- lapply(seq(length(spatial)), function(s){
    spt <- spatial[s,]
    if(!is.null(buffer)){
      spt <- gBuffer(spt, width = buffer, byid = byid)
    }
    plot <- crop(raster, spt)
    return(plot)
  })
