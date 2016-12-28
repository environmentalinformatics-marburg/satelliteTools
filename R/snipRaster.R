#' Tile \code{Raster*} object based on \code{Spatial*} object
#'
#' @description
#' Tile \code{Raster*} object based on \code{Spatial*} object using an optional 
#' buffer.
#'
#' @param raster \code{Raster*} object
#' @param spatial \code{Spatial*} object
#' @param selector Selector variable, if not NULL, only one raster tile per 
#' unique value of the selector variable is created (e.g. if one has multiple
#' observations per single geographical location).
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
snipRaster <- function(raster, spatial, selector = NULL, buffer = NULL, 
                       byid = TRUE){
  if(!is.null(selector)){
    spatial <- spatial[!duplicated(spatial@data[, selector]),]
    raster_snip_ids <- spatial@data[, selector]
  }
  lspt <- length(spatial)
  plots <- lapply(seq(lspt), function(i){
    if(i %% 10 == 0) print(paste0("Processing ", i, " of ", lspt))
    spt <- spatial[i,]
    if(!is.null(buffer)){
      spt <- gBuffer(spt, width = buffer, byid = byid)
    }
    plot <- crop(raster, spt)
    return(plot)
  })
  if(!is.null(selector)){
    names(plots) <- raster_snip_ids
  }
  return(plots)
}