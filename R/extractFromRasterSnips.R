#' Extract data from \code{Raster*}  snips based on selector from 
#' \code{Spatial*} object 
#'
#' @description
#' Extract data from \code{Raster*} snips based on selector from 
#' \code{Spatial*} objects. Aside from the actual raster values, the mean and
#' standard deviation of raster values within a buffer can be returned.
#' 
#' This function is usefull if the snips have been
#' computed by \code{\link{snipRaster}} using a selector ID since in this case,
#' the sequence of snips is not related to the sequence of \code{Spatial*}
#' entities in the vector layer.
#'
#' @param raster \code{Raster*} object
#' @param spatial \code{SpatialPointDataFrame} object
#' @param selector Selector variable, if not NULL, only one raster tile per 
#' unique value of the selector variable is created (e.g. if one has multiple
#' observations per single geographical location).
#' @param buffer Buffer size (default NULL does not use buffer) if the mean and
#' standard deviation within the buffer should be retruned, too.
#'
#' @return
#' A \code{SpatialPointDataFrame} object with the values of the raster added to the attribute
#' table.
#'
#' @author
#' Thomas Nauss
#'
#' @references
#'
#' @examples
#' \notrun{
#' }
#'
#' @export extractFromRasterSnips
#' @name extractFromRasterSnips
#'
extractFromRasterSnips <- function(raster, spatial, selector = NULL, buffer = NULL){
  
  raster_selector_ids <- as.numeric(names(raster))
  
  lspt <- length(spatial)
  
  plots <- lapply(seq(lspt), function(i){
    if(i %% 10 == 0) print(paste0("Processing ", i, " of ", lspt))
    if(!is.null(selector)){
      rid <- which(raster_selector_ids %in% spatial[i,]@data[selector])
      if(length(rid) > 1){
        rid <- rid[1]
        raster_selector_ids[rid] <- NA
      }
    } else {
      rid <- i
    }
    plots <- extract(raster[[rid]], 
                     spatial[i,], sp = TRUE)
    
    if(!is.null(buffer)){
      plots_buffer <- extract(raster[[rid]], spatial[i,], buffer = buffer)
      
      plots_buffer_stat <- cbind(
        #spatial[i, ]@data[selector],
        as.data.frame(t(apply(plots_buffer[[1]], 2, mean))),
        as.data.frame(t(apply(plots_buffer[[1]], 2, sd))))
      l <- length(colnames(plots_buffer_stat))
      colnames(plots_buffer_stat)[1:(l/2)] <- paste0(colnames(plots_buffer_stat)[1:(l/2)], "_mean")
      colnames(plots_buffer_stat)[(l/2+1):l] <- paste0(colnames(plots_buffer_stat)[(l/2+1):l], "_sd")
      
      plots <- merge(plots, plots_buffer_stat)
    }
    return(plots)
  })
  plots <- do.call("rbind", plots)
  return(plots)
}

