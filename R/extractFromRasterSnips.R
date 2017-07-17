#' Extract data from \code{Raster*}  snips based on selector from 
#' \code{Spatial*} object 
#'
#' @description
#' Extract data from \code{Raster*} snips based on selector from 
#' \code{Spatial*} objects. Aside from the actual raster values, the mean, 
#' standard deviation, variance as well as the same values based on the 
#' mahalanobis distance within a buffer can be returned.
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
#' @param buffer Buffer size (default 0 does not add a buffer) if the mean and
#' standard deviation within the buffer should be retruned, too.
#' @param mahal Compute mahalanobis distance mean, standard deviation and 
#' variance.
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
#' \dontrun{
#' }
#'
#' @export extractFromRasterSnips
#' @name extractFromRasterSnips
#'
extractFromRasterSnips <- function(raster, spatial, selector = NULL, buffer = 0,
                                   mahal = FALSE){
  
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
    
    if(class(raster[[rid]]) == "list"){
      plots_buffer <- extract(stack(raster[[rid]]), spatial[i,], buffer = buffer)  
    } else {
      plots_buffer <- extract(raster[[rid]], spatial[i,], buffer = buffer)
    }
    
    
    medians <- as.data.frame(t(apply(plots_buffer[[1]], 2, median)))
    means <- as.data.frame(t(apply(plots_buffer[[1]], 2, mean)))
    sds <- as.data.frame(t(apply(plots_buffer[[1]], 2, sd)))
    vars <- sds/means
    
    colnames(medians) <- paste0(colnames(medians), "_median")
    colnames(means) <- paste0(colnames(means), "_mean")
    colnames(sds) <- paste0(colnames(sds), "_sd")
    colnames(vars) <- paste0(colnames(vars), "_var")
    
    plots_buffer_stat <- cbind(means, medians, sds, vars)
    
    if(mahal){
      mahal <- mahalanobis(plots_buffer[[1]], 
                           colMeans(plots_buffer[[1]]), 
                           var(plots_buffer[[1]]))
      plots_buffer_stat <- cbind(plots_buffer_stat,
                                 data.frame(Mahal_mean = mean(mahal)),
                                 data.frame(Mahal_sd = sd(mahal)),
                                 data.frame(Mahal_vars = sd(mahal)/mean(mahal)))
    }
    

    plots <- merge(spatial[i,], plots_buffer_stat)
    return(plots)
  })
  plots <- do.call("bind", plots)
  return(plots)
}
