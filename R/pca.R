if ( !isGeneric("pca") ) {
  setGeneric("pca", function(x, ...)
    standardGeneric("pca"))
}
#' Calculate PCA
#' 
#' @description Calculate principal components from raster data values. The
#' raster can either be supplied as \code{Raster*} object, \code{satellite*}
#' object or as a list of \code{Raster*} objects. The latter is usefull if the
#' raster dataset has been tiled into small observation areas using e.g. 
#' \code{\link{snipRaster}}.
#' 
#' @param x \code{Satellite}, \code{Raster*} or \code{list} of \code{Raster*} 
#' objects
#' @param bcde Band code(s) to be used for computing the pca
#' @param ... Further arguments passed on to \code{\link{RStoolbox::rasterPCA}}
#' or \code{\link{stats::prcomp}}
#' @param ignore_names If list is supplied and if the band names of the 
#' individual rasters do not match (e.g. because of different observation dates),
#' ignore the names when building a single data frame for the PCA computation
#' (does not hurt).
#' @param center See \code{\link{stats::prcomp}}
#' @param scale See \code{\link{stats::prcomp}}
#' 
#' @details 
#' The method for \code{Raster*} and \code{Satellite*} objects is basically a
#' wrapper arround the respective function in the RStoolbox package. For the list
#' (i.e. snip) related implementation, the stats::prcomp function is used to actually
#' compute the PCA.
#' 
#' @return If x is a Satellite object, a Satellite object with added image
#' textures; if x is a \code{raster::Raster*} object, a \code{raster::Raster*} 
#' object with converted layer(s).
#' 
#' @export pca
#' 
#' @name pca
#' 
#' @references
#' Benjamin Leutner and Ned Horning (2016). RStoolbox: Tools for Remote Sensing 
#' Data Analysis. R package version 0.1.6. 
#' https://CRAN.R-project.org/package=RStoolbox
#' 
#' @examples
#' \dontrun{
#' path <- system.file("extdata", package = "satellite")
#' files <- list.files(path, pattern = glob2rx("LC8*.TIF"), full.names = TRUE)
#' sat <- satellite(files)
#' 
#' sat <- pca(sat, bcde = "B004n")
#' }
NULL


# Function using satellite object ----------------------------------------------
#' @rdname pca
#' 
setMethod("pca", 
          signature(x = "Satellite"), 
          function(x, bcde, prfx=NULL, return_raster = FALSE, ...){
            
            act <- pca(stack(getSatDataLayers(x, bcde = bcde)), ...)
            
            meta_param <- getSatMeta(x, bcde[1])
            
            if(is.null(prfx)){
              meta_bcde <- paste0(bcde[1], "_", names(act$map))
            } else {
              meta_bcde <- paste0(prfx, "_", names(act$map))
            }
            
            meta_param[, 6:ncol(meta_param)] <- NA
            meta_param <- 
              meta_param[rep(seq_len(nrow(meta_param)), each = length(meta_bcde)),]
            meta_param$TYPE <- "PCA"
            
            info <- sys.calls()[[1]]
            info <- act$model
            x <- addSatDataLayer(x, bcde = meta_bcde, data = act$map, 
                                 meta_param = meta_param,
                                 info = info, in_bcde = bcde[1])
            return(x)
          })


# Function using raster::RasterStack object ------------------------------------
#' 
#' @rdname pca
#'
setMethod("pca", 
          signature(x = "RasterStack"), 
          function(x, return_raster = FALSE, ...){
            x <- RStoolbox::rasterPCA(x, ...)
            if(return_raster){
              x <- x$map
            }
            return(x)
          })




# Function using list object of RasteR* ----------------------------------------
#' 
#' @rdname pca
#'
setMethod("pca", 
          signature(x = "list"), 
          function(x, ignore_names = FALSE, center = TRUE, scale = TRUE, ...){
            
            values <- lapply(seq(length(x)), function(i){
              df <- data.frame(getValues(x[[i]]),
                               id = i)
              if(ignore_names){
                colnames(df) <- c(paste0("band_", seq(ncol(df)-1)), "id")
              }
              return(df)
            })
            values <- do.call("rbind", values)
            
            pca <- prcomp(values[, -which(colnames(values) == "id")],
                          center = center, scale = scale, retx = TRUE)
            
            results <- lapply(seq(length(x)), function(i){
              setValues(x[[i]], pca$x[which(values$id == i), ])
            })
            names(results) <- names(x)
            return(results)
          })