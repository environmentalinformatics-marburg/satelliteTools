if ( !isGeneric("pca") ) {
  setGeneric("pca", function(x, ...)
    standardGeneric("pca"))
}
#' Calculate image textures
#' 
#' @param x Satellite object or raster stack.
#' @param bcde Band code(s) to be used for computing the pca
#' @param ... Further arguments passed on to \code{\link{RStoolbox::rasterPCA}}
#' 
#' @details 
#' The method is a wrapper for of glcm:glcm by A. Zvoleff
#' (see \url{https://cran.r-project.org/web/packages/glcm/index.html})
#' 
#' @return If x is a Satellite object, a Satellite object with added image
#' textures; if x is a \code{raster::Raster*} object, a \code{raster::Raster*} 
#' object with converted layer(s).
#' 
#' @export pca
#' 
#' @name pca
#' 
#' @references None.
#' 
#' @examples
#' path <- system.file("extdata", package = "satellite")
#' files <- list.files(path, pattern = glob2rx("LC8*.TIF"), full.names = TRUE)
#' sat <- satellite(files)
#' 
#' sat <- pca(sat, bcde = "B004n")
NULL


# Function using satellite object ----------------------------------------------
#' 
#' @rdname pca
#'
setMethod("pca", 
          signature(x = "Satellite"), 
          function(x, bcde, prfx=NULL, ...){
            
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
          function(x, ...){
            x <- RStoolbox::rasterPCA(x, ...)
            return(x)
          })


