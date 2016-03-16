if ( !isGeneric("imageTextures") ) {
  setGeneric("imageTextures", function(x, ...)
    standardGeneric("imageTextures"))
}
#' Calculate image textures
#' 
#' @param x Satellite object, raster stack or raster layer.
#' @param bcde Band code(s) to be used for computing the image textures
#' @param mask raster layer with the same geometry as x with NA values at
#' positions where image textures should not be computed.
#' @param ... Further arguments passed on to \code{\link{glcm::glcm}}
#' 
#' @details 
#' The method is a wrapper for of glcm:glcm by A. Zvoleff
#' (see \url{https://cran.r-project.org/web/packages/glcm/index.html})
#' 
#' @return If x is a Satellite object, a Satellite object with added image
#' textures; if x is a \code{raster::Raster*} object, a \code{raster::Raster*} 
#' object with converted layer(s).
#' 
#' @export imageTextures
#' 
#' @name imageTextures
#' 
#' @references None.
#' 
#' @examples
#' path <- system.file("extdata", package = "satellite")
#' files <- list.files(path, pattern = glob2rx("LC8*.TIF"), full.names = TRUE)
#' sat <- satellite(files)
#' 
#' sat <- imageTextures(sat, bcde = "B004n")
NULL


# Function using satellite object ----------------------------------------------
#' 
#' @rdname imageTextures
#'
setMethod("imageTextures", 
          signature(x = "Satellite"), 
          function(x, bcde, mask=NULL, ...){
            for(act_bcde in bcde){
              act <- imageTextures(getSatDataLayer(x, act_bcde), 
                                     mask = mask, ...)
              
              meta_param <- getSatMeta(x, act_bcde)
              meta_bcde <- paste0(act_bcde, "_", names(act))
              
              meta_param[, 6:ncol(meta_param)] <- NA
              meta_param <- 
                meta_param[rep(seq_len(nrow(meta_param)), each = length(meta_bcde)),]
              meta_param$TYPE <- "IT"
              
              info <- sys.calls()[[1]]
              info <- paste0("Compute image texture based on ", act_bcde)
              x <- addSatDataLayer(x, bcde = meta_bcde, data = act, 
                                   meta_param = meta_param,
                                   info = info, in_bcde = act_bcde)
            }
            return(x)
          })


# Function using raster::RasterStack object ------------------------------------
#' 
#' @rdname imageTextures
#'
setMethod("imageTextures", 
          signature(x = "RasterStack"), 
          function(x, mask=NULL, ...){
            for(l in seq(nlayers(x))){
              x[[l]] <- imageTextures(x[[l]], mask)
            }
            return(x)
          })



# Function using raster::RasterLayer object ------------------------------------
#' 
#' @param mask A \code{raster::RasterLayer} in which area of interest are marked
#' with 1, NA otherwise. 
#' @rdname imageTextures
#'
setMethod("imageTextures", 
          signature(x = "RasterLayer"), 
          function(x, mask=NULL, ...){
            if (!is.null(mask)){
              x <- raster::mask(x, mask)
            }
            x <- glcm::glcm(x, ...)
            return(x)
          })

