if ( !isGeneric("sampleRasterFromPolygons") ) {
  setGeneric("sampleRasterFromPolygons", function(x, ...)
    standardGeneric("sampleRasterFromPolygons"))
}
#' Sample raster values from polygons
#' 
#' @description In the default settings, this is just a wrapper arround
#' raster::extract but with additional options in oder to select just a 
#' subsample of the extracted raster values based on properties of the size
#' of the polygons. The reason behind this is to be able to use e.g. 
#' training polygons for land-cover classifications of very different sizes 
#' without running into problems during model delevopment later.
#' 
#' @param x Satellite object, raster stack or raster layer.
#' 
#' @details 
#' The method is a wrapper for raster:extract with some extensions
#' 
#' @return If x is a Satellite object, a Satellite object with added image
#' textures; if x is a \code{raster::Raster*} object, a \code{raster::Raster*} 
#' object with converted layer(s).
#' 
#' @export sampleRasterFromPolygons
#' 
#' @name sampleRasterFromPolygons
#' 
#' @references None.
#' 
#' @examples
#' \dontrun{
#' }
NULL


# Function using satellite object ----------------------------------------------
#' 
#' @rdname sampleRasterFromPolygons
#'
setMethod("sampleRasterFromPolygons", 
          signature(x = "Satellite"), 
          function(x, bcde, mask=NULL, ...){
            #TODO
          })


# Function using raster::RasterStack object ------------------------------------
#' 
#' @rdname sampleRasterFromPolygons
#'
setMethod("sampleRasterFromPolygons", 
          signature(x = "RasterStack"), 
          function(x, poly, nbr = 50, res = raster::res(x), ...){
          
            points <- lapply(seq(length(poly)), function(i){
              if(i%%10 == 0) print(paste0("Processing polygon ", i))
              buffer <- gBuffer(gCentroid(poly[i, ]), width = nbr)
              
              bb <- resolution * round(sp::bbox(buffer)/resolution, 0)
              grid <- sp::GridTopology(cellcentre.offset = bb[,1],
                                       cellsize = c(resolution, resolution),
                                       cells.dim = (c(diff(bb[1,]), 
                                                      diff(bb[2,]))/resolution)
                                       + 1)
              df <- poly[i,]@data
              points <- sp::SpatialPointsDataFrame(grid, 
                                                   data = df[rep(row.names(df), grid@cells.dim[1]*grid@cells.dim[2]),],
                                                   proj4string = CRS(proj4string(poly)))
              intersects <- gIntersects(points, poly[i,], byid = TRUE, returnDense = TRUE)
              
              set.seed(i)
              intersects_true <- which(intersects == TRUE)
              smpl <- sample(intersects_true, min(nbr, length(intersects_true)))
              return(points[smpl,])
            })
            points <- do.call("rbind", points)
            
            data <- extract(x, points, sp = TRUE)
            return(data)
          })


# Function using raster::RasterLayer object ------------------------------------
#' 
#' @param mask A \code{raster::RasterLayer} in which area of interest are marked
#' with 1, NA otherwise. 
#' @rdname sampleRasterFromPolygons
#'
setMethod("sampleRasterFromPolygons", 
          signature(x = "RasterLayer"), 
          function(x, poly, nbr = NULL,...){
            x <- sampleRasterFromPolygons(stack(x), poly, nbr)
            return(x)
          })

