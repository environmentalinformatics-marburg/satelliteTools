if ( !isGeneric("otbcli_MeanShiftSmoothing") ) {
  setGeneric("otbcli_MeanShiftSmoothing", function(x, ...)
    standardGeneric("otbcli_MeanShiftSmoothing"))
}
#' Calculates a mean shift filtering (required for object based classification)
#' @description Calculates a mean shift filtering
#' 
#' @note the otb is used for the calculation of the statistics.
#' 
#' @param x A \code{Raster*} object or a gdall raster containing 1 or more gray 
#' value bands
#' @param outfile_filter The filtered output file
#' @param outfile_spatial The spatial output file
#' @param path_temp Path of temporary output files
#' @param spatialr Computational radius in euclidian distance (m)
#' @param ranger ranger in multi-spectral space to be grouped into one
#' @param thres Mode convergence threshold
#' @param maxiter Maximum number of iterations
#' @param rangeramp Spectral ranger coefficient
#' @param ram reserved memory in MB
#' @param return_raster boolean if TRUE a raster stack is returned
#' @param verbose switch for system messages default is FALSE
#' @author Thomas Nauss
#' @name otbcli_MeanShiftSmoothing
#' @export otbcli_MeanShiftSmoothing
#' @examples 
#' \dontrun{
#' url<-"http://www.ldbv.bayern.de/file/zip/5619/DOP%2040_CIR.zip"
#' res <- curl::curl_download(url, "testdata.zip")
#' unzip(res,junkpaths = TRUE,overwrite = TRUE)
#' otbcli_MeanShiftSmoothing(input=paste0(getwd(),"4490600_5321400.tif"),spatialr=5)
#' }
NULL

# Function using RasterBrick ---------------------------------------------------
#' 
#' @rdname otbcli_MeanShiftSmoothing
#'
setMethod("otbcli_MeanShiftSmoothing", 
          signature(x = "RasterBrick"), 
          function(x,
                   path_temp = NULL,
                   return_raster = TRUE,
                   spatialr = 5,
                   ranger = 15,
                   thres = 0.1,
                   maxiter = 100,
                   rangeramp = 0,
                   verbose=FALSE,
                   ram="8192"){
            raster::writeRaster(x, file = paste0(path_temp, "tmp.tif"), overwrite = TRUE)
            x <- paste0(path_temp, "tmp.tif")
            tmp_time <- format(Sys.time(), "%Y-%m-%d-%H%M%S")
            outfile_filter <- paste0(path_temp, "filter_", tmp_time, ".tif")
            outfile_spatial <- paste0(path_temp, "spatial_", tmp_time, ".tif")
            mss <- otbcli_MeanShiftSmoothing(x = x,
                                             outfile_filter = outfile_filter,
                                             outfile_spatial = outfile_spatial,
                                             return_raster = return_raster,
                                             spatialr = spatialr,
                                             ranger = ranger,
                                             thres = thres,
                                             maxiter = maxiter,
                                             rangeramp = rangeramp,
                                             verbose = verbose,
                                             ram = ram)
            file.remove(x)
            tmpfiles <- list.files(path_temp, 
                                   pattern = glob2rx(paste0("*", tempout, "*")),
                                   full.names = TRUE)
            file.remove(tmpfiles)
            return(mss)
          })


# Function using RasterLayer ---------------------------------------------------
#' 
#' @rdname otbcli_MeanShiftSmoothing
#'
setMethod("otbcli_MeanShiftSmoothing", 
          signature(x = "RasterLayer"), 
          function(x,
                   path_temp = NULL,
                   return_raster = TRUE,
                   spatialr = 5,
                   ranger = 15,
                   thres = 0.1,
                   maxiter = 100,
                   rangeramp = 0,
                   verbose=FALSE,
                   ram="8192"){
            mss <- otbcli_MeanShiftSmoothing(x = raster::brick(x),
                                             path_temp = path_temp,
                                             return_raster = return_raster,
                                             spatialr = spatialr,
                                             ranger = ranger,
                                             thres = thres,
                                             maxiter = maxiter,
                                             rangeramp = rangeramp,
                                             verbose = verbose,
                                             ram = ram)
            return(mss)
          })


# Function using RasterStack ---------------------------------------------------
#' 
#' @rdname otbcli_MeanShiftSmoothing
#'
setMethod("otbcli_MeanShiftSmoothing", 
          signature(x = "RasterStack"), 
          function(x,
                   path_temp = NULL,
                   return_raster = TRUE,
                   spatialr = 5,
                   ranger = 15,
                   thres = 0.1,
                   maxiter = 100,
                   rangeramp = 0,
                   verbose=FALSE,
                   ram="8192"){
            mss <- otbcli_MeanShiftSmoothing(x = raster::brick(x),
                                             path_temp = path_temp,
                                             return_raster = return_raster,
                                             spatialr = spatialr,
                                             ranger = ranger,
                                             thres = thres,
                                             maxiter = maxiter,
                                             rangeramp = rangeramp,
                                             verbose = verbose,
                                             ram = ram)
            return(mss)
          })


# Function using GeoTiff -------------------------------------------------------
#' 
#' @rdname otbcli_MeanShiftSmoothing
#'
setMethod("otbcli_MeanShiftSmoothing", 
          signature(x = "character"), 
          function(x,
                   outfile_filter = NULL,
                   outfile_spatial = NULL,
                   return_raster = FALSE,
                   spatialr = 5,
                   ranger = 15,
                   thres = 0.1,
                   maxiter = 100,
                   rangeramp = 0,
                   verbose=FALSE,
                   ram="8192"){
            
            command<-paste0(otbPath, "otbcli_MeanShiftSmoothing",
                            " -in ", x,
                            " -fout ", outfile_filter,
                            " -foutpos ", outfile_spatial,
                            " -spatialr ", spatialr,
                            " -ranger ", ranger,
                            " -thres ", thres,
                            " -maxiter ", maxiter,
                            " -rangeramp ", rangeramp,
                            " -ram ",ram)
            
            if (verbose) {
              cat("\nrunning cmd:  ", command,"\n")
              system(command)
            } else {
              system(command,intern = TRUE,ignore.stdout = TRUE)
            }  
            
            if(return_raster){
              mss <- readAll(raster::stack(c(outfile_filter, outfile_spatial)))
              names(mss) <- paste0(c("filter_r", "spatial_r"), spatialr, "_rng", ranger)
            } else {
              mss <- NULL
            }
            return(mss)
          })