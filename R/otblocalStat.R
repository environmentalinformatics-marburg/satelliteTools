if ( !isGeneric("otbLocalStat") ) {
  setGeneric("otbLocalStat", function(x, ...)
    standardGeneric("otbLocalStat"))
}
#' Calculates local statistics for a given kernel size
#' @description Calculate local statistics using OTB functions.
#' 
#' @note the otb is used for the calculation of the statistics. Please provide a GeoTiff file  
#' 
#' @param x A \code{Raster*} object or a GeoTiff containing 1 or more gray 
#' value bands
#' @param output_name string pattern for individual naming of the output file(s)
#' @param radius computational window in pixel
#' @param channel sequence of bands to be processed
#' @param ram reserved memory in MB
#' @param return_raster boolean if TRUE a raster stack is returned
#' @param verbose switch for system messages default is FALSE
#' @author Chris Reudenbach, Thomas Nauss
#' @name otbLocalStat
#' @export otbLocalStat
#' @examples 
#' \dontrun{
#' url<-"http://www.ldbv.bayern.de/file/zip/5619/DOP%2040_CIR.zip"
#' res <- curl::curl_download(url, "testdata.zip")
#' unzip(res,junkpaths = TRUE,overwrite = TRUE)
#' otbLocalStat(input=paste0(getwd(),"4490600_5321400.tif"),radius=5)
#' }
NULL

# Function using RasterBrick ---------------------------------------------------
#' @rdname otbLocalStat
setMethod("otbLocalStat", 
          signature(x = "RasterBrick"), 
          function(x,
                   output_name="locstat",
                   radius=3,
                   path_output=NULL,
                   return_raster=TRUE,
                   channel=NULL,
                   verbose=FALSE,
                   ram="8192"){
            raster::writeRaster(x, file = paste0(path_output, "tmp.tif"), overwrite = TRUE)
            x <- paste0(path_output, "tmp.tif")
            tempout <- format(Sys.time(), "%Y-%m-%d-%H%M%S")
            locstat <- otbLocalStat(x = x,
                                    output_name=tempout,
                                    radius=radius,
                                    path_output=path_output,
                                    return_raster=return_raster,
                                    channel=channel,
                                    verbose=verbose,
                                    ram=ram)
            file.remove(x)
            tmpfiles <- list.files(path_output, 
                                   pattern = glob2rx(paste0("*", tempout, "*")),
                                   full.names = TRUE)
            file.remove(tmpfiles)
            return(locstat)
          })


# Function using RasterLayer ---------------------------------------------------
#' @rdname otbLocalStat
setMethod("otbLocalStat", 
          signature(x = "RasterLayer"), 
          function(x,
                   output_name="locstat",
                   radius=3,
                   path_output=NULL,
                   return_raster=TRUE,
                   channel=NULL,
                   verbose=FALSE,
                   ram="8192"){
           locstat <- otbLocalStat(x = raster::brick(x),
                                           output_name=output_name,
                                           radius=radius,
                                           path_output=path_output,
                                           return_raster=return_raster,
                                           channel=channel,
                                           verbose=verbose,
                                           ram=ram)
            return(locstat)
          })


# Function using RasterStack ---------------------------------------------------
#' @rdname otbLocalStat
setMethod("otbLocalStat", 
          signature(x = "RasterStack"), 
          function(x,
                   output_name="locstat",
                   radius=3,
                   path_output=NULL,
                   return_raster=TRUE,
                   channel=NULL,
                   verbose=FALSE,
                   ram="8192"){
            locstat <- otbLocalStat(x = raster::brick(x),
                                           output_name=output_name,
                                           radius=radius,
                                           path_output=path_output,
                                           return_raster=return_raster,
                                           channel=channel,
                                           verbose=verbose,
                                           ram=ram)
            return(locstat)
          })


# Function using GeoTiff -------------------------------------------------------
#' @rdname otbLocalStat
setMethod("otbLocalStat", 
          signature(x = "character"), 
          function(x,
                   output_name="locstat",
                   radius=3,
                   path_output=NULL,
                   return_raster=FALSE,
                   channel=NULL,
                   verbose=FALSE,
                   ram="8192"){

  if (is.null(channel)){
    channel <- seq(length(grep(gdalUtils::gdalinfo(x,nomd = TRUE),
                               pattern = "Band ")))
  } 
  
  locstat <- lapply(channel, function(band){
    path_outfile <- paste0(path_output,
                           output_name, "_",
                           "band_", band, "_", 
                           radius,
                           ".tif")

    command<-paste0(otbPath, "otbcli_LocalStatisticExtraction",
                    " -in ", x,
                    " -channel ", band,
                    " -out ", path_outfile,
                    " -radius ", radius,
                    " -ram ",ram)
    
    if (verbose) {
      cat("\nrunning cmd:  ", command,"\n")
      system(command)
    } else {
      system(command,intern = TRUE,ignore.stdout = TRUE)
    }  
    
    if(return_raster){
      locstat <- readAll(raster::stack(path_outfile))
      names(locstat) <- paste0("b", band,
                               "r", radius,
                               c("mean", "variance", "skewness", "kurtosis"))
    } else {
      locstat <- NULL
    }
    return(locstat)
  })
  if(is.null(locstat[[1]])){
    return(NULL)
  } else {
    return(raster::stack(locstat))  
  }
})