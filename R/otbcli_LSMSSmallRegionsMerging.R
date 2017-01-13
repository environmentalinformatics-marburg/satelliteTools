if ( !isGeneric("otbcli_LSMSSmallRegionsMerging") ) {
  setGeneric("otbcli_LSMSSmallRegionsMerging", function(x, ...)
    standardGeneric("otbcli_LSMSSmallRegionsMerging"))
}
#' Calculates a mean shift filtering (required for object based classification)
#' @description Calculates a mean shift filtering
#' 
#' @param x A \code{Raster*} object or a gdall raster containing the filterd input file
#' @param inseg A \code{Raster*} object or a gdall raster containing the segmented input file
#' @param out The full path and name of the output file
#' @param minsize Minimum size of the extracted segments
#' @param tilesizex Size of individual processing tiles in x direction
#' @param tilesizey Size of individual processing tiles in y direction
#' @param ram reserved memory in MB
#' @param return_raster boolean if TRUE a raster stack is returned
#' @param verbose switch for system messages default is FALSE
#' 
#' @note the otb is used for the calculation of the statistics.
#' 
#' @author Thomas Nauss
#' 
#' @name otbcli_LSMSSmallRegionsMerging
#' @export otbcli_LSMSSmallRegionsMerging
#' 
#' @examples 
#' \dontrun{
#' url<-"http://www.ldbv.bayern.de/file/zip/5619/DOP%2040_CIR.zip"
#' res <- curl::curl_download(url, "testdata.zip")
#' unzip(res,junkpaths = TRUE,overwrite = TRUE)
#' otbcli_LSMSSmallRegionsMerging(input=paste0(getwd(),"4490600_5321400.tif"),spatialr=5)
#' }
NULL


# Function using GeoTiff -------------------------------------------------------
#' 
#' @rdname otbcli_LSMSSmallRegionsMerging
#'
setMethod("otbcli_LSMSSmallRegionsMerging", 
          signature(x = "character"), 
          function(x,
                   inseg = NULL,
                   out = NULL,
                   minsize = 20,
                   tilesizex = 500,
                   tilesizey = 500,
                   verbose = FALSE,
                   return_raster = FALSE,
                   ram="8192"){
       
            command<-paste0(otbPath, "otbcli_LSMSSmallRegionsMerging",
                            " -in ", x,
                            " -inseg ", inseg,
                            " -out ", out,
                            " -minsize ", minsize,
                            " -tilesizex ", tilesizex,
                            " -tilesizey ", tilesizey,
                            " -ram ", ram)
            
            if (verbose) {
              cat("\nrunning cmd:  ", command,"\n")
              system(command)
            } else {
              system(command,intern = TRUE,ignore.stdout = TRUE)
            }  
            
            if(return_raster){
              mss <- readAll(raster::stack(out))
              names(mss) <- paste0("mins", minsize)
            } else {
              mss <- NULL
            }
            return(mss)
          })