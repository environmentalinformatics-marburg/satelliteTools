if ( !isGeneric("otbcli_LSMSVectorization") ) {
  setGeneric("otbcli_LSMSVectorization", function(x, ...)
    standardGeneric("otbcli_LSMSVectorization"))
}
#' Calculates a mean shift filtering (required for object based classification)
#' @description Calculates a mean shift filtering
#' 
#' @param x A \code{Raster*} object or a gdall raster containing the original input file
#' @param inseg A \code{Raster*} object or a gdall raster containing the finally semgented  file
#' @param out The full path and name of the output file (shp)
#' @param tilesizex Size of individual processing tiles in x direction
#' @param tilesizey Size of individual processing tiles in y direction
#' @param ram reserved memory in MB
#' @param verbose switch for system messages default is FALSE
#' 
#' @note the otb is used for the calculation of the statistics.
#' 
#' @author Thomas Nauss
#' 
#' @name otbcli_LSMSVectorization
#' @export otbcli_LSMSVectorization
#' 
#' @examples 
#' \dontrun{
#' url<-"http://www.ldbv.bayern.de/file/zip/5619/DOP%2040_CIR.zip"
#' res <- curl::curl_download(url, "testdata.zip")
#' unzip(res,junkpaths = TRUE,overwrite = TRUE)
#' otbcli_LSMSVectorization(input=paste0(getwd(),"4490600_5321400.tif"),spatialr=5)
#' }
NULL


# Function using GeoTiff -------------------------------------------------------
#' @rdname otbcli_LSMSVectorization
#' 
setMethod("otbcli_LSMSVectorization", 
          signature(x = "character"), 
          function(x,
                   inseg = NULL,
                   out = NULL,
                   tilesizex = 500,
                   tilesizey = 500,
                   verbose = FALSE,
                   ram="8192"){
            
            command<-paste0(otbPath, "otbcli_LSMSVectorization",
                            " -in ", x,
                            " -inseg ", inseg,
                            " -out ", out,
                            " -tilesizex ", tilesizex,
                            " -tilesizey ", tilesizey,
                            " -ram ", ram)
            
            if (verbose) {
              cat("\nrunning cmd:  ", command,"\n")
              system(command)
            } else {
              system(command,intern = TRUE,ignore.stdout = TRUE)
            }
            return("Done")
          })
