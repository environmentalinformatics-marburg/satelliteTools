if ( !isGeneric("otbcli_ExactLargeScaleMeanShiftSegmentation") ) {
  setGeneric("otbcli_ExactLargeScaleMeanShiftSegmentation", function(x, ...)
    standardGeneric("otbcli_ExactLargeScaleMeanShiftSegmentation"))
}
#' Calculates a mean shift filtering (required for object based classification)
#' @description Calculates a mean shift filtering
#' 
#' @note the otb is used for the calculation of the statistics.
#' 
#' @param x A \code{Raster*} object or a gdall raster containing the filterd input file
#' @param inpos A \code{Raster*} object or a gdall raster containing the spatial input file
#' @param out The full path and name of the output file
#' @param tmpdir Path of temporary output files
#' @param spatialr Computational radius in euclidian distance (m)
#' @param ranger Range in multi-spectral space to be grouped into one
#' @param minsize Minimum size of the extracted segments
#' @param tilesizex Size of individual processing tiles in x direction
#' @param tilesizey Size of individual processing tiles in y direction
#' @param return_raster boolean if TRUE a raster stack is returned
#' @param verbose switch for system messages default is FALSE
#' @author Thomas Nauss
#' @name otbcli_ExactLargeScaleMeanShiftSegmentation
#' @export otbcli_ExactLargeScaleMeanShiftSegmentation
#' @examples 
#' \dontrun{
#' url<-"http://www.ldbv.bayern.de/file/zip/5619/DOP%2040_CIR.zip"
#' res <- curl::curl_download(url, "testdata.zip")
#' unzip(res,junkpaths = TRUE,overwrite = TRUE)
#' otbcli_ExactLargeScaleMeanShiftSegmentation(input=paste0(getwd(),"4490600_5321400.tif"),spatialr=5)
#' }
NULL


# Function using GeoTiff -------------------------------------------------------
#' 
#' @rdname otbcli_ExactLargeScaleMeanShiftSegmentation
#'
setMethod("otbcli_ExactLargeScaleMeanShiftSegmentation", 
          signature(x = "character"), 
          function(x,
                   inpos = NULL,
                   out = NULL,
                   tmpdir = NULL,
                   spatialr = 5,
                   ranger = 15,
                   minsize = 0,
                   tilesizex = 500,
                   tilesizey = 500,
                   verbose = FALSE,
                   return_raster = FALSE){
            
            command<-paste0(otbPath, "otbcli_LSMSSegmentation",
                            " -in ", x,
                            " -inpos ", inpos,
                            " -out ", out,
                            " -spatialr ", spatialr,
                            " -ranger ", ranger,
                            " -minsize ", minsize,
                            " -tilesizex ", tilesizex,
                            " -tilesizey ", tilesizey,
                            " -tmpdir ", tmpdir)
            
            if (verbose) {
              cat("\nrunning cmd:  ", command,"\n")
              system(command)
            } else {
              system(command,intern = TRUE,ignore.stdout = TRUE)
            }  
            
            if(return_raster){
              mss <- readAll(raster::stack(out))
              names(mss) <- paste0("r", spatialr, "_rng", ranger, "_mins", minsize)
            } else {
              mss <- NULL
            }
            return(mss)
          })