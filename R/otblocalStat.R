#' Calculates local statistics for a given kernel size
#' @description Calculate local statistics using OTB functions.
#' @note the otb is used for the calculation of the statistics. Please provide a GeoTiff file  
#' @param input of GeoTiff containing 1 ore more gray value bands
#' @param out string pattern vor individual naming of the output file(s)
#' @param radius computational window in pixel
#' @param channel sequence of bands to be processed
#' @param ram reserved memory in MB
#' @param retRaster boolean if TRUE a raster stack is returned
#' @param verbose switch for system messages default is FALSE
#' @author Chris Reudenbach
#' @export otbLocalStat
#' @examples 
#' \dontrun{
#' url<-"http://www.ldbv.bayern.de/file/zip/5619/DOP%2040_CIR.zip"
#' res <- curl::curl_download(url, "testdata.zip")
#' unzip(res,junkpaths = TRUE,overwrite = TRUE)
#' otbLocalStat(input=paste0(getwd(),"4490600_5321400.tif"),radius=5)
#' }

otbLocalStat<- function(input=NULL,
                        out="localStat",
                        ram="8192",
                        radius=3,
                        channel=NULL,
                        retRaster=FALSE,
                        outDir=NULL,
                        verbose=FALSE){
  
  directory<-getOutputDir(outDir)
  retStack<-list()
  if (is.null(channel)) channel<-seq(length(grep(gdalUtils::gdalinfo(input,nomd = TRUE),pattern = "Band ")))
  for (band in channel) {
    
    outName<-paste0(directory,
                    "band_",
                    band,
                    "_",
                    out,
                    "_",
                    radius,
                    ".tif")
    
    command<-paste0(otbPath,"otbcli_LocalStatisticExtraction")
    command<-paste(command, " -in ", input)
    command<-paste(command, " -channel ", channel)
    command<-paste(command, " -out ", outName)
    command<-paste(command, " -ram ",ram)
    command<-paste(command, " -radius ",radius)
    if (verbose) {
      cat("\nrunning cmd:  ", command[band],"\n")
      system(command[band])}
    else{
      system(command[band],intern = TRUE,ignore.stdout = TRUE)}  
    
    if (retRaster) retStack[[band]]<-assign(paste0(tools::file_path_sans_ext(basename(outName)),"band_",band),raster::stack(outName))
  }
  return(retStack)
}