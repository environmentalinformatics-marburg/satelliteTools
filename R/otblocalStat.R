#' Calculates local statistics for a given kernel size
#' @description Calculate local statistics using OTB functions.
#' @note the otb is used for the calculation of the statistics. Please provide a GeoTiff file  
#' @param input of GeoTiff containing 1 ore more gray value bands
#' @param output_name string pattern for individual naming of the output file(s)
#' @param radius computational window in pixel
#' @param channel sequence of bands to be processed
#' @param ram reserved memory in MB
#' @param return_raster boolean if TRUE a raster stack is returned
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
                        output_name="localStat",
                        ram="8192",
                        radius=3,
                        channel=NULL,
                        return_raster=FALSE,
                        path_output=NULL,
                        verbose=FALSE){
  
  directory<-getOutputDir(path_output)
  retStack<-list()
  if (is.null(channel)) channel<-seq(length(grep(gdalUtils::gdalinfo(input,nomd = TRUE),pattern = "Band ")))
  for (band in channel) {
    
    output_name<-paste0(directory,
                    "band_",
                    band,
                    "_",
                    output_name,
                    "_",
                    radius,
                    ".tif")
    
    command<-paste0(otbPath,"otbcli_LocalStatisticExtraction")
    command<-paste(command, " -in ", input)
    command<-paste(command, " -channel ", channel)
    command<-paste(command, " -output_name ", output_name)
    command<-paste(command, " -ram ",ram)
    command<-paste(command, " -radius ",radius)
    if (verbose) {
      cat("\nrunning cmd:  ", command[band],"\n")
      system(command[band])}
    else{
      system(command[band],intern = TRUE,ignore.stdout = TRUE)}  
    
    if (return_raster) retStack[[band]]<-assign(paste0(tools::file_path_sans_ext(basename(output_name)),"band_",band),raster::stack(output_name))
  }
  return(retStack)
}