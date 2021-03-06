#' Calculates edges for a given kernel size
#' 
#' @note the otb is used for filtering. please provide a GeoTiff file
#' @param input of GeoTiff containing 1 ore more gray value band(s)
#' @param output_name the output mono band image containing the edge features
#' @param filter the choice of edge detection method (gradient/sobel/touzi)
#' @param filter.touzi.xradius x radius of the Touzi processing neighborhood (if filter==touzi) (default value is 1 pixel)
#' @param filter.touzi.yradius y radius of the Touzi processing neighborhood (if filter==touzi) (default value is 1 pixel)
#' @param channel sequence of bands to be processed
#' @param ram reserved memory in MB
#' @param return_raster boolean if TRUE a raster stack is returned
#' @param verbose switch for system messages default is FALSE
#' @return list of geotiffs containing thelocal statistics for each channel 
#' @author Chris Reudenbach
#' @name otbEdge
#' @export otbEdge
#' @examples 
#' \dontrun{
#' url<-"http://www.ldbv.bayern.de/file/zip/5619/DOP%2040_CIR.zip"
#' res <- curl::curl_download(url, "testdata.zip")
#' unzip(res,junkpaths = TRUE,overwrite = TRUE)
#' otbedge(input=paste0(getwd(),"4490600_5321400.tif"),filter = "sobel")
#' }

otbEdge<- function(input=NULL,
                   output_name="edge",
                   ram="8192",
                   filter="gradient",
                   filter.touzi.xradius=1,
                   filter.touzi.yradius=1,
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
                    filter,
                    "_",
                    output_name,
                    ".tif")
    
    command<-paste0(otbPath,"otbcli_EdgeExtraction")
    command<-paste(command, " -in ", input)
    command<-paste(command, " -channel ", channel)
    command<-paste(command, " -filter ", filter)
    if (filter == "touzi") {
      command<-paste(command, " -filter.touzi.xradius ", filter.touzi.xradius)
      command<-paste(command, " -filter.touzi.yradius ", filter.touzi.yradius)
    }
    command<-paste(command, " -output_name ", output_name)
    command<-paste(command, " -ram ",ram)
    if (verbose) {
      cat("\nrunning cmd:  ", command[band],"\n")
      system(command[band])}
    else{
      system(command[band],intern = TRUE,ignore.stdout = TRUE)}          
    
    if (return_raster) retStack[[band]]<-assign(paste0(tools::file_path_sans_ext(basename(output_name)),"band_",band),raster::stack(output_name))
  }
  return(retStack)
}