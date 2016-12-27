#' Calculates Gray scale morphological operations for a given kernel size
#' 
#' @note the otb is used for filtering. please provide a GeoTiff file
#' @param input of GeoTiff containing 1 ore more gray value bands
#' @param output_name the output mono band image containing the edge features
#' @param filter the choice of the morphological operation (dilate/erode/opening/closing) (default value is dilate)
#' @param structype the choice of the structuring element type (ball/cross)
#' @param structype.ball.xradius x the ball structuring element X Radius (only if structype==ball)
#' @param structype.ball.yradius y the ball structuring element Y Radius (only if structype==ball)
#' @param channel sequence of bands to be processed
#' @param ram reserved memory in MB
#' @param return_raster boolean if TRUE a raster stack is returned
#' @param verbose switch for system messages default is FALSE
#' @return list of geotiffs containing thelocal statistics for each channel 
#' @author Chris Reudenbach
#' @name otbGrayMorpho
#' @export otbGrayMorpho
#' @examples 
#' \dontrun{
#' url<-"http://www.ldbv.bayern.de/file/zip/5619/DOP%2040_CIR.zip"
#' res <- curl::curl_download(url, "testdata.zip")
#' unzip(res,junkpaths = TRUE,overwrite = TRUE)
#' gm<-otbGrayMorpho(input=paste0(getwd(),"4490600_5321400.tif"),return_raster = TRUE)
#' raster::plot(gm[[1]])
#' }

otbGrayMorpho<- function(input=NULL,
                         output_name="edge",
                         ram="8192",
                         filter="dilate",
                         structype="ball",
                         structype.ball.xradius=5,
                         structype.ball.yradius=5,
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
                    structype,
                    "_",
                    output_name,
                    ".tif")
    
    command<-paste0(otbPath,"otbcli_GrayScaleMorphologicalOperation")
    command<-paste(command, " -in ", input)
    command<-paste(command, " -channel ", channel)
    command<-paste(command, " -filter ", filter)
    if (structype == "ball") {
      command<-paste(command, " -structype.ball.xradius ", structype.ball.xradius)
      command<-paste(command, " -structype.ball.yradius ", structype.ball.yradius)
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