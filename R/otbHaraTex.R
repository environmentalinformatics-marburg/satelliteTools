if ( !isGeneric("otbHaraTex") ) {
  setGeneric("otbHaraTex", function(x, ...)
    standardGeneric("otbHaraTex"))
}
#' Calculate selected texture parameters on gray level properties
#' 
#' @note the otb is used for filtering. please provide a GeoTiff file
#' @param x A \code{Raster*} object or a GeoTiff containing 1 or more gray 
#' value bands
#' @param out string pattern vor individual naming of the output file(s)
#' @param parameters.xyrad list with the x and y radius in pixel indicating the kernel sizes for which the textures are calculated
#' @param parameters.xyoff  vector containg the directional offsets. Valid combinations are: list(c(1,1),c(1,0),c(0,1),c(1,-1))
#' @param n_grey Number of grey values. 
#' @param parallel A logical value indicating whether parameters are calculated parallely or not
#' @param parameters.minmax   minimum/maximum gray value which can occur. 
#' @param parameters.nbbin number of gray level bins (classes)
#' @param texture type of filter "simple" "advanced" "higher"
#' @param channel sequence of bands to be processed
#' @param ram reserved memory in MB
#' @param retRaster boolean if TRUE a raster stack is returned
#' @param verbose switch for system messages default is FALSE
#' @return A list of RasterStacks containing the texture parameters for each 
#' combination of channel and filter  

#' @author Chris Reudenbach, Thomas Nauss
#' @note More information at the texture tutorial site of
#' \link{http://www.fp.ucalgary.ca/mhallbey/more_informaton.htm}(Mryka Hall-Beyer)
#' Keep in mind that:\cr
#' Homogeneity is correlated with Contrast,  r = -0.80
#' Homogeneity is correlated with Dissimilarity, r = -0.95
#' GLCM Variance is correlated with Contrast,  r= 0.89
#' GLCM Variance is correlated with Dissimilarity,  r= 0.91
#' GLCM Variance is correlated with Homogeneity,  r= -0.83
#' Entropy is correlated with ASM,  r= -0.87
#' GLCM Mean and Correlation are more independent. For the same image,
#' GLCM Mean shows  r< 0.1 with any of the other texture measures demonstrated in this tutorial.
#' GLCM Correlation shows  r<0.5 with any other measure.
#' @name otbHaraTex
#' @export otbHaraTex
#' @examples 
#' \dontrun{
#' url<-"http://www.ldbv.bayern.de/file/zip/5619/DOP%2040_CIR.zip"
#' res <- curl::curl_download(url, "testdata.zip")
#' unzip(res,junkpaths = TRUE,overwrite = TRUE)
#' otbHaraTex(x=paste0(getwd(),"4490600_5321400.tif"),texture="simple")
#' }
NULL


# Function using RasterBrick ---------------------------------------------------
#' 
#' @rdname otbHaraTex
#'
setMethod("otbHaraTex", 
          signature(x = "RasterBrick"), 
          function(x,
                   outDir=NULL,
                   retRaster=TRUE,
                   ram="8192",
                   parameters.xyrad=list(c(1,1)),
                   parameters.xyoff=list(c(1,1)),
                   parameters.minmax=c(0,255),
                   parameters.nbbin=8,
                   texture="advanced",
                   channel=NULL,
                   verbose=FALSE){
            writeRaster(x, file = paste0(outDir, "tmp.tif"), overwrite = TRUE)
            x <- paste0(outDir, "tmp.tif")
            tempout <- format(Sys.time(), "%Y-%m-%d-%H%M%S")
            retStack <- otbHaraTex(x = x,
                                   out = tempout,
                                   outDir = outDir,
                                   retRaster = TRUE,
                                   parameters.xyrad = parameters.xyrad,
                                   parameters.xyoff = parameters.xyoff,
                                   parameters.minmax = parameters.minmax,
                                   parameters.nbbin = parameters.nbbin,
                                   texture = texture,
                                   channel = channel,
                                   verbose = verbose,
                                   ram = ram)
            file.remove(x)
            tmpfiles <- list.files(outDir, 
                                   pattern = glob2rx(paste0("*", tempout, "*")),
                                   full.names = TRUE)
            file.remove(tmpfiles)
            return(retStack)
          })
          

# Function using RasterLayer ---------------------------------------------------
#' 
#' @rdname otbHaraTex
#'
setMethod("otbHaraTex", 
          signature(x = "RasterLayer"), 
          function(x,
                   outDir=NULL,
                   retRaster=TRUE,
                   ram="8192",
                   parameters.xyrad=list(c(1,1)),
                   parameters.xyoff=list(c(1,1)),
                   parameters.minmax=c(0,255),
                   parameters.nbbin=8,
                   texture="advanced",
                   channel=NULL,
                   verbose=FALSE){
            retStack <- otbHaraTex(x = raster::brick(x),
                                   outDir = outDir,
                                   retRaster = retRaster,
                                   parameters.xyrad = parameters.xyrad,
                                   parameters.xyoff = parameters.xyoff,
                                   parameters.minmax = parameters.minmax,
                                   parameters.nbbin = parameters.nbbin,
                                   texture = texture,
                                   channel = channel,
                                   verbose = verbose,
                                   ram = ram)
            return(retStack)
          })


# Function using RasterStack ---------------------------------------------------
#' 
#' @rdname otbHaraTex
#'
setMethod("otbHaraTex", 
          signature(x = "RasterStack"), 
          function(x,
                   outDir=NULL,
                   retRaster=TRUE,
                   ram="8192",
                   parameters.xyrad=list(c(1,1)),
                   parameters.xyoff=list(c(1,1)),
                   parameters.minmax=c(0,255),
                   parameters.nbbin=8,
                   texture="advanced",
                   channel=NULL,
                   verbose=FALSE){
            retStack <- otbHaraTex(x = raster::brick(x),
                                   outDir = outDir,
                                   retRaster = retRaster,
                                   parameters.xyrad = parameters.xyrad,
                                   parameters.xyoff = parameters.xyoff,
                                   parameters.minmax = parameters.minmax,
                                   parameters.nbbin = parameters.nbbin,
                                   texture = texture,
                                   channel = channel,
                                   verbose = verbose,
                                   ram = ram)
            return(retStack)
          })

# Function using GeoTiff -------------------------------------------------------
#' 
#' @rdname otbHaraTex
#'
setMethod("otbHaraTex", 
          signature(x = "character"), 
          function(x,
                   out="hara",
                   outDir=NULL,
                   retRaster=FALSE,
                   parameters.xyrad=list(c(1,1)),
                   parameters.xyoff=list(c(1,1)),
                   parameters.minmax=c(0,255),
                   parameters.nbbin=8,
                   texture="advanced",
                   channel=NULL,
                   verbose=FALSE,
                   ram="8192"){
            
            # initialize the return raster stack
            retStack<-list()
            
            # if no channel number is provided take all tif bands
            if (is.null(channel)){
              channel <- seq(length(grep(gdalUtils::gdalinfo(x,nomd = TRUE),
                                         pattern = "Band ")))
            } 
            
            # for each band do
            for (band in channel) {
              # the following filters
              for (xyrad in parameters.xyrad) {
                for (xyoff in parameters.xyoff) {
                  # generate the putputfilename
                  outName<-paste0(outDir,
                                  "band_", band, "_", 
                                  out, "_",
                                  texture, "_",
                                  xyrad[1], xyrad[2], "_",
                                  xyoff[1], xyoff[2],
                                  ".tif")
                  # start otb command generation with the valid path to otbcli and the module name
                  command<-paste0(otbPath,"otbcli_HaralickTextureExtraction")
                  # now add all arguments
                  command<-paste(command, " -in ", x)
                  command<-paste(command, " -channel ", channel)
                  command<-paste(command, " -out ", outName)
                  command<-paste(command, " -ram ",ram)
                  command<-paste(command, " -parameters.xrad ",xyrad[1])
                  command<-paste(command, " -parameters.yrad ",xyrad[2])
                  command<-paste(command, " -parameters.xoff ",xyoff[1])
                  command<-paste(command, " -parameters.yoff ",xyoff[2])
                  command<-paste(command, " -parameters.min ",parameters.minmax[1])
                  command<-paste(command, " -parameters.max ",parameters.minmax[2])
                  command<-paste(command, " -parameters.nbbin ",parameters.nbbin)
                  command<-paste(command, " -texture ",texture)
                  # if verbose is true
                  if (verbose) {
                    cat("\nrunning cmd:  ", command[band],"\n")
                    system(command[band])
                  } else{
                    system(command[band],intern = TRUE,ignore.stdout = TRUE)
                  }  
                  # if you want to have a rasterstack returned do it
                  if (retRaster) retStack[[band]] <- 
                    assign(paste0(tools::file_path_sans_ext(basename(outName)),
                                  "band_",band),raster::stack(outName))
                }
              }
            }
            return(retStack)
          })