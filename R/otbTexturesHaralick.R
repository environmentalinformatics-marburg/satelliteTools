if ( !isGeneric("otbTexturesHaralick") ) {
  setGeneric("otbTexturesHaralick", function(x, ...)
    standardGeneric("otbTexturesHaralick"))
}
#' Calculate selected texture parameters on gray level properties (mainly after
#' Haralick).
#' 
#' @note the otb is used for filtering. please provide a GeoTiff file
#' @param x A \code{Raster*} object or a GeoTiff containing 1 or more gray 
#' value bands
#' @param output_name string pattern vor individual naming of the output file(s)
#' @param parameters.xyrad list with the x and y radius in pixel indicating the kernel sizes for which the textures are calculated
#' @param parameters.xyoff  vector containg the directional offsets. Valid combinations are: list(c(1,1),c(1,0),c(0,1),c(1,-1))
#' @param n_grey Number of grey values. 
#' @param parallel A logical value indicating whether parameters are calculated parallely or not
#' @param parameters.minmax   minimum/maximum gray value which can occur. 
#' @param parameters.nbbin number of gray level bins (classes)
#' @param texture type of filter "simple" "advanced" "higher"
#' @param channel sequence of bands to be processed
#' @param ram reserved memory in MB
#' @param return_raster boolean if TRUE a raster stack is returned
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
#' @name otbTexturesHaralick
#' @export otbTexturesHaralick
#' @examples 
#' \dontrun{
#' url<-"http://www.ldbv.bayern.de/file/zip/5619/DOP%2040_CIR.zip"
#' res <- curl::curl_download(url, "testdata.zip")
#' unzip(res,junkpaths = TRUE,overwrite = TRUE)
#' otbTexturesHaralick(x=paste0(getwd(),"4490600_5321400.tif"),texture="simple")
#' }
NULL


# Function using RasterBrick ---------------------------------------------------
#' 
#' @rdname otbTexturesHaralick
#'
setMethod("otbTexturesHaralick", 
          signature(x = "RasterBrick"), 
          function(x,
                   texture="all",
                   path_output=NULL,
                   return_raster=TRUE,
                   parameters.xyrad=list(c(1,1)),
                   parameters.xyoff=list(c(1,1)),
                   parameters.minmax=c(0,255),
                   parameters.nbbin=8,
                   channel=NULL,
                   verbose=FALSE,
                   ram="8192"){
            writeRaster(x, file = paste0(path_output, "tmp.tif"), overwrite = TRUE)
            x <- paste0(path_output, "tmp.tif")
            tempout <- format(Sys.time(), "%Y-%m-%d-%H%M%S")
            ret_textures <- otbTexturesHaralick(x = x,
                                            texture = texture,
                                            output_name = tempout,
                                            path_output = path_output,
                                            return_raster = TRUE,
                                            parameters.xyrad = parameters.xyrad,
                                            parameters.xyoff = parameters.xyoff,
                                            parameters.minmax = parameters.minmax,
                                            parameters.nbbin = parameters.nbbin,
                                            channel = channel,
                                            verbose = verbose,
                                            ram = ram)
            file.remove(x)
            tmpfiles <- list.files(path_output, 
                                   pattern = glob2rx(paste0("*", tempout, "*")),
                                   full.names = TRUE)
            file.remove(tmpfiles)
            return(ret_textures)
          })


# Function using RasterLayer ---------------------------------------------------
#' 
#' @rdname otbTexturesHaralick
#'
setMethod("otbTexturesHaralick", 
          signature(x = "RasterLayer"), 
          function(x,
                   texture="all",
                   path_output=NULL,
                   return_raster=TRUE,
                   parameters.xyrad=list(c(1,1)),
                   parameters.xyoff=list(c(1,1)),
                   parameters.minmax=c(0,255),
                   parameters.nbbin=8,
                   channel=NULL,
                   verbose=FALSE,
                   ram="8192"){
            ret_textures <- otbTexturesHaralick(x = raster::brick(x),
                                            texture = texture,
                                            path_output = path_output,
                                            return_raster = return_raster,
                                            parameters.xyrad = parameters.xyrad,
                                            parameters.xyoff = parameters.xyoff,
                                            parameters.minmax = parameters.minmax,
                                            parameters.nbbin = parameters.nbbin,
                                            channel = channel,
                                            verbose = verbose,
                                            ram = ram)
            return(ret_textures)
          })


# Function using RasterStack ---------------------------------------------------
#' 
#' @rdname otbTexturesHaralick
#'
setMethod("otbTexturesHaralick", 
          signature(x = "RasterStack"), 
          function(x,
                   texture="all",
                   path_output=NULL,
                   return_raster=TRUE,
                   parameters.xyrad=list(c(1,1)),
                   parameters.xyoff=list(c(1,1)),
                   parameters.minmax=c(0,255),
                   parameters.nbbin=8,
                   channel=NULL,
                   verbose=FALSE,
                   ram="8192"){
            ret_textures <- otbTexturesHaralick(x = raster::brick(x),
                                            texture = texture,
                                            path_output = path_output,
                                            return_raster = return_raster,
                                            parameters.xyrad = parameters.xyrad,
                                            parameters.xyoff = parameters.xyoff,
                                            parameters.minmax = parameters.minmax,
                                            parameters.nbbin = parameters.nbbin,
                                            channel = channel,
                                            verbose = verbose,
                                            ram = ram)
            return(ret_textures)
          })

# Function using GeoTiff -------------------------------------------------------
#' 
#' @rdname otbTexturesHaralick
#'
setMethod("otbTexturesHaralick", 
          signature(x = "character"), 
          function(x,
                   texture="all",
                   output_name="hara",
                   path_output=NULL,
                   return_raster=FALSE,
                   parameters.xyrad=list(c(1,1)),
                   parameters.xyoff=list(c(1,1)),
                   parameters.minmax=c(0,255),
                   parameters.nbbin=8,
                   channel=NULL,
                   verbose=FALSE,
                   ram="8192"){
            
            if(texture == "all"){
              texture <- c("simple", "advanced", "higher")
            }
            
            if (is.null(channel)){
              channel <- seq(length(grep(gdalUtils::gdalinfo(x,nomd = TRUE),
                                         pattern = "Band ")))
            } 
            
            ret_textures <- lapply(channel, function(band){
              ret_textures <- lapply(parameters.xyrad, function(xyrad){
                ret_textures <- lapply(parameters.xyoff, function(xyoff){
                  ret_textures <- lapply(texture, function(txt){
                    path_outfile <- paste0(path_output,
                                           "band_", band, "_", 
                                           output_name, "_",
                                           txt, "_",
                                           xyrad[1], xyrad[2], "_",
                                           xyoff[1], xyoff[2],
                                           ".tif")
                    command<-paste0(otbPath,"otbcli_HaralickTextureExtraction",
                                    " -in ", x,
                                    " -channel ", band,
                                    " -out ", path_outfile,
                                    " -ram ",ram,
                                    " -parameters.xrad ",xyrad[1]
                                    , " -parameters.yrad ",xyrad[2]
                                    , " -parameters.xoff ",xyoff[1]
                                    , " -parameters.yoff ",xyoff[2]
                                    , " -parameters.min ",parameters.minmax[1]
                                    , " -parameters.max ",parameters.minmax[2]
                                    , " -parameters.nbbin ",parameters.nbbin,
                                    " -texture ",txt)
                    if (verbose) {
                      cat("\nrunning cmd:  ", command[band],"\n")
                      system(command)
                    } else{
                      system(command,intern = TRUE,ignore.stdout = TRUE)
                    }
                    if (return_raster){
                      if(txt == "simple"){
                        bnames <- c("Energy", "Entropy", "Correlation", 
                                    "Inverse_Difference_Moment", "Inertia", 
                                    "Cluster_Shade", "Cluster_Prominence",
                                    "Haralick_Correlation")
                      } else if(txt == "advanced"){
                        bnames <- c("Mean", "Variance", "Dissimilarity",
                                    "Sum_Average", 
                                    "Sum_Variance", "Sum_Entropy", 
                                    "Difference_of_Variances", 
                                    "Difference_of_Entropies", 
                                    "IC1", "IC2")
                      } else if(txt == "higher"){
                        bnames <- c("Short_Run_Emphasis", 
                                    "Long_Run_Emphasis", 
                                    "Grey-Level_Nonuniformity", 
                                    "Run_Length_Nonuniformity", 
                                    "Run_Percentage", 
                                    "Low_Grey-Level_Run_Emphasis", 
                                    "High_Grey-Level_Run_Emphasis", 
                                    "Short_Run_Low_Grey-Level_Emphasis", 
                                    "Short_Run_High_Grey-Level_Emphasis", 
                                    "Long_Run_Low_Grey-Level_Emphasis",
                                    "Long_Run_High_Grey-Level_Emphasis")
                      }
                      ret_textures <- raster::stack(path_outfile)
                      names(ret_textures) <- paste0(bnames, "-", 
                                                    "b", band,
                                                    "r", xyrad[1],
                                                    "o", xyoff[1],
                                                    "m", parameters.minmax[1],
                                                    parameters.minmax[2])
                    } else {
                      ret_textures <- NULL
                    }
                    return(ret_textures)
                  })
                  raster::stack(ret_textures)
                })
                raster::stack(ret_textures)
              })
              raster::stack(ret_textures)
            })
            raster::stack(ret_textures)
          })