#' Create texture variables based on glcm
#' 
#' @description  Wrapper arround glcm in order to create a set of texture 
#' variables.
#' @note for the use glcmTextures a glcm wrapper function 
#'       a raster* object is required
#' @param x rasterLayer or a rasterStack containing different channels
#' @param n_rasters vector of channels to use from x. Default =nlayers(x)
#' @param kernel_size vector of numbers indicating the environment sizes for 
#' which the textures are calculated
#' @param stats string vector of parameters to be calculated. See 
#' \code{\link{glcm}}
#' @param n_grey number of grey values. see \code{\link{glcm}}
#' @param parallel logical value indicating whether parameters are calculated 
#' parallel or not
#' @param min_x for each channel the minimum value which can occur. If NULL then
#' the minimum value from the rasterLayer is used.
#' @param max_x for each channel the maximum value which can occur. If NULL then
#' the maximum value from the rasterLayer is used.
#' @return list of RasterStacks containing the texture parameters for each 
#' combination of channel and kernel_size  
#' @details This functions calls the glcm function from \link{glcm} with 
#' standard settings
#' @author Hanna Meyer
#' 
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
#' GLCM Mean shows  r< 0.1 with any of the other texture measures demonstrated 
#' in this tutorial.
#' GLCM Correlation shows  r<0.5 with any other measure.
#' @name glcmTextures
#' @export glcmTextures
#' @seealso \code{\link{glcm}}
#' @examples 
#' \dontrun{
#' ## example on how to calculate texture from a list of channels
#' 
#' url<-"http://www.ldbv.bayern.de/file/zip/5619/DOP%2040_CIR.zip"
#' res <- curl::curl_download(url, "testdata.zip")
#' unzip(res,junkpaths = TRUE,overwrite = TRUE)
#' r<- raster::stack(paste0(getwd(),"4490600_5321400.tif"))
#' 
#' # call glcm wrapper
#' result <- glcmTextures(r,n_rasters=1:3,
#' stats=c("mean", "variance", "homogeneity"))
#' 
#' #plot the results from VIS0.6 channel:
#' raster::plot(unlist(unlist(glcm$size_3$X4490600_5321400.1)))
#' }
glcmTextures <- function(x,
                         n_rasters=1:nlayers(x),
                         kernel_size=c(3),
                         stats=c("mean", "variance", "homogeneity", 
                                 "contrast", "dissimilarity", "entropy", 
                                 "second_moment", "correlation"),
                         shift=list(c(0,1), c(1,1), c(1,0),c(1,-1)),
                         parallel=TRUE,
                         n_grey = 32,
                         min_x=NULL,
                         max_x=NULL){
  require(glcm) 
  require(raster)
  if (parallel){
    require(doParallel)
    registerDoParallel(detectCores()-1)
  }
  
  
  #set values larger than the max/min value to the max/minvalue. 
  #Otherwise NA would be used
  if(!is.null(min_x)){
    if (length(n_rasters)>1){
      for (i in n_rasters){
        x[[i]]=reclassify(x[[i]], c(max_x[i],Inf,max_x[i]))
        x[[i]]=reclassify(x[[i]], c(-Inf,min_x[i],min_x[i]))
      }
    } else { # only one raster
      x=reclassify(x, c(max_x,Inf,max_x))
      x=reclassify(x, c(-Inf,min_x,min_x)) 
    }
  }
  
  
  glcm_filter<-list()
  for (j in 1:length(kernel_size)){
    if (class (x)=="RasterStack"||class (x)=="RasterBrick"){  
      if (parallel){
        glcm_filter[[j]]<-foreach(i=n_rasters,
                                  .packages= c("glcm","raster"))%dopar%{
                                    glcm(x[[i]], 
                                         window = c(kernel_size[j], 
                                                    kernel_size[j]), 
                                         shift=shift,
                                         statistics=stats,n_grey=n_grey,
                                         min_x=min_x[i],max_x=max_x[i],
                                         na_opt="center")
                                  } 
      } else {
        glcm_filter[[j]]<-foreach(i=n_rasters,
                                  .packages= c("glcm","raster"))%do%{
                                    mask(glcm(x[[i]], 
                                              window = c(kernel_size[j], 
                                                         kernel_size[j]), 
                                              shift=shift,
                                              statistics=stats,n_grey=n_grey,
                                              min_x=min_x[i],max_x=max_x[i],
                                              na_opt="center"), x[[i]])
                                  }
      }
      names(glcm_filter[[j]])<-names(x)[n_rasters]
    } else {
      glcm_filter[[j]]<-mask(glcm(x, window = c(kernel_size[j], kernel_size[j]), 
                                  shift=shift,
                                  statistics=stats,n_grey=n_grey,
                                  min_x=min_x,max_x=max_x,
                                  na_opt="center"), x)
    }   
  }
  names(glcm_filter)<-paste0("size_",kernel_size)
  return(glcm_filter)
}