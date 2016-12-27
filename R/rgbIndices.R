#' RGB indices 
#' 
#' @description
#' This function calculates various spectral indices from a RGB. It returns at least red green and blue as splitted channels in a stack. Additionally you can choose RGB indices.
#' \code{Raster*} object.  
#' 
#' @param rgb a \code{RasterStack} or \code{RasterBrick} object. 3
#' bands are mandatory (for RGB indices they should be: "red", "green" and "blue").
#' @param rgbi the implemented RGB indices currently \link{seealso}

#' @return 
#' @seealso 
#' A \code{RasterLayer} with the index calculates as:\cr
#' VARI (green-red)/(green+red-blue). A Visible Atmospherically Resistant Index (VARI)\cr
#' BI  sqrt((R**2+G**2+B*2)/3 Brightness Index\cr
#' SI (R-B)/(R+B) Spectra Slope Saturation Index\cr
#' HI (2*R-G-B)/(G-B) Primary colours Hue Index\cr
#' CI (R-G)/(R+G) Soil Colour Index\cr
#' RI R**2/(B*G**3) Redness Index\cr
#' NDTI (R-G)/(R+G) Normalized difference turbidity index Water\cr
#' NGRDI (G-R)/(G+R) Normalized green red difference index (sometimes GRVI) Tucker (1979)
#' VVI  (1-(r-30)/(r+30))*(1-(g-50)/(g+50))*(1-(b-1)/(b+1))\cr
#' TGI  -0.5[190(R670-R550)-120(R670 - R480)] The triangular greenness index (TGI) estimates chlorophyll concentration in leaves and canopies\cr
#' GLI Green leaf index Vis GLI (2*g - r - b)/(2*g + r + b) Louhaichi et al. (2001)
#' 
#' @author
#' Chris Reudenbach
#' 
#' @name rgbIndices
#' @export rgbIndices
#' 
#' @references
#' 
#' Planetary Habitability Laboratory (2015): Visible Vegetation Index (VVI). Available online via \url{http://phl.upr.edu/projects/visible-vegetation-index-vvi}.
#' 
#' Lacaux, J. P., Tourre, Y. M., Vignolles, C., Ndione, J. A., and Lafaye, M.: Classification of ponds from high-spatial resolution remote sensing: Application to Rift Valley Fever epidemics in Senegal, Remote Sens. Environ., 106, 66-74, 2007. 
#' 
#' Gitelson, A., et al.: Vegetation and Soil Lines in Visible Spectral Space: A Concept and Technique for Remote Estimation of Vegetation Fraction.  International Journal of Remote Sensing 23 (2002): 2537-2562. (VARI)
#' 
#' MADEIRA, J., BEDIDI, A., CERVELLE, B., POUGET, M. and FLAY, N., 1997, Visible spectrometric indices of hematite (Hm) and goethite (Gt) content in lateritic soils: 5490 N. Levin et al. the application of a Thematic Mapper (TM) image for soil-mapping in Brasilia, Brazil. International Journal of Remote Sensing, 18, pp. 2835-2852.
#'  
#' MATHIEU, R., POUGET, M., CERVELLE, B. and ESCADAFAL, R., 1998, Relationships between satellite-based radiometric indices simulated using laboratory reflectance data and typic soil colour of an arid environment. Remote Sensing of Environment, 66, pp. 17-28. 
#' 
#' Louhaichi, M., Borman, M.M., Johnson, D.E., 2001. Spatially located platform and aerial photography for documentation of grazing impacts on wheat. Geocarto International 16, 65-70.
#' 
#' Tucker, C.J., 1979. Red and photographic infrared linear combinations for monitoring vegetation. Remote Sensing of Environment 8, 127-150.
#' 
#' @seealso 
#' For a comprehensive overview of remote sensing indices have a look at: \url{http://www.indexdatabase.de/db/i.php}(A database for remote sensing indices)\cr
#' Wavelength ranges for overlapping digital camera bands are: red 580-670 nm, green 480-610 nm, and blue 400-520 nm (Hunt et al., 2005)
#' http://digitalcommons.unl.edu/cgi/viewcontent.cgi?article=2161&context=usdaarsfacpub
#' 
#' @examples
#' \notrun{
#' library(raster)
#' url <- "https://upload.wikimedia.org/wikipedia/commons/2/28/RGB_illumination.jpg"
#' dFile <- download.file(url, "Image.jpg")
#' img <- stack("Image.jpg") 
#' plotRGB(img)
#' rgbi <- rgbI(img)
#' plot(rgbI, col = gray(255:0/255))
#' }
#' 
#' 
rgbIndices<- function(rgb,
                rgbi=c("red","green","blue","VVI","VARI","NDTI","RI","CI","BI","SI","HI","TGI","GLI","NGRDI"),
                write_tif=FALSE,
                path_output=getwd(),
                output_name="rgbi") {
  
  ## compatibility check
  if (raster::nlayers(rgb) < 3)
    stop("Argument 'rgb' needs to be a Raster* object with at least 3 layers (usually red, green and blue).")
  
  ### processing
  
  
  ## separate visible bands
  red <- rgb[[1]]
  green <- rgb[[2]]
  blue <- rgb[[3]]
  resultStack<-raster::stack(red,green,blue)
  names(resultStack) <- c("red","green","blue")
  for (item in rgbi) {
    ## calculate Visible Vegetation Index vvi
    if (item=="VVI"){
      cat("\ncalculate Visible Vegetation Index (VVI)")
      VVI <- (1 - abs((red - 30) / (red + 30))) * 
        (1 - abs((green - 50) / (green + 50))) * 
        (1 - abs((blue - 1) / (blue + 1)))
      resultStack<-  raster::stack(resultStack,VVI)
      names(resultStack) <- c(names(resultStack)[1:length(names(resultStack))-1],"VVI")
      
    } else if (item=="VARI"){
      # calculate Visible Atmospherically Resistant Index (VARI)
      cat("\ncalculate Visible Atmospherically Resistant Index (VARI)")
      VARI<-(green-red)/(green+red-blue)
      resultStack<-  raster::stack(resultStack,VARI)
      names(resultStack) <- c(names(resultStack)[1:length(names(resultStack))-1],"VARI")
      
    } else if (item=="NDTI"){
      ## Normalized difference turbidity index
      cat("\ncalculate Normalized difference turbidity index (NDTI)")
      NDTI<-(red-green)/(red+green)
      resultStack<-  raster::stack(resultStack,NDTI)
      names(resultStack) <- c(names(resultStack)[1:length(names(resultStack))-1],"NDTI")
      
    } else if (item=="RI"){
      # redness index
      cat("\ncalculate redness index (RI)")
      RI<-red**2/(blue*green**3)
      resultStack<-  raster::stack(resultStack,RI)
      names(resultStack) <- c(names(resultStack)[1:length(names(resultStack))-1],"RI")
      
    } else if (item=="CI"){
      # CI Soil Colour Index
      cat("\ncalculate Soil Colour Index (CI)")
      CI<-(red-green)/(red+green)
      resultStack<-  raster::stack(resultStack,CI)
      names(resultStack) <- c(names(resultStack)[1:length(names(resultStack))-1],"CI")
      
    } else if (item=="BI"){
      #  Brightness Index
      cat("\ncalculate Brightness Index (BI)")
      BI<-sqrt((red**2+green**2+blue*2)/3)
      resultStack<-  raster::stack(resultStack,BI)
      names(resultStack) <- c(names(resultStack)[1:length(names(resultStack))-1],"BI")
      
    } else if (item=="SI"){
      # SI Spectra Slope Saturation Index
      cat("\ncalculate Spectra Slope Saturation Index (SI)")
      SI<-(red-blue)/(red+blue) 
      resultStack<-  raster::stack(resultStack,SI)
      names(resultStack) <- c(names(resultStack)[1:length(names(resultStack))-1],"SI")
      
    } else if (item=="HI"){    
      # HI Primary colours Hue Index
      cat("\ncalculate Primary colours Hue Index (HI)")
      HI<-(2*red-green-blue)/(green-blue)
      resultStack<-  raster::stack(resultStack,HI)
      names(resultStack) <- c(names(resultStack)[1:length(names(resultStack))-1],"HI")
      
    } else if (item=="TGI"){
      # Triangular greenness index
      cat("\ncalculate Triangular greenness index (TGI)")
      TGI <- -0.5*(190*(red - green)- 120*(red - blue))
      resultStack<-  raster::stack(resultStack,TGI)
      names(resultStack) <- c(names(resultStack)[1:length(names(resultStack))-1],"TGI")
      
    } else if (item=="GLI"){
      cat("\ncalculate Green leaf index (GLI)")
      # Green leaf index
      GLI<-(2*green-red-blue)/(2*green+red+blue)
      resultStack<-  raster::stack(resultStack,GLI)
      names(resultStack) <- c(names(resultStack)[1:length(names(resultStack))-1],"GLI")
      
    } else if (item=="NGRDI"){
      # NGRDI Normalized green red difference index 
      cat("\ncalculate Normalized green red difference index  (NGRI)")
      NGRDI<-(green-red)/(green+red) 
      resultStack<-  raster::stack(resultStack,NGRDI)
      names(resultStack) <- c(names(resultStack)[1:length(names(resultStack))-1],"NGRI")
      
    }  
    
  }
  
  if (write_tif) raster::writeRaster(x = resultStack,filename = paste0(path_output,output_name,".tif"),overwrite=TRUE)
  ## return rgbi
  return(resultStack)
}
