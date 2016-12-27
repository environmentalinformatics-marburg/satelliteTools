#' getOTB setup the orfeo toolbox bindings for an rsession
#'@description  
#'Try to find all valid OTB installation and returns the pathes and environment settings 
#'@param path_otb string contains path to otb binaries
#'@param dl hard drive letter
#'@return 
#' add otb pathes to the enviroment and creates global variables otbPath
#' 
#'@name initOTB
#'
#'@export initOTB
#'
#'@examples 
#'\dontrun{
#'## call it for a default OSGeo4W oinstallation of SAGA
#'initOTB("C:\\OSGeo4W64\\bin\\")
#'}
#'


initOTB <- function(path_otb=NULL,path_otb_root=paste0(dirname(path_otb), "/"), otb_type=NULL,dl="C:"){
  
  # (R) set pathes  of OTB  binaries depending on OS WINDOWS
  if (is.null(path_otb)){
    
    # if no path is provided  we have to search
    otbParams<-searchOSgeo4WOTB(dl=dl)
    
    # if just one valid installation was found take it
    if (nrow(otbParams) == 1) {  
      otbPath<-setOtbEnv(defaultOtb=otbParams$path_bin[1],path_otb_root=otbParams$path_base[2])
      
      # if more than one valid installation was found you have to choose 
    } else if (nrow(otbParams) > 1) {
      cat("You have more than one valid OTB version\n")
      #print("installation folder: ",otbParams$path_base,"\ninstallation type: ",otbParams$installation_type,"\n")
      print(otbParams[1],right = FALSE,row.names = TRUE) 
      if (is.null(otb_type)) {
        ver<- as.numeric(readline(prompt = "Please choose one:  "))
        otbPath<-setOTBEnv(defaultOtb=otbParams$path_bin[[ver]],path_otb_root = otbParams$path_base[[ver]])
      } else {
        otbPath<-setOTBEnv(defaultOtb=otbParams[otbParams["installation_type"]==otb_type][1],path_otb_root = otbParams[otbParams["installation_type"]==otb_type][2])
      }
    }
    # if a setDefaultOTB was provided take this 
  } else {
    otbPath <- setOTBEnv(path_otb = path_otb, path_otb_root = path_otb_root)  
  }
  return(otbPath)
}