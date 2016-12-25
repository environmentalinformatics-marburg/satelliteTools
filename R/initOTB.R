#' getOTB setup the orfeo toolbox bindings for an rsession
#'@description  
#'Try to find all valid OTB installation and returns the pathes and environment settings 
#'@param defaultOTBPath string contains path to otb binaries
#'@param DL hard drive letter
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


initOTB <- function(defaultOTBPath=NULL,installationRoot=paste0(dirname(defaultOTBPath), "/"), otbType=NULL,DL="C:"){
  
  # (R) set pathes  of OTB  binaries depending on OS WINDOWS
  if (is.null(defaultOTBPath)){
    
    # if no path is provided  we have to search
    otbParams<-searchOSgeo4WOTB(DL=DL)
    
    # if just one valid installation was found take it
    if (nrow(otbParams) == 1) {  
      otbPath<-setOtbEnv(defaultOtb=otbParams$binDir[1],installationRoot=otbParams$baseDir[2])
      
      # if more than one valid installation was found you have to choose 
    } else if (nrow(otbParams) > 1) {
      cat("You have more than one valid OTB version\n")
      #print("installation folder: ",otbParams$baseDir,"\ninstallation type: ",otbParams$installationType,"\n")
      print(otbParams[1],right = FALSE,row.names = TRUE) 
      if (is.null(otbType)) {
        ver<- as.numeric(readline(prompt = "Please choose one:  "))
        otbPath<-setOTBEnv(defaultOtb=otbParams$binDir[[ver]],installationRoot = otbParams$baseDir[[ver]])
      } else {
        otbPath<-setOTBEnv(defaultOtb=otbParams[otbParams["installationType"]==otbType][1],installationRoot = otbParams[otbParams["installationType"]==otbType][2])
      }
    }
    # if a setDefaultOTB was provided take this 
  } else {
    otbPath <- setOTBEnv(defaultOTBPath = defaultOTBPath, installationRoot = installationRoot)  
  }
  return(otbPath)
}