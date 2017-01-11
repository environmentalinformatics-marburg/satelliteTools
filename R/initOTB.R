#' initOTB
#'@title initOTB setup the orfeo toolbox bindings for an rsession
#'@description  initOTB trys to find all valid OTB installation and returns the pathes and environment settings 
#'@param defaultOTBPath string contains path to otb binaries
#'@param DL hard drive letter
#'@author CR
#'@return 
#' add otb pathes to the enviroment and creates global variables otbPath
#' 
#'@export initOTB
#'
#'@examples
#' \dontrun{
#'## call it for a default OSGeo4W oinstallation of SAGA
#'initOTB("C:\\OSGeo4W64\\bin\\")
#'}


initOTB <- function(defaultOTBPath=NULL,
                    installationRoot= NULL, 
                    otbType=NULL,
                    DL="C:",
                    otbselect=FALSE) {
  
  if(Sys.info()["sysname"] == "Linux"){
    # if no path is provided  we have to search
    
    otbParams<- system2("find", paste("/usr"," ! -readable -prune -o -type f -executable -iname 'otbcli' -print"),stdout = TRUE)
    defaultOTBPath<-substr(otbParams,1,nchar(otbParams)-6) }
  makGlobalVar("otbPath", defaultOTBPath)
  
  # (R) set pathes  of OTB  binaries depending on OS WINDOWS
  if (is.null(defaultOTBPath)) {
    otbParams<- searchOSgeo4WOTB()
    # if just one valid installation was found take it
    if (nrow(otbParams) == 1) {  
      otbPath<-setOtbEnv(defaultOtb=otbParams$binDir[1],installationRoot=otbParams$baseDir[2])
      
      # if more than one valid installation was found you have to choose 
    } else if (nrow(otbParams) > 1 & otbselect ) {
      cat("You have more than one valid OTB version\n")
      #print("installation folder: ",otbParams$baseDir,"\ninstallation type: ",otbParams$installationType,"\n")
      print(otbParams[1],right = FALSE,row.names = TRUE) 
      if (is.null(otbType)) {
        ver<- as.numeric(readline(prompt = "Please choose one:  "))
        otbPath<-setOTBEnv(defaultOtb=otbParams$binDir[[ver]],installationRoot = otbParams$baseDir[[ver]])
      } else {
        otbPath<-setOTBEnv(defaultOtb=otbParams[otbParams["installationType"]==otbType][1],installationRoot = otbParams[otbParams["installationType"]==otbType][2])
      }
    } else {
      otbPath<-setOTBEnv(defaultOtb=otbParams$binDir[[1]],installationRoot = otbParams$baseDir[[1]])
      
    }
    
    # if a setDefaultOTB was provided take this 
  } 
  return(otbPath)
}


