#' Set texture directory if not supplied for OTB functions
#' 
#' @description Set texture output directory if not supplied for OTB functions
#' @export getOutputDir
#' @name getOutputDir
#' @examples 
#'\dontrun{
#'}
getOutputDir<- function (outDir){
  if (!is.null(outDir)) {
    otbOutputDir<-outDir
    if (!file.exists(paste0(otbOutputDir, "/texture/"))) dir.create(file.path(paste0(otbOutputDir, "/texture/")), recursive = TRUE,showWarnings = FALSE)
  } else {
    otbOutputDir<-paste0(getwd(),"/texture/")
    if (!file.exists(paste0(otbOutputDir, "/texture/"))) dir.create(file.path(paste0(otbOutputDir, "/texture/")), recursive = TRUE,showWarnings = FALSE)
  }
  return(otbOutputDir)
}



#'Set environment parameters of OTB
#'@description  during a rsession you will have full access to OTB via the command line 
#'
#'@param otbPath string contains path to otb binaries
#'@param sagaCmd string contains the full string to call otb launcher
#'
#'@return 
#' add otb pathes to the enviroment and creates global variables otbCmd
#'@name setOTBEnv
#' 
#'@export setOTBEnv
#'
#' @examples 
#'\dontrun{
#'}


setOTBEnv <- function(defaultOTBPath = "C:\\OSGeo4W64\\bin",installationRoot="C:\\OSGeo4W64"){
  
  if (substr(Sys.getenv("COMPUTERNAME"),1,5)=="PCRZP") {
    defaultOTBPath <- shQuote("C:\\Program Files\\QGIS 2.14\\bin")
    installationRoot <- shQuote("C:\\Program Files\\QGIS 2.14")
    Sys.setenv(GEOTIFF_CSV=paste0(Sys.getenv("OSGEO4W_ROOT"),"\\share\\epsg_csv"),envir = .GlobalEnv)
    
  } else {
    
    # (R) set pathes  of otb modules and binaries depending on OS  
    if(Sys.info()["sysname"] == "Windows"){
      
      makGlobalVar(name="otbPath", value=defaultOTBPath)
      add2Path(newPath = defaultOTBPath)
      Sys.setenv(OSGEO4W_ROOT=installationRoot)
      Sys.setenv(GEOTIFF_CSV=paste0(Sys.getenv("OSGEO4W_ROOT"),"\\share\\epsg_csv"),envir = .GlobalEnv)
      
    } else {
      makGlobalVar("otbPath", "(usr/bin/")
    }
  }
  return(defaultOTBPath)
}


#' Search for valid OTB installations on a given windows drive 
#'@description  provides a pretty good estimation of valid OTB installations on your Windows system
#'@param DL drive letter default is "C:"
#'@return a dataframe with the OTB root dir the Version name and the installation type
#'@author Chris Reudenbach
#'@name searchOSgeo4WOTB
#'@export searchOSgeo4WOTB
#' @examples 
#'\dontrun{
#'}
searchOSgeo4WOTB <- function(DL = "C:"){
  
  
  if (substr(Sys.getenv("COMPUTERNAME"),1,5)=="PCRZP") {
    defaultOTBPath <- shQuote("C:\\Program Files\\QGIS 2.14\\bin")
    otbInstallations<- data.frame(instDir = shQuote("C:\\Program Files\\QGIS 2.14\\bin"), installationType = "osgeo4wOTB",stringsAsFactors = FALSE)
    Sys.setenv(GEOTIFF_CSV=paste0(Sys.getenv("OSGEO4W_ROOT"),"\\share\\epsg_csv"),envir = .GlobalEnv)
    
  } else {
    
    # trys to find a osgeo4w installation on the whole C: disk returns root directory and version name
    # recursive dir for otb*.bat returns all version of otb bat files
    cat("\nsearching for OTB installations - this may take a while\n")
    cat("Alternatively you can provide a path like: C:\\OSGeo4W64\\bin\\\n")
    cat("You can also provide a installation type like: 'osgeo4w64OTB'\n")
    rawOTB <- system(paste0("cmd.exe /c dir /B /S ",DL,"\\","otbcli.bat"),intern = TRUE)
    
    # trys to identify valid otb installations and their version numbers
    otbInstallations <- lapply(seq(length(rawOTB)), function(i){
      # convert codetable according to cmd.exe using type
      batchfileLines <- rawOTB[i]
      installerType<-""
      # if the the tag "OSGEO4W" exists set installationType
      if (length(unique(grep(paste("OSGeo4W64", collapse = "|"), rawOTB[i], value = TRUE))) > 0){
        rootDir<-unique(grep(paste("OSGeo4W64", collapse = "|"), rawOTB[i], value = TRUE))
        rootDir<- substr(rootDir,1, gregexpr(pattern = "otbcli.bat", rootDir)[[1]][1] - 1)
        installDir<-substr(rootDir,1, gregexpr(pattern = "bin", rootDir)[[1]][1] - 2)
        installerType<- "osgeo4w64OTB"
      }    
      
      # if the the tag "OSGEO4W" exists set installationType
      else if (length(unique(grep(paste("OSGeo4W", collapse = "|"), rawOTB[i], value = TRUE))) > 0){
        rootDir<-unique(grep(paste("OSGeo4W", collapse = "|"), rawOTB[i], value = TRUE))
        rootDir<- substr(rootDir,1, gregexpr(pattern = "otbcli.bat", rootDir)[[1]][1] - 1)
        installDir<-substr(rootDir,1, gregexpr(pattern = "bin", rootDir)[[1]][1] - 2)
        installerType<- "osgeo4wOTB"
      }
      # if the the tag "QGIS" exists set installationType
      else if (length(unique(grep(paste("QGIS", collapse = "|"), batchfileLines, value = TRUE))) > 0){
        rootDir<-unique(grep(paste("QGIS", collapse = "|"), rawOTB[i], value = TRUE))
        rootDir<- substr(rootDir,1, gregexpr(pattern = "otbcli.bat", rootDir)[[1]][1] - 1)
        installDir<-substr(rootDir,1, gregexpr(pattern = "bin", rootDir)[[1]][1] - 2)
        installerType<- "qgisOTB"
      }
      
      # put the existing GISBASE directory, version number  and installation type in a data frame
      data.frame(binDir = rootDir, baseDir=installDir, installationType = installerType,stringsAsFactors = FALSE)
      
    }) # end lapply
    
    # bind the df lines
    otbInstallations <- do.call("rbind", otbInstallations)
  }
  return(otbInstallations)
}


#'Assign variable in .GlobalEnv
#'@description  Assign variable in .GlobalEnv
#'@param name Variable name
#'@param value Value of varaible
#'@return NONE
#'@author Chris Reudenbach
#'@name makGlobalVar
#'@export makGlobalVar
#' @examples 
#'\dontrun{
#'}
makGlobalVar <- function(name,value) {
  if(exists(name, envir = .GlobalEnv)) {
    warning(paste0("The variable '", name,"' already exist in .GlobalEnv"))
    assign(name, value, envir = .GlobalEnv, inherits = TRUE)
  } else {
    assign(name, value, envir = .GlobalEnv, inherits = TRUE)
  } 
}


#'Add a variable to the system path
#'@description Add a variable to the system path
#'@param newPath Path to be added
#'@return NONE
#'@author Chris Reudenbach
#'@name add2Path
#'@export add2Path
#' @examples 
#'\dontrun{
#'}
add2Path<- function(newPath) {
  exist<-FALSE
  if(Sys.info()["sysname"] == "Windows"){
    del<-";"  
  } else {
    del<-":"  
  } 
  p <- Sys.getenv("PATH")
  if(substr(p, 1,nchar(newPath)) == newPath){
    exist <- TRUE
  }
  # if not exist append path to systempath
  if (!exist){
    Sys.setenv(PATH=paste0(newPath,del,Sys.getenv("PATH")))
  }
}