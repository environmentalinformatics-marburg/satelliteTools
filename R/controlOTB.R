#' Set outpath directory if not supplied for OTB functions
#' 
#' @description set output directory to working_directory/outpathOTB if not supplied for OTB functions
#' @export getOutputDir
#' @name getOutputDir
#' @examples 
#'\dontrun{
#'}
getOutputDir <- function(path_output) {
  if (!is.null(path_output)) {
    path_output <- path_output
    if (!file.exists(paste0(path_output, "/outpathOTB/"))) dir.create(file.path(paste0(path_output, "/outpathOTB/")), recursive = TRUE,showWarnings = FALSE)
  } else {
    path_output <- paste0(getwd(),"/outpathOTB/")
    if (!file.exists(paste0(path_output, "/outpathOTB/"))) dir.create(file.path(paste0(path_output, "/outpathOTB/")), recursive = TRUE,showWarnings = FALSE)
  }
  return(path_output)
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


setOTBEnv <- function(path_otb = "C:\\OSGeo4W64\\bin", rootPathOtb = "C:\\OSGeo4W64"){
  
  if (substr(Sys.getenv("COMPUTERNAME"),1,5) == "PCRZP") {
    path_otb <- shQuote("C:\\Program Files\\QGIS 2.14\\bin")
    path_otb_root <- shQuote("C:\\Program Files\\QGIS 2.14")
    Sys.setenv(GEOTIFF_CSV = paste0(Sys.getenv("OSGEO4W_ROOT"),"\\share\\epsg_csv"), envir = .GlobalEnv)
  } else {
    # (R) set pathes  of otb modules and binaries depending on OS  
    if (Sys.info()["sysname"] == "Windows") {
      makGlobalVar(name = "otbPath", value = path_otb)
      add2Path(path_new = path_otb)
      Sys.setenv(OSGEO4W_ROOT = path_otb_root)
      Sys.setenv(GEOTIFF_CSV = paste0(Sys.getenv("OSGEO4W_ROOT"),"\\share\\epsg_csv"),envir = .GlobalEnv)
     } else {
      makGlobalVar("otbPath", "(usr/bin/")
    }
  }
  return(path_otb)
}


#' Search for valid OTB installations on a given windows drive 
#'@description  provides a pretty good estimation of valid OTB installations on your Windows system
#'@param dl drive letter default is "C:"
#'@return a dataframe with the OTB root dir the Version name and the installation type
#'@author Chris Reudenbach
#'@name searchOSgeo4WOTB
#'@export searchOSgeo4WOTB
#' @examples 
#'\dontrun{
#'}
searchOSgeo4WOTB <- function(dl = "C:"){
  
  
  if (substr(Sys.getenv("COMPUTERNAME"),1,5) == "PCRZP") {
    path_otb <- shQuote("C:\\Program Files\\QGIS 2.14\\bin")
    otb_installations <- data.frame(instDir = shQuote("C:\\Program Files\\QGIS 2.14\\bin"), installation_type = "osgeo4wOTB",stringsAsFactors = FALSE)
    Sys.setenv(GEOTIFF_CSV = paste0(Sys.getenv("OSGEO4W_ROOT"),"\\share\\epsg_csv"),envir = .GlobalEnv)
    
  } else {
    
    # trys to find a osgeo4w installation on the whole C: disk returns root directory and version name
    # recursive dir for otb*.bat returns all version of otb bat files
    cat("\nsearching for OTB installations - this may take a while\n")
    cat("Alternatively you can provide a path like: C:\\OSGeo4W64\\bin\\\n")
    cat("You can also provide a installation type like: 'osgeo4w64OTB'\n")
    raw_otb <- system(paste0("cmd.exe /c dir /B /S ",dl,"\\","otbcli.bat"),intern = TRUE)
    
    # trys to identify valid otb installations and their version numbers
    otb_installations <- lapply(seq(length(raw_otb)), function(i){
      # convert codetable according to cmd.exe using type
      batchfileLines <- raw_otb[i]
      installer_type <- ""
      # if the the tag "OSGEO4W" exists set installation_type
      if (length(unique(grep(paste("OSGeo4W64", collapse = "|"), raw_otb[i], value = TRUE))) > 0) {
        path_otb_root  <- unique(grep(paste("OSGeo4W64", collapse = "|"), raw_otb[i], value = TRUE))
        path_otb_root  <- substr(path_otb_root,1, gregexpr(pattern = "otbcli.bat", path_otb_root)[[1]][1] - 1)
        path_install   <- substr(path_otb_root,1, gregexpr(pattern = "bin", path_otb_root)[[1]][1] - 2)
        installer_type <- "osgeo4w64OTB"
      }    
      
      # if the the tag "OSGEO4W" exists set installation_type
      else if (length(unique(grep(paste("OSGeo4W", collapse = "|"), raw_otb[i], value = TRUE))) > 0) {
        path_otb_root  <- unique(grep(paste("OSGeo4W", collapse = "|"), raw_otb[i], value = TRUE))
        path_otb_root  <- substr(path_otb_root,1, gregexpr(pattern = "otbcli.bat", path_otb_root)[[1]][1] - 1)
        path_install   <- substr(path_otb_root,1, gregexpr(pattern = "bin", path_otb_root)[[1]][1] - 2)
        installer_type <- "osgeo4wOTB"
      }
      # if the the tag "QGIS" exists set installation_type
      else if (length(unique(grep(paste("QGIS", collapse = "|"), batchfileLines, value = TRUE))) > 0) {
        path_otb_root  <- unique(grep(paste("QGIS", collapse = "|"), raw_otb[i], value = TRUE))
        path_otb_root  <- substr(path_otb_root,1, gregexpr(pattern = "otbcli.bat", path_otb_root)[[1]][1] - 1)
        path_install   <- substr(path_otb_root,1, gregexpr(pattern = "bin", path_otb_root)[[1]][1] - 2)
        installer_type <- "qgisOTB"
      }
      
      # put the existing GISBASE directory, version number  and installation type in a data frame
      data.frame(path_bin = path_otb_root, path_base = path_install, installation_type = installer_type, stringsAsFactors = FALSE)
      
    }) # end lapply
    
    # bind the df lines
    otb_installations <- do.call("rbind", otb_installations)
  }
  return(otb_installations)
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
  if (exists(name, envir = .GlobalEnv)) {
    warning(paste0("The variable '", name,"' already exist in .GlobalEnv"))
    assign(name, value, envir = .GlobalEnv, inherits = TRUE)
  } else {
    assign(name, value, envir = .GlobalEnv, inherits = TRUE)
  } 
}


#'Add a variable to the system path
#'@description Add a variable to the system path
#'@param path_new Path to be added
#'@return NONE
#'@author Chris Reudenbach
#'@name add2Path
#'@export add2Path
#' @examples 
#'\dontrun{
#'}
#'
add2Path <- function(path_new) {
  exist <- FALSE
  if (Sys.info()["sysname"] == "Windows") {
    del <- ";"  
  } else {
    del <- ":"  
  } 
  p <- Sys.getenv("PATH")
  if (substr(p, 1,nchar(path_new)) == path_new) {
    exist <- TRUE
  }
  # if not exist append path to systempath
  if (!exist) {
    Sys.setenv(PATH = paste0(path_new,del,Sys.getenv("PATH")))
  }
}