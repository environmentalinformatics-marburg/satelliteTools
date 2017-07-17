clc <- function() {
  rm(list = ls(envir = globalenv()),envir = globalenv()) #clear Vars from global enviroment
  gc()  #garbage collector
  cat("\014") #clc
  .rs.restartR() #clear session
}