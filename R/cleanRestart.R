clc <- function() {
  rm(list = ls(envir = globalenv()),envir = globalenv()) #clear Vars from global enviroment
  gc()  #grabage colector
  cat("\014") #clc
  .rs.restartR() #clear session
}