#' Smorgasboard for remote sensing functions
#'
#' The package provides a variety of functions which are useful for 
#' computing higher level remote sensing products.
#'
#' @name satelliteTools-package
#' @aliases satelliteToolspackage
#' @docType package
#' @title Smorgasboard for remote sensing functions.
#' @author Thomas Nauss, Hanna Meyer, Florian Detsch, Tim Appelhans \cr
#' \cr
#' \emph{Maintainer:} Environmental Informatics \email{admin@@environmentalinformatics-marburg.de}
#'
#' @import glcm methods raster Rcpp RcppArmadillo RStoolbox satellite rgeos sp rgdal
#' @importFrom stats mahalanobis median prcomp princomp sd var
#' @importFrom stats4 plot
#' @importFrom utils glob2rx
#' 
#' @useDynLib satelliteTools
#' 
#' @references Some functions are taken and/or adopted from Sarah C. Goslee 
#' (2011). Analyzing Remote Sensing Data in R: The landsat Package. Journal of 
#' Statistical Software, 43(4), 1-25. Online available at 
#' \url{http://www.jstatsoft.org/v43/i04/}.
#' 
#' @keywords package
#'
NULL
