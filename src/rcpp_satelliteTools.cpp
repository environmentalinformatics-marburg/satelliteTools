#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List mSpecIndicesCPP(NumericVector blue, NumericVector green, NumericVector red, NumericVector nir){
  
  NumericVector ndvi = (nir - red) / (nir + red);
  NumericVector gndvi = (nir - green) / (nir + green);
  NumericVector sr = nir/red;

  return List::create(ndvi, gndvi, sr);
}