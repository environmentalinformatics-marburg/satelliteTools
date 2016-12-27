
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List rcpp_hello_world() {

    CharacterVector x = CharacterVector::create( "foo", "bar" )  ;
    NumericVector y   = NumericVector::create( 0.0, 1.0 ) ;
    List z            = List::create( x, y ) ;

    return z ;
}

// [[Rcpp::export]]
List mSpecIndicesCPP(NumericVector blue, NumericVector green, NumericVector red, NumericVector nir){
  
  NumericVector ndvi = (nir - red) / (nir + red);
  NumericVector gndvi = (nir - green) / (nir + green);
  NumericVector sr = nir/red;

  return List::create(ndvi, gndvi, sr);
}