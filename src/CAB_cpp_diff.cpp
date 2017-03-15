#include <Rcpp.h>
using namespace Rcpp;

//Stuff again

// [[Rcpp::export]]
NumericVector CAB_cpp_diff(NumericVector x){
    return diff(x);
}
