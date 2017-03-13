#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector CAB_cpp_diff(NumericVector x){
    return diff(x);
}
