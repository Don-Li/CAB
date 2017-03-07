#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
NumericVector CAB_cpp_bitwise_sampler( int pop_size, NumericMatrix parents ){
    int get_n = pop_size*parents.nrow();
    NumericVector selection = rbinom( get_n, 1, 0.5 );
    IntegerVector x = seq( 1, get_n );
    NumericVector index = (get_n)*(1-selection)+as<NumericVector>(x);
    return wrap(index);
}
