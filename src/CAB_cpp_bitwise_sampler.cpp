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
NumericVector CAB_cpp_bitwise( int pop_size, NumericMatrix fathers, NumericMatrix mothers, double bias ){
    int get_n = pop_size * fathers.nrow();
    NumericVector selection = rbinom( get_n, 1, bias );

    NumericVector z = fathers*selection + mothers*(1-selection);
    z.attr( "dim" ) = Dimension( fathers.nrow(), pop_size );
    return( z );
}
