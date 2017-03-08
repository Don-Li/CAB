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
NumericVector CAB_cpp_bitwise( int pop_size, NumericMatrix parents, double bias ){
    int get_n = pop_size*parents.nrow();
    NumericVector selection = rbinom( get_n, 1, bias );
    NumericMatrix father = parents( _, Range(0,pop_size-1) );
    NumericMatrix mother = parents( _, Range(pop_size, pop_size*2-1) );
    NumericVector z = father*selection + mother*(1-selection);
    z.attr("dim") = Dimension( parents.nrow(), pop_size );
    return wrap( z );
}