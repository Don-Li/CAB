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
NumericMatrix CAB_cpp_indicator_matrix( NumericVector x ){

    NumericVector unique_x = sort_unique(x);
    int unique_length = unique_x.length();
    int x_length = x.length();

    NumericMatrix return_matrix(x_length, unique_length);

    for ( int col = 0; col < unique_length; col ++ ){
        for ( int row = 0; row < x_length; row ++ ){
            if ( x[row] == unique_x[col] ){
                return_matrix(row, col) = 1;
            }
        }
    }

    return( return_matrix );
}
