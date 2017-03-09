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
NumericMatrix CAB_cpp_srswo( int size, int group_size ,int n ){
    NumericMatrix samples( group_size, size );
    double i = 0;

    while ( i < size ){
        NumericVector x = floor( runif( group_size, 1, n+1 ) );
        bool z = is_false( any( duplicated( x ) ) );
        if ( z ){
            samples(_,i) = x;
            i++;
        }
    }

    return samples;
}

// [[Rcpp::export]]
NumericMatrix CAB_cpp_srswo2( int size, int n ){

    NumericMatrix samples( 2, size );
    int sampled = 0;

    while ( sampled < size ){
        NumericVector x1 = floor( runif( size*2, 1, n+1 ) );
        NumericVector x2 = floor( runif( size*2, 1, n+1 ) );
        LogicalVector z = x1 != x2;
        NumericVector x1a = x1[z], x2a = x2[z];
        if ( x1a.length() > size ){
            sampled += size;
            samples( 0, _ ) = x1a;
            samples( 1, _ ) = x2a;
        }
    }

    return samples;
}
