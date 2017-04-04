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
IntegerVector CAB_cpp_int2bin( int bits, IntegerVector n ){
    IntegerMatrix stack(bits, n.length() );
    for ( int integer_n = 0; integer_n < n.length(); integer_n++ ){
        int temp_n = n[ integer_n ];
        int i = 0;
        while ( ( temp_n != 0 )){
            int remainder = temp_n % 2;
            stack(i++, integer_n) = remainder;
            temp_n /= 2;
        }
    }
    return( stack );
}
