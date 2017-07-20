// [[Rcpp::depends(RcppArmadillo)]]

#include <RcppArmadillo.h>
using namespace Rcpp ;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

//'@rdname EBD_utilities
//'@export EBD_B_premutate
// [[Rcpp::export]]
List EBD_B_premutate( double mutation_rate, int pop_size, int total_ticks, int n_bits ){

    List mutant_indices = List( total_ticks+1 );

    NumericVector binom_vect = rbinom( total_ticks+1, pop_size, mutation_rate );
    for ( int i = 0; i < total_ticks+1; i ++ ){
        IntegerVector x = sample( pop_size, binom_vect[i], false );
        IntegerVector y = sample( n_bits, binom_vect[i], true );
        mutant_indices[i] = (x-1)*n_bits + y;
    }

    return( mutant_indices );
}
