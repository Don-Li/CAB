//[[Rcpp:depends(RccpArmadillo)]]

#include "RcppArmadillo.h"

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
double CAB_ks( arma::colvec sample_1, arma::colvec sample_2 ){

    double n1 = sample_1.n_rows;
    double n2 = sample_2.n_rows;

    arma::colvec all_samples = arma::join_cols( sample_1, sample_2 );
    arma::uvec sorted = arma::sort_index( all_samples );

    arma::colvec sorted_diff = arma::diff( all_samples(sorted) );

    all_samples.fill( -1/n2 );
    all_samples( arma::find( sorted <= n1-1 ) ).ones();
    all_samples.replace( 1, 1/n1 );

    arma::colvec cum_sum = arma::cumsum( all_samples );

    if ( any( sorted_diff == 0 ) ){
        cum_sum( arma::find( sorted_diff == 0 ) ).zeros();
        cum_sum( n1 + n2 - 1 ) = 0;
    }

    return( max(abs( cum_sum ) ) );
}
