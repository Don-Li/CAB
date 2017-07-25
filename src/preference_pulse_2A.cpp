//[[Rcpp:depends(RccpArmadillo)]]

#include <RcppArmadillo.h>
using namespace Rcpp ;

//'@export preference_pulse_2A
// [[Rcpp::export]]
List preference_pulse_2A( CharacterVector event, NumericVector time, CharacterVector rft_labels, CharacterVector component_label, CharacterVector response_labels, double max_iri, double bin_resolution, double offset = 0){

    IntegerVector rft_match = match( event, rft_labels );
    IntegerVector component_start_match = match( event, component_label );
    IntegerVector resp_match = match( event, response_labels ) - 1;

    int max_bin = ceil( max_iri );
    int n_bins = floor( max_bin / bin_resolution );
    arma::uvec time_bins = as<arma::uvec>( wrap( floor( time / bin_resolution ) ) );

    int response_len = response_labels.length();

    arma::umat response_matrix1( n_bins, response_len, arma::fill::zeros );
    arma::umat response_matrix2( n_bins, response_len, arma::fill::zeros );

    int rft_time = 0;
    int response_time = 0;

    int last_food_loc = 0;
    int component_rft = 0;

    for ( int i = 0; i < event.length(); i ++ ){
        if ( component_start_match(i) != NA_INTEGER ){
            component_rft = 0;
        }
        if ( rft_match(i) != NA_INTEGER ){
            component_rft += 1;
            rft_time = time_bins(i);
            last_food_loc = rft_match(i);
        }
        if ( component_rft > 0 ){
            if ( resp_match(i) != NA_INTEGER ){
                response_time = time_bins(i) - rft_time - offset;
                if ( response_time < n_bins ){
                    if ( last_food_loc == 1 ){
                        response_matrix1( response_time, resp_match(i) ) ++;
                    }
                    else{
                        response_matrix2( response_time, resp_match(i) ) ++;
                    }
                }
            }
        }
    }
    return( List::create( response_matrix1, response_matrix2 ) );
}
