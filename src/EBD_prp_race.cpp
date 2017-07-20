// [[Rcpp::depends(RcppArmadillo)]]

#include <RcppArmadillo.h>
using namespace Rcpp;

//'@export EBD_prp_race
//'@rdname EBD_prp_race
// [[Rcpp::export]]
List EBD_prp_race( arma::uvec phenotypes, arma::uvec oc_lower, int prp_size ){

    int n_phenotypes = phenotypes.size();
    int n_oc = oc_lower.size();

    arma::fvec class_probs( n_oc, arma::fill::zeros );
    arma::uvec operant_classes( n_phenotypes );
    int oc = 0;

    for ( int i = 0; i < n_phenotypes; i ++ ){
        oc = sum( phenotypes[i] >= oc_lower ) - 1;
        class_probs( oc ) += 1;
        operant_classes(i) = oc;
    }

    class_probs = class_probs / phenotypes.size();

    arma::fvec prp( n_oc );
    prp.fill( arma::datum::inf );
    for ( int i = 0; i < n_oc; i ++ ){
        if ( class_probs(i) > 0 ){
            prp(i) = R::rnbinom( prp_size, class_probs(i) );
        }
    }

    int fastest_class = -1;
    int fastest_prp = min( prp );

    arma::uvec winning_classes = find( abs( prp- fastest_prp ) < 0.0000000001 );
    fastest_class = winning_classes(0);

    if ( winning_classes.size() > 0 ){
        for ( int i = 0; i < winning_classes.size(); i ++ ){
            operant_classes( find( operant_classes ) == winning_classes(i) ).fill( fastest_class );
        }
    }

   int response = -1;
   arma::uvec responses_in_oc = phenotypes( find( operant_classes == fastest_class ) );
   int response_index = sample( responses_in_oc.size(), 1, true)(0);
   // Subtract 1 because sample samples from [1:n]
   response = responses_in_oc( response_index-1 );

    List return_list = List::create( Named("response") = response, Named("time") = fastest_prp );
    return( return_list );
}
