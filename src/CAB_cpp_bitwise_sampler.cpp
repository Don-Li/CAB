// [[Rcpp::depends(RcppArmadillo)]]

#include <RcppArmadillo.h>
using namespace Rcpp ;

//'@export EBD_BR
//'@rdname EBD.reproduction
// [[Rcpp::export]]
arma::umat EBD_BR( arma::umat fathers, arma::umat mothers ){

    arma::umat p = arma::randi<arma::umat>( fathers.n_rows, fathers.n_cols, arma::distr_param(0,1) );
    arma::umat return_matrix = p % fathers + (1-p) % mothers;

    return( return_matrix );
}

//'@export EBD_RS_BR
//'@rdname EBD.reproduction
// [[Rcpp::export]]
arma::umat EBD_RS_BR( arma::umat parents ){

    IntegerVector choose = wrap( seq_len( parents.n_cols ) -1 );

    arma::umat fathers( parents.n_rows, parents.n_cols );
    arma::umat mothers( parents.n_rows, parents.n_cols );

    IntegerVector selector(2);

    for ( int i = 0; i < parents.n_cols; i ++ ){
        selector = sample( choose, 2, false );
        fathers.col(i) = parents.col( selector(0) );
        mothers.col(i) = parents.col( selector(1) );
    }

    arma::umat p = arma::randi<arma::umat>( parents.n_rows, parents.n_cols, arma::distr_param(0,1) );
    arma::umat return_matrix = p % fathers + (1-p) % mothers;

    return( return_matrix );
}
