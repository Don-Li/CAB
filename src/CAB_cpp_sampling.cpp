// [[Rcpp::depends(RcppArmadillo)]]

#include <RcppArmadillo.h>
using namespace Rcpp;

//'@export mcdowell_sampling
//'@rdname CAB_samplers
// [[Rcpp::export]]
NumericMatrix mcdowell_sampling( NumericVector fitness, NumericVector fitness_weights ){
    int size = fitness.length();
    NumericVector indices = wrap( seq( 0, size-1 ) );

    LogicalVector unique_fitness_indicator = !duplicated( fitness );

    NumericVector fitness_weights2 = clone( fitness_weights );
    NumericVector fitness2 = clone( fitness );

    fitness_weights2[ !unique_fitness_indicator ] = 0;
    NumericVector selected_father_indices = sample( indices, size, true, fitness_weights2 );
    LogicalVector matching_vector(size);
    NumericVector selected_mother_indices(size);
    int duplicated_value = 0;
    NumericVector selected_mother(1);
    double father_index = 0;

    for ( int i = 0; i < size; i ++ ){
        father_index = selected_father_indices(i);

        fitness2( father_index ) = R_PosInf;
        fitness_weights2( father_index ) = 0;

        matching_vector = fitness( father_index ) == fitness2;

        if ( is_true( any( matching_vector ) ) ){
            fitness_weights2[ matching_vector ] = fitness_weights[ matching_vector ];
            duplicated_value = 1;
        }
        selected_mother_indices(i) = sample( indices, 1, true, fitness_weights2 )(0);

        fitness2( father_index ) = fitness( father_index );
        fitness_weights2( father_index ) = fitness_weights[ father_index ];
        fitness_weights2[ matching_vector ] = 0;
        duplicated_value = 0;

    }
    NumericMatrix return_matrix( size, 2 );
    return_matrix.column(0) = selected_father_indices;
    return_matrix.column(1) = selected_mother_indices;

    return( return_matrix );
}

//'@export srswo
//'@rdname CAB_samplers
// [[Rcpp::export]]
NumericMatrix srswo( NumericVector choose, int repeats, int sample_size ){

    NumericMatrix return_matrix( repeats, sample_size );

    for ( int i = 0; i < repeats; i++ ){
        return_matrix.row(i) = sample( choose, sample_size, false );
    }

    return( return_matrix );
}

//'@export srs
//'@rdname CAB_samplers
// [[Rcpp::export]]
NumericMatrix srs( int choose_max, int repeats, int sample_size ){

    NumericMatrix return_matrix( repeats, sample_size );

    for ( int i = 0; i < repeats; i++ ){
        return_matrix.row(i) = sample( choose_max, sample_size, true );
    }

    return( return_matrix );
}
