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
List CAB_cpp_geometric_fitness_selection( NumericVector fitness, int pop_size, double p ){

    IntegerVector father_index = rep( NA_INTEGER , pop_size );
    IntegerVector mother_index = rep( NA_INTEGER, pop_size );
    bool got_father = false;
    bool got_mother = false;

    for ( int parent = 0; parent < pop_size; parent ++ ){
        IntegerVector matching(1);
        while ( ! got_father ){
            NumericVector father_fitness = rgeom( 1, p );
            matching = match( father_fitness, fitness )[0];
            if ( !is_na(matching)[0] ){
                father_index[parent] = matching[0];
                got_father = !got_father;
            }

        }
        while ( ! got_mother ){
            NumericVector mother_fitness = rgeom( 1, p );
            matching = match( mother_fitness, fitness );
            if ( ( !is_na(matching)[0] ) & ( matching[0] != father_index[parent] ) ){
                mother_index[parent] = matching[0];
                got_mother = !got_mother;
            }
        }
        got_father = !got_father;
        got_mother = !got_mother;
    }
    List return_list = List::create( Named("father_index") = father_index, Named("mother_index") = mother_index );

    return( return_list );
}
