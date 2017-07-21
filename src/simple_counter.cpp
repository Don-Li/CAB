// [[Rcpp::depends(RcppArmadillo)]]
#include "RcppArmadillo.h"
using namespace Rcpp;

//'@export simple_factorial_counts
//'@rdname tabulation
// [[Rcpp::export]]
arma::umat simple_factorial_counts(
        CharacterVector event,
        NumericVector time,
        CharacterVector rft_labels,
        CharacterVector component_label,
        CharacterVector response_labels,
        int max_rfts ){

    // Indicators for the event types
    IntegerVector rft_match = match( event, rft_labels );
    IntegerVector component_start_match = match( event, component_label );
    IntegerVector resp_match = match( event, response_labels );

    // Counters
    int n_resp_types = response_labels.length();
    int i_max = event.length();
    int component_rft = 0;

    // Contains the counts. Rows for the IRI, cols for response types
    arma::umat return_matrix = arma::umat( max_rfts, n_resp_types, arma::fill::zeros );

    for ( int i = 0; i < i_max; i ++ ){

        // Check for new component
        if ( component_start_match(i) != NA_INTEGER ){
            component_rft = 0;
        }

        // Check for rft
        if ( rft_match(i) != NA_INTEGER ){
            component_rft += 1;
            if ( component_rft > max_rfts ){
                stop( "More rft than maximum" );
            }
        }

        // Check if it is a response
        if ( resp_match(i) != NA_INTEGER ){
            // match() indexes from 1, subtract 1
            return_matrix( component_rft, resp_match(i)-1 ) += 1;
        }
    }

    return( return_matrix );
}

//'@export simple_ixyi
//'@rdname ixyi
// [[Rcpp::export]]
List simple_ixyi(
        CharacterVector event,
        NumericVector time,
        CharacterVector x_events,
        CharacterVector y_events,
        CharacterVector break_events ){

    // Indicators for the event types
    IntegerVector break_indicators = match( event, break_events );
    // We are using match_x and match_y to index C++ arrays, so subtract 1
    IntegerVector match_x = match( event, x_events ) - 1;
    IntegerVector match_y = match( event, y_events ) - 1;

    // Counters
    int i_max = event.length();
    int n_x_types = x_events.length();

    double x_time = 0;
    double y_time = 0;
    int x_code = 0;
    int y_code = 0;
    int ixyi_code = 0;
    bool got_x = false;

    // Return objects
    std::vector<double> ixyi_container(0);
    std::vector<int> ixyi_indicator(0);
    std::vector<int> ixyi_counter( n_x_types * y_events.length() );

    for ( int i = 0; i < i_max; i ++ ){

        // If we do not have an x event stored, check for an x event
        if ( !got_x ){
            if ( match_x(i) != NA_INTEGER ){
                x_code = match_x(i);
                x_time = time(i);
                got_x = true;
            }

        } // Else, check for a y event, check if it is also an x event, check for breaking events
        else{
            // Check for y event
            if ( match_y(i) != NA_INTEGER ){
                y_code = match_y(i);
                y_time = time(i);
                ixyi_container.push_back( y_time - x_time );
                ixyi_code = n_x_types * x_code + y_code;
                ixyi_indicator.push_back( ixyi_code );
                ixyi_counter[ ixyi_code ] ++;
                got_x = false;
            }
            // Check if event was also an x event
            if ( match_x(i) != NA_INTEGER ){
                x_code = match_x(i);
                x_time = time(i);
                got_x = true;
            }
            // Check for breaking events
            if ( break_indicators(i) != NA_INTEGER ){
                got_x = false;
            }
        }
    }

    List return_list = List::create( Named("ixyi") = ixyi_container, Named("ixyi_type") = ixyi_indicator, Named("ixyi_counts") = ixyi_counter );
    return( return_list );
}
