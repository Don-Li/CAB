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
List CAB_cpp_local_times_formal_event_record( DataFrame data, String event, String marker, double event_offset, int n_markers ){

    NumericVector times = data["time"];
    CharacterVector events = data["event"];

    std::vector<double> local_times(0);
    std::vector<double> visit_lengths(0);
    int rows = times.length();
    double current_marker = -1;
    int i = 0;
    int j = 0;
    int n_visits = 0;

    for ( i; i < rows; i ++ ){
        if ( events[i] == marker ){
            current_marker = times[i];
            j = i+1;
            i = rows;
        }
    }

    for ( j; j < rows; j ++ ){
        if ( events[j] == event ){
            if ( event_offset == 0 ){
                local_times.push_back( times[j] - current_marker );
            }
            else{
                local_times.push_back( times[j] - current_marker + n_visits * event_offset );
                n_visits ++;
            }
        }
        if ( events[j] == marker ){
            n_visits = 0;
            current_marker = times[j];
            visit_lengths.push_back( local_times.back() );
            if ( visit_lengths.size() >= n_markers - 1 ){
                j = rows;
            }
        }
    }

    List return_list = List::create(Named("local_times") = local_times, Named("visit_lengths") = visit_lengths );
    return( return_list );
}

// [[Rcpp::export]]
List CAB_cpp_local_binning( NumericVector local_times, NumericVector visit_lengths, int max_bin, double bin_resolution ){
    NumericVector copy_visit_lengths = visit_lengths / bin_resolution;

    NumericVector copy_local_times =  local_times / bin_resolution;

    IntegerVector visit_bins( max_bin + 1 );
    IntegerVector response_bins( max_bin + 1 );

    IntegerVector reference_time_bins = wrap( seq( 0, max_bin ) );
    LogicalVector temp_indicator = reference_time_bins == -1;

    for ( int i = 0; i < copy_visit_lengths.length(); i ++ ){
        temp_indicator = reference_time_bins <= copy_visit_lengths[i];
        visit_bins += as<IntegerVector>(temp_indicator);
    }

    IntegerVector int_local_times = as<IntegerVector>(copy_local_times);

    for ( int i = 0; i < copy_local_times.length(); i ++ ){
        if ( int_local_times[i] <= max_bin ) response_bins[int_local_times[i]] ++;
    }

    List return_list = List::create(Named("visit_bins") = visit_bins, Named("response_bins") = response_bins );
    return( return_list );
}

// [[Rcpp::export]]
List CAB_cpp_local_times_ragged_event_record( NumericVector event_times, NumericVector marker_times, double event_offset ){

    std::vector<double> local_times(0);
    int n_markers = marker_times.length();
    int event = 0;
    int n_visits = 0;
    std::vector<double> visit_lengths(0);

    for ( event; event_times[ event ] <= marker_times[ 0 ]; event ++ ){
    }

    for ( int marker = 0; marker < n_markers-1; marker ++ ){
        n_visits = 0;
        for ( event; event_times[ event ] > marker_times[marker] && event_times[ event ] <= marker_times[ marker + 1 ]; event ++ ){

            if ( event_offset == 0 ){
                local_times.push_back( event_times[ event ] - marker_times[ marker ] );
            }
            else{
                local_times.push_back( event_times[ event ] - marker_times[ marker ] + n_visits * event_offset );
            }
            n_visits ++;
        }
        visit_lengths.push_back( local_times.back() );
    }


    List return_list = List::create(Named("local_times") = local_times, Named("visit_lengths") = visit_lengths );
    return( return_list );
}


