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
LogicalVector CAB_cpp_clean_short_ixyi( DataFrame data, String x_name, String y_name, double gap ){

    NumericVector times = data["time"];
    CharacterVector events = data["event"];

    LogicalVector keep = rep( true, times.length() );

    double last_x_time(0);
    bool got_x = false;

    for ( int i = 0; i < times.length(); i ++ ){

        if ( events[i] == x_name ){
            last_x_time = times[i];
            got_x = true;
        }
        else if ( events[i] == y_name && got_x ){
            if ( times[i] - last_x_time < gap ){
                keep[i] = false;
            }
            else got_x = false;
        }
    }

    return( keep );
}
