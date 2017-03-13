#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp:export]]
NumericVector CAB_cpp_compute__I_xy_I__formal_event_record( DataFrame data, CharacterVector x_event, CharacterVector y_event, CharacterVector break_event ){

    NumericVector times = data["time"];
    CharacterVector events = data["event"];
    std::vector<double> differences(0);
    double x_val = R_PosInf;
    IntegerVector breaking = match( events, break_event );

    for( int i = 0; i < times.length(); i ++ ){
        if ( events[i] == x_event[0] ){
            x_val = times[i];
        }
        else if ( breaking[i] != NA_INTEGER ){
            x_val = R_PosInf;
        }
        else if ( ( x_val <= times[i] ) & ( events[i] == y_event[0] ) ){
            differences.push_back( times[i] - x_val );
            x_val = R_PosInf;
        }
    }
    return wrap( differences );
}
