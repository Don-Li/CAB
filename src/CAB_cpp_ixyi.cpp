#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector compute_ixyi_FER( List data, CharacterVector x_event, CharacterVector y_event, double x_offset ){

    NumericVector times = data["time"];
    CharacterVector events = data["event"];

    double x_time = 0;
    double y_time = 0;
    int i = 0;

    std::vector<double> ixyi(0);

    // Get the first x_event time
    for ( ; i < times.length(); i ++ ){
        if ( events(i) == x_event(0) ){
            x_time = times(i);
            break;
        }
    }

    bool got_x = true;

    for ( ; i < times.length(); i ++ ){
        if ( got_x ){
            if ( events(i) == y_event(0) ){
                y_time = times(i);
                ixyi.push_back( y_time - x_time );
                got_x = false;
            }
        } else{
            if ( events(i) == x_event(0) ){
                x_time = times(i);
                got_x = true;
            }
        }
    }

    if ( x_offset != 0 ){
       for ( int j = 0; j < ixyi.size(); j ++ ){
            ixyi[j] += x_offset;
        }
    }

    return( wrap( ixyi ) );
}

// [[Rcpp::export]]
NumericVector compute_ixyi_FER_breaks( List data, CharacterVector x_event, CharacterVector y_event, CharacterVector break_events, double x_offset ){

    NumericVector times = data["time"];
    CharacterVector events = data["event"];
    IntegerVector breaking = match( events, break_events );

    double x_time = 0;
    double y_time = 0;
    int i = 0;

    std::vector<double> ixyi(0);

    // Get the first x_event time
    for ( ; i < times.length(); i ++ ){
        if ( events(i) == x_event(0) ){
            x_time = times(i);
            break;
        }
    }

    bool got_x = true;

    for ( ; i < times.length(); i ++ ){
        if ( got_x ){
            if ( events(i) == y_event(0) ){
                y_time = times(i);
                ixyi.push_back( y_time - x_time );
                got_x = false;
            }
            else if ( breaking(i) != NA_INTEGER ){
                got_x = false;
            }
        }
        else{
            if ( events(i) == x_event(0) ){
                x_time = times(i);
                got_x = true;
            }
        }
    }

    if ( x_offset != 0 ){
       for ( int j = 0; j < ixyi.size(); j ++ ){
            ixyi[j] += x_offset;
        }
    }

    return( wrap( ixyi ) );
}

// [[Rcpp::export]]
NumericVector compute_ixxi_FER( List data, CharacterVector x_event, double x_offset ){

    NumericVector times = data["time"];
    CharacterVector events = data["event"];

    double x_time1 = 0;
    double x_time2 = 0;
    int i = 0;

    std::vector<double> ixxi(0);

    // Get the first x_event time
    for ( ; i < times.length(); i ++ ){
        if ( events(i) == x_event(0) ){
            x_time1 = times(i);
            break;
        }
    }

    i += 1;
    bool got_x1 = true;
    for ( ; i < times.length(); i ++ ){
        if ( got_x1 ){
            if ( events(i) == x_event(0) ){
                x_time2 = times(i);
                ixxi.push_back( x_time2 - x_time1 );
                x_time1 = x_time2;
            }
        }
    }

    if ( x_offset != 0 ){
       for ( int j = 0; j < ixxi.size(); j ++ ){
            ixxi[j] += x_offset;
        }
    }

    return( wrap( ixxi ) );
}

// [[Rcpp::export]]
NumericVector compute_ixxi_FER_breaks( List data, CharacterVector x_event, CharacterVector break_events, double x_offset ){

    NumericVector times = data["time"];
    CharacterVector events = data["event"];
    IntegerVector breaking = match( events, break_events );

    double x_time1 = 0;
    double x_time2 = 0;
    int i = 0;

    std::vector<double> ixxi(0);

    // Get the first x_event time
    for ( ; i < times.length(); i ++ ){
        if ( events(i) == x_event(0) ){
            x_time1 = times(i);
            break;
        }
    }

    i += 1;
    bool got_x1 = true;
    for ( ; i < times.length(); i ++ ){
        if ( got_x1 ){
            if ( events(i) == x_event(0) ){
                x_time2 = times(i);
                ixxi.push_back( x_time2 - x_time1 );
                x_time1 = x_time2;
           } else if( breaking(i) != NA_INTEGER ){
                got_x1 = false;
            }
        }
        else if ( events(i) == x_event(0) ){
            x_time1 = times(i);
            got_x1 = true;
        }
    }

    if ( x_offset != 0 ){
       for ( int j = 0; j < ixxi.size(); j ++ ){
            ixxi[j] += x_offset;
        }
    }

    return( wrap( ixxi ) );
}


