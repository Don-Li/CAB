// #include <Rcpp.h>
// using namespace Rcpp;
//
// // This is a simple example of exporting a C++ function to R. You can
// // source this function into an R session using the Rcpp::sourceCpp
// // function (or via the Source button on the editor toolbar). Learn
// // more about Rcpp at:
// //
// //   http://www.rcpp.org/
// //   http://adv-r.had.co.nz/Rcpp.html
// //   http://gallery.rcpp.org/
// //
//
// // [[Rcpp::export]]
// NumericVector CAB_cpp_local_count_helper_formal_event_record( DataFrame data, String event, String marker, double event_offset, double marker_offset, int n_markers ){
//
//     NumericVector times = data["time"];
//     CharacterVector events = data["event"];
//
//     std::vector<double> local_times(0);
//     int rows = times.length();
//
//     double last_marker_time = -1;
//     double n_mark = 0;
//     int n_visits = 0;
//
//     for ( int i = 1; i < rows+1; i ++ ){
//
//         if ( events[i] == marker ){
//             last_marker_time = times[i];
//             n_mark++;
//
//             if ( marker_offset != 0 ){
//                 n_visits = 0;
//             }
//         }
//         else if ( events[i] == event && n_mark > 0 && n_mark < n_markers ){
//             if ( event_offset == 0 ){
//                 if ( marker_offset != 0 ){
//                     local_times.push_back( times[i] - last_marker_time + marker_offset );
//                 }
//                 else {
//                     local_times.push_back( times[i] - last_marker_time );
//                 }
//             }
//             else{
//                 n_visits++;
//                 if ( marker_offset != 0 ){
//                     local_times.push_back( times[i] - last_marker_time + marker_offset + n_visits * event_offset );
//                 }
//                 else{
//                     local_times.push_back( times[i] - last_marker_time + n_visits * event_offset );
//                 }
//             }
//         }
//
//     }
//     return( wrap( local_times ) );
// }
//
// // [[Rcpp::export]]
// NumericVector CAB_cpp_local_count_helper_ragged_event_record( NumericVector event_times, NumericVector marker_times, double event_offset, double marker_offset ){
//
//     std::vector<double> local_times(0);
//     int n_marker = marker_times.length();
//     int n_visits = 0;
//     int event = 0;
//
//     for ( int marker = 0; marker < n_marker - 1; marker ++ ){
//         n_visits = 0;
//
//         while ( event_times[ event ] <= marker_times[marker] ){
//             event++;
//         }
//
//         while ( event_times[event] > marker_times[marker] && event_times[event] <= marker_times[marker+1] ) {
//
//             if ( event_offset == 0 ){
//
//                 if ( marker_offset != 0 ){
//                     local_times.push_back( event_times[event] - marker_times[marker] + marker_offset );
//                 }
//                 else{
//                     local_times.push_back( event_times[event] - marker_times[marker] );
//                 }
//             }
//
//             else{
//                 n_visits++;
//                 if ( marker_offset != 0 ){
//                     local_times.push_back( event_times[event] - marker_times[marker] + marker_offset + n_visits * event_offset );
//                 }
//                 else {
//                     local_times.push_back( event_times[event] - marker_times[marker] + n_visits * event_offset );
//                 }
//             }
//             event++;
//         }
//
//     }
//
//     return( wrap( local_times ) );
// }
//
// // [[Rcpp::export]]
// List CAB_cpp_local_rate_helper_ragged_event_record( NumericVector event_times, NumericVector marker_times, double event_offset, double marker_offset ){
//
//     std::vector<double> local_times(0);
//     int n_marker = marker_times.length();
//     int n_visits = 0;
//     int event = 0;
//     std::vector<int> interior_markers(0);
//     interior_markers.push_back(0);
//     int pre_events = 0;
//
//     for ( int marker = 0; marker < n_marker - 1; marker ++ ){
//         n_visits = 0;
//
//         while ( event_times[ event ] <= marker_times[marker] ){
//             event++;
//             pre_events++;
//         }
//
//         while ( event_times[event] > marker_times[marker] && event_times[event] <= marker_times[marker+1] ) {
//             if ( event_offset == 0 ){
//
//                 if ( marker_offset != 0 ){
//                     local_times.push_back( event_times[event] - marker_times[marker] + marker_offset );
//                 }
//                 else{
//                     local_times.push_back( event_times[event] - marker_times[marker] );
//                 }
//             }
//
//             else{
//                 n_visits++;
//                 if ( marker_offset != 0 ){
//                     local_times.push_back( event_times[event] - marker_times[marker] + marker_offset + n_visits * event_offset );
//                 }
//                 else {
//                     local_times.push_back( event_times[event] - marker_times[marker] + n_visits * event_offset );
//                 }
//             }
//             event++;
//         }
//         interior_markers.push_back(event - pre_events );
//     }
//
//     List return_list = List::create(Named("local_times") = local_times, Named("interior_markers") = interior_markers );
//
//     return( return_list );
// }
//
// // [[Rcpp::export]]
// List CAB_cpp_local_rate_helper_formal_event_record( DataFrame data, String event, String marker, double event_offset, double marker_offset, int n_markers ){
//
//     NumericVector times = data["time"];
//     CharacterVector events = data["event"];
//
//     std::vector<double> local_times(0);
//     int rows = times.length();
//     std::vector<int> interior_markers;
//
//     double last_marker_time = -1;
//     double n_mark = 0;
//     int n_visits = 0;
//
//     for ( int i = 1; i < rows+1; i ++ ){
//
//         if ( events[i] == marker ){
//             last_marker_time = times[i];
//             n_mark++;
//             interior_markers.push_back( i - n_mark );
//
//             if ( marker_offset != 0 ){
//                 n_visits = 0;
//             }
//         }
//         else if ( events[i] == event && n_mark > 0 && n_mark < n_markers ){
//             if ( event_offset == 0 ){
//                 if ( marker_offset != 0 ){
//                     local_times.push_back( times[i] - last_marker_time + marker_offset );
//                 }
//                 else {
//                     local_times.push_back( times[i] - last_marker_time );
//                 }
//             }
//             else{
//                 n_visits++;
//                 if ( marker_offset != 0 ){
//                     local_times.push_back( times[i] - last_marker_time + marker_offset + n_visits * event_offset );
//                 }
//                 else{
//                     local_times.push_back( times[i] - last_marker_time + n_visits * event_offset );
//                 }
//             }
//         }
//
//     }
//
//     List return_list = List::create(Named("local_times") = local_times, Named("interior_markers") = interior_markers );
//     return( return_list  );
// }
