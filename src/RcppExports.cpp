// This file was generated by Rcpp::compileAttributes
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// CAB_cpp_bitwise
NumericVector CAB_cpp_bitwise(int pop_size, NumericMatrix fathers, NumericMatrix mothers, double bias);
RcppExport SEXP CAB_CAB_cpp_bitwise(SEXP pop_sizeSEXP, SEXP fathersSEXP, SEXP mothersSEXP, SEXP biasSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< int >::type pop_size(pop_sizeSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type fathers(fathersSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type mothers(mothersSEXP);
    Rcpp::traits::input_parameter< double >::type bias(biasSEXP);
    __result = Rcpp::wrap(CAB_cpp_bitwise(pop_size, fathers, mothers, bias));
    return __result;
END_RCPP
}
// CAB_cpp_clean_short_ixyi
LogicalVector CAB_cpp_clean_short_ixyi(DataFrame data, String x_name, String y_name, double gap);
RcppExport SEXP CAB_CAB_cpp_clean_short_ixyi(SEXP dataSEXP, SEXP x_nameSEXP, SEXP y_nameSEXP, SEXP gapSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< DataFrame >::type data(dataSEXP);
    Rcpp::traits::input_parameter< String >::type x_name(x_nameSEXP);
    Rcpp::traits::input_parameter< String >::type y_name(y_nameSEXP);
    Rcpp::traits::input_parameter< double >::type gap(gapSEXP);
    __result = Rcpp::wrap(CAB_cpp_clean_short_ixyi(data, x_name, y_name, gap));
    return __result;
END_RCPP
}
// CAB_cpp_clean_short_ixxi
LogicalVector CAB_cpp_clean_short_ixxi(DataFrame data, String x_name, double gap);
RcppExport SEXP CAB_CAB_cpp_clean_short_ixxi(SEXP dataSEXP, SEXP x_nameSEXP, SEXP gapSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< DataFrame >::type data(dataSEXP);
    Rcpp::traits::input_parameter< String >::type x_name(x_nameSEXP);
    Rcpp::traits::input_parameter< double >::type gap(gapSEXP);
    __result = Rcpp::wrap(CAB_cpp_clean_short_ixxi(data, x_name, gap));
    return __result;
END_RCPP
}
// CAB_cpp_compute__I_xy_I__formal_event_record
NumericVector CAB_cpp_compute__I_xy_I__formal_event_record(DataFrame data, CharacterVector x_event, CharacterVector y_event, CharacterVector break_event);
RcppExport SEXP CAB_CAB_cpp_compute__I_xy_I__formal_event_record(SEXP dataSEXP, SEXP x_eventSEXP, SEXP y_eventSEXP, SEXP break_eventSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< DataFrame >::type data(dataSEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type x_event(x_eventSEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type y_event(y_eventSEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type break_event(break_eventSEXP);
    __result = Rcpp::wrap(CAB_cpp_compute__I_xy_I__formal_event_record(data, x_event, y_event, break_event));
    return __result;
END_RCPP
}
// CAB_cpp_diff
NumericVector CAB_cpp_diff(NumericVector x);
RcppExport SEXP CAB_CAB_cpp_diff(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    __result = Rcpp::wrap(CAB_cpp_diff(x));
    return __result;
END_RCPP
}
// CAB_cpp_geometric_fitness_selection
List CAB_cpp_geometric_fitness_selection(NumericVector fitness, int pop_size, double p);
RcppExport SEXP CAB_CAB_cpp_geometric_fitness_selection(SEXP fitnessSEXP, SEXP pop_sizeSEXP, SEXP pSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< NumericVector >::type fitness(fitnessSEXP);
    Rcpp::traits::input_parameter< int >::type pop_size(pop_sizeSEXP);
    Rcpp::traits::input_parameter< double >::type p(pSEXP);
    __result = Rcpp::wrap(CAB_cpp_geometric_fitness_selection(fitness, pop_size, p));
    return __result;
END_RCPP
}
// CAB_cpp_indicator_matrix
NumericMatrix CAB_cpp_indicator_matrix(NumericVector x);
RcppExport SEXP CAB_CAB_cpp_indicator_matrix(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    __result = Rcpp::wrap(CAB_cpp_indicator_matrix(x));
    return __result;
END_RCPP
}
// CAB_cpp_int2bin
IntegerVector CAB_cpp_int2bin(int bits, IntegerVector n);
RcppExport SEXP CAB_CAB_cpp_int2bin(SEXP bitsSEXP, SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< int >::type bits(bitsSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type n(nSEXP);
    __result = Rcpp::wrap(CAB_cpp_int2bin(bits, n));
    return __result;
END_RCPP
}
// CAB_cpp_local_times_formal_event_record
List CAB_cpp_local_times_formal_event_record(DataFrame data, String event, String marker, double event_offset, int n_markers);
RcppExport SEXP CAB_CAB_cpp_local_times_formal_event_record(SEXP dataSEXP, SEXP eventSEXP, SEXP markerSEXP, SEXP event_offsetSEXP, SEXP n_markersSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< DataFrame >::type data(dataSEXP);
    Rcpp::traits::input_parameter< String >::type event(eventSEXP);
    Rcpp::traits::input_parameter< String >::type marker(markerSEXP);
    Rcpp::traits::input_parameter< double >::type event_offset(event_offsetSEXP);
    Rcpp::traits::input_parameter< int >::type n_markers(n_markersSEXP);
    __result = Rcpp::wrap(CAB_cpp_local_times_formal_event_record(data, event, marker, event_offset, n_markers));
    return __result;
END_RCPP
}
// CAB_cpp_local_binning
List CAB_cpp_local_binning(NumericVector local_times, NumericVector visit_lengths, int max_bin, double bin_resolution);
RcppExport SEXP CAB_CAB_cpp_local_binning(SEXP local_timesSEXP, SEXP visit_lengthsSEXP, SEXP max_binSEXP, SEXP bin_resolutionSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< NumericVector >::type local_times(local_timesSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type visit_lengths(visit_lengthsSEXP);
    Rcpp::traits::input_parameter< int >::type max_bin(max_binSEXP);
    Rcpp::traits::input_parameter< double >::type bin_resolution(bin_resolutionSEXP);
    __result = Rcpp::wrap(CAB_cpp_local_binning(local_times, visit_lengths, max_bin, bin_resolution));
    return __result;
END_RCPP
}
// CAB_cpp_local_times_ragged_event_record
List CAB_cpp_local_times_ragged_event_record(NumericVector event_times, NumericVector marker_times, double event_offset);
RcppExport SEXP CAB_CAB_cpp_local_times_ragged_event_record(SEXP event_timesSEXP, SEXP marker_timesSEXP, SEXP event_offsetSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< NumericVector >::type event_times(event_timesSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type marker_times(marker_timesSEXP);
    Rcpp::traits::input_parameter< double >::type event_offset(event_offsetSEXP);
    __result = Rcpp::wrap(CAB_cpp_local_times_ragged_event_record(event_times, marker_times, event_offset));
    return __result;
END_RCPP
}
// CAB_cpp_not_duplicated
LogicalVector CAB_cpp_not_duplicated(NumericVector x);
RcppExport SEXP CAB_CAB_cpp_not_duplicated(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    __result = Rcpp::wrap(CAB_cpp_not_duplicated(x));
    return __result;
END_RCPP
}
// CAB_cpp_srswo
NumericMatrix CAB_cpp_srswo(int size, int group_size, int n);
RcppExport SEXP CAB_CAB_cpp_srswo(SEXP sizeSEXP, SEXP group_sizeSEXP, SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< int >::type size(sizeSEXP);
    Rcpp::traits::input_parameter< int >::type group_size(group_sizeSEXP);
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    __result = Rcpp::wrap(CAB_cpp_srswo(size, group_size, n));
    return __result;
END_RCPP
}
// CAB_cpp_srswo2
NumericMatrix CAB_cpp_srswo2(int size, int n);
RcppExport SEXP CAB_CAB_cpp_srswo2(SEXP sizeSEXP, SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< int >::type size(sizeSEXP);
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    __result = Rcpp::wrap(CAB_cpp_srswo2(size, n));
    return __result;
END_RCPP
}
// CAB_cpp_unique
NumericVector CAB_cpp_unique(NumericVector x);
RcppExport SEXP CAB_CAB_cpp_unique(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    __result = Rcpp::wrap(CAB_cpp_unique(x));
    return __result;
END_RCPP
}
// timesTwo
NumericVector timesTwo(NumericVector x);
RcppExport SEXP CAB_timesTwo(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    __result = Rcpp::wrap(timesTwo(x));
    return __result;
END_RCPP
}
