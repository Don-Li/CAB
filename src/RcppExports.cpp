// This file was generated by Rcpp::compileAttributes
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// CAB_cpp_bitwise
NumericVector CAB_cpp_bitwise(int pop_size, NumericMatrix parents, double bias);
RcppExport SEXP CAB_CAB_cpp_bitwise(SEXP pop_sizeSEXP, SEXP parentsSEXP, SEXP biasSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< int >::type pop_size(pop_sizeSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type parents(parentsSEXP);
    Rcpp::traits::input_parameter< double >::type bias(biasSEXP);
    __result = Rcpp::wrap(CAB_cpp_bitwise(pop_size, parents, bias));
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
