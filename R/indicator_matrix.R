#### Make a univariate indicator matrix ####

#' @include RcppExports.R
NULL

#' Construct an indicator matrix
#'
#' From a vector \code{x}, construct an indicator matrix
#'
#' @usage indicator_matrix( x )
#' @param x A \code{numeric} vector
#'
#' @return A matirx with the rows as the length of \code{x} and columns the number of unique \code{x} values.
#'
#' @export indicator_matrix

indicator_matrix = function( x ){
    CAB_cpp_indicator_matrix(x)
}

