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
    unique_x = CAB_cpp_unique( x )
    unique_len = length( unique_x )
    x_len = length(x)
    indicator_matrix = matrix( 0, x_len, unique_len )
    indicator_matrix[ 1:x_len + x_len*{ rep( 1:unique_len, each = unique_len ) - 1 } ] = 1
    indicator_matrix
}

