#### not duplicated ####

#' @include RcppExports.R
NULL

#' Find not duplicated values
#'
#' Calls the \code{duplicated} function in \code{Rcpp} and does a logical inversion. This returns a logical value giving \code{TRUE} for values that are not duplicated and \code{FALSE} otherwise.
#'
#' @export not_duplicated
#' @rdname not_duplicted
#'

not_duplicated = function( x ){
    CAB_cpp_not_duplicated( x )
}

