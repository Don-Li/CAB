#### Parental sampling ####

#' @include RcppExports.R
NULL

#' For parental sampling for Evolutionary Behaviour Dynamics
#'
#' Stuff1212
#'
#' @references
#' McDowell, J. J. (2004). A computational model of selection by consequences. Journal of the Experimental Analysis of Behavior, 81(3), 297–317. \url{https://doi.org/10.1901/jeab.2004.81-297}
#'
#'
#' @export bitwise_reproduction
#' @rdname bitwise_reproduction

bitwise_reproduction = function( pop_size, parents, parent_sampling_bias = 0.5 ){
    CAB_cpp_bitwise( pop_size, parents, parent_sampling_bias )
}


