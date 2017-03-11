#### Parental sampling ####

#' @include RcppExports.R
NULL

#' For parental sampling12121
#'
#' Stuff1212
#'
#'
#' @export bitwise_reproduction
#' @rdname bitwise_reproduction

bitwise_reproduction = function( pop_size, parents, parent_sampling_bias = 0.5 ){
    CAB_cpp_bitwise( pop_size, parents, parent_sampling_bias )
}


