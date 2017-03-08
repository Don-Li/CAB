#### Parental sampling ####

#' @include RcppExports.R

#' For parental sampling
#'
#' Stuff
#'
#'
#' @export bitwise_reproduction

bitwise_reproduction = function( pop_size, parents, parent_sampling_bias = 0.5 ){
    CAB_cpp_bitwise( pop_size, parents, parent_sampling_bias )
}


