#### Parental sampling ####

#' For parental sampling
#'
#' Stuff
#'
#'
#' @export bitwise_reproduction

bitwise_reproduction = function( pop_size, parents ){
    index = CAB_cpp_bitwise_sampler( pop_size, parents )
    x = parents[ index ]
    dim(x) = c( nrow(parents), pop_size )
    x
}
