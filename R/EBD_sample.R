#### Sample behaviour EBD ####

#' @include EBD_pop.R
NULL

#' Emit a behaviour from an EBD population
#'
#' In Evolutionary Behaviour Dynamics (McDowell, 2004), a behaviour is emitted from the algorithm by sampling a behaviour from the population with equal probability. The \code{EBD_sample} function is simply a wrapper for the \code{sample} function.
#'
#' @param pop A \code{EBD_pop} object.
#' @param size An \code{integer} specifying the number of behaviours to sample. Defaults to 1.
#' @param replace A \code{logical} specifying whether to sample with repalcement. Defaults to \code{TRUE}.
#' @param prob A \code{numeric} vector specifying the sampling probabilities for each behaviour. Defaults to \code{NULL} for equal probabilityies.
#'
#' @return Returns a vector of integers sampled from the \code{EBD_pop} genotypes.
#'
#' @seealso \code{\link{sample}}
#' @references
#' McDowell, J. J. (2004). A computational model of selection by consequences. Journal of the Experimental Analysis of Behavior, 81(3), 297–317. \link{https://doi.org/10.1901/jeab.2004.81-297}
#'
#' @exportMethod EBD_sample

setGeneric( "EBD_sample", function( pop, size = 1, replace = T, prob = NULL ) standardGeneric( "EBD_sample" ) )

EBD_sampler = function( pop, size = 1, replace = T, prob = NULL ){
    sample( pop@phenotype$phenotype, size = size, replace = replace, prob = prob )
}

setMethod( "EBD_sample", signature( pop = "EBD_pop" ),
    function( pop, size, replace, prob ){
        EBD_sampler( pop = pop, size = size, replace = replace, prob = prob )
    } )
