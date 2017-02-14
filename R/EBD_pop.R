#### EBD population

#' @include e_show.R elemental_get_helpers.R elemental.R
NULL

#' Evolutionary Behaviour Dynamics population class
#'
#' In Evolutionary Behaviour Dynamics, the organism is represented as a population of behaviours. This represents the domain of behaviours that the organism can emit at a given time. Each behaviour has dual representation as an integer and as a binary string. The integer represents a "phenotype" and the binary string represents a "genotype". The \code{EBD_pop} class is for these populations. See McDowell (2004).
#'
#' Objects of the \code{EBD_pop} class have two slots. A vector of integers for the phenotypes and a matrix of binaries for the genotypes. The indices are such that the same index for the genotype vector corresponds to the row of the genotype matrix. In addition, there is a class of \code{info} to contain two attributes: the size of the population and a vector containing the domain of integers that can be behaviours.
#'
#' @slot phenotype A vector of integers.
#' @slot genotype A matrix of Booleans. Each column corresponds to a place on the binary string. Each row corresponds to a different behaviour.
#' @slot info A list with two elements: \code{size} and \code{domain}.
#' @slot name A name for the population. Inherited from \code{\link{class.elemental}}.
#' @slot type Contains the string "elemental". Inherited from \code{\link{class.elemental}}.
#'
#' @rdname class.EBD_pop
#' @references
#' McDowell, J. J. (2004). A computational model of selection by consequences. Journal of the Experimental Analysis of Behavior, 81(3), 297–317. \link{https://doi.org/10.1901/jeab.2004.81-297}
#'
#' @export make.EBD_pop

class.EBD_pop = setClass( "EBD_pop", slots = list( phenotype = "numeric", genotype = "matrix", info = "list"), contains = "elemental" )

make.EBD_pop = function( pop_size, behaviour_domain, obj_name = "population" ){
    if ( !is.numeric( pop_size ) ) stop( "Enter pop_size as 'numeric'")
    if ( any( !is.numeric( behaviour_domain ), length(behaviour_domain) != 2 ) ) stop( "behaviour_domain must be a 'numeric' vector of length 2" )
    new( "EBD_pop",
        phenotype = vector("numeric", pop_size),
        genotype = matrix( NaN, nrow = pop_size, ncol = log( max(behaviour_domain), 2 ) ),
        info = list( size = pop_size, domain = behaviour_domain ),
        name = obj_name,
        type = "elemental")
}




# char_mat = matrix( "a", nrow = 10, ncol = 10000 )
# # num_mat = matrix( 1, nrow = 10, ncol = 10000 )
# #
# system.time(
#     for ( k in 1:10){
#     for (i in 1:10000) char_mat[,i] = as.character( as.integer( intToBits( floor( runif( 1, 0, 1024) ) )[10:1] ) )
# })
#
# system.time(
#     for ( k in 1:10){
#     for (i in 1:10000) num_mat[,i] = as.integer( intToBits( floor( runif( 1, 0, 1024) ) )[10:1] )
# })
