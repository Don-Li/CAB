#### EBD population

#' @include e_show.R elemental_get_helpers.R elemental.R
NULL

#' Evolutionary Behaviour Dynamics population class
#'
#' In Evolutionary Behaviour Dynamics, the organism is represented as a population of behaviours. This represents the domain of behaviours that the organism can emit at a given time. Each behaviour has dual representation as an integer and as a binary string. The integer represents a "phenotype" and the binary string represents a "genotype". The \code{EBD_pop} class is for these populations. See McDowell (2004).
#'
#' Objects of the \code{EBD_pop} class have two slots. A vector of integers for the phenotypes and a matrix of binaries for the genotypes. The indices are such that the same index for the genotype vector corresponds to the column of the genotype matrix. In addition, there is a class of \code{info} to contain two attributes: the size of the population and a vector containing the domain of integers that can be behaviours.
#'
#' The underlying implementaiton is that the \code{phenotype} and \code{genotype} slots contain an \code{environment} that contains their respective values. This means that the \code{phenotype}s and \code{genotype}s can be modified in place.
#'
#' When the binary strings are specified for vectors in the \code{phenotype} matrix, the binary string is reversed. For example, a binary string of length 10 representing the integer "1" would be "100000000" instead of "000000001". This is just because of the way \code{R} formats \code{raw} objects.
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

class.EBD_pop = setClass( "EBD_pop", slots = list( phenotype = "environment", genotype = "environment", info = "list"), contains = "elemental" )

make.EBD_pop = function( pop_size, behaviour_domain, obj_name = "population" ){
    if ( !is.numeric( pop_size ) ) stop( "Enter pop_size as 'numeric'")
    if ( any( !is.numeric( behaviour_domain ), length(behaviour_domain) != 2 ) ) stop( "behaviour_domain must be a 'numeric' vector of length 2" )
    p = new.env()
    p$phenotype = vector("numeric", pop_size)
    g = new.env()
    g$genotype = matrix( NaN, ncol = pop_size, nrow = ceiling( log( max(behaviour_domain), 2 ) ) )
    new( "EBD_pop",
        phenotype = p,
        genotype = g,
        info = list( size = pop_size, domain = behaviour_domain ),
        name = obj_name,
        type = "elemental")
}


#' @rdname class.EBD_pop
#' @aliases insert_phenotype insert_genotype
#' @section Change the \code{genotype} or \code{phenotype} in a population:{
#' The \code{pop.set} method is available for changing the behaviours in a population.
#'     \subsection{Usage}{
#'         \code{pop.set(pop, phenotypes, index, genotypes)}
#'     }
#'     \subsection{Arguments}{
#'         \describe{
#'             \item{\code{pop}}{A \code{EBD_pop} object}
#'             \item{\code{phenotypes}}{A vector of integers}
#'             \item{\code{index}}{A vector of indices for each behaviour to be inserted}
#'             \item{\code{genotypes}}{A matrix of binary strings with each binary string as a column}
#'         }
#'     }
#'     \subsection{Notes}{
#'         To use the \code{pop.set} method, either the \code{genotypes} or \code{phenotypes} argument must be specified. If the \code{genotypes} argument is specified and the \code{phenotypes} argument is missing, then the integers contained in the \code{genotypes} argument are inserted into the population at the indices specified by \code{index}. The associated phenotypes are obtained by converting the integers into binary strings and are inserted into the column that corresponds to the indices. If the \code{phenotypes} argument is provided and the \code{genotypes} argument is missing, the phenotypes are obtained by converting the genotypes provided and inserting them in the appropriate indices.
#'     }
#' }
#'
#' @exportMethod pop.set

setGeneric( "pop.set", function( pop, phenotypes, index, genotypes ) standardGeneric( "pop.set" ) )

EBD.insert_phenotype = function( pop, phenotypes, index ){
    pop@phenotype$phenotype[ index ] = phenotypes
    pop@genotype$genotype[, index ] = int2bin( phenotypes, ceiling( log( max( pop@info$domain ), 2 ) ) )
}

EBD.insert_genotype = function( pop, genotypes, index ){
    pop@genotype$genotype[, index ] = genotypes
    pop@phenotype$phenotype[ index ] = bin2int( genotypes )
}

setMethod( "pop.set", signature( pop = "EBD_pop",  phenotypes = "numeric", index = "integer", genotypes = "missing" ), function( pop, phenotypes, index ){
    EBD.insert_phenotype( pop, phenotypes, index )
} )


setMethod( "pop.set", signature( pop = "EBD_pop", phenotypes = "missing", index = "integer", genotypes = "matrix" ), function( pop, index, genotypes ){
    EBD.insert_genotype( pop, genotypes, index )
} )


