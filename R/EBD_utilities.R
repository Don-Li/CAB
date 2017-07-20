#### Evolutionary Behaviour Dynamics utilities ####

#' @include RcppExports.R
NULL

#' Evolutionary Behaviour Dynamics Utilities
#'
#' This is the documentation for the EBD utility functions.
#'
#'
#' @rdname EBD_utilities

#' @rdname EBD_utilities
#' @aliases bin2int
#'
#' @section \code{bin2int}:{
#'     Convert a binary string from a binary vector to an integer. Works for matrices where each column is a binary string.
#'     \subsection{Usage}{
#'         \code{bin2int( binaries )}
#'     }
#'     \subsection{Arguments}{
#'         \describe{
#'             \item{\code{binaries}}{A \code{numeric} vector where each element corresponds to a binary digit, or a \code{matrix} where each column corresponds to a binary digit.}
#'         }
#'     }
#'     \subsection{Value}{
#'         Returns a \code{numeric} vector of the corresponding integers
#'     }
#' }
#' @export bin2int

bin2int = function( binaries, digits ){
    crossprod( binaries, 2^(0:(digits-1)) )[,1]
}

#' @rdname EBD_utilities
#' @aliases int2bin
#'
#' @section \code{int2bin}:{
#'     Converts integers to their corresponding binary representation. Note that the binary string is reversed. For example \code{1} is \code{10} as opposed to \code{01}.
#'     \subsection{Usage}{
#'         \code{int2bin( integers, digits )}
#'     }
#'     \subsection{Arguments}{
#'         \describe{
#'             \item{\code{integers}}{A \code{numeric} vector of integers}
#'             \item{\code{digits}}{The number of digits for the binary string}
#'         }
#'     }
#'     \subsection{Value}{
#'         Returns a \code{matrix} where each column is a binary string and each row is the digit on the given string.
#'     }
#' }
#'
#' @export int2bin
#' @aliases int2bin
#' @rdname EBD_utilities

int2bin = function( integers, digits ){
    CAB_cpp_int2bin( digits, integers )
}

# # #' @rdname EBD_utilities
# # #' @aliases EBD_set
# # #'
# # #' @section \code{EBD_set}:{
# # #'     Special functions for inserting behaviours into the \code{organism} slot of a \code{EBD} model. Note that this is specific to the implementation of \code{EBD} in the \code{CAB} package.
# # #'     \subsection{Usage}{
# # #'         \code{EBD_set( EBD_model, phynotypes, index, genotypes }
# # #'     }
# # #'     \subsection{Arguments}{
# # #'         \describe{
# # #'             \item{\code{EBD_model}}{A class \code{EBD} object.}
# # #'             \item{\code{phenotypes}}{A \code{numeric} vector representing behaviours.}
# # #'             \item{\code{index}}{A \code{numeric} vector of indices.}
# # #'             \item{\code{genotypes}}{A \code{matrix} with each column corresponding to a binary string of a behaviour.}
# # #'         }
# # #'     }
# # #'     \subsection{Value}{
# # #'         Modifies the \code{genotypes} and \code{phenotypes} in the \code{organism} slot of a \code{EBD} object.
# # #'     }
# # #'     \subsection{Notes}{
# # #'     Only one of \code{phenotypes} or \code{genotypes} must be specified. When one argument (e.g. \code{phenotypes} is specified, the corresponding behaviours in the \code{genotypes} are also changed accordingly.
# # #'     \code{index} is an optional argument. If it is missing, the entire population is replaced.
# # #'     The following variables must be in the \code{organism} slot for these methods to work:  "genotypes", "phenotypes", "domain", "pop_size"
# # #'     }
# # #' }
# # #'
# # #' @export int2bin
# # #' @aliases int2bin
# # #' @rdname EBD_utilities
#
# setGeneric( "EBD_set", function( EBD_model, phenotypes, index, genotypes ) standardGeneric( "EBD_set" ) )
#
# EBD.insert_phenotype = function( env, phenotypes, index ){
#     env$phenotypes[ index ] = phenotypes
#     env$genotypes[, index ] = int2bin( phenotypes, env$n_bits )
# }
#
# EBD.insert_genotype = function( env, genotypes, index ){
#     env$genotypes[, index ] = genotypes
#     env$phenotypes[ index ] = bin2int( genotypes, env$n_bits )
# }
#
# EBD.replace_phenotype = function( env, phenotypes ){
#     if ( length(phenotypes) != env$pop_size ) stop( "New population is not the right size" )
#     env$phenotypes = phenotypes
#     env$genotypes = int2bin( phenotypes, env$n_bits )
# }
#
# EBD.replace_genotype = function( env, genotypes ){
#     if ( ncol( genotypes ) != env$pop_size ) stop( "New population is not the right size" )
#     env$genotypes = genotypes
#     env$phenotypes = bin2int( genotypes, env$n_bits )
# }
#
# # #' @rdname EBD_utilities
# # #' @aliases EBD_set
# # #' @exportMethod EBD_set
#
# setMethod( "EBD_set", signature( EBD_model = "EBD",  phenotypes = "integer", index = "integer", genotypes = "missing" ), function( EBD_model, phenotypes, index ){
#     EBD.insert_phenotype( EBD_model@organism, phenotypes, index )
# } )
#
# setMethod( "EBD_set", signature( EBD_model = "EBD", phenotypes = "missing", index = "integer", genotypes = "matrix" ), function( EBD_model, index, genotypes ){
#     EBD.insert_genotype( EBD_model@organism, genotypes, index )
# } )
#
# setMethod( "EBD_set", signature( EBD_model = "EBD", phenotypes = "missing", index = "missing", genotypes = "matrix" ), function( EBD_model, index, genotypes ){
#     EBD.replace_genotype( EBD_model@organism, genotypes )
# } )
#
# setMethod( "EBD_set", signature( EBD_model = "EBD",  phenotypes = "integer", index = "missing", genotypes = "missing" ), function( EBD_model, phenotypes, index ){
#     EBD.replace_phenotype( EBD_model@organism, phenotypes )
# } )
