#### EBD fitnesses ####

#' @include EBD_pop.R
NULL

#' Evolutionary Behaviour Dynamics Fitness Definition class
#'
#' In Evolutionary Behaviour Dynamics (McDowell, 2004), each behaviour has an associated fitness. This determines the probability with which they are selected to be parents. Behaviours in the population are sampled to be parents from a probability distribution (fitness function) with its domain defined by fitness values. The fitnesses definition is implemented as a \code{elemental} object that contains a function that calculates the fitnesses according to the specified fitness definition.
#'
#' @slot EBD_fitness_def Contains a function for calculating fitness values.
#' @slot name The name of the \code{EBD_fitness_def} object. Inherited from \code{elemental}.
#' @slot type This will be \code{elemental} because \code{EBD_fitness_def} is a \code{elemental} object. Inherited from \code{elemental}.
#'
#' @section Built-in fitness definitions:{
#' Here is a list of the built-in fitness definition functions.
#'     \describe{
#'         \item{\code{wrapped_specific_individual_fitness}}{Fitness is defined as the minimum absolute difference between each behaviour in the population and a reference behaviour, with the distance wrapped around a circle.
#'         }
#'     }
#' }
#'
#' @export wrapped_specific_individual_fitness
#' @export make.EBD_fitness_def

class.EBD_fitness_def = setClass( "EBD_fitness_def", slots = list( EBD_fitness_def = "function" ), contains = "elemental" )

wrapped_specific_individual_fitness = function( pop, reference){
    maximum = max( pop@info$domain )+1
    abs_diff = abs( pop@phenotype$phenotype - reference )
    dist = abs( ( abs_diff > as.integer( maximum/2 ) )*maximum - abs_diff )
    dist
}

make.EBD_fitness_def = function( EBD_fitness_d, name ){
    if ( !is.function( EBD_fitness_d ) ) stop( "Enter EBD_fitness_d as 'function'")
    if ( !is.character( name ) ) stop( "Enter name as 'character'" )
    new( "EBD_fitness_def", EBD_fitness_d = EBD_fitness_fx, name = name, type = "elemental" )
}
