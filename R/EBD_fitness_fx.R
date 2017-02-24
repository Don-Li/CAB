#### EBD parental fitness function ####

#' @include EBD_pop.R EBD_fitness_def.R
NULL

#' Evolutionary Behaviour Dynamics Parental Fitness Function class
#'
#' In Evolutionary Behaviour Dynamics (McDowell, 2004), each behaviour has an associated fitness. This determines the probability with which they are selected to be parents. Behaviours in the population are sampled to be parents from a probability distribution (fitness function) with its domain defined by fitness values. The fitness function is implemented as a \code{elemental} object that contains the fitness function.
#'
#' Do this later
#'
#' @export geometric_fitness_fx
#' @export make.EBD_fitness_fx

class.EBD_fitness_fx = setClass( "EBD_fitness_fx", slots = list( EBD_fitness_fx = "function" ), contains = "elemental" )

geometric_fitness_fx = function( fitness, p ){
    dgeom( fitness, p )
}

make.EBD_fitness_fx = function( EBD_fitness_f, name ){
    if ( !is.function( EBD_fitness_f ) ) stop( "Enter EBD_fitness_f as 'function'" )
    if ( !is.character( name ) ) stop( "Enter name as 'character'" )
    new( "EBD_fitness_fx", EBD_fitness_fx = EBD_fitness_f, name = name, type = "elemental" )
}
