#### Emission function ####

#' @include e_show.R analysis_helpers.R elemental.R
NULL

#' Emission Function class
#'
#' Emission functions are implemented specially for specially for Catania's Operant Reserve (Catania, 2005). The Emission function takes some input and emits behaviour or the time at which behaviour occurs. In the case of Catania's Operant Reserve, the input is the reserve level.
#'
#' In the CAB package, objects from classes like \code{emission} are called 'elemental' objects That is, they comprise a particular element of the simulation without any specified parameter values. 'Control' objects are elemental objects with an associated list of parameter values. Refer to \code{\link{class.DOR}} for information for putting a \code{emission} in a model.
#'
#' The \code{show} method for the \code{emission} class can be removed with the function \code{remove.emission.show()} and reinstated with \code{emission.show}.
#'
#' @slot emission This will contain an Emission function. See examples.
#' @slot name The name of the \code{emission} object.
#' @slot type This will be \code{elemental} because the \code{emission} is an \code{elemental} object.
#'
#' @section Built-in emission functions:{
#' A list of the built-in emission functions. The function definitions can be seen by calling the name of the function without brackets. Each function returns the time at which the next response will occur.
#'     \describe{
#'         \item{\code{G_E_emission_fx( reserve_value, time, min_IRT )}}{An Emission function where the time at which the next response will occur is represented as a Geometrically distributed random variable with the probability of emitting a response being equal to the momentary reserve level, and then transformed from a Geometric distribution to an Exponential dsitribution so that responses are emitted in continous time. An additional argument is added for the minimum inter-response time. The distribution is given as: \deqn{ t ~ Exponential( rate = -log(1-reserve) ) + min_IRT } where \eqn{t} is the emitted inter-response time. See \code{\link{rexp}}.}
#'     }
#' }
#'
#' @section Make an \code{emission} object from an Emission function:{
#' Use the \code{make.emission} function to make a \code{emission} object.
#'     \subsection{Usage}{
#'         \code{make.emission( emission_fx, name )}
#'     }
#'     \subsection{Arguments}{
#'         \describe{
#'             \item{\code{emission_fx}}{A function that represents the Emission of interest}
#'             \item{\code{name}}{A character vector specifying the name of your \code{emission} object}
#'         }
#'     }
#'     \subsection{Value}{
#'         Returns a \code{emission} object.
#'         }
#' }
#'
#' @examples
#' # Look at the definition of the "G_E_emission_fx" Emission function
#' G_E_emission_fx
#' # Make an emission object out of the "G_E_emission_fx" Emission function
#' G_E_emission = make.emission( emission_fx = G_E_emission_fx, name = "G_E_emission" )
#'
#' @seealso
#' \code{\link{make.control}} for making \code{DOR_control} objects.
#'
#' \code{\link{set.custom_elemental}} for setting custom elemental classes.
#'
#' \code{\link{make.custom_elemental}} for making custom elemental objects.
#'
#' \code{\link{e_show}} for the function that is called by the \code{show} method.
#'
#' \code{\link{accessor_helpers}} for the helper functions used in the built-in DOR functions.
#'
#' \code{\link{class.elemental}} for the parent \code{elemental} class.
#'
#' @rdname class.emission
#' @aliases emission
#'
#' @references
#' Catania, A. C. (2005). The operant reserve: A computer simulation in (accelerated) real time. Behavioural Processes, 69(2), 257-278. \link{https://doi.org/10.1016/j.beproc.2005.02.009}
#'

class.emission = setClass( "emission", slots = list( emission = "function" ), contains = "elemental" )

#### Make an Emission object from a function ####

#' @rdname make.emission
#' @export make.emission

make.emission = function( emission_fx, name ){
    if ( !is.function( emission_fx ) ) stop( "Please enter your emission funciton as a function" )
    if ( !is.character( name ) ) stop( "Enter emission function name as character" )
    new( "emission", emission = emission_fx, name = name, type = "elemental" )
}

#### Built-in Emission functions ####
#' @rdname G_E_emission_fx
#' @export G_E_emission_fx

G_E_emission_fx = function( reserve_value, time, min_IRT ){
    rexp( 1, -log(1- get.reserve( reserve_value ) ) ) + min_IRT + get.time( time )
}

#### Emission show methods ####

#' @rdname class.emission
#' @format The \code{show} method prints the function that is contained in the \code{emission} object as well as the type of object (i.e. "elemental") and the class (i.e. "emission" ).
#' @export emission.show

emission.show = setMethod( "show", signature( object = "emission" ), function( object ) e_show( object ) )

#' @rdname class.emission
#' @export remove.emission.show

remove.emission.show = function() removeMethod( "show", signature( object = "emission" ) )
