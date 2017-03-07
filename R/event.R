#### Event function ####

#' @include e_show.R
NULL

#' Event Function class
#'
#' Event functions are general functions for representing environmental events. That is, their primary use is for reinforcement schedules.
#'
#' In the CAB package, objects from classes like \code{event} are called 'elemental' objects That is, they comprise a particular element of the simulation without any specified parameter values. 'Control' objects are elemental objects with an associated list of parameter values. Refer to \code{\link{class.DOR}} for information for putting a \code{event} in a model.
#'
#' When the \code{event} object is used to contain a reinforcement schedule, the entire schedule should be programmed into the Event function that is contained in the \code{event} object. Alternatively, one may define different custom classes such as \code{event1} and \code{event2} to manage different parts of the schedule (for example, \code{event1} and \code{event2} may manage reinforcement schedules on two keys on a concurrent schedule). If the user wishes to create separate classes for parts of reinforcement schedules, then it is up to the user to make sure that they interact properly.
#'
#' The \code{show} method for the \code{event} class can be removed with the function \code{remove.event.show()} and reinstated with \code{event.show}.
#'
#' @slot event This will contain a event function. See examples.
#' @slot name The name of the \code{event} object.
#' @slot type This will be \code{elemental} because the \code{event} is an \code{elemental} object.
#'
#' @section Built-in event functions:{
#' A list of the built-in event functions. The function definitions can be seen by calling the name of the function without brackets. Each of these functions return the next time (or number of responses) when reinforcement is arranged.
#'     \describe{
#'         \item{\code{true_VI_fx(VI_interval, time)}}{A variable-interval schedule where the next food is arranged at a time that is sampled from an Exponential distribution. The density is: \deqn{f(t; \lambda) = 1/\lambda * e^(- 1/\lambda * t ) } where \eqn{\lambda} is the arranged inter-reinforcement interval. See \code{\link{rexp}}}
#'         \item{\code{FI_fx(FI_interval, time)}}{A fixed-interval schedule where the next food is arranged at a fixed time since the last food was delivered.}
#'         \item{\code{poisson_VR_fx(VR_length, resps)}}{A variable-ratio schedule where the next food is arranged after a number of responses that is sampled from a Poisson distribution. The density is: \deqn{f(t; \lambda) = \lambda^t * e^-t / t! } where \eqn{\lambda} is the arranged ratio requirement. See \code{\link{rpois}}}
#'         \item{\code{FR_fx(FR_length, resps)}}{A fixed-ratio schedule where the next food is arranged at a fixed number of responses since the last food was delivered.}
#' } }
#'
#' @section Make an \code{event} object from a event function:{
#' Use the \code{make.event} function to make a \code{event} object.
#'     \subsection{Usage}{
#'         \code{make.event( event_fx, name )}
#'     }
#'     \subsection{Arguments}{
#'         \describe{
#'             \item{\code{event_fx}}{A function that represents the event of interest}
#'             \item{\code{name}}{A character vector specifying the name of your \code{event} object}
#'         }
#'     }
#'     \subsection{Value}{
#'         Returns a \code{event} object.
#'         }
#' }
#'
#' @examples
#' # Make a variable-interval schedule
#' # Look at the definition of the "true_VI_fx" event function
#' true_VI_fx
#' # Make a event object out of the "true_VI_fx" event function
#' vi_schedule = make.event( event_fx = true_VI_fx, name = "vi_schedule" )
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
#' @rdname class.event
#' @aliases event
#'
#'
#' @export make.event
#' @export true_VI_fx
#' @export FI_fx
#' @export poisson_VR_fx
#' @export FR_fx

class.event = setClass( "event", slots = list( event = "function" , name = "character", type = "character") )

#### Make an Event object from a function ####

make.event = function( event_fx, name ){
    if ( !is.function( event_fx ) ) stop( "Please enter your event schedule as a function" )
    if ( !is.character( name ) ) stop( "Enter event function name as character" )
    new( "event", event = event_fx, name = name, type = "elemental" )
}

#### Built-in Event functions

true_VI_fx = function( VI_interval, time ) rexp( 1, 1 / VI_interval ) + get.time( time )

FI_fx = function( FI_interval, time ) FI_interval + get.time( time )

poisson_VR_fx = function( VR_length, resps ) rpois( 1, VR_length ) + get.resps( resps )

FR_fx = function( FR_length, resps ) FR_length + get.resps( resps )

#### Event show methods ####

#' @rdname class.event
#' @format The \code{show} method prints the function that is contained in the \code{event} object as well as the type of object (i.e. "elemental") and the class (i.e. "event" ).
#' @export event.show

event.show = setMethod( "show", signature( object = "event" ), function( object ) e_show( object ) )

# #' @rdname class.event
# #' @export remove.event.show
#
# remove.event.show = function() removeMethod( "show", signature( object = "event" ) )
