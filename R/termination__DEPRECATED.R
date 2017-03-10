# #### Termination function ####
#
# #' @include e_show.R elemental.R
# NULL
#
# #' Termination Function class
# #'
# #' Termination functions are general functions for representing the stopping conditions for an experiment.
# #'
# #' In the CAB package, objects from classes like \code{termination} are called 'elemental' objects That is, they comprise a particular element of the simulation without any specified parameter values. 'Control' objects are elemental objects with an associated list of parameter values. Refer to \code{\link{class.DOR}} for information for putting a \code{termination} in a model.
# #'
# #' When the \code{termination} object is used to contain many termination condtiions, each termination condition should be programmed into a single Termination function that is contained in the \code{termination} object. Alternatively, one may define different custom classes to manage each termination condition. For more details on implementing multiple classes for multiple termination conditions, see \code{\link{class.termination}}.
# #'
# #' The \code{show} method for the \code{termination} class can be removed with the function \code{remove.termination.show()} and reinstated with \code{termination.show}.
# #'
# #' @slot termination This will contain a Termination function. See examples.
# #' @slot name The name of the \code{termination} object.
# #' @slot type This will be \code{elemental} because the \code{termination} is an \code{termination} object.
# #'
# #' @section Built-in Termination functions:{
# #' A list of the built-in Termination functions. The function definitions can be seen by calling the name of the function without brackets. Each function returns a \code{logical} indicating whether or not the termination condition is met.
# #'     \describe{
# #'         \item{\code{time_termination_fx( time, finish_time)}}{Experimental terminates after the set time.}
# #'         \item{\code{resp_termination_fx( resp, finish_resp)}}{Experiment terminates after set number of responses.}
# #' } }
# #'
# #' @section Make an \code{termination} object from a Termination function:{
# #' Use the \code{make.termination} function to make a \code{termination} object.
# #'     \subsection{Usage}{
# #'         \code{make.termination( termination_fx, name )}
# #'     }
# #'     \subsection{Arguments}{
# #'         \describe{
# #'             \item{\code{termination_fx}}{A function that represents the Termination function of interest}
# #'             \item{\code{name}}{A character vector specifying the name of your \code{termination} object}
# #'         }
# #'     }
# #'     \subsection{Value}{
# #'         Returns a \code{termination} object.
# #'         }
# #' }
# #'
# #' @examples
# #' # Terminate the experiemnt at 3000 responses
# #' # Look at the definition of the "resp_termination_fx" Termination function
# #' resp_termination_fx
# #' # Make a termiantion object out of the "resp_termination_fx" Termination function
# #' resp_termination = make.termination( termination_fx = resp_termination_fx, name = "resp_termination" )
# #'
# #' @seealso
# #' \code{\link{make.control}} for making \code{DOR_control} objects.
# #'
# #' \code{\link{set.custom_elemental}} for setting custom elemental classes.
# #'
# #' \code{\link{make.custom_elemental}} for making custom elemental objects.
# #'
# #' \code{\link{e_show}} for the function that is called by the \code{show} method.
# #'
# #' \code{\link{class.elemental}} for the parent \code{elemental} class.
# #'
# #' @rdname class.termination
# #' @aliases termination
# #'
# #'
# #' @export make.termination
# #' @export time_termination_fx
# #' @export resp_termination_fx
#
# class.termination = setClass( "termination", slots = list( termination = "function" ), contains = "elemental" )
#
# #### Make a termination object from a function ####
#
# make.termination = function( termination_fx, name ){
#     if ( !is.function( termination_fx ) ) stop( "Enter termination function as 'function'" )
#     if ( !is.character( name ) ) stop ("Enter termination name as character" )
#     new( "termination", termination = termination_fx, name = name, type = "elemental" )
# }
#
# #### Built-in Termiantion functions ####
#
# time_termination_fx = function( time, finish_time ){
#     if (get.time( time ) >= finish_time) TRUE
#     else FALSE
# }
#
# resp_termination_fx = function( resp, finish_resp ){
#     if ( get.resps( resp ) >= finish_resp) TRUE
#     else FALSE
# }
#
# #### Termination show methods ####
#
# #' @rdname class.termination
# #' @format The \code{show} method prints the function that is contained in the \code{termination} object as well as the type of object (i.e. "elemental") and the class (i.e. "termination" ).
# #' @export termination.show
#
# termination.show = setMethod( "show", signature( object = "termination"), function( object ) e_show( object ) )
#
# # #' @rdname class.termination
# # #' @export remove.termination.show
# #
# # remove.termination.show = function() removeMethod( "show", signature( object = "termination") )
#
