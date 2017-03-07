#### Food duration function ####

#' @include e_show.R elemental.R
NULL

#' Food Duration Function class
#'
#' Food Duration functions are general functions for representing the duration of reinforcement.
#'
#' In the CAB package, objects from classes like \code{food_duration} are called 'elemental' objects That is, they comprise a particular element of the simulation without any specified parameter values. 'Control' objects are elemental objects with an associated list of parameter values. Refer to \code{\link{class.DOR}} for information for putting a \code{food_duration} in a model.
#'
#' The \code{show} method for the \code{food_duration} class can be removed with the function \code{remove.food_duration.show()} and reinstated with \code{food_duration.show}.
#'
#' @slot food_duration This will contain a Food Duration function. See examples.
#' @slot name The name of the \code{food_duration} object.
#' @slot type This will be \code{elemental} because the \code{food_duration} is an \code{elemental} object.
#' @section Built-in event functions:{
#' A list of the built-in event functions. The function definitions can be seen by calling the name of the function without brackets. Each function returns the time after the food delivery.
#'     \describe{
#'         \item{\code{constant_food_duration( food_duration, time )}}{Food duration is a fixed constant.}
#' } }
#'
#' @section Make an \code{food_duration} object from a Food Duration function:{
#' Use the \code{make.food_duration} function to make a \code{food_duration} object.
#'     \subsection{Usage}{
#'         \code{make.food_duration( food_duration_fx, name )}
#'     }
#'     \subsection{Arguments}{
#'         \describe{
#'             \item{\code{food_duration_fx}}{A function that represents the Food Duration of interest}
#'             \item{\code{name}}{A character vector specifying the name of your \code{food_duration} object}
#'         }
#'     }
#'     \subsection{Value}{
#'         Returns a \code{food_duration} object.
#'         }
#' }
#'
#' @examples
#' # Make a food_duration object
#' # Look at the definition of the "constant_food_duration" Food Duration function
#' constant_food_duration
#' # Make a food_duration object out of the "constant_food_duration" Food Duration function
#' food_duration = make.food_duration( food_duration_fx = constant_food_duration, name = "food_duration" )
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
#' \code{\link{class.elemental}} for the parent \code{elemental} class.
#'
#' @rdname class.food_duration
#' @aliases event
#'
#' @export make.food_duration
#' @export constant_food_duration_fx
#'

class.food_duration = setClass( "food_duration", slots = list( food_duration = "function" ), contains = "elemental" )

#### Make a food_duration object from a function ####

make.food_duration = function( food_duration_fx, name ){
    if ( !is.function( food_duration_fx ) ) stop( "Please enter your food duration as a function " )
    if ( !is.character( name ) ) stop( "Enter food duration function name as character" )
    new( "food_duration", food_duration = food_duration_fx, name = name, type = "elemental" )
}

#### Built-in food duration functions ####

constant_food_duration_fx = function( food_dur, time ){
    get.time(time) + food_dur
}

#### Food duration show methods ####

#' @rdname class.food_duration
#' @format The \code{show} method prints the function that is contained in the \code{food_duration} object as well as the type of object (i.e. "elemental") and the class (i.e. "food_duration" ).
#' @export food_duration.show

food_duration.show = setMethod( "show", signature( object = "food_duration" ), function( object ) e_show( object ) )

# #' @rdname class.food_duration
# #' @export remove.food_duration.show
#
# remove.food_duration.show = function() removeMethod( "show", signature( object = "termination" ) )
