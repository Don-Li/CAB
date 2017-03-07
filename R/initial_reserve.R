#### Initial Reserve function ####

#' @include e_show.R elemental.R
NULL

#' Initial Reserve Function class
#'
#' Initial Reserve functions are implemented specially for Catania's Operant Reserve (Catania, 2005). Catania (2005) and Berg & McDowell (2011) used constants for the reserve depletion. However, we have generalised this to a function. The \code{initial_reserve} class is used to set up the Initial Reserve function, that is, the parameters are not set here. Instead, the \code{initial_reserve} will go into a \code{initial_reserve_control} object that contains both the \code{initial_reserve} as well as its associated parameters.
#'
#' In the CAB package, objects from classes like \code{initial_reserve} are called 'elemental' objects That is, they comprise a particular element of the simulation without any specified parameter values. 'Control' objects are elemental objects with an associated list of parameter values. Refer to \code{\link{class.DOR}} for information for putting a \code{initial_reserve} in a model.
#'
#' The \code{show} method for the \code{initial_reserve} class can be removed with the function \code{remove.initial_reserve.show()} and reinstated with \code{initial_reserve.show}.
#'
#' @slot initial_reserve This will contain an Initial Reserve function. See examples.
#' @slot name The name of the \code{initial_reserve} object.
#' @slot type This will be \code{elemental} because the \code{initial_reserve} is an \code{elemental} object.
#'
#' @section Built-in Initial Reserve functions:{
#' A list of the built-in Initial Reserve functions. The function definitions can be seen by calling the name of the function without brackets. These functions return a scalar that is the initial reserve value.
#'     \describe{
#'         \item{\code{constant_initial_fx}}{An Initial Reserve function where the initial reserve level is a constant. \deqn{ f(t = 0) = x }
#'         where \eqn{x} is the amount that the reserve will begin with at \eqn{time = 0}.}
#'     }
#' }
#'
#' @section Make a \code{initial_reserve} object from an Initial Reserve function:{
#' Use the \code{make.initial_reserve} function to make a \code{initial_reserve} object.
#'     \subsection{Usage}{
#'         \code{make.initial_reserve( initial_reserve_fx, name )}
#'     }
#'     \subsection{Arguments}{
#'         \describe{
#'             \item{\code{initial_reserve_fx}}{A function that represents the Initial Reserve level of interest}
#'             \item{\code{name}}{A character vector specifying the name of your \code{initial_reserve} object}
#'         }
#'     }
#'     \subsection{Value}{
#'         Returns a \code{initial_reserve} object.
#'         }
#' }
#'
#' @examples
#' # Look at the definition of the "constant_initial_fx" Initial Reserve function.
#' constant_initial_fx
#' # Make an initial_reserve object out of the "constant_initial_fx" function.
#' initial_reserve = make.initial_reserve( initial_reserve_fx = constant_initial_fx, name = "initial_reserve" )
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
#' @rdname class.initial_reserve
#' @aliases initial_reserve
#'
#' @references
#' Berg, J. P., & McDowell, J. J (2011). Quantitative, steady-state properties of Catania's computational model of the operant reserve. Behavioural Processes, 87(1), 71-83. \link{https://doi.org/10.1016/j.beproc.2011.01.006}
#'
#' Catania, A. C. (2005). The operant reserve: A computer simulation in (accelerated) real time. Behavioural Processes, 69(2), 257-278. \link{https://doi.org/10.1016/j.beproc.2005.02.009}
#'
#' @export constant_initial_fx
#' @export make.initial_reserve

class.initial_reserve = setClass( "initial_reserve", slots = list( initial_reserve = "function"), contains = "elemental" )

#### Make an initial_reserve object from a function ####

make.initial_reserve = function( initial_reserve_fx, name ){
    if ( !is.function( initial_reserve_fx ) ) stop( "Please enter inital reserve as a function" )
    if ( !is.character( name ) ) stop( "Enter initial reserve function name as character" )
    new( "initial_reserve", initial_reserve = initial_reserve_fx, name = name, type = "elemental" )
}

#### Built-in initial_reserve functions ####

constant_initial_fx = function( initial ){
    initial
}

#### initial_reserve show methods ####

#' @rdname class.initial_reserve
#' @format The \code{show} method prints the function that is contained in the \code{initial_reserve} object as well as the type of object (i.e. "elemental") and the class (i.e. "initial_reserve" ).

initial_reserve.show = setMethod( "show", signature( object = "initial_reserve" ), function( object ) e_show( object ) )

# #' @rdname class.initial_reserve
# #' @export remove.initial_reserve.show
#
# remove.initial_reserve.show = function() removeMethod( "show", signature( object = "initial_reserve" ) )
