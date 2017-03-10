# #### Reserve depletion function ####
#
# #' @include e_show.R elemental.R
# NULL
#
# #' Reserve Depletion Function class
# #'
# #' Reserve Depletion functions are impelmented specially for Catania's Operant Reserve (Catania, 2005). Catania (2005) and Berg & McDowell (2011) used constants for the reserve depletion. However, we have generalised this to a function. The \code{dep} class is used to set up the dep function, that is, the parameters are not set here. Instead, the \code{dep} will go into a \code{dep_control} object that contains both the \code{dep} as well as its associated parameters.
# #'
# #' In the CAB package, objects from classes like \code{dep} are called 'elemental' objects That is, they comprise a particular element of the simulation without any specified parameter values. 'Control' objects are elemental objects with an associated list of parameter values. Refer to \code{\link{class.DOR}} for information for putting a \code{dep} in a model.
# #'
# #' The \code{show} method for the \code{dep} class can be removed with the function \code{remove.dep.show()} and reinstated with \code{dep.show}.
# #'
# #' @slot dep This will contain a dep function. See examples.
# #' @slot name The name of the \code{dep} object.
# #' @slot type This will be \code{elemental} because the \code{dep} is an \code{elemental} object.
# #'
# #' @section Built-in dep functions:{
# #' A list of the built-in dep functions. The function definitions can be seen by calling the name of the function without brackets. Each function returns the value of the reserve after depletion.
# #'     \describe{
# #'         \item{\code{constant_dep_fx}}{A dep function where the depletion to the reserve is a constant. \deqn{ f(t) = -d }
# #'         where \eqn{d} is the amount that the reserve is depleted. Note that the reserve will not be depleted below zero.}
# #'     }
# #' }
# #'
# #' @section Make a \code{dep} object from a dep function:{
# #' Use the \code{make.dep} function to make a \code{dep} object.
# #'     \subsection{Usage}{
# #'         \code{make.dep( dep_fx, name )}
# #'     }
# #'     \subsection{Arguments}{
# #'         \describe{
# #'             \item{\code{dep_fx}}{A function that represents the dep of interest}
# #'             \item{\code{name}}{A character vector specifying the name of your \code{dep} object}
# #'         }
# #'     }
# #'     \subsection{Value}{
# #'         Returns a \code{dep} object.
# #'         }
# #' }
# #'
# #' @examples
# #' # Look at the definition of the "constant_dep_fx" dep function
# #' constant_dep_fx
# #' # Make a dep object out of the "constant_dep_fx" dep function
# #' constant_dep = make.dep( dep_fx = constant_dep_fx, name = "constant_dep" )
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
# #' \code{\link{get.reserve}} for a helper function tht gets the reserve value.
# #'
# #' @rdname class.dep
# #' @aliases dep
# #'
# #' @references
# #' Berg, J. P., & McDowell, J. J (2011). Quantitative, steady-state properties of Catania's computational model of the operant reserve. Behavioural Processes, 87(1), 71-83. \link{https://doi.org/10.1016/j.beproc.2011.01.006}
# #'
# #' Catania, A. C. (2005). The operant reserve: A computer simulation in (accelerated) real time. Behavioural Processes, 69(2), 257-278. \link{https://doi.org/10.1016/j.beproc.2005.02.009}
# #'
# #' @export constant_dep_fx
# #' @export make.dep
#
# class.dep = setClass( "dep", slots = list( dep = "function" ), contains = "elemental" )
#
# #### Make a dep object from a function ####
#
# make.dep = function( dep_fx, name ) {
#     if ( !is.function( dep_fx ) ) stop( "Please enter your depletion function as a function" )
#     if ( !is.character( name ) ) stop( "Enter depletion function mae as character" )
#     new( "dep", dep = dep_fx, name = name, type = "elemental" )
# }
#
# #### Built-in dep functions ####
#
# constant_dep_fx = function( deplete, reserve_value ){
#     reserve = get.reserve( reserve_value )
#     pmax( reserve - deplete, 0 )
# }
#
# #### dep show methods ####
#
# #' @rdname class.dep
# #' @format The \code{show} method prints the function that is contained in the \code{dep} object as well as the type of object (i.e. "elemental") and the class (i.e. "dep" ).
# #' @export dep.show
#
# dep.show = setMethod( "show", signature( object = "dep" ), function( object ) e_show( object ) )
#
# # #' @rdname class.dep
# # #' @export remove.dep.show
# #
# # remove.dep.show = function() removeMethod( "show", signature( object = "dep" ) )
