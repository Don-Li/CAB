# #### Delay of reinforcement gradient ####
#
# #' @include e_show.R elemental_get_helpers.R elemental.R
# NULL
#
# #' Delay of Reinforcement Gradient (DOR) class
# #'
# #' Delay of Reinforcement Gradients are represented as elements in the \code{DOR} class. The use of DORs can be found in Catania (2005) and Berg & McDowell (2011). The \code{DOR} class is used to set up the DOR function, that is, its parameters are not set here. Instead, the \code{DOR} will go into a \code{DOR_control} object that contains both the \code{DOR} as well as its associted parameters.
# #'
# #' In the CAB package, objects from classes like \code{DOR} are called 'elemental' objects That is, they comprise a particular element of the simulation without any specified parameter values. 'Control' objects are elemental objects with an associated list of parameter values.
# #'
# #' The way to put a DOR in your model is as follows:
# #' \enumerate{
# #'     \item Code up the function that you want to use for the DOR or use an existing function.
# #'     \item Construct a \code{DOR} object from the function.
# #'     \item Make a list specifying the values of the parameters for the DOR function.
# #'     \item Make a \code{DOR_control} object, which contains the \code{DOR} object and the parameters. See \code{\link{make.control}}.
# #' }
# #'
# #' The \code{show} method for the \code{DOR} class can be removed with the function \code{remove.DOR.show()} and reinstated with \code{DOR.show}.
#
# #'
# #' @slot DOR This will contain a DOR function. See examples.
# #' @slot name The name of the \code{DOR} object. Inherited from \code{elemental}.
# #' @slot type This will be \code{elemental} because the \code{DOR} is an \code{elemental} object. Inherited from \code{elemental}.
# #'
# #' @section Built-in DOR functions:{
# #' Here is a list of the built-in DOR functions. The function definitions can be seen by calling the name of the function without brackets. For each of these functions, \eqn{t} is the time difference between a response and the latest reinforcement. Each function takes three parameters, \eqn{t}, \eqn{max} as the maximum reinforcement effect of an individual behaviour, and \eqn{scale} as a scaling parameter of the DOR.
# #'     \describe{
# #'         \item{\code{linear_DOR_fx}}{A DOR where the reinforcement effect is a linear function of time since latest reinforcement. \deqn{ f(t) = max - 1/scale * t}
# #'         Note that the reinforcemet effect is bounded by zero.}
# #'         \item{\code{exponential_DOR_fx}}{A DOR where the reinforcement effect is an exponential function of the time since latest reinforcement. \deqn{ f(t) = max * exp( -t/scale ) }}
# #'         \item{\code{reciprocal_DOR_fx}}{A DOR where the reinforcement effect is a reciprocal function of the time since latest reinforcement. \deqn{ f(t) = max / ( t+1 )^( 1/scale ) }}
# #'         \item{\code{hyperbolic_DOR_fx}}{A DOR where the reinforcement effect is a hyperbolic function of the time since latest reinforcement. \deqn{ f(t) = 2 * max / ( ( t + 1 )^( 1/scale ) + 1 )}}
# #'     }
# #' With respect to the actual implementation of these DOR functions, each of the built-in DOR functions have five arguments.
# #' \describe{
# #'     \item{\code{max}}{The maximum increment from an individual response.}
# #'     \item{\code{scale}}{A scaling factor on the DOR. The scaling is independent of \code{max}.}
# #'     \item{\code{IRI_resp_times}}{A vector containing response times counting backward from the most recent reinforcement time.}
# #'     \item{\code{reserve}}{A numeric specifying the value of the reserve.}
# #'     \item{Each of these arguments can instead be a string specifying the variable that should be taken as the argument from a \code{input} object.}{}
# #' }
# #' The DOR function will return the value of the reserve after the increment.
# #' }
# #'
# #' @section Make a \code{DOR} object from a DOR function:{
# #' Use the \code{make.DOR} function to make a \code{DOR} object.
# #'     \subsection{Usage}{
# #'         \code{make.DOR(DOR_fx, name)}
# #'     }
# #'     \subsection{Arguments}{
# #'         \describe{
# #'             \item{\code{DOR_fx}}{A function that represents the DOR of interest}
# #'             \item{\code{name}}{A character vector specifying the name of your \code{DOR} object}
# #'         }
# #'     }
# #'     \subsection{Value}{
# #'         Returns a \code{DOR} object.
# #'         }
# #' }
# #'
# #' @examples
# #' # Look at the definition of the linear DOR
# #' linear_DOR_fx
# #' # Look at the form of the DOR
# #' iri_resp_times = seq(0, 15 , by = 0.1 )
# #' my_DOR = linear_DOR_fx( max = 1, scale = 10, iri_resp_times, reserve_value = 0 )
# #' plot( iri_resp_times, my_DOR, ylab = "increment to the reserve", xlab = "time since last rft" )
# #' # Make a DOR object out of the linear DOR function
# #' linear_DOR = make.DOR( linear_DOR_fx, "linear_DOR" )
# #' # Look at the form of the DOR with the "DOR" object
# #' my_DOR2 = linear_DOR@DOR( max = 1, scale = 10, iri_resp_times, reserve_value = 0, ylab = "increment to the reserve", xlab = "time since last rft" )
# #' plot( iri_resp_times, my_DOR2 )
# #'
# #' # Look at the exponential DOR
# #' exponential_DOR_fx
# #' my_expo_DOR = exponential_DOR_fx( max = 1, scale = 10, iri_resp_times, reserve_value = 0 )
# #' plot( iri_resp_times, my_expo_DOR, ylab = "increment to the reserve", xlab = "time since last rft" )
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
# #' \code{\link{accessor_helpers}} for the helper functions used in the built-in DOR functions.
# #'
# #' \code{\link{class.elemental}} for the parent \code{elemental} class.
# #'
# #' \code{\link{get.reserve}} for a helper function that gets the reserve value.
# #'
# #' \code{\link{get.IRI_resp_times}} for a helper function that gets the inter-reinforcement interval response times.
# #'
# #' @rdname class.DOR
# #' @aliases DOR
# #'
# #' @references
# #' Berg, J. P., & McDowell, J. J (2011). Quantitative, steady-state properties of Catania's computational model of the operant reserve. Behavioural Processes, 87(1), 71-83. \link{https://doi.org/10.1016/j.beproc.2011.01.006}
# #'
# #' Catania, A. C. (2005). The operant reserve: A computer simulation in (accelerated) real time. Behavioural Processes, 69(2), 257-278. \link{https://doi.org/10.1016/j.beproc.2005.02.009}
# #'
# #' @export linear_DOR_fx
# #' @export exponential_DOR_fx
# #' @export reciprocal_DOR_fx
# #' @export hyperbolic_DOR_fx
# #' @export make.DOR
#
# class.DOR = setClass( "DOR", slots = list( DOR = "function" ), contains = "elemental" )
#
# #### Make a DOR object from a function ####
#
# make.DOR = function( DOR_fx, name ){
#     if ( !is.function( DOR_fx ) ) stop( "Enter DOR function as 'function'" )
#     if ( !is.character( name ) ) stop( "Enter DOR name as 'character'" )
#     new( "DOR", DOR = DOR_fx, name = name, type = "elemental" )
# }
#
# #### Built-in DOR functions ####
#
# linear_DOR_fx = function( max, scale, IRI_resp_times, reserve_value ){
#     reserve = get.reserve( reserve_value )
#     iri_resp_times = get.IRI_resp_times( IRI_resp_times )
#     pmax( max - 1/scale * iri_resp_times, 0 )
# }
#
# exponential_DOR_fx = function( max, scale, IRI_resp_times, reserve_value ){
#     reserve = get.reserve( reserve_value )
#     iri_resp_times = get.IRI_resp_times( IRI_resp_times )
#     max * exp( -iri_resp_times / scale )
# }
#
# reciprocal_DOR_fx = function( max, scale, IRI_resp_times, reserve_value ){
#     reserve = get.reserve( reserve_value )
#     iri_resp_times = get.IRI_resp_times( IRI_resp_times )
#     max / ( iri_resp_times + 1 ) ^ ( 1 / scale )
# }
#
# hyperbolic_DOR_fx = function( max, scale, IRI_resp_times, reserve_value ){
#     reserve = get.reserve( reserve_value )
#     iri_resp_times = get.IRI_resp_times( IRI_resp_times )
#     2 * max / ( ( iri_resp_times + 1 ) ^ ( 1 / scale ) + 1 )
# }
#
# #### DOR show methods ####
#
# #' @rdname class.DOR
# #' @format The \code{show} method prints the function that is contained in the \code{DOR} object as well as the type of object (i.e. "elemental") and the class (i.e. "DOR" ).
# #' @export DOR.show
#
# DOR.show = setMethod( "show", signature( object = "DOR" ), function( object ) e_show( object ) )
#
# # #' @rdname class.DOR
# # #' @export remove.DOR.show
# #
# # remove.DOR.show = function() removeMethod( "show", signature( object = "DOR" ) )
