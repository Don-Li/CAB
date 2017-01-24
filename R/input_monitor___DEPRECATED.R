# #### Accessing the input monitor ####
#
# #' Making and accessing the \code{sim_input} input monitor
# #'
# #' In a simulation, an \code{input_monitor} is used to monitor the arguments (the arguments not pre-set in a condition) in a simulation. An example of this would be the responses that occur within an inter-reinforcement interval for the calculation of the reserve input from the DOR (see \code{class.DOR} and \code{model.COR}). This document contains functions for accessing and changing elements in the input monitor.
# #'
# #' An \code{input_monitor} is an environment that contians a \code{sim_input}, where each slot in the sim_input contains a list of some given length to store the data. An input monitor is made from a \code{model} object, which contains a \code{sim_input} object inside it. The function to make an input monitor is \code{make.input_monitor}.
# #'
# #' Accessing an element of an input monitor at a given dimension (e.g. the vector containing the time at which responses occurred at element 1 ) is done by using the \code{access.input_monitor} function.
# #'
# #' @seealso
# #' \code{\link{trim.input_monitor}} For trimming the input monitor after a simulation is finished.
# #'
# #'
# #### Make input monitor ####
#
# #' @rdname input_monitor
# #' @aliases make.input_monitor
# #' @section Make a simulation monitor:{
# #' The \code{make.input_monitor( model_object, length )} takes a \code{sim_input} that is contained in a model object and essentially makes a new \code{sim_input} that stores the values of the inputs throughout the simulation. \code{length} is how many elements each input in \code{sim_input} can store. It is best to specify a longer \code{length} than needed because unused (i.e. \code{NAN}) elements will be trimmed off. See \code{\link{trim.input_monitor}}.
# #'
# #' \subsection{Arguments}{
# #'     \describe{
# #'         \item{\code{model_object}}{A model object}
# #'         \item{\code{length}}{A numeric specifying the number of elements contained in each element of the \code{sim_input}}
# #'     }
# #' }
# #'
# #' }
#
# make.input_monitor = function( model_object, length, name = NULL ){
#     nam = names( model_object@input@input )
#     x = lapply( 1:length( nam ), function( x ) rep( NaN, length ) )
#     names( x ) = nam
#     if ( is.null( name ) ) assign( "input_monitor", new( "sim_input", input = x ), envir = globalenv())
#     if ( !is.null( name ) ) assign( name, new( "sim_input", input = x ), envir = globalenv())
# }
#
# # make.input_monitor = function( model_object, length ){
# #     nam = names( model_object@input@input )
# #     input_monitor_env = new.env( parent = emptyenv() )
# #     x = lapply( 1:length( nam ), function( x ) rep( NaN, length ) )
# #     names( x ) = nam
# #     input_monitor_env$input_monitor = new( "sim_input", input = x )
# #
# #     return( input_monitor_env )
# # }
#
# # setClass( "sim_input_env", list( info = "environment" ) )
# # v = new( "sim_input_env", info = my_input_monitor )
#
# #### access.input_monitor ####
#
# #' @rdname input_monitor
# #' @aliases access.input_monitor
# #' @section Access an element in a dimension of an input monitor:{
# #' Sometimes it is necessary to manually access a particular element in an input monitor.
# #' \subsection{Usage}{
# #'     {\code{access.input_monitor( input_monitor, dimension, index, new_value )}}
# #' }
# #' \subsection{Arguments}{
# #'     \describe{
# #'         \item{\code{dimension}}{A character specifying the input dimension. See example.}
# #'         \item{\code{index}}{A numeric vector of indices specifying which elements of the given dimension to change.}
# #'         \item{\code{new_value}}{The new values. The recycling rule is applied if the length of \code{index} is longer than the length of \code{new_value}.}
# #'          \item{\code{input_monitor_name}}{An input_monitor name. Defaults to \code{NULL}. If \code{NULL}, the function looks for an input monitor named \code{input_monitor}. Otherwise, searches for an object with the entered name.}
# #'     }
# #' }
# #' }
# #'
# #' @examples
# #' #### An example for accessing an input monitor ####
# #'
# #' # Following from example in ?model.COR
# #' my_COR = make.COR( DOR_ctrl, emission_ctrl, dep_ctrl, initial_ctrl, schedule_ctrl, termination_ctrl, food_duration_ctrl, my_inputs )
# #' # Make an input monitor
# #' input_monitor = make.input_monitor( my_COR, 100 )
# #' # Change elements 1 to 5 in the "reserve" element of the input monitor to 1:5
# #' access.input_moitor( input_monitor, dimension = "reserve", index = 1:5, new_value = 1:5 )
# #' input_monitor@input$reserve
# #'
# #'@export access.input_monitor
#
# access.input_monitor = function( dimension, index, new_value, input_monitor_name = NULL ){
#     if ( length( dimension ) > 1 ) stop( "Only change one dimension at a time" )
#     if ( is.null(input_monitor_name) ) input_monitor@input[[dimension]][index] <<- new_value
#     if ( !is.null(input_monitor_name) ){
#         txt = paste( input_monitor_name, "@input[[dimension]][index] <<- new_value", sep = "" )
#         eval( parse( text = txt ) )
#     }
# }
#
# #### set.input_monitor ####
#
# #' @rdname input_monitor
# #' @aliases set.input_monitor
# #' @section Set the next empty element in a dimension of the input monitor:{
# #' The \code{set.input_monitor} function is essentially the same as \code{access.input_monitor} with the index value being the last index that is \code{NaN}.
# #'
# #' Note that if the user decides to use an input monitor that is not labelled \code{input_monitor}, then the argument \code{input_monitor_name} should be a name of an input monitor. If so, the function searches in the global environment for an object of the specified name.
# #' }
# #' \subsection{Usage}{
# #'     {\code{set.input_monitor( dimension, new_value, input_monitor_name )}}
# #' }
# #' \subsection{Arguments}{
# #'     \describe{
# #'         \item{\code{dimension}}{A character specifying the input dimension. See example.}
# #'         \item{\code{new_value}}{The new values. The recycling rule is applied if the length of \code{index} is longer than the length of \code{new_value}.}
# #'         \item{\code{input_monitor_name}}{An input_monitor name. Defaults to \code{NULL}. If \code{NULL}, the function looks for an input monitor named \code{input_monitor}. Otherwise, searches for an object with the entered name.}
# #'     }
# #' }
# #' @examples
# #' #### An example for setting the next unused element in an input monitor ####
# #' # Following the example above
# #' set.input_monitor( dimension = "resp_time", new_value = 1 )
# #' set.input_monitor( dimension = "resp_time", new_value = 2 )
# #' input_monitor@input$resp_time
# #'
# #' @export set.input_monitor
#
# set.input_monitor = function( dimension, new_value, input_monitor_name = NULL ){
#     if ( length( dimension ) > 1 ) stop( "Only change one dimension at a time" )
#     if ( is.null(input_monitor_name) ){
#         first_NaN_loc = which( is.nan( input_monitor@input[[dimension]] ) )[1]
#         input_monitor@input[[dimension]][first_NaN_loc] <<- new_value
#     }
#
#     if ( !is.null(input_monitor_name) ){
#         first_NaN_loc = which( is.nan( get(input_monitor_name, envir = globalenv())@input[[dimension]] ) )[1]
#         txt = paste( input_monitor_name, "@input[[dimension]][first_NaN_loc] <<- new_value", sep = "" )
#         eval( parse( text = txt ) )
#     }
# }
#
# #### next.input_monitor ####
#
# #' @rdname input_monitor
# #' @aliases next.input_monitor
# #' @section Set the next empty element of a dimension of the input monitor to be a transformation of the last non-empty element:{
# #'
# #' The \code{next.input_monitor} sets the first \code{NaN} element of the input monitor to be a transformation of the last non\code{NaN} element of the same dimension. For example, say the last non-\code{NaN} element of some dimension of the input monitor that monitors the number of responses is \code{10}, after the next response, we would want to set it to \code{11}. The \code{next.input_monitor} allows us to set this without having to know what the last non-\code{NaN} value is or the index that it is at.
# #' }
# #' \subsection{Usage}{
# #'     {\code{next.input_monitor( dimension, value, fun )}}
# #' }
# #' \subsection{Arguments}{
# #'     \describe{
# #'         \item{\code{dimension}}{A character specifying the input dimension. See example.}
# #'         \item{\code{value_list}}{A list containing the value with a name specifying which function argument the entered value should be. If the list element is not named, it is taken as the first argument into \code{fun}.}
# #'         \item{\code{fun}}{A binary function, such as \code{"+"} or \code{"-"}.}
# #'         \item{\code{input_monitor_name}}{An input_monitor name. Defaults to \code{NULL}. If \code{NULL}, the function looks for an input monitor named \code{input_monitor}. Otherwise, searches for an object with the entered name.}
# #'     }
# #' \subsection{Notes}{
# #' For the binary operators (\code{+, -, /, *}), the formal arguments are \code{e1} and \code{e2}. The form of the function is \code{e1 . e2} where \code{.} is the operator.
# #' }
# #' }
#
# next.input_monitor = function( dimension, value_list, fun, input_monitor_name = NULL ){
#     if ( is.null( input_monitor_name ) ){
#         first_NaN_loc = which( is.nan( input_monitor@input[[dimension]] ) )[1]
#         formal_args = names( formals ( args( fun ) ) )
#         arg_list = c( value_list, input_monitor@input[[ dimension ]][ first_NaN_loc - 1 ] )
#         names( arg_list)[ names( arg_list ) == "" ] = formal_args[ ! formal_args %in% names(arg_list) ]
#         input_monitor@input[[ dimension ]][ first_NaN_loc ] <<- do.call( fun, arg_list )
#     }
#
#     if ( !is.null( input_monitor_name ) ){
#         first_NaN_loc = which( is.nan( get(input_monitor_name, envir = globalenv())@input[[dimension]] ) )[1]
#         formal_args = names( formals ( args( fun ) ) )
#         arg_list = c( value_list, get(input_monitor_name, envir = globalenv())@input[[ dimension ]][ first_NaN_loc - 1 ] )
#         names( arg_list)[ names( arg_list ) == "" ] = formal_args[ ! formal_args %in% names(arg_list) ]
#         txt = paste( input_monitor_name, "@input[[dimension]][first_NaN_loc] <<- do.call( fun, arg_list )", sep = "" )
#         eval( parse( text = txt ) )
#     }
#
# }
