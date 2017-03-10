# ### Simulation input ####
#
# #' @importFrom data.table rbindlist
#
# #' @include input.R elemental_get_helpers.R
# NULL
#
# #' Simulation input class
# #'
# #' Objects of the \code{sim_input} class store objects of the \code{input} class (see \code{\link{class.input}}. The use of the \code{sim_input} class is to put all of the necessary \code{input} objects together for a simulation. During the simulation, the \code{sim_input} object will contain the values of the inputs at any given time.
# #'
# #' \code{make.sim_input} for making a \code{sim_input} object.
# #'
# #' There is a \code{show} method for the \code{sim_input} class. This method can be removed with the \code{remove.sim_inout.show} function and reinstated with the \code{sim_input.show}. This show method does not do anything different to if the object were just printed normally.
# #'
# #' In a simulation, \code{sim_input} is used to monitor the value of the inputs, which serve as arguments for the functions that are necessary to operate a given model. See the section below. See \code{\link{input_monitor}}.
# #'
# #' @slot input This is a list of the input objects that are required for a simulation.
# #'
# #' @rdname class.sim_input
# #'
#
# class.sim_input = setClass( "sim_input", slots = list( input = "list" ) )
#
# #### Make a simulation input object ####
#
# #' @rdname class.sim_input
# #'
# #' @section Make a simulation input object:{
# #' To create an object of \code{sim_input}, use the function \code{make.sim_input}.
# #' }
# #' @param ... The names of the desired \code{input} objects, separated by commas.
# #'
# #' @examples
# #' # Make some input objects
# #' test_input1 = make.custom_input( "test_input1" )
# #' test_input2 = make.custom_input( "test_input2" )
# #' # Make a sim_input object
# #' make.sim_input( test_input1, test_input2 )
# #'
# #' @seealso
# #' \code{\link{make.custom_input}} to make \code{input} objects.
# #'
# #' \code{\link{input_monitor}} for when \code{sim_input} is used in a simulation.
# #'
# #' @export make.sim_input
#
# make.sim_input = function( ... ){
#     dot_args = list( ... )
#     input_checker( dot_args )
#     check = sapply( dot_args, function(x) is(x, "input") )
#     names = sapply( dot_args, e_get.name )
#     if ( !any( check ) ){
#         message = paste( names[!check], collapse = ", " )
#         stop( paste( "The arguments:", message, "are not of class 'input'" ) )
#     }
#     x = lapply( dot_args, function( x ) slot(x, "input") )
#     names(x) = names
#     new( "sim_input", input = x )
# }
#
# input_checker = function( dot_args ){
#     x = sapply( dot_args, class )
#     if ( any( x != "input" ) ) stop( "Only enter objects of type 'input'" )
# }
#
# #' @rdname class.sim_input
# #' @export sim_input.show
# #' @format The \code{show} method prints the \code{sim_input} object as a \code{data frame}, which is what it would print out as without the \code{show} method.
# sim_input.show = setMethod( "show", signature( object = "sim_input" ), function( object ) {
#     print( object@input )
# } )
#
# # #' @rdname class.sim_input
# # #' @export remove.sim_input.show
# # remove.sim_input.show = function() removeMethod( "show", signature( object = "sim_input" ) )
# #
#
#
