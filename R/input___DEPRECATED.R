# #### Simulation inputs ####
#
# #' Input class
# #'
# #' Objects of the \code{input} class are for specifying simulation inputs. This includes \code{input} that go into the computational model as well as \code{input}s into the experimental protocol. If one wishes to make an \code{input} object, the \code{make.custom_input} function should be used.
# #'
# #' The use of the \code{input} class is to define \code{input} objects, which get collected together into a \code{\link{sim_input}} class. The \code{sim_input} contains all of the inputs that are needed to run a given simulation.
# #'
# #' The \code{make.input} function is for creating custom \code{input} objects.
# #'
# #' The \code{show} method for the \code{input} class can be removed with the function \code{remove.input.show()} and reinstated with \code{input.show}.
# #'
# #' @slot input This is a \code{data.frame} object. It does not matter what you put in here because the value of an input should always be initialised before each simulation.
# #' @slot name This is the name of the \code{input} object. This name will be used as arguments for other functions, such as those in the computational model or in the experimental protocol.
# #'
# #' @rdname class.input
#
# class.input = setClass( "input", slots = list( input = "numeric", name = "character" ) )
#
# #### Make custom input object ####
#
# #' @rdname class.input
# #'
# #' @section Make an input object:{
# #' To create an input, use the \code{make.input} function.
# #' }
# #' @param  name A character vector that is the name of the desired input. This should name should not be shared with any other existing input.
# #' @examples
# #' # Make a new input object named reserve_1
# #' reserve_1 = make.input( name = "reserve_1" )
# #'
# #' @seealso \code{\link{class_catalogue}} for a function to check whether \code{input} names are being used.
# #'
# #' \code{\link{make.sim_input}} for the function that combines objects of \code{input} into \code{\link{sim_input}}
# #'
# #' @export make.input
#
# make.input = function( name ){
#     if ( !is.character( name ) ) stop( "Enter input name as character." )
#     new( "input", input = NaN, name = name )
# }
#
# #### input show methods
#
# #' @rdname class.input
# #' @format The \code{show} method prints the name of the \code{input} object.
# #' @exportMethod input.show
#
# input.show = setMethod( "show", signature( object = "input" ), function( object ) {
#     message = "Input object: "
#     cat( message, object@name, sep = "" )
# } )
#
# #' @rdname class.input
# #' @export remove.input.show
#
# remove.input.show = function() removeMethod( "show", signature( object = "input" ) )
