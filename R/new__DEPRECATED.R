# #### Simulation inputs ####
#
# #' Input class
# #'
# #' Objects of the \code{input} class are for specifying simulation inputs. This includes \code{input} that go into the computational model as well as \code{input}s into the experimental protocol. If one wishes to make an \code{input} object, the \code{make.custom_input} function should be used.
# #'
# #' @slot input This is a numeric value. It does not matter what you put in here because the value of an input should always be initialised before each simulation.
# #' @slot name This is the name of the \code{input} object. This name will be used as arguments for other functions, such as those in the computational model or in the experimental protocol.
# #'
# #' @rdname class.input
#
# df = setClass( "input", slots = list( input = "numeric", name = "character" ) )
#
# #### Make custom input object ####
#
# #' @rdname class.input
# #' @aliases make.expt_input
# #'
# #' @section Make a custom input object:{
# #' To create a custom input, use the \code{make.custom_input} function.
# #' }
# #' \subsection{1}{
# #' 2
# #' }
# #' @usage make.custom_input( name )
# #' @param  name A character vector that is the name of the desired input. This should name should not be shared with any other existing input.
# #'
# #'
# #' @export make.custom_input
#
# osdf = function( name ){
#     if ( !is.character( name ) ) stop( "Enter input name as character." )
#     new( "input", input = NaN, name = name )
# }


