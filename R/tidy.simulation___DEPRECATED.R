# #### tidy.simulation_data ####
#
# #' Tidy up the data from a simulation
# #'
# #' This function combines \code{\link{trim.input}}, \code{\link{format.time_event}}, and \code{\link{make.simulation_analysis_object}}.
# #'
# #' @usage tidy.simulation_data( input_monitor, dims, condition_info )
# #' @param input_object An input object from an experiment. See \code{\link{class.input}}.
# #' @param dims.time_event A character vector specifying the dimensions of \code{input} to keep. Defaults to \code{NULL} which means that everything is kept. See \code{\link{format.time_event}} for more information.
# #' @param condition_info From the \code{conditions} slot in the model object. See \code{\link{class.model}}.
# #' @param trim_excess_time A numeric value. Events and responses that occur later than some specified time are also trimmed. Defaults to \code{NULL} which means that only the \code{NaN} valus are trimmed.
# #' @param dims.input_monitor A character vector specifying the dimensions of the \code{input} object to keep. Defaults to \code{NULL} which means that all dimensions are dept.
# #'
# #' In broad terms, the \code{tidy.simulation_data} takes an object of \code{sim_input}, a character vector specifying the dimensions of the \code{sim_input}, and a list of the condition information. The function then returns the object described.
# #'
# #' The sub-functions \code{format.time_event} and \code{trim.input_monitor} have their own \code{dim} arguments. We have differentiated them here with suffixes.
# #'
# #' @seealso \code{\link{trim.input_monitor}}
# #' @seealso \code{\link{format.time_event}}
# #' @seealso \code{\link{make.simulation_analysis_object}}
# #'
# #' @rdname tidy.simulation_data
# #' @exportMethod tidy.simulation_data
#
# setGeneric( "tidy.simulation_data", function( input_object, condition_info, dims.time_event, trim_excess_time = NULL, dims.input_monitor = NULL ) standardGeneric( "tidy.simulation_data" ) )
#
# setMethod( "tidy.simulation_data", signature( input_object = "input", condition_info = "list" ),
#     function( input_object, condition_info, dims.time_event, trim_excess_time , dims.input_monitor){
#         trim.input( input_object, trim_excess_time, dims.input_monitor )
#         time_event_data = format.time_event( trimmed, dims.time_event )
#         make.simulation_analysis_object( data = time_event_data, meta_data = condition_info, input = trimmed@input )
#     }
# )
#
#
# ##########Change the arguments for this. Filter the variables at dims.time_event instead of both.
