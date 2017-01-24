#### Simulation data format conversion ####

#' Convert the data format created by a simulation to time-event format
#'
#' In a simulation, the typical data format (the default from a \code{input} object) will be an environment that contains vectors of times at which events occurred, with separate lists for each different type of event (see \code{\link{class.input}}). The generic data format is "time-event", where the data is an \eqn{n} by 2 matrix, where \eqn{n} is the number of events that occurred during the experiment. The first column  gives the time at which an event occurred and the second column gives the event that occurred. Outside of a simulation, the "time-event" format is preferred. Hence, we have functionality to convert from the "event-list" format to the "time-event" format.
#'
#' @exportMethod format.time_event
#' @rdname format.time_event

setGeneric( "format.time_event", function( data, dims ) standardGeneric( "format.time_event" ) )

#' @rdname format.time_event
#' @param data A \code{input} object.
#' @param dims A character vector specifying the dimensions of \code{input} to put in the time-event data. Defaults to \code{NULL} which means that everything is kept. See notes.
#'
#' @note The ordering of the elements in \code{dims} is very important. In many cases, events will occur at the same time. For example, if reinforcement is contingent on responding, then the time associated with reinforcement will be recorded as the same time as the response.
#'
#' The implementation of the \code{format.time_event} function is that an \eqn{n} by 2 matrix is constructed, where \eqn{n} is computed by summing over the counts for the events specified by \code{dim}. Then, going through each type of event, as they are listed in \code{dims}, the "time" vector is filled in by copying the times in the associated \code{input} for the given variable. This is repeated for all event types in \code{dims}. The \eqn{n} by 2 matrix is then ordered by the time at which events occurred. Ties are resolved by the original ordering of the events. Hence, the order of events in the \code{dims} argument is important for getting the contingency of events correct. For example, if reinforcement is contingent on responding, then \code{dims = c("resp_time", "rft_time") )} is the required order so that responses come before their associated reinforcement delivery.
#'
#' @seealso
#' \code{\link{tidy.simulation}} which uses \code{format.time_event}.
#'
#' @exportMethod format.time_event

setMethod( "format.time_event", signature( data = "input" ),
    function( data, dims = NULL ){
        if ( is.null( dims ) ) dims = names( data@names )
        format.time_event_helper( data, dims )
    }
)

format.time_event_helper = function( input_object, dims ){
    temp_data = list( time = NaN, event = NaN )
    temp_data$time = unlist( lapply( dims, function(x) data[[x]]$data ), use.names = F )
    temp_data$event = rep( dims, times = length(temp_data$time) )
    attr( temp_data, "class" ) = "data.frame"
    rownames( temp_data )  = 1:length( temp_data$time )
    temp_data[ order( temp_data$time), ]
}
