#### Calculate local rates ####

#' @include analysis_object.R dataset.R
NULL

#' Compute local rates
#'
#' \code{compute.local_rates} is a method for calculating the within-session local rate of some variable. Currently, methods are defined for the classes \code{analysis_object} and its child class \code{simulation_analysis_object}.
#'
#' @param data Either an object of class \code{simulation_analysis_object}, \code{analysis_object}, or \code{dataset}.
#' @param x_event A variable with which to calculate the local rates over.
#' @param marker A variable with which to calculate the local rates within.
#' @param x_offset The duration of the \code{x_event}. Defaults to 0.
#' @param marker_offset The duration of the \code{marker_offset}. Defaults to 0.
#'
#' @details
#' \subsection{\code{x_offset} and \code{marker_offset}}{
#' If the \code{x_event} or the \code{marker} event have non-zero durations, the \code{x_offset} and \code{marker_offset} allow for the correction of the \code{x_event}s in each bin. For example, if the local response rate is defined by the reinforcement deliveries and the reinforcement delivery lasts 3 seconds, set \code{marker_offset = -3} to subtract out the reinforcement time.
#' }
#' \subsection{Exceptions}{
#' In situations where there is only one \code{marker} event, \code{compute.local_rates} returns a single \code{Inf}. This would be the case if the user were to calculate the inter-reinforcement response times, but only one reinforcement delivery occurred during a particular simulation.
#' }
#'
#' \subsection{Method for the \code{dataset} class}{
#' When \code{compute.local_rates} is called on a \code{dataset} class, it calls the appropriate \code{compute.local_rates} for each element in the \code{analysis_objects} slot
#' }
#'
#' @examples
#' #Suppose "d" is your data in an "analysis_object", "resp_time" is the response time, "rft_time" is the reinforcement time, reinforcement is delivered for 3 units of time and the operandum takes 1 unit of time to operate.
#' #Compute the local response rate accounting for the time to make a response and the time taken up by reinforcment
#' local_resp_rate = compute.local_rates( d, x_event = "resp_time", marker = "rft_time", x_event_offset = -1, marker_offset = -3 )
#'
#' @seealso
#' \code{\link{class.analysis_object}} For constructing arguments for \code{data} parameter.
#' @rdname compute.local_rates
#' @aliases local_rates
#' @exportMethod compute.local_rates


setGeneric( "compute.local_rates", function( data, x_event, marker, x_offset = 0, marker_offset = 0 ) standardGeneric( "compute.local_rates" ) )

setMethod( "compute.local_rates", signature( data = "analysis_object" ),
    function( data, x_event, marker, x_offset, marker_offset ){
        local_rate_helper( data@analysis_object$time[ data$event %in% marker ], data@analysis_object$time[ data$event %in% x_event ], x_offset, marker_offset )
    }
)

#' @rdname compute.local_rates
#' @aliases local_rates
#' @exportMethod compute.local_rates

setMethod( "compute.local_rates", signature( data = "simulation_analysis_object" ),
    function( data, x_event, marker, x_offset, marker_offset ){
        local_rate_helper( data@input_list[[ marker ]], data@input_list[[ x_event ]], x_offset, marker_offset )
    }
)

local_rate_helper = function( marker_times, event_times, x_offset, marker_offset ){
    event_between_marker = findInterval( event_times, marker_times, left.open = T )
    marker_ends = !event_between_marker %in% c(0, length(marker_times))
    event_times = event_times[ marker_ends ]
    event_between_marker = event_between_marker[ marker_ends ]

    iri_assignment = rle( event_between_marker )
    start_times = rep( marker_times[ -length(marker_times)], times = iri_assignment$lengths )

    reset_event_times = event_times - start_times
    if ( length(reset_event_times) == 0 ) return( Inf )
    if ( ! marker_offset %in% 0 ) reset_event_times = reset_event_times + marker_offset
    if ( ! x_offset %in% 0 ){
        x_durations = duplicated( event_between_marker ) * x_offset
        reset_event_times = reset_event_times + x_durations
    }
    reset_event_times
}

#' @rdname compute.local_rates
#' @aliases local_rates
#' @exportMethod compute.local_rates

setMethod( "compute.local_rates", signature( data = "analysis_object" ),
    function( data, x_event, marker, x_offset, marker_offset ){
        data = data@analysis_object
        event_times = data[ data[,"event"] %in% x_event, "time" ]
        marker_times = data[ data[,"event"] %in% marker, "time" ]
        local_rate_helper( marker_times = marker_times, event_times = event_times, x_offset = x_offset, marker_offset = marker_offset )
    } )

#' @rdname compute.local_rates
#' @aliases local_rates
#' @exportMethod compute.local_rates

setMethod( "compute.local_rates", signature( data = "dataset" ),
    function( data, x_event, marker, x_offset, marker_offset ){
        expt_data = data@analysis_objects
        lapply( expt_data, compute.local_rates, x_event = x_event, marker = marker, x_offset = x_offset, marker_offset = marker_offset )
    }
)

