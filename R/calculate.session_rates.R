#### Compute session rates ####

#' @include analysis_object.R dataset.R event_record.R
NULL

#' Compute session rates
#'
#' \code{compute.session_rates} is a method for calculating the within-session rate of some variable. Currently, methods are defined for the classes \code{analysis_object} and its child class \code{simulation_analysis_object}.
#'
#' @param data Either an object of class \code{simulation_analysis_object}, \code{analysis_object}, or \code{dataset}.
#' @param rft_duration A list with each element being a numeric specifying the reinforcemnt duration and the name of each element corresponding to the reinforcement event.
#' @param dims A character vector of variable names to calculate the rates of. Defaults to \code{NULL}. If \code{NULL}, the rates of all the events in the \code{data} argument are computed.
#' @param session_duration A numeric value specifying the duration of the session over which the rates are calculated. Defaults to \code{NULL}. If \code{NULL}, the latest time associated with an event is taken as the session duration. See details.
#'
#' @details
#' \subsection{\code{session_duration = NULL}}{
#' When \code{data} is of class \code{simulation_analysis_object}, the session duration is taken as the maximum recorded time for the list of variables for which rates are calculated. Note that the variables associated with reinforcement delivery in \code{rft_duration} are automatically included in determining the session duration.
#'
#' When the \code{data} is of class \code{analysis_object}, the session duration is taken as the time associated with the last entry in the \code{event_time} data frame.
#'
#' When the \code{data}is of class \code{dataset}, the \code{compute.session_rates} method for the \code{analysis_object} is just looped over all the \code{analysis_object}s contained in the \code{dataset} object.
#' }
#'
#' \subsection{The computation of the rate}{
#' When the rate is calculated, regardless of the method, the number of times that a particular event occurs is divided by the session time. The session time is corrected by the time that is taken up by reinforcement deliveries. The session time taken up by reinforcement is calculated as \eqn{rft_time * rft_number}. A further correction is done when the last event in the simulation or experiment is a reinforcement delivery, where the session time taken by reinforcement is calculated as \eqn{rft_time * (rft_number-1)}.
#' }
#'
#' \subsection{Computing rates for objects of \code{dataset}}{
#' To calculate the rates for the multiple sessions that are stored in a \code{dataset} object, we essentially call \code{lapply} over all of the \code{analysis_objct}s in the \code{dataset}.
#'}
#'
#' With respect to efficiency, the \code{compute.session_rates} method for \code{simulation_analysis_object} is about 5 times faster than the method for \code{analysis_object}. For standard data analysis, where time is not a big concern, either method is fine. In fact, the method using \code{analysis_object} may be better because it uses the \code{event_time} dataframe in the parent \code{analysis_object} class, so it is functionally more convenient. When doing simulations, time is a concern. Hence, the method with \code{simulation_analysis_object} should be used. Hopefully, this gives the reader some understanding about the design rationale for this method.
#'
#' @seealso
#' \code{\link{class.analysis_object}} For constructing arguments for \code{data} parameter.
#'
#' @rdname compute.session_rates
#' @exportMethod compute.session_rates

setGeneric( "compute.session_rates", function( data, event_offsets, dims = NULL, session_duration = NULL ) standardGeneric( "compute.session_rates" ) )

ragged_event_record.session_rate_helper = function( ragged_event_record, event_offset, session_duration, dims ){
    offset_times = vapply( names( event_offset ), function(x){
        length( ragged_event_record@events[[ x ]] < session_duration ) * event_offset[[ x ]]
    }, FUN.VALUE = 1 )

    session_duration_post_offsets = session_duration + sum(offset_times)

    lengths( mget( dims, ragged_event_record@events ) )/session_duration_post_offsets
}

formal_event_record.session_rate_helper = function( formal_event_record, event_offset, session_duration, dims ){
    event = formal_event_record@events$event
    time = formal_event_record@events$time

    counts = vapply( unique( c(dims, names(event_offset) ) ), function(x) sum(event==x) ,FUN.VALUE = 1 )

    offset_at_end = names( event_offset )[which( names( event_offset ) %in% utils::tail( event, 1 ) )]
    offset_counts = counts[ names(counts) %in% names( event_offset ) ]

    if ( length( offset_at_end ) == 1 ){
        offset_counts[ offset_at_end ] =offset_counts[ offset_at_end ] - 1
    }

    offset_time = vapply( names(event_offset), function(x){
        event_offset[[ x ]] * offset_counts[ x ]
    }, FUN.VALUE = 1 )

    adjusted_session_duration = session_duration + offset_time

    counts[dims] / adjusted_session_duration
}

#' @rdname compute.session_rates
#' @exportMethod compute.session_rates

setMethod( "compute.session_rates", signature( data = "formal_event_record", event_offsets = "list", dims = "character", session_duration = "numeric" ),
    function( data, event_offsets, dims, session_duration ){
        formal_event_record.session_rate_helper( data, event_offsets, session_duration, dims )
    }
)

#' @rdname compute.session_rates
#' @exportMethod compute.session_rates

setMethod( "compute.session_rates", signature( data = "formal_event_record", event_offsets = "list", dims = "missing", session_duration = "numeric" ),
    function( data, event_offsets, session_duration ){
        dims = data@variables
        formal_event_record.session_rate_helper( data, event_offsets, session_duration, dims )
    }
)

#' @rdname compute.session_rates
#' @exportMethod compute.session_rates

setMethod( "compute.session_rates", signature( data = "formal_event_record", event_offsets = "list", dims = "missing", session_duration = "missing" ),
    function( data, event_offsets ){
        dims = data@variables
        session_duration = max( data@events$time )
        formal_event_record.session_rate_helper( data, event_offsets, session_duration, dims )

    }
)

#' @rdname compute.session_rates
#' @exportMethod compute.session_rates

setMethod( "compute.session_rates", signature( data = "formal_event_record", event_offsets = "list", dims = "character", session_duration = "missing" ),
    function( data, event_offsets, dims, session_duration ){
        session_duration = max( formal@events$time )
        formal_event_record.session_rate_helper( data, event_offsets, session_duration, dims )
    }
)

#' @rdname compute.session_rates
#' @exportMethod compute.session_rates

setMethod( "compute.session_rates", signature( data = "ragged_event_record", event_offsets = "list", dims = "character", session_duration = "numeric" ),
    function( data, event_offsets, dims, session_duration ){
        ragged_event_record.session_rate_helper( data, event_offsets, session_duration, dims )
    }
)

#' @rdname compute.session_rates
#' @exportMethod compute.session_rates

setMethod( "compute.session_rates", signature( data = "ragged_event_record", event_offsets = "list", dims = "missing", session_duration = "numeric" ),
    function( data, event_offsets, session_duration ){
        dims = data@variables
        ragged_event_record.session_rate_helper( data, event_offsets, session_duration, dims )
    }
)

#' @rdname compute.session_rates
#' @exportMethod compute.session_rates

setMethod( "compute.session_rates", signature( data = "ragged_event_record", event_offsets = "list", dims = "character", session_duration = "missing" ),
    function( data, event_offsets, dims ){
        session_duration = max( vapply( data@variables,
            function( x ){
                utils::tail( data@events[[x]], 1 )
            }, FUN.VALUE = 1 ) )
        ragged_event_record.session_rate_helper( data, event_offsets, session_duration, dims )
    }
)

#' @rdname compute.session_rates
#' @exportMethod compute.session_rates

setMethod( "compute.session_rates", signature( data = "ragged_event_record", event_offsets = "list", dims = "missing", session_duration = "missing" ),
    function( data, event_offsets ){
        session_duration = max( vapply( data@variables,
            function( x ){
                utils::tail( data@events[[x]], 1 )
            }, FUN.VALUE = 1 ) )
        dims = data@variables
        ragged_event_record.session_rate_helper( data, event_offsets, session_duration, dims )
    }
)
