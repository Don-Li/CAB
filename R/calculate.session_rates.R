#### Compute session rates ####

#' @include analysis_object.R dataset.R
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
#' With respect to efficiency, the \code{compute.session_rates} method for \code{simulation_analysis_object} is about 5 times faster than the method for \code{analysis_object}. For standard data analysis, where time is not a big concern, either method is fine. In fact, the method using \code{analysis_object} may be better because it uses the \code{event_time} dataframe in the parent \code{analysis_object} class, so it is functionally more convenient. When doing simulations, time is a concern. Hence, the method with \code{simulation_analysis_object} should be used. Hopefully, this gives the reader some understanding about the design rationale for this method.
#'
#' \subsection{Computing rates for objects of \code{dataset}}{
#' To calculate the rates for the multiple sessions that are stored in a \code{dataset} object, we essentially call \code{lapply} over all of the \code{analysis_objct}s in the \code{dataset}.
#'}
#' @seealso
#' \code{\link{class.analysis_object}} For constructing arguments for \code{data} parameter.
#'
#' @rdname compute.session_rates
#'
#' @exportMethod compute.session_rates

setGeneric( "compute.session_rates", function( data, rft_duration, dims = NULL, session_duration = NULL ) standardGeneric( "compute.session_rates" ) )

event_time_compute.session_rate = function( event_time_data, rft_duration, session_duration = NULL, dims = NULL){
    counts = table( event_time_data$event )
    nrows = nrow( event_time_data )
    rft_time = drop( ( sum( counts[ names(rft_duration) ] ) ) %*% unlist( rft_duration, use.names = F ) )
    if ( event_time_data[nrows,]$event == names( rft_duration ) ){
        rft_time = rft_time - unlist( rft_duration, use.names = F )
    }
    if ( is.null(session_duration) ){
        session_duration = event_time_data[nrows,]$time
    }
    session_time = session_duration - rft_time

    if ( is.null( dims ) ) dims = names( counts )
    x = as.numeric( (counts / session_time)[dims] )
    names( x ) = dims
    x
}

analysis_object_compute.session_rate = function( simulation_analysis_object, rft_duration, session_duration = NULL, dims = NULL ){
    if ( is.null( dims ) ) dims = names( simulation_analysis_object )
    if ( is.null(session_duration) ){
        search_dims = unique( c( names( rft_duration ), dims ) )
        session_duration = max( vapply( simulation_analysis_object[ search_dims ] , max, 1 ) )
    }
    rfts = vapply( simulation_analysis_object[ names(rft_duration) ], function( x ) length(x < session_duration ), 1 )
    rft_time = drop( rfts %*% unlist( rft_duration, use.names = F ) )
    session_duration = session_duration - rft_time
    lengths( simulation_analysis_object[ dims ] ) / session_duration
}

#' @rdname compute.session_rates
#' @exportMethod compute.session_rates

setMethod( "compute.session_rates", signature( data = "analysis_object", rft_duration = "list" ),
    function( data, rft_duration, dims, session_duration){
        if ( !is.null(session_duration) && !is.numeric(session_duration) ) stop( "'session_duration' must be numeric" )
        if ( !is.null(dims) && !is.character(dims ) ) stop("'dimension' must be a character vector")
        event_time_compute.session_rate( data@analysis_object, rft_duration, session_duration, dims )
    }
)

#' @rdname compute.session_rates
#' @exportMethod compute.session_rates

setMethod( "compute.session_rates", signature( data = "simulation_analysis_object", rft_duration = "list" ),
    function( data, rft_duration, dims, session_duration){
        if ( !is.null(session_duration) && !is.numeric(session_duration) ) stop( "'session_duration' must be numeric" )
        if ( !is.null(dims) && !is.character(dims ) ) stop("'dimension' must be a character vector")
        analysis_object_compute.session_rate( data@input_list, rft_duration, session_duration, dims )
    }
)

#' @rdname compute.session_rates
#' @exportMethod compute.session_rates

setMethod( "compute.session_rates", signature( data = "dataset", rft_duration = "list" ),
    function( data, rft_duration, dims, session_duration ){
        if ( !is.null(session_duration) && !is.numeric(session_duration) ) stop( "'session_duration' must be numeric" )
        if ( !is.null(dims) && !is.character(dims ) ) stop("'dimension' must be a character vector")
        expt_data = data@analysis_objects
        rates = lapply( expt_data, compute.session_rates, rft_duration = rft_duration, dims = dims, session_duration = session_duration )
        dim_names = names( rates[[1]] )
        rates = data.table::transpose( rates )
        attr( rates, "class" ) = "data.frame"
        attr( rates, "row.names" ) = 1:length(rates[[1]])
        attr( rates, "names" ) = dim_names
        rates
    }
)

