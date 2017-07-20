#### Calulating the time elapsed between events

#' @include analysis_object.R dataset.R event_record.R RcppExports.R
NULL

#' Calulating the time elapsed between events in a simulation
#'
#' Given some data from either an experiment or a simulation, the time that elapsed between two kinds of specified events is computed for all instances of the pairing of the two events. The generic description of this elapsed time is the "IxyI" time, read as the "inter-x-y-interval". When x=y, and x = a time at which responses occurred, then the IxyI is an inter-response time (IRT). For another example, when x != y, x = a time at which a reinforcement delivery occurred and y = a time at which a response occured, then the IxyI is a post-reinforcement pause. Functionality is included for skipping over specific events (such as Computing IRTs, while excluding the IRTs with an intervening reinforcement delivery). Further functionality is included for adding an offset to the \code{x_event}, for example, if computing the post-reinforcement pause, it is desirable to subtract out the reinforcement delivery.
#'
#' @param data A \code{analysis_object} or \code{simulation_analysis_object}.
#' @param x_event A character string specifying the "x" in "IxyI".
#' @param x_offset A numeric specifying the duration of the "x". Defaults to 0.
#' @param y_event A character specifying the "y" in "IxyI". Defaults to \code{NULL}, which means the function computes the "IxxI".
#' @param break_event A character vector specifying the break events over which the "IxyI" or "IxxI" cannot be computed over. Defaults to \code{"ALL"} for which only the "IxyI", where "y" must immediate follow "x" is computed. Also takes \code{"NONE"} for which the "IxyI" is calculated for all "y" that follow "x" regardless of the intervening events.
#'
#' @examples
#' #Suppose "d" is your data in an "analysis_object", "resp_time" is the response time, "rft_time" is the reinforcement time
#' #Compute the inter-response times, including those with intervening reinforcement deliveries
#' irts_inc_rft = compute.IxyI( d, x_event = "resp_time" )
#' #Compute the IRTs excluding those with intervening rft deliveries
#' irts = compute.IxyI( d, x_event = "resp_time", break_event = "rft_time" )
#'
#' #Compute post-rft pauses including rft duration
#' prp = compute.IxyI( d, x_event = "rft_time", y_event = "resp_time" )
#' #Compute prp excluding rft duration, if rft duration is 3
#' prp_less_rft = compute.IxyI( d, x_event = "rft_time", y_event = "resp_time", x_offset = -3 )
#'
#' @details
#' With respect to efficiency, the \code{compute.IxyI} method for \code{simulation_analysis_object} is anywhere between twice to five times as fast as the method for \code{analysis_object}. However, to achieve a noticible difference in actual time, the functions would need to be run on the order of 10000 times.
#'
#' In cases where there is only once instance of a particular event, the value returned is a single \code{Inf}. An example is for the calculation of the inter-response times when only once response occurs during a simulation. Another example is is for the calculation of post-reinforcement pauses, but no responses follow a reinforcement delivery.
#'
#' When the \code{compute.IxyI} is called for objects of class \code{dataset.R}, a list of the requested statistic is computed.
#'
#' @seealso
#' \code{\link{class.analysis_object}} For constructing arguments for \code{data} parameter.
#'
#' @rdname compute.IxyI
#' @aliases inter-response_time
#' @aliases irt
#' @aliases post-reinforcement_pause
#' @aliases prp
#' @exportMethod compute.IxyI

setGeneric( "compute.IxyI", function( data, x_event, x_offset = 0 , y_event, break_event ) standardGeneric( "compute.IxyI" ) )

setMethod( "compute.IxyI", signature( data = "formal_event_record", y_event = "missing", break_event = "character" ),
    function( data, x_event, x_offset = 0, break_event ){
        ixxi = compute_ixxi_FER_breaks( data@events, x_event, break_events, x_offset )
        if ( length(ixxi) == 0 ) return( Inf )
        else ixxi
    }
)

setMethod( "compute.IxyI", signature( data = "formal_event_record", y_event = "missing", break_event = "missing" ),
    function( data, x_event, x_offset = 0 ){
        ixxi = compute_ixxi_FER( data@events, x_event, x_offset )
        if ( length(ixxi) == 0 ) return( Inf )
        else ixxi
    }
)

setMethod( "compute.IxyI", signature( data = "formal_event_record", y_event = "character", break_event = "character" ),
    function( data, x_event, x_offset = 0, y_event, break_event ){
        ixyi = compute_ixyi_FER_breaks( data = data@events, x_event = x_event, y_event = y_event, break_events = break_events, x_offset = x_offset )
        if ( length(ixxi) == 0 ) return( Inf )
        else ixxi
    }
)

setMethod( "compute.IxyI", signature( data = "formal_event_record", y_event = "character", break_event = "missing" ),
    function( data, x_event, x_offset = 0, y_event ){
        ixyi = compute_ixyi_FER( data@events, x_event, y_event, x_offset )
        if ( length(ixxi) == 0 ) return( Inf )
        else ixyi
    }
)

#' @rdname compute.IxyI
#' @exportMethod compute.IxyI

#Skips over specified intervening events
compute.I_xx_I.ragged_event_record = function( data, x_event, break_event, x_offset = 0 ){
    break_event_times = unlist( mget( break_event, mode = "numeric" ,envir = data@events ), use.names = F )
    x_event_times = data@events[[x_event]]

    if ( length( x_event_times ) <= 1 ) return( Inf )

    if ( is.nan( break_event_times ) && length( break_event_times ) == 1 ){
        x = CAB_cpp_diff( x_event_times )
    } else{
        break_on_x = findInterval( break_event_times, x_event_times )
        x = CAB_cpp_diff( x_event_times )[ - break_on_x ]
    }

    if ( length(x) == 0 ) return( Inf )

    if ( x_offset != 0 ) x + x_offset
    else x
}

#Does not skip over intervening events
compute.I_xx_I.no_break.ragged_event_record = function( data, x_event, x_offset = 0 ){
    x = CAB_cpp_diff( data@events[[x_event]] )
    if ( length( x ) == 0 ) return( Inf )
    if ( x_offset != 0 ) x + x_offset
    else x
}

#Skips over all intervening events
compute.I_xx_I.all_break.ragged_event_record = function( data, x_event, x_offset = 0){
    x_event_times = data@events[[x_event]]
    if ( length( x_event_times ) <= 1 ) return( Inf )
    var_names = data@variables
    non_x_event_names = var_names[ ! var_names %in% x_event ]
    non_x_events = unlist( mget( non_x_event_names, mode = "numeric", data@events ), use.names = F )
    non_event_on_x = findInterval( non_x_events, x_event_times )

    x = CAB_cpp_diff( x_event_times )[ -non_event_on_x ]
    if ( length( x ) == 0 ) return( Inf )

    if ( x_offset != 0 ) x + x_offset
    else x
}

#Skips over all intervening events
compute.I_xy_I.all_break.ragged_event_record = function( data, x_event, y_event, x_offset = 0 ){
    data = mget( data@variables, envir = data@events, mode = "numeric" )

    at_x_find_next_y = unique( findInterval( data[[ y_event ]], data[[ x_event ]] ) )
    at_x_find_next_y = at_x_find_next_y[ at_x_find_next_y > 0 ]
    if ( length( at_x_find_next_y ) <= 1 ) return(Inf)

    at_y_with_previous_x = unique( findInterval( data[[ x_event ]], data[[ y_event ]] ) )+1
    new_y_times = data[[ y_event ]][ at_y_with_previous_x ]
    if ( NA %in% new_y_times ){
        at_x_find_next_y = at_x_find_next_y[ !is.na( new_y_times ) ]
        new_y_times = new_y_times[ !is.na( new_y_times ) ]
    }

    name = names( data )
    non_event_name_indices = which( !name %in% c(x_event, y_event) )
    remove_intervening = NULL
    if ( length(non_event_name_indices) > 0 ){
        non_event_times = unlist( data[ non_event_name_indices ], use.names = F )
        non_event_after_x = unique( findInterval( data[[ x_event ]], non_event_times ) )
        non_event_new_y = unique( findInterval( non_event_times, new_y_times, all.inside = T ) )
        remove_intervening = which( non_event_times[ non_event_after_x ] < new_y_times[ non_event_new_y ] )
    }

    x = new_y_times - data[[ x_event ]][ at_x_find_next_y ]
    if ( length( remove_intervening ) > 0 ){
        x = x[ -remove_intervening]
    }

    if ( x_offset != 0 ) x+ x_offset
    else x[!is.na(x)]
}

#Does not skip over intervening events
compute.I_xy_I.no_break.ragged_event_record = function( data, x_event, y_event, x_offset = 0 ){
    data = mget( data@variables, envir = data@events, mode = "numeric" )

    for_x_next_y = unique( findInterval( data[[ y_event ]], data[[ x_event ]] ) )
    for_x_next_y = for_x_next_y[ for_x_next_y > 0 ]
    if ( length( for_x_next_y ) <= 1 ) return( Inf )

    for_y_previous_x = findInterval( data[[ x_event ]], data[[ y_event ]] ) + 1
    new_y_times = data[[ y_event ]][ unique( for_y_previous_x ) ]

    x = new_y_times - data[[ x_event ]][ for_x_next_y ]
    if ( NA %in% x ) x = x[ !is.na(x) ]

    if ( x_offset != 0 ) x + x_offset
    else x
}

#Skips over selected intervening events
compute.I_xy_I.ragged_event_record = function( data, x_event, y_event, break_event, x_offset = 0 ){
    data = mget( data@variables, envir = data@events, mode = "numeric" )

    at_x_find_next_y = unique( findInterval( data[[ y_event ]], data[[ x_event ]] ) )
    at_x_find_next_y  = at_x_find_next_y[ at_x_find_next_y > 0 ]
    if ( length( at_x_find_next_y ) <= 1 ) return( Inf )

    at_y_with_previous_x = unique( findInterval( data[[ x_event ]], data[[ y_event ]] ) )+1
    new_y_times = data[[ y_event ]][ at_y_with_previous_x ]
    if ( NA %in% new_y_times ) new_y_times = new_y_times[ !is.na( new_y_times ) ]

    non_event_times = unlist( data[ break_event ], use.names = F )
    non_event_after_x = unique( findInterval( data[[ x_event ]], non_event_times ) )
    non_event_new_y = unique( findInterval( non_event_times, new_y_times, all.inside = T ) )

    remove = which( non_event_times[ non_event_after_x ] < new_y_times[ non_event_new_y ] )

    x = new_y_times - data[[ x_event ]][ at_x_find_next_y ]
    if ( length( remove ) > 0 ){
        x = x[ -remove]
    }
    if ( length(x) < 1 ) return( Inf )

    if ( x_offset != 0 ) x+ x_offset
    else x[!is.na(x)]
}

setMethod( "compute.IxyI", signature( data = "ragged_event_record", y_event = "missing", break_event = "character" ),
    function( data, x_event, x_offset, break_event ){
        if ( break_event == "ALL" ) compute.I_xx_I.all_break.ragged_event_record( data, x_event, x_offset )
        else compute.I_xx_I.ragged_event_record( data, x_event, break_event, x_offset )
    }
)

setMethod( "compute.IxyI", signature( data = "ragged_event_record", y_event = "missing", break_event = "missing" ),
    function( data, x_event, x_offset ){
        compute.I_xx_I.no_break.ragged_event_record( data, x_event, x_offset )
    }
)

setMethod( "compute.IxyI", signature( data = "ragged_event_record", y_event = "character", break_event = "character" ),
    function( data, x_event, x_offset, y_event, break_event ){
        if ( break_event == "ALL" ) compute.I_xy_I.all_break.ragged_event_record( data, x_event, y_event, x_offset )
        else compute.I_xy_I.ragged_event_record( data, x_event, y_event, break_event, x_offset )
    }
)

setMethod( "compute.IxyI", signature( data = "ragged_event_record", y_event = "character", break_event = "missing" ),
    function( data, x_event, x_offset, y_event ){
    compute.I_xy_I.no_break.ragged_event_record( data, x_event, y_event, x_offset )
    }
)

#' #' @rdname compute.IxyI
#' #' @exportMethod compute.IxyI
#'
#' setMethod( "compute.IxyI", signature( data = "dataset" ),
#'     function( data, x_event, x_offset, y_event, break_event ){
#'         expt_data = data@analysis_objects
#'         lapply( expt_data, compute.IxyI, x_event = x_event, x_offset = x_offset, y_event = y_event, break_event = break_event )
#' } )
