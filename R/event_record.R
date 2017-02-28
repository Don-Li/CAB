#### Event record ####

#' \code{event_record} class
#'
#' When a simulation is run in the \code{CAB} package, events, such as the time of a response, can be stored in a \code{event_record} object.
#'
#' The construction of an \code{event_record} object requires the specification of the names of the events to be recorded. An \code{event_record} object has two slots.
#'
#' The first slot, called \code{events} is an environment that contains two lists. The first list is a list of vectors, where each vector contains the times of the associated events. For example, if we wished to record the response times and the reinforcement times, this could be a list containing a vector named "response_times" and a vector named "reinforcement_times". The second list is a list of counts for how many of each type of recorded event has occurred. It follows then instead of the typical time-event format for data, we have the times of each event with different vectors for each event.
#'
#' The second slot, \code{variables} is a vector that contains the names of the variables to be recorded. This is just for easy access.
#'
#' @section The \code{event_record} class:{
#'     The \code{event_record} can be used for recording events that occur during a simulation.
#'     \subsection{Slots}{
#'         \describe{
#'             \item{\code{events}}{An environment that contains two lists. One is a nested list and contains vectors to record the times of each specified type of event, and the other records the counts for these events.}
#'             \item{\code{variables}}{A character vector containing the types of events to be recorded.}
#'         }
#'     }
#' }
#'
#' @section \code{make.event_record}:{
#'     For making a \code{event_record} object.
#'     \subsection{Usage}{
#'         \code{make.event_record( variables, len ) }
#'     }
#'     \subsection{Arguments}{
#'         \describe{
#'             \item{\code{variables}}{A character vector containing the types of events to be recorded.}
#'             \item{\code{len}}{A numeric vector containing the length of the vectors for storing each variable type. \code{len} is recycled if the length of \code{len} is smaller than the number of variables specified.}
#'         }
#'     \subsection{Value}{
#'         Returns a \code{event_record} object.
#'     }
#'     }
#' }
#'
#' @section \code{assign_event}:{
#'     For changing events and counts in an \code{event_record}.
#'     \subsection{Usage}{
#'         \code{assign_event( event_record, variable, index, values, counts )}
#'     }
#'     \subsection{Arguments}{
#'         \describe{
#'             \item{\code{event_record}}{A object of class \code{event_record}.}
#'             \item{\code{variable}}{A character string specifying the variable to change.}
#'             \item{\code{index}}{A numeric vector of indices for the events to change for the specified variable. May also be "next" for which events at indices after the index specified by the counts are changed. Defaults to \code{NULL} and no values are changed. See example.}
#'             \item{\code{values}}{Values of the new events. Defaults to \code{NULL} and no values are changed.}
#'             \item{\code{counts}}{New counts for the variable events being changed.}
#'         }
#'     \subsection{Value}{
#'         Modifies the \code{event_record} object in place.
#'     }
#'     }
#' }
#'
#' @section \code{get_event}:{
#'     Extract event values.
#'     \subsection{Usage}{
#'         \code{get_event{ event_record, variable, index, counts }}
#'     }
#'     \subsection{Arguments}{
#'         \describe{
#'             \item{\code{event_record}}{An object of class \code{event_record}.}
#'             \item{\code{variable}}{A character string specifying the variable to change.}
#'             \item{\code{index}}{A numeric vector of indices. Defaults to \code{NULL} for which no events are returned.}
#'             \item{\code{counts}}{A logical indicating whether or not to return the counts for the variable. Defaults to \code{FALSE}}
#'         }
#'     }
#'     \subsection{Value}{
#'         Returns the counts for the specified variable if \code{counts} is TRUE. Returns the event values if \code{counts} is \code{FALSE} and \code{index} is not \code{NULL}
#'     }
#'
#' }
#'
#'
#' @section \code{reset_event}:{
#'     Reset all variables in an \code{event_record}.
#'     \subsection{Usage}{
#'         \code{reset_event{ event_record } }
#'     }
#'     \subsection{Arguments}{
#'         \describe{
#'             \item{\code{event_record}}{A object of class \code{event_record}.}
#'         }
#'     \subsection{Value}{
#'         Modifies the \code{event_record} object in place. All values for each variable are reset to \code{NaN}.
#'     }
#'     }
#' }
#'
#'
#' @rdname class.event_record
#' @aliases event_record
#'
#' @export make.event_record
#' @exportMethod show
#' @exportMethod assign_event
#'
#'
#'
#' @examples
#' # Create an "event_record" for storing the variables: "resp_time" and "rft_time"
#' # Record a maximum of 100 of each type of event
#' my_record = make.event_record( variables = c( "resp_time", "rft_time" ), len = 100 )
#' my_record
#'
#' # Imagine an organism that emits responses every 1 time unit. Suppose that the experiment ran for 90 seconds
#' # The "resp_time" in "my_record" should contain 1:90, so there should be 90 counts
#' assign_event( event_record = my_record, variable = "resp_time", index = 1:90, values = 1:90, counts = 90 )
#' my_record
#'
#' # Suppose the organism took a break for 2 time units and then made 3 responses. The next response times should then be 93:95.
#' assign_event( event_record = my_record, variable = "resp_time", index = "next", values = 93:95 )
#' my_record
#'
#' # Get the value of an event at a given index:
#' get_event( event_record = my_record, variable = "resp_time", index = 1 )
#' get_event( event_record = my_record, variable = "resp_time", index = 1:10 )
#' # get the counts for a variable:
#' get_event( event_record = my_record, variable = "resp_time", counts = T )
#'
#' # Reset "my_record"
#' reset_event( my_record )


class.event_record = setClass( "event_record", slots = list( events = "environment", variables = "character" ) )

make.event_record = function( variables, len ){
    if ( length(len) > length(variables ) ) stop( "the length of 'len' is longer than the number of variables" )
    dummy_list = mapply( function(x,y) rep(NaN, x), y = variables, x = len, SIMPLIFY = F )
    dummy_list$counts = as.list( rep(0, length(variables) ) )
    names( dummy_list$counts ) = variables
    new( "event_record", events = list2env( dummy_list, parent = emptyenv() ), variables = variables )
}

setMethod( "show", signature( object = "event_record" ),
    function( object ){
        variables = object@variables
        cat( "Event record: \n" )
        lapply( variables, event_record_show_helper, object = object )
    } )

event_record_show_helper = function(x, object ){
    cat( x, ":", object@events$counts[[x]] ,"\n" )
    print( object@events[[x ]] )
    cat( "\n" )
}

setGeneric( "assign_event", function( event_record, variable, index = NULL, values = NULL, counts = NULL ) standardGeneric( "assign_event" ) )

setMethod( "assign_event", signature( event_record = "event_record" , variable = "character" ),
    function( event_record, variable, index, values, counts ){
        if ( length( variable ) > 1 ) stop( "Only change one variable at a time" )
        assign_event_helper( event_record, variable, index, values, counts )
    } )


assign_event_helper = function( event_record, variable, index, values = NULL,counts = NULL ){
    if ( !is.null( values ) ){
        event_record@events[[ variable ]][index] <- values
    }
    if ( !is.null( counts ) ){
        event_record@events$counts[[variable]] <- counts
    }
}

setMethod( "assign_event", signature( event_record = "event_record", variable = "character", index = "character" ),
    function( event_record, variable, index, values, counts ){
        if ( index != "next" ) stop( "if 'index' is character, it must be 'next'" )
        next_event_helper( event_record, variable, values )
    } )

next_event_helper = function( event_record, variable, values ){
    len_values = length(values)
    count = event_record@events$counts[[ variable ]]
    event_record@events[[ variable ]][(count+1):(count+len_values)] <- values
    event_record@events$counts[[ variable ]] <- count + len_values
}

setGeneric( "reset_event", function( event_record ) standardGeneric( "reset_event" ) )

setMethod( "reset_event", signature( event_record = "event_record" ),
    function( event_record ){
        reset_event_helper( event_record )
        invisible()
    }
)

reset_event_helper = function( event_record ){
    variables = event_record@variables
    dims = length( event_record@events[[ variables[1] ]] )
    lapply( variables, function(x){
        event_record@events[[x]] = rep( NaN, dims )
        event_record@events$counts[x] = 0
    } )
}

setGeneric( "get_event", function( event_record, variable, index = NULL, counts = F ) standardGeneric( "get_event" ) )

setMethod( "get_event", signature( event_record = "event_record" ),
    function( event_record, variable, index, counts ){
        get_event_helper( event_record, variable, index, counts )
    }
)

get_event_helper = function( event_record, variable, index, counts ){
    if ( !counts ){
        return( event_record@events[[ variable ]][index] )
    }
    if ( counts ){
        return( event_record@events$counts[[ variable ]] )
    }
}



