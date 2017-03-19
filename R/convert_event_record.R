#### Convert between event records ####

#' @include event_record.R
NULL

#' Convert between \code{ragged_event_record} and \code{formal_event_record}
#'
#' \code{convert_event_record} is for converting objects of \code{ragged_event_record} to \code{formal_event_record} or vise versa.
#'
#' @param event_record An object of class \code{ragged_event_record} or \code{formal_event_record}.
#' @param dim An argument when the \code{event_record} is \code{event_record}. A character vector specifying the variables to retain and the order in which they are retained.
#'
#' If \code{event_record} is a \code{ragged_event_record}, it will be converted to a \code{format_event_record}, and vice versa.
#'
#' The ordering of the \code{dim} arguemnt is very important. Often, events may be recorded at the same time but there exists a causal relation between them. For example, response-contingent reinforcement will result in reinforcement and the response being recorded at the same time. To ensure that an \code{formal_event_record} is generated such that responses are placed before their associated reinforcement deliveries, order the \code{dims} argument as such: \code{dims = c("resp_time", "rft_time" )}.
#'
#' @rdname convert_event_record
#' @seealso \code{\link{class.event_record}}
#'
#' @exportMethod convert_event_record

setGeneric( "convert_event_record", function( event_record, dims = NULL ) standardGeneric( "convert_event_record" ) )

setMethod( "convert_event_record", signature( event_record = "ragged_event_record", dims = "character" ),
    function( event_record, dims ){
        convert_event_record_ragged_to_formal( event_record, dims )
    }
)

setMethod( "convert_event_record", signature( event_record = "formal_event_record", dims = "missing" ),
    function( event_record ){
        dims = event_record@variables
        z = lapply( dims, function(x) event_record@events[ event == x, time ] )
        names( z ) = dims
        lens = lengths(z)
        z$counts = as.list( lens )
        new( "ragged_event_record", events = list2env( z, parent = emptyenv() ), variables = dims, lengths = lens )
    }
)

convert_event_record_ragged_to_formal = function( event_record, dims ){
    times = unlist( lapply( dims, function( x ) event_record@events[[x]] ), use.names = F )
    row_n = unlist( event_record@events$counts[ dims ], use.names = F  )
    events = rep( dims, times = row_n )
    x = data.table::data.table( time = times, event = events )
    data.table::setorder( x, time )
    new( "formal_event_record", events = x, variables = dims, lengths = sum(row_n) )
}
