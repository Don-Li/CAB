#### Copy an event record ####

#' @include event_record.R RcppExports.R
NULL

#' Copy a \code{event_record}
#'
#' A \code{event} record is typically implemented as an \code{environment} type class. This means that they have pass-by-reference semantics. As a result, assignment will not produce a copy of the object. Therefore, changing the object from the assigned value will change the original version.
#'
#' @usage copy_event_record( event_record )
#'
#' @param event_record Either a \code{formal_event_record} or \code{ragged_event_record}.
#'
#' @exportMethod copy_event_record
#' @rdname copy_event_record

setGeneric( "copy_event_record", function( event_record ) standardGeneric( "copy_event_record" ) )

setMethod( "copy_event_record", signature( event_record = "formal_event_record" ),
    function( event_record ){
        copy_formal_event_record( event_record )
    }
)

setMethod( "copy_event_record", signature( event_record = "ragged_event_record" ),
    function( event_record ){
        copy_ragged_event_record( event_record )
    }
)

copy_ragged_event_record = function( event_record ){
    record = event_record@events
    variables = event_record@variables
    lengths = event_record@lengths
    methods::new( "ragged_event_record", events = list2env( as.list( record, parent = emptyenv() ) ), variables = variables, lengths = lengths )
}

copy_formal_event_record = function( event_record ){
    variables = event_record@variables
    lengths = event_record@lengths
    methods::new( "formal_event_record", events = data.table::copy( event_record@events ), variables = variables, lengths = lengths )
}
