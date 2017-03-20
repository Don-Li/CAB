#### tidy data ####

#' @include event_record.R RcppExports.R
NULL

#' Clean your data
#'
#' A method for cleaning short inter-x-y-intervals.
#'
#' @param data An event record. At the moment, methods are defined only for "formal_event_record".
#' @param x_event A character string specifying the first event type.
#' @param y_event A character string specifying the second eevnt type.
#' @param gap A numeric giving the gap between \code{x_event} and \code{y_event}
#'

setGeneric( "clean_short_IxyI", function( data, x_event, y_event, gap ) standardGeneric( "clean_short_IxyI" ) )

setMethod( "clean_short_IxyI", signature( data = "formal_event_record", x_event = "character", y_event = "character", gap = "numeric" ),
    function( data, x_event, y_event, gap ){
        keep_vector = CAB_cpp_clean_short_ixyi( data@events, x_event, y_event, gap )
        data@events <- data@events[ keep_vector ]
        data
    }
)




