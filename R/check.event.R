#### check event ####

#' @include sim_input.R input.R
NULL

#' Check whether an event has occurred or not
#'
#' Check whether or not an event that has been arranged has occurred or not. This function can be used for any function that specifies something in the future.
#'
#' @seealso
#' \code{\link{input_monitor}} For how the events are recorded and set up.
#'
#' @rdname check.event
#' @exportMethod check.event

setGeneric( "check.event", function( input_object, event_var, setup_var ) standardGeneric( "check.event" ) )

#' @rdname check.event
#'
#' @usage check.event( input_monitor, event_var, setup_var )
#' @param input_monitor A \code{sim_input} object.
#' @param event_var A character string speicfying the event variable in the \code{sim_input} object.
#' @param setup_var A character string specifying the variable that sets up the event in the \code{sim_input} object.
#'
#' @examples
#' # From the ?do.sim_fx example
#' set.seed(1)
#' access.input_monitor( "rft_setup", 1, do.sim( event, input_monitor ) )
#' check.event( input_monitor = input_monitor, event_var = "rft_time", setup_var = "rft_setup" )
#'
#' @return
#' Returns \code{TRUE} if the event has occurred.

setMethod( "check.event", signature( input_object = "input", event_var = "character", setup_var = "character" ),
    function( input_object, event_var, setup_var ){
        event = tail.input( input_object, event_var )
        setup = tail.input( input_object, setup_var )

        if ( length( event ) == 0 ) event = -1

        if ( event < setup ) return( F )
        else T
    }
)
