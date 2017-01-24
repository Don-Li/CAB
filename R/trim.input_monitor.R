#### trim input  ####

#' Trim the \code{input} object
#'
#' When running a simulation, it is best to set the \code{\link{input}} to contain vectors longer than what is needed to store all of the information. After running the simulation, the \code{trim.input} function can be used to trim off the unused elements.
#'
#' @param input_object An object of class \code{input}.
#' @param trim_excess_time A numeric value. Events and responses that occur later than some specified time are also trimmed. Defaults to \code{NULL} which means that only the \code{NaN} valus are trimmed.
#' @param dims A character vector specifying the variables of the \code{input} object to keep. Defaults to \code{NULL} which means that all variables are kept.
#'
#' @seealso
#' \code{\link{class.input}}
#'
#' \code{\link{tidy.simulation_data}} which uses \code{trim.input}.
#'
#' @rdname trim.input
#' @exportMethod trim.input

setGeneric( "trim.input", function( input_object, trim_excess_time = NULL, dims = NULL ) standardGeneric( "trim.input" ) )

#' @rdname trim.input
#' @param input_object A \code{input} object.
#' @exportMethod trim.input

setMethod( "trim.input", signature( input_object = "input" ),
    function( input_object, trim_excess_time, dims ){
        if ( is.null(dims) ) dims = input_object@names
        for ( i in dims ){
            if ( !is.null( trim_excess_time ) ){
                input_object@info[[ i ]]$data = input_object@info[[ i ]]$data[ which( input_object@info[[i]]$data < trim_excess_time ) ]
            }
            else {
                input_object@info[[ i ]]$data = input_object@info[[ i ]]$data[ 1: input_object@info[[i]]$counts ]
            }
        }
    }
)
