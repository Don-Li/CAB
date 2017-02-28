#### Extract method for CAB ####

#' @include model.CAB.R
NULL

#' Extract parts of a \code{CAB.model organism} slot
#'
#' An extraction method is implemented for the \code{CAB.model} object for extracting objects from the \code{organism} slot. For assignment to the \code{organism} slot, see \code{\link{o_set}}.
#'
#' @usage x[v]
#' @param \code{x} A \code{CAB.model} object.
#' @param \code{v} String specifying a variable in the \code{organism} slot of \code{x}. Must be a named argument.
#' @return The object in the \code{organism} slot of the specified \code{CAB.model} object.
#'
#' @note Further indexing of the returend object can be done with additional use of the \code{[} function.
#'
#' @examples
#' # Using the good_times_model from class.CAB.models
#' good_times_model
#'
#' # Get the value of happiness_level
#' good_times_model[ v = "happiness_level" ]
#' # Second level indexing. Returns the same thing
#' good_times_model[ v = "happiness_level" ][1]
#' Out of range:
#' good_times_model[ v = "happiness_level" ][2]
#'
#' @rdname CAB.extract
#' @seealso
#' \code{\link{o_set}} for assignment
#'
#' @exportMethod "["

setMethod( "[", signature( x = "CAB.model", i = "character" ),
    function( x, i ){
        x@organism[[i]]
    }
)
