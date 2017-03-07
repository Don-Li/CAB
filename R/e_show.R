#### Elementary show ####

#' @include elemental_get_helpers.R elemental_get_helpers.R
NULL

#' Show an elementary object
#'
#' This is a \code{show} function for elementary objects. This is just to print the elementary objects in a nice way.
#'
#' For each elemental object, there exists a method that calls the \code{e_show} function. For custom elementary objects, the \code{e_show} method is defined when the custom elementary object is defined.
#'
#' The \code{e_show} for a specific elemental object can be removed by calling \code{remove.X.show} function (where 'X' is the elemental class) that comes with the specific elemental object. See the documentation for the particular custom elemental object for more information.
#'
#' If one wishes to remove the \code{e_show} from a custom elemental object, then one may us the \code{remove.custom_elemental.show} function.
#'
#' @examples
#' # Make a show method for the \code{DOR} class from scratch.
#' setMethod( "show", signature( object = "DOR" ), function( object ) e_show( object ) )
#' # If a custom elemental object is made, the "show" method is automatically defined.
#' # Remove the custom show method.
#' remove.show.custom_elemental( "DOR" )
#'
#' @seealso
#' \code{\link{set.custom_elemental}} For the arguments that control whether or not an \code{e_show} method is defined when a custom elemental class is defined.
#'
#' @export e_show

e_show = function( object ){
    name = e_get.name(object)
    fx = deparse( e_get.fx(object) )
    fx_string = fx[ fx != "{" & fx != "}" ]
    cat( name, ":\n", sep = "")
    cat( paste("    ", fx_string, collapse = "\n" ), "\n" , sep = "\t")
    cat( "Object type: Elemental", "\n" )
    cat( "Class:", class(object) )
}
