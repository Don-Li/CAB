#### Change the parameters in a model ####

#' @include model.R
NULL

#' Change model parameters
#'
#' The \code{change_params()} function is for changing the parameters in a \code{model} object that has already been constructed. Note that this function is implemented only for \code{model} objects that have one \code{elemental} function for each \code{control} slot. In the future, this may be changed.
#'
#' @param mutables A named list. The name of each element should refer to a \code{control} object and the elements should be the names of arguments for the associated \code{elemental} function. These are the parameters that will be changed.
#' @param new_params A vector of new parameter values. The order of the vector should correspond to the order of arguments in \code{mutables}.
#' @param model_object A \code{model} object or any of its child classes.
#'
#' @examples
#' # Suppose a model, "model_obj", has a "DOR_control" slot with one parameter "max" that is initially set to 100.
#' # To change the value to 200:
#' mutables = list( DOR_control = "max" )
#' new_params = 200
#' change_params( mutables, new_params, model_obj )
#'
#' @seealso
#' \code{\link{class.model}}
#'
#' @rdname change_params
#' @exportMethod change_params

setGeneric( "change_params", function( mutables, new_params, model_object ) standardGeneric( "change_params" ) )

setMethod( "change_params", signature( mutables = "list", new_params = "numeric", model_object = "model" ),
    function( mutables, new_params, model_object ){
        mutate_parameters( mutables, new_params, model_object )
    }
)

mutate_parameters = function( mutables, new_params, model_object ){
    mapply( function( x, y, z ){
        dims = dim( model_object@control_list[[ x ]][[ 1 ]][ y ] )
        model_object@control_list[[ x ]][[ 1 ]][ y ] <- matrix(z, nrow = dims[1], ncol = dims[2], byrow = T )
    },
    x = names( mutables ), y = mutables, z = new_params, SIMPLIFY = F, USE.NAMES = F )
}
