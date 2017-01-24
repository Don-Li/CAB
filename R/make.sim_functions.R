#### Make simulation closures ####

#' @include get.model_conditions.R e_show.R
NULL

#' Make simulation functions (closures)
#'
#' Makes a closure from the functions specified in the \code{elemental} objects for a model and the parameters in the respective \code{control} objects. Specifically, the closure is implemented as its own class.
#'
#' A \code{closure} is a function that contains some variables with some set values, i.e., the function closes over these variables. In the \code{CAB} package, the simulation of a model is done by creating closures using the functions that are specified in the \code{elemental} objects and with parameters that have been specified in the \code{control} objects. That is, each time a model is run with some parameters, each of the functions that are required to run the model are closed over the conditions for that particular combination of parameters.
#'
#' @param conditions A list of conditions. See \code{\link{get.model_conditions}} for a nice way to specify this argument.
#' @param model_object A model object.
#'
#' @examples
#' # Following from the example in ?get.model_conditions
#' cond1 = get.model_conditions( my_COR, 1 )
#' my_sim_closures = make.sim_functions( cond1, my_COR )
#'
#' @seealso
#' \code{\link{class.model.COR}} For the example.
#'
#' \code{\link{get.model_conditions}} Where \code{get.model_conditions} is a nice way to specify the \code{conditions} argument for \code{make.sim_functions}.
#'
#' \code{\link{do.sim_fx}} For calling the functions made by \code{make.sim_functions}.
#'
#' @rdname make.sim_functions
#' @export make.sim_functions

make.sim_functions = function( conditions, model_object ){

    condition_fx_names = conditions.get.fx_names( conditions )
    control_names = names( conditions )
    condition_fxs = conditions.get.fx( model_object, control_names, condition_fx_names )

    lst = lapply( control_names, function( x ){
    make.closure( condition_fxs[[x]], conditions[[x]], model_object@input )
} )
    names( lst ) = control_names
    lst
}

conditions.get.fx_names = function( conditions ){
    sapply( conditions, function( x ) x$fx )
}

conditions.get.fx = function( model_object, control_names, condition_fx_names ){
    x = lapply( 1:length( control_names), function( x ){
        slot( model_object, control_names[x] )[[ condition_fx_names[x] ]]
    } )
    names( x ) = names( condition_fx_names )
    x
}

make.closure = function( condition_fx, condition, input ){
    param_list = condition[ -ncol(condition) ]
    attr( param_list, "class" ) = "list"
    input_names = names( input )

    input_matcher = unlist( condition[ which( condition %in% input_names ) ] )

    f = function( input_object ){
        if ( !is.null( input_matcher ) ) param_list[ names( input_matcher ) ] = mget( input_matcher, input_object@info )
        do.call( condition_fx, param_list )
    }

    new( "element_fx_closure", element_fx = condition_fx, condition_info = param_list, closure = f )
}

#' @rdname make.sim_functions
#'
#' @slot element_fx The elemental function.
#' @slot condition_info The parameters from the control object.
#' @slot closure The closure.
#'
#' @aliases class.element_fx_closure

class.element_fx_closure = setClass( "element_fx_closure", slots = list(
    element_fx = "function",
    condition_info = "list",
    closure = "function"
) )

#' @rdname make.sim_functions
#'
#' @format In \code{R}, closures are shown with the code with which they were defined. Hence, the actual information that is contained in the closure will not be shown. Here, we have a special \code{show} function that shows the elemental function as well as the associated parameters in the closure. As with all \code{show} functions in \code{CAB}, the user is able to remove the \code{show} method with \code{remove.show.element_fx_closure()} and reinstate it with \code{show.closure.helper}.
#'
#' @aliases show.element_fx_closure
#' @export show.element_fx_closure

show.element_fx_closure = setMethod( "show", signature( object = "element_fx_closure" ), function( object ) show.closure.helper( object ) )

show.closure.helper = function( closure ){
    name = closure@condition_info$fx
    fx = deparse( closure@element_fx )
    fx_string = fx[ fx != "{" & fx != "}" ]
    cat( "Elemental function:", name, "\n" )
    cat( paste("    ", fx_string, collapse = "\n" ), "\n" , sep = "\t")
    cat( "Condition information:", "\n" )
    print( closure@condition_info )
    cat( "\n" )
}

#' @rdname make.sim_functions
#' @export remove.show.element_fx_closure

remove.show.element_fx_closure = function() removeMethod( "show", signature( object = "element_fx_closure" ) )
