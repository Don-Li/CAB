#### Custom elemental classes ####

#' Create a custom elemental class and custom elemental objects
#'
#' For the implementation of a custom model, it is necessary to define a custom elemental class as well as objects for that class. When a custom elemental class is defined, a \code{show} method is also defined at the same time by default. To remove the assocaited \code{show} method, use \code{remove.show.custom_element( "class_name" ) }.
#'
#'
#' @param class_name A character string specifying the name of the custom elemental class. The name of the slot in the custom elemental object that contains the elemental function will be the same as the \code{class} name.
#' @param custom_elemental_fx A function for the elemental object.
#' @param custom_elemental_name A name for the elemental object.
#' @param class The type of elemental that you want your elemental object to be.
#'
#' @examples
#' #Make a DOR class from scratch
#' set.custom_elemental( class_name = "DOR2" )
#' #use linear_DOR_fx()
#' linear_DOR = make.custom_elemental( linear_DOR_fx, "linear_DOR", "DOR2" )
#'
#' @note
#' If there are no errors, the \code{set.custom_elemental} function will print a message saying that the custom elemental class has been set.
#'
#' @seealso
#' \code{\link{set.custom_control}} Setting and making custom elemental classes is necessary to make a custom control class.
#'
#' \code{\link{make.custom_control}} As above.
#'
#' \code{\link{e_show}} For the show functions
#'
#' @export set.custom_elemental

set.custom_elemental = function( class_name, set_show_method = T ){
    slot_name = class_name
    if ( !is.character( class_name ) ) stop( "Enter class name as 'character'" )
    slot_list = list( "function" )
    names( slot_list ) = c( slot_name )
    x = list( Class = class_name , slots = slot_list, contains = "elemental" )
    do.call( "setClass", x )

    if ( set_show_method ){
        setMethod( "show", class_name, function( object ) e_show( object ) )
    }
    cat( "Custom elemental class","'", class_name, "'","has been set." )
}

#' @rdname set.custom_elemental
#' @export make.custom_elemental

make.custom_elemental = function( custom_elemental_fx, custom_elemental_name, class ){
    if ( !is.function( custom_elemental_fx ) ) stop( "Enter custom elemental function as 'function'" )
    if ( !is.character( custom_elemental_name ) ) stop( "Enter custom elemental name as 'character'" )
    if ( !is.character( class ) ) stop( "Enter the class of your elemental as 'character'" )
    arg_list = list( class, custom_elemental_fx, custom_elemental_name, "type" )
    names( arg_list ) = c( "Class", slotNames( class ) )
    do.call( new, arg_list )
}

#' @rdname set.custom_elemental
#' @export remove.show.custom_elemental

remove.show.custom_elemental = function( class_name ){
    removeMethod( "show", signature( object = class_name ) )
}
