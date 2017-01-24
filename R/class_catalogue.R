#### Class catalog ####

#' View the objects in a class
#'
#' This function \code{class_catalogue} is for getting the names of objects from a specified class. It is most useful for when you want to view the objects that you have defined. For example, the \code{\link{make.custom_input}} function should be used to define inputs with unique names. \code{class_catalogue} can be used to check if your custom input names are unique.
#'
#' @param class_name A string that gives the name of the class that you want to view the objects of.
#' @param environment A character vector that gives the environments that you want to search for objects in. The default value is \code{NULL} and by default, the \code{class_catalogue} searches through the global environment and also the \code{CAB:package} environment.
#'
#' @return Returns a character vector containing the names of all the instances of the class of interest.
#'
#' @examples
#' # Make a test class
#' test_input = make.custom_input( name = "test_input" )
#' # Check that it exists
#' class_catalogue( "input" )
#'
#' @export class_catalogue

class_catalogue = function( class_name, custom_env = NULL ){
    environments = c( "package:CAB", custom_env )
    envir = unlist( sapply( environments, function( x ) filtering( class_name, x ) ) )
    global = filtering( class_name, globalenv() )
    c( global, envir )
}

filtering = function( class_name, environment ){
    Filter( function(x) class_name %in% class( get(x) ), ls( name = environment ) )
}
