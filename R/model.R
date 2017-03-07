#### Model class ####

#' @importFrom data.table rbindlist
#'
#' @include elemental_get_helpers.R control.R
NULL
#' Construction of \code{model} objects
#'
#' \code{model} objects are used for implementing computational models in the \code{CAB} package. The general implementation of a model is that the \code{model} class is a parent class. A specific computational model, such as Catania's Operant Reserve (\code{\link{model.COR}}) is then a child class that inherits slots from the parent \code{model} class. An object of a child \code{model} class contains all the \code{control} objects that are nedded for the model to run. See \code{\link{model.COR}} for an example.
#'
#' A \code{model} object contains all of the \code{control} objects for all of the elemental objects that are required to implement a given model. Hence, the \code{model} object for a given model will contain all of the necessary functions and parameters as well as an iterator which is a design matrix out of all of the parameter levels across each \code{control} object. Although it is up to the user to define which \code{control} objects the \code{model} object contains, there are some slots that are mandatory (e.g. the iterator that controls the conditions that are run). The mandatory slots are documented here.
#'
#' @slot control_list A list of the \code{control} object names.
#' @slot enumerated_conditions A list of the conditions coded as numbers.
#' @slot iterator A data frame that stores the condition information.
#'
#' @examples
#' # Set a custom model object
#'
#' # First, set some custom elemental objects
#' set.custom_elemental( "test_elemental1" )
#' set.custom_elemental( "test_elemental2" )
#' # Make the custom elemental objects.
#' test_element1 = make.custom_elemental( function(a, time){a}, "test_element1", "test_elemental1" )
#' test_element2 = make.custom_elemental( function(a, time){a}, "test_element2", "test_elemental2" )
#'
#' #Make a 'time' input
#' time_input = make.input( "time_input" )
#' test_input = make.input( time_input )
#'
#' # Set up custom control class
#' set.custom_control( "test_elemental1" )
#' set.custom_control( "test_elemental2" )
#' # Note the names of the control objects are "test_elemental1_control" and "test_elemental2_control"
#' # Set up parameters
#' test1_param = list( a = 1, time = "time_input" )
#' test2_param = list( a = 3, time = "time_input" )
#' #Make control objects
#' test1_ctrl = make.control( test_element1, test1_param )
#' test2_ctrl = make.control( test_element2, test2_param )
#'
#' # Set up the custom class
#' set.custom_model( c("test_elemental1_control", "test_elemental2_control" ) ,model_class_name = "test_model" )
#' # Make a 'test_model" object
#' test_model = make.custom_model( test_elemental1_control = test1_ctrl, test_elemental2_control = test2_ctrl, input = test_input, model_class = "test_model" )
#'
#'
#' @seealso
#' \code{\link{set.custom_elemental}} For setting and making custom elemental objects.
#'
#' \code{\link{class.control}} For setting and making custom control objects.
#'
#' \code{\link{class.input}} For the \code{input} class.
#'
#' \code{\link{model.COR}} For an example of an implemented model.
#'
#' @section Set a custom \code{model} class:{
#' To set a custom \code{model} class:
#' \subsection{Usage}{
#'     \code{set.custom_model( control_objs, model_obj_name )}
#' }
#' \subsection{Arguments}{
#'     \describe{
#'         \item{\code{control_objs}}{A character string giving the name of the \code{control} classes in a model. See \code{\link{class.control}} for setting and making custom control classes/objects.}
#'         \item{\code{model_class_name}}{A character string specifying the name of the \code{model}.}
#'         }
#'     }
#' }
#' \subsection{Value}{
#' Sets a \code{model} object. If there were no errors, a message will be printed stating that the \code{model} class has been set.
#' }
#'
#' @rdname class.model
#' @export class.model

class.model = setClass( "model", slots = list( control_list = "environment", enumerated_conditions = "list", iterator = "data.frame", input = "input") )


# #' @rdname class.model
# #' @export set.custom_model
#
# set.custom_model = function( control_objs, model_class_name ){
#     if ( !is.character( control_objs ) ){
#         stop( "The 'control_objs' should be a character vector" )
#     }
#     ctrls = rep( list( "list"), length( control_objs ) )
#     names( ctrls ) = control_objs
#
#     setClass( model_class_name, slots = ctrls, contains = "model" )
#     cat( "Custom model object", "'", model_obj_name, "'", "has been set." )
# }

#' @rdname class.model
#'
#' @section Make a custom \code{model} model:{
#' To set a custom \code{model} object:
#' \subsection{Usage}{
#'     \code{make.custom_model( ..., input, model_class )}
#' }
#' \subsection{Arguments}{
#'     \describe{
#'         \item{\code{...}}{A collection of \code{control} objects that belong to the model. It is best to name each argument. See example.}
#'         \item{\code{input}}{A \code{input} object containing all the required inputs for the model.}
#'         \item{\code{model_class}}{A character string giving the name of the model class.}
#'         }
#'     }
#' }
#'
#' @export make.custom_model

make.custom_model = function( ..., input, model_class ){
    model_elements = list( ... )

    slot_names = slotNames( model_class )
    elemental_classes = slot_names[- ( (length(slot_names)-3) : length(slot_names) ) ]

    control_list = lapply( model_elements, function( x ) x@params )
    names( control_list ) = elemental_classes

    enumerated_conditions = lapply( control_list, make.enumerated_conditions )

    ranges = lapply( enumerated_conditions, function(x) 1:nrow(x) )
    iterator = expand.grid(ranges)

    listerinos = lapply( model_elements, get_control_fx )
    names( listerinos ) = elemental_classes

    x = c( list( Class = model_class ), listerinos, list( control_list = control_list, enumerated_conditions = enumerated_conditions, iterator = iterator, input = input ) )

    do.call( "new", x )
}

get_control_fx = function( control_obj ){
    name = names( control_obj@fx )
    class = class( control_obj@fx[[1]] )
    lst = lapply( 1:length( name ), function( x ) slot( control_obj@fx[[ name[ x ] ]], class ) )
    lst_names = sapply( 1:length( name ), function( x ) slot( control_obj@fx[[ name[ x ] ]], "name" ) )
    names( lst ) = lst_names
    lst
}

make.enumerated_conditions = function( control_c ){
    x = 1:length( control_c )
    container = lapply( control_c, data.frame )
    y = rbindlist( lapply( x, function(y) expand.grid( y, 1:nrow( container[[y]] ) ) ) )
    attr( y, "class" ) = "data.frame"
    y
}

control.get_elemental_class = function( x ){
    slotNames( x@fx[[1]] )[1]
}
