#### CAB models ####

#' Construction of models in CAB
#'
#' The Computational Analysis of Behaviour (CAB) software package is a general purpose software package for implementing computational models of behaviour. A model is implemented in the \code{CAB.model} class. The \code{CAB.model} is a parent class that contains a slot \code{organism} that contains all the parameters that are necessary for the functions that are required to operate the model. In order to create a model, create a parent class that inherits from \code{CAB.model} with slots for each of the functions that are associated with that model.
#'
#' For constructing a model:
#' \enumerate{
#'     \item Make a constructor function with the \code{model_constructor()} function.
#'     \item Use the constructor to make a model object.
#' }
#'
#' @section The \code{CAB.model} class:{
#'     The \code{CAB.model} is the parent class for specific model objects.
#'     \subsection{Slots}{
#'         \describe{
#'             \item{\code{organism}}{An \code{environment} that contains all the parameters needed for the model.}
#'             \item{\code{derived_params}}{An \code{expression} for calculating parameters that require values from the \code{organism} slot.}
#'             }
#'     }
#' }
#'
#'@section The \code{model_constructor} function:{
#'     \code{model_constructor} is a function returns another function. The returned function contains all of the information that is required to construct the desired model.
#'     \subsection{Usage}{
#'         \code{model_constructor( model_name, slot_names ) }
#'     }
#'     \subsection{Arguments}{
#'         \describe{
#'             \item{\code{model_name}}{A character string giving the name of the model class to be constructed}
#'             \item{\code{slot_names}}{Names of the sub-functions required for the model}
#'         }
#'     }
#'     \subsection{Value}{
#'         The \code{model_constructor} returns a function with the arguments \code{organism_params}, \code{...}, and \code{derived_params}. \code{organism_params} takes a list with the names of each element specifying the model parameters. \code{...} takes named arguments with the names of the arguments being \code{slot_names} in the constructor function and the argument values being functions for the associated model sub-function. \code{derived_params} takes an expression that is used to calculate values using variables in \code{organism_params}, defaults to \code{NULL} if no derived parameters are needed. See example for notes on writing the \code{expression} for \code{derived_params}
#'     }
#'
#'}
#'
#' @rdname class.CAB.model
#' @aliases model_constructor
#'
#' @examples
#' # Make a model called "good_times"
#' # First set up the function to construct "good_time" models
#' # Give the model two subfunctions: "plus_happy" and "minus_happy"
#' make_good_times = model_constructor( model_name = "good_times", slot_names = c( "plus_happy", "minus_happy" ) )
#'
#' # Let the "good_times" model have 3 parameters: happiness level and set it to 100, plus and set to 10, minus and set to 1
#' organism_parameters = list( happiness_level = 100, plus = 10, minus = 1 )
#' # Define the "plus_happy" and "minus_happy" functions
#' plus_happy_fx = function( happiness_level, plus ){
#'     happiness_level + plus
#' }
#' minus_happy_fx = function( happiness_level, minus ){
#'     happiness_level - minus
#' }
#' # Add a derived parameter:
#' # The expression must have either curly bracers or the formal <- assignment operator:
#' indifference = expression({ indifference = plus - minus })
#' # or
#' indifference = expression( indifference <- plus - minus )
#'
#' # Make a "good_times" model:
#' good_times_model = make_good_times( organism_params = organism_parameters, plus_happy = plus_happy_fx, minus_happy = minus_happy_fx, derived_params = indifference )
#' good_times_model
#'
#' @export model_constructor
#' @exportMethod show

class.CAB.model = setClass( "CAB.model", slots = list( organism = "environment", derived_params = "expression") )

model_constructor = function( model_name, slot_names ){
    slots = rep( "function", length( slot_names ) )
    names( slots ) = slot_names
    setClass( model_name, slots = slots, contains = "CAB.model", where = globalenv() )

    function( organism_params , ..., derived_params = NULL ){
        dot_args = list( ... )
        if ( !all( slot_names %in% names( dot_args ) ) ){
            stop( paste( "A slot in", model_name, "has not been specified" ) )
        }
        if ( !is.list( organism_params ) ){
            stop( "organism_params must be a list" )
        }
        dot_args$organism = list2env( organism_params )
        dot_args$derived_params = derived_params
        eval( derived_params, envir = dot_args$organism )
        dot_args$Class = model_name

        do.call( new, dot_args )
    }
}

setMethod( "show", signature( object = "CAB.model" ), function( object ){
    generic_show = getMethod( show, signature = "ANY" )
    slotnames = slotNames( object )
    child_slots = slotnames[ -which(slotnames == "organism" ) ]
    lapply( child_slots, function(x){
        cat( "Slot:", x, "\n" )
        generic_show( slot(object,x) )
        cat( "\n" )
        })
    print( ls.str( object@organism ) )
} )

setGeneric( "set_derived", function( model, derived_params ) standardGeneric( "set_derived" ) )

setMethod( "set_derived", signature( model = "CAB.model" ), function( model, derived_params ){
    eval( derived_params, envir = model@organism )
} )




