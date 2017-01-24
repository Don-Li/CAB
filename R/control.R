#### Make a control object from elemental objects ####

#' @include custom_elemental.R elemental_get_helpers.R analysis_helpers.R
NULL

#' Control objects
#'
#' Objects from the \code{control} class contain objects of a particular type of 'elemental' class as well as their associated parameters. For example, we may have a \code{control} class for \code{DOR} objects. This control class would contain all the \code{DOR} objects that we wish to use in our simulation as well as all of the parameters that we wish to use. The \code{control} class objects are associated with their elemental objects. For example, the \code{control} object for the \code{DOR} class would be a \code{DOR_control} object. Hence, this document details the generic properties of \code{control} classes.
#'
#' @slot fx A list of elemental objects of the same type.
#' @slot params A list of parameters.
#' @slot type Set to 'control'.
#'
#' @section Built-in classes of \code{control} objects:{
#' This is a list of the built-in \code{control} objects.
#' \describe{
#'     \item{\code{DOR_control}}{For \code{DOR} objects, \code{\link{class.DOR}}}
#'     \item{\code{dep_control}}{For \code{dep} objects, \code{\link{class.dep}}}
#'     \item{\code{emission_control}}{For \code{emission} objects, \code{\link{class.emission}}}
#'     \item{\code{initial_reserve_control}}{For \code{initial_reserve} objects, \code{\link{class.initial_reserve}}}
#'     \item{\code{stimulus_control}}{For \code{stimulus} objects, \code{\link{class.stimulus}}}
#'     \item{\code{termination_control}}{For \code{termination} objects, \code{\link{class.termination}}}
#' }
#' }
#'
#' @examples
#' #Make some 'DOR' objects:
#' linear_DOR = make.DOR( DOR_fx = linear_DOR_fx, name = "linear_DOR" )
#' exponential_DOR = make.DOR( DOR_fx = exponential_DOR_fx, name = "exponential_DOR" )
#'
#' #Make parameter lists for each 'DOR' object.
#' #linear_DOR_fx has the parameters: 'max', 'scale', 'resp_times', 'rft_times', 'reserve_value'.
#' #The parameters 'max' and 'scale' are fixed within each simulation while the other parameters are inputs that change within each simulation. Hence, we put the name of the input that we want to use for these parameters.
#' linear_DOR_params = list( scale = 100, max = c(1, 0.5), resp_times = "resp_time", rft_times = "rft_time", reserve_value = "reserve" )
#' #Apply the same logic to the exponential DOR parameters:
#' #If we do not name the parameters in the list, they are applied in the order that they are specified.
#' exponential_DOR_params = list( 100, 1, "resp_time", "rft_time", "reserve" )
#'
#' #Combine the parameter lists:
#' #If the lists are not named, they are specified in the order that they appear.
#' DOR_params = list( linear_DOR = linear_DOR_params, exponential_DOR_params )
#' #Put the DOR objects into a list:
#' DOR_list = list( linear_DOR, exponential_DOR )
#'
#' #Make the control object
#' DOR_ctrl = make.control( DOR_list, DOR_params )
#' #If we only have one DOR object, we can directly specify them in the make.control() function without putting them inside another list.
#'
#' #To make a custom control object
#' #Assume that you already have an elemental class, 'test' and and you want to make a 'test_control' object.
#' #This will define a 'test_control' class.
#' test_ctrl = set.custom_control( "test" )
#' #Then you use "make.control()" as shown above.
#'
#' @seealso
#' \code{\link{set.custom_elemental}} For setting up custom elemental classes to make custom control objects.
#'
#' \code{\link{e_show}} For the show methods.
#'
#' @rdname class.control
#'
#' @export DOR_control
#' @export dep_control
#' @export emission_control
#' @export initial_reserve_control
#' @export termination_control
#' @export food_duration_control

DOR_control = setClass( "DOR_control", slots = list( fx = "list", params = "list", type = "character" ) )

dep_control = setClass( "dep_control", slots = list( fx = "list", params = "list", type = "character" ) )

emission_control = setClass( "emission_control", slots = list( fx = "list", params = "list", type = "character" ) )

stimulus_control = setClass( "stimulus_control", slots = list( fx = "list", params = "list", type = "character" ) )

initial_reserve_control = setClass( "initial_reserve_control", slots = list( fx = "list", params = "list", type = "character" ) )

termination_control = setClass( "termination_control", slots = list( fx = "list", params = "list", type = "character" ) )

food_duration_control = setClass( "food_duration_control", slots = list( fx = "list", params = "list", type = "character" ) )

#### Show methods for built-in control classes ####

#' @rdname class.control
#'
#' @section \code{show} methods for the built-in \code{control} objkects:{
#' This is a list of \code{show} methods for the built-in \code{control} objects:
#' \describe{
#'     \item{\code{show.DOR_control}}{For \code{DOR} objects, \code{\link{class.DOR}}}
#'     \item{\code{show.dep_control}}{For \code{dep} objects, \code{\link{class.dep}}}
#'     \item{\code{show.emission_control}}{For \code{emission} objects, \code{\link{class.emission}}}
#'     \item{\code{show.initial_reserve_control}}{For \code{initial_reserve} objects, \code{\link{class.initial_reserve}}}
#'     \item{\code{show.stimulus_control}}{For \code{stimulus} objects, \code{\link{class.stimulus}}}
#'     \item{\code{show.termination_control}}{For \code{termination} objects, \code{\link{class.termination}}}
#' }
#' Each of these methods have a corresponding \code{remove.show.X} function. If a \code{show} method is removed, it may be reinstated with the code above.
#' }
#'
#' @export show.DOR_control
#' @export show.dep_control
#' @export show.emission_control
#' @export show.initial_reserve_control
#' @export show.termination_control
#' @export show.food_duration_control
#' @export show.food_duration_control
#' @export remove.show.DOR_control
#' @export remove.show.dep_control
#' @export remove.show.emission_control
#' @export remove.show.initial_reserve_control
#' @export remove.show.termination_control
#' @export remove.show.food_duration_control

helper.show.control = function( fx, params ){
    for ( i in names( fx ) ){
        show( fx[[i]] )
        cat( "\n" )
        cat( "Parameters:" )
        cat( "\n" )
        show( params[[i]] )
        cat( "\n")
    }
}

show.DOR_control = setMethod( "show", signature( object = "DOR_control" ), function( object ){
    fx = object@fx; params = object@params
    helper.show.control( fx, params )
} )

show.dep_control = setMethod( "show", signature( object = "dep_control" ), function( object ){
    fx = object@fx; params = object@params
    helper.show.control( fx, params )
} )

show.emission_control = setMethod( "show", signature( object = "emission_control" ), function( object ){
    fx = object@fx; params = object@params
    helper.show.control( fx, params )
} )

show.stimulus_control = setMethod( "show", signature( object = "stimulus_control" ), function( object ){
    fx = object@fx; params = object@params
    helper.show.control( fx, params )
} )

show.initial_reserve_control = setMethod( "show", signature( object = "initial_reserve_control" ), function( object ){
    fx = object@fx; params = object@params
    helper.show.control( fx, params )
} )

show.termination_control = setMethod( "show", signature( object = "termination_control" ), function( object ){
    fx = object@fx; params = object@params
    helper.show.control( fx, params )
} )

show.food_duration_control = setMethod( "show", signature( object = "food_duration_control" ), function( object ){
    fx = object@fx; params = object@params
    helper.show.control( fx, params )
} )

remove.show.DOR_control = function() removeMethod( "show", signature( object = "DOR_control" ) )

remove.show.dep_control = function() removeMethod( "show", signature( object = "dep_control" ) )

remove.show.emission_control = function() removeMethod( "show", signature( object = "emission_control" ) )

remove.show.stimulus_control = function() removeMethod( "show", signature( object = "stimulus_control" ) )

remove.show.initial_reserve_control = function() removeMethod( "show", signature( object = "initial_reserve_control" ) )

remove.show.termination_control = function() removeMethod( "show", signature( object = "termination_control" ) )

remove.show.food_duration_control = function() removeMethod( "show", signature( object = "food_duration_control" ) )


#### Make a control object ####

#' @rdname class.control
#'
#' @section Make a \code{control} object from 'elemental' objects:{
#' To create a \code{control} object from a list of 'elemental' objects:
#' \enumerate{
#'     \item Make sure that the type of \code{control} object that you want to make has been defined. If the type of \code{control} object that you wish to make has not been defined, see the following section for how to define a custom type of \code{control} object.
#'     \item Use the \code{make.control} function
#' }
#' \subsection{Usage}{
#'     \code{make.control( elemental_objs, parameter_lists )}
#' }
#' \subsection{Arguments}{
#'     \describe{
#'         \item{\code{elemental_objs}}{Either a list of elemental objects of the same class or a single elemental object}
#'         \item{\code{parameter_lists}}{A list of parameter lists for the function contained in the elemental objects. All parameters must be strings that have been previously specified as \code{input}. The parameter list need not be named. Unnamed parameters are set up in the order that they appear. Alternatively, one may specify which elemental object each parameter list belongs to. See example.
#'         }
#'     }
#' }
#' \subsection{Value}{
#' Returns a \code{control} object.
#' }
#' }
#'
#' @export make.control

make.control = function( elemental_objs, parameter_lists ){
    #If a single elemental obj is added as argument
    if ( !is.list( elemental_objs ) ) {
        x = make_list( elemental_objs, parameter_lists )
        elemental_objs = x[[1]]
        parameter_lists = x[[2]]
        name = names( elemental_objs )
    } else {
        name = sapply( elemental_objs, e_get.name )
        names( elemental_objs ) = name
        if ( is.null( names( parameter_lists ) ) ){
            names( parameter_lists ) = name
        } else {
            names( parameter_lists )[ names( parameter_lists ) == "" ] = name[ ! name %in% names( parameter_lists ) ]
        }
    }

    classes = sapply( elemental_objs, class )
    if ( length( unique( classes ) ) > 1 ) stop( "Only use the same type of elementals" )

    formal_args_names = lapply( elemental_objs, function( x ) names( formals( e_get.fx( x ) ) ) )
    custom_args_names = lapply( parameter_lists, replace_null_names )

    check_arg_length( formal_args_names, custom_args_names )

    for ( i in 1:length( formal_args_names ) ){
        names( parameter_lists[[ i ]] )[ custom_args_names[[ i ]] == "" ] = formal_args_names[[ i ]][ ! formal_args_names[[ i ]] %in% custom_args_names[[ i ]] ]
    }

    parameter_df = lapply( 1:length( parameter_lists ), function( x ){
        data.frame(parameter_lists[[x]], "fx" = names( parameter_lists )[x], stringsAsFactors = F )
    })
    names( parameter_df ) = name

    switch( classes[1],
        DOR = new( "DOR_control", fx = elemental_objs, params = parameter_df, type = "control" ),
        dep = new( "dep_control", fx = elemental_objs, params = parameter_df, type = "control" ),
        emission = new( "emission_control", fx = elemental_objs, params = parameter_df, type = "control" ),
        initial_reserve = new( "initial_reserve_control", fx = elemental_objs, params = parameter_df, type = "control" ),
        stimulus = new( "stimulus_control", fx = elemental_objs, params = parameter_df, type = "control" ),
        termination = new( "termination_control", fx = elemental_objs, params = parameter_df, type = "control" ),
        food_duration = new( "food_duration_control", fx = elemental_objs, params = parameter_df, type = "control" ),
        make.custom_control( classes[1], elemental_objs, parameter_df )
    )
}

make_list = function( elemental_objs, parameter_lists ){
    name = e_get.name( elemental_objs )
    elemental_objs = list( elemental_objs )
    names( elemental_objs ) = name
    parameter_lists = list( parameter_lists )
    names( parameter_lists ) = name
    list( elemental_objs, parameter_lists )
}

check_arg_length = function( formal_args_names, custom_args_names ){
    x = sapply( names( formal_args_names ), function( x ) {
        length( formal_args_names[[x]] ) == length( custom_args_names[[x]] )
    } )
    names( x ) = names( formal_args_names )
    if ( !all( x ) ){
        message = names( x )[ which( x == F ) ]
        message = paste( message, collapse = ", " )
        stop( paste( "The following elemental functions",
            message,
            "do not have the correct number of arguments" ) )
    }
}

replace_null_names = function( parameter_list ){
    x = names( parameter_list )
    if ( is.null( x ) ) x = rep( "", length( parameter_list ) )
    x
}

#### Set a custom control class ####

#' @rdname class.control
#'
#' @section Set a custom \code{control} object:{
#' To set a custom \code{control} object:
#' \subsection{Usage}{
#'     \code{set.custom_control( elemental_class_name, make.print_method )}
#' }
#' \subsection{Arguments}{
#'     \describe{
#'         \item{\code{elemental_class_name}}{A character string giving the name of an elemental class. See \code{\link{class.custom_elemental}} for setting and making custom elemental classes/objects.}
#'         \item{\code{make.print_method}}{A logical specifying whether or not to make a \code{show} method for printing the \code{control} object. Defaults to \code{T}.
#'         }
#'     }
#' }
#' \subsection{Value}{
#' Sets a \code{control} class If there were no errors, a message will be printed stating that the \code{control} class has been set. Note that the \code{control} object that is set will have the name "X_control", where "X" is the name of the elemental object.
#' }
#' }
#'
#' @export set.custom_control

set.custom_control = function( elemental_class_name, make.print_method = T ){
    if ( !is.character( elemental_class_name ) ) stop( "Enter the elemental class name as 'character'")
    name = paste( elemental_class_name, "_control", sep = "" )
    slot_list = list( "list", "list", "character" )
    names( slot_list ) = c( "fx", "params", "type" )
    setClass( name, slots = slot_list )

    if ( make.print_method ){
        names( name ) = "object"
        setMethod( "show", name, function( object ){
            fx = object@fx
            params = object@params
            helper.show.control( fx, params )
        } )
    }

    cat( "Custom control object", "'", name, "'", "has been set." )
}

make.custom_control = function( elemental_class_name, elemental_objs, parameter_lists ){
    control_name = paste( elemental_class_name, "_control", sep = "" )
    slot_list = list( control_name, elemental_objs, parameter_lists, "control" )
    names( slot_list ) = c( "Class", "fx" ,"params", "type" )
    do.call( "new", slot_list )
}

#' @rdname class.control
#'
#' @section \code{show} methods for custom control objects:{
#' Whenever a custom control object is set up, the default is that an associated \code{show} method is also defined. This \code{show} method can be removed by using the function \code{remove.show.custom_control( object_name )}.
#' }
#' @export remove.show.custom_control

remove.show.custom_control = function( object_name ){
    removeMethod( "show", signature( object = object_name ) )
}
