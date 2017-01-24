#### Simulation inputs ####

#' \code{input} class
#'
#' When a simulation is run in the \code{CAB} package, the within-simulation information (i.e., the variables that are not pre-set before the simulation, e.g. the number of responses during an inter-reinforcement itnerval) are stored in an \code{input} object.
#'
#' The underlying implementation of the \code{input} class is that the \code{input} class has two slots. One slot is called \code{info}, which holds an environment. Each element in the environment is a list. For each list, one element of the list is a vector that stores the events that occurred (for example, a vector may store the response times) and the other element is a single numeric that counts the number of elements that stored in the corresponding vector. The second slot is called \code{names}, which is just for easy access of the variable names in the \code{input} object.
#'
#' Before using a \code{input} object to store data or count things in a simulation, set it up with the \code{setup.input_monitor} function.
#'
#' A \code{show} method for the \code{input} class has been defined for printing \code{input} objects (see below; \code{input.show()}). An associated function \code{remove.input.show()} has been defined for removing the \code{show} method.
#'
#' @slot info An \code{environment} that contains a list, which in turn contains a vector to store events that occur in the simulation, as well as a numeric to count the events. The vector that contains the data is called \code{data} and the numeric that stores the counts is \code{counts}.
#' @slot names Stores the names of the variables held in the \code{info} slot.
#'
#' @section Make a new \code{input} object:{
#' Before doing a simulation, a \code{input} object needs to be constructed to store the data that is generated (e.g. response times) and to count the instances of some particular event (e.g. number of responses).
#'
#' Note that when \code{input} object is first constructed, each of the variables in the \code{info} slot will be a vector of \code{NaN} of size 1.
#'
#' \subsection{Usage}{
#'     \code{make.input( var_names )}
#' }
#' \subsection{Arguments}{
#'     \describe{
#'         \item{\code{var_names}}{A vector of character strings that give names for the events to be stored. See example.}
#'     }
#' }
#' \subsection{Value}{
#'     A \code{input} object is returned.
#' }
#' }
#'
#' @examples
#' #### Create an "input" for storing the variables: "time_input", "resp_time", "rft_time", "rft_setup", and to count the events: "resp_counter", "rft_counter". ####
#' var_names = c( "time_input", "vect", "resp_time", "rft_time", "rft_setup" )
#' my_input = make.input( var_names )
#' my_input
#'
#' #### Indexing on an "input" object ####
#' # Extract the response times
#' response_times = my_input[ v = "resp_time" ]
#' # Remember that for a newly created "input" object, the variable contains a single \code{NaN} in the \code{data} vector and a zero in the \code{counts} vector.
#'
#' #### Set up an "input" object ####
#' # Set the "input" to hold a maximum of 10 response times, 11 reinforcement times, and 20 "time_input". Note that the length for "rft_setup" is not specified, so it becomes the specified maximum (20).
#' length_list = list( resp_time = 5, rft_time = 11, time_input = 20 )
#' setup.input( my_input, length_list )
#' my_input
#'
#' # Set up "input" to hold 50 elements for all variables
#' setup.input( my_input, 50 )
#' my_input
#'
#' # Set the first element of the "time_input" variable to 0, set the fifth element of the "time_input" variable to 1, and set the associated "counts" to 100.
#' assign.input( my_input, v = "time_input", indices = c(1, 5), values = c(0, 1 ), counts = 100 )
#'
#' # Store a sequence of "1" in the "resp_time" vector. Also, note the value of the associated "counts"
#' next.input( my_input, "resp_time", 1 )
#' my_input
#' next.input( my_input, "resp_time", 1 )
#' my_input
#' next.input( my_input, "resp_time", 1 )
#' my_input
#'
#' #Set the "resp_time" values to be twice larger than the previously specified "resp_time_value"
#' fx = function( x, y ) x * y
#' transform.input( my_input, "resp_time", fx, y = 2 )
#' my_input
#' transform.input( my_input, "resp_time", fx, y = 2 )
#' my_input
#' # Set the "resp_time" values to be thrice larger
#' transform.input( my_input, "resp_time", fx, y = 3 )
#' my_input
#'
#' #Get the last element in an "input" variable data vector
#' tail.input( my_input, "resp_time" )
#'
#' #Reset
#' reset.input( my_input, "resp_time" )
#' my_input
#'
#' @rdname class.input
#' @export class.input

class.input = setClass( "input", slots = list( info = "environment", names = "character" ) )

#' @rdname class.input
#' @export make.input

make.input = function( var_names ){
    dummy_list = lapply( var_names, function(x) list( data = NaN, counts = 0 ) )
    names( dummy_list ) = var_names
    new( "input", info = list2env( dummy_list, parent = emptyenv() ), names = var_names )
}

#' @rdname class.input
#' @section Indexing on an \code{input} object:{
#'     \subsection{Usage}{
#'         \code{"["( x, v )}
#'         or
#'         \code{input[v]}
#'     }
#'     \subsection{Arguments}{
#'         \describe{
#'             \item{\code{x}}{An \code{input} object.}
#'             \item{\code{v}}{A string specifting a variable in the \code{input} object.}
#'             \item{\code{input}}{An \code{input} object.}
#'         }
#'      }
#'      \subsection{Value}{
#'          Returns the vector in the \code{input} object that is specified by \code{v}. See example.
#'      }
#' }
#' @export index.input

index.input = setMethod( "[", signature( x = "input" ),
    function( x, v ){
        x@info[[v]]
    }
)

#' @rdname class.input
#' @format The \code{show} method for the \code{input} class prints the names of the variables in the \code{input} object as well as the events/counts that they store. Remove this \code{show} method by calling \code{remove.input.show().}
#' @export input.show

input.show = setMethod( "show", signature( object = "input" ),
    function( object ){
        variables = object@names
        cat( "Input object: \n" )
        lapply( variables, input.show_helper, object = object )
        }
)

input.show_helper = function( x, object ){
    cat( x, "\n" )
    print( object@info[[x]] )
    cat( "\n" )
}

#' @rdname class.input
#' @export remove.input.show

remove.input.show = function() removeMethod( "show", signature( object = "input" ) )

#' @rdname class.input
#' @aliases setup.input
#' @section Set up a \code{input} object for a simulation:{
#'     \subsection{Usage}{
#'         \code{setup.input( input, lengths )}
#'     }
#'     \subsection{Arguments}{
#'         \describe{
#'             \item{\code{input}}{An object of class \code{input}}.
#'             \item{\code{lengths}}{Either a \code{list} or a single numeric, giving the length of the data storage vectors. See details.}
#'         }
#'     }
#'     \subsection{Details}{
#'         If the argument for \code{lengths} is a list, the names of each element in the list should be a \code{"vect"} variable. The element associated with the name gives the length of the \code{"vect"} vector and fills it with \code{NaN}. If some \code{"vect"} variables are not given in the list, then they are set to be the length of the longest \code{"vect"} specified by the user.
#'
#'         If the argument is a single numeric, then all of the variables of \code{"vect"} are set to be \code{NaN} vectors of the specified length.
#'     }
#'     \subsection{Value}{
#'         Returns a \code{input} object with each \code{"vect"} variable being a vector filled with \code{NaN} of the specified length.
#'     }
#' }
#'
#' @exportMethod setup.input

setGeneric( "setup.input", function( input, lengths ) standardGeneric( "setup.input" ) )

setMethod( "setup.input", signature( input = "input", lengths = "list" ),
    function( input, lengths ){
        vect_dims = input@names
        specified_lengths = vect_dims[ vect_dims %in% names( lengths ) ]
        maximum = max( unlist(lengths, use.names = F ) )
        for ( i in vect_dims ){
            if ( i %in% specified_lengths ){
                input@info[[i]]$data <- rep( NaN, lengths[[i]] )
                input@info[[i]]$counts <- 0
            }
            else{
                input@info[[i]]$data <- rep( NaN, maximum )
                input@info[[i]]$counts <- 0
            }
        }
    }
)

#' @rdname class.input
#' @exportMethod setup.input

setMethod( "setup.input", signature( input = "input", lengths = "numeric" ),
    function( input, lengths ){
        if ( length(lengths) > 1 ) stop( "'lengths' must be a single numeric" )
        vect_dims = input@names
        for ( i in vect_dims ){
            input@info[[i]]$data <- rep( NaN, lengths )
            input@info[[i]]$counts <- 0
        }
    }
)

#' @rdname class.input
#' @aliases assign.input
#' @section Assign values to variables in a \code{input} object:{
#' This function is for assiging the \code{i}th to \code{j}th element of a variable in an \code{input} object. See examples.
#'     \subsection{Usage}{
#'         \code{assign.input( input, v, indices, values, counts = NULL)}
#'     }
#'     \subsection{Arugments}{
#'         \describe{
#'             \item{\code{input}}{An object of class \code{input}.}
#'             \item{\code{v}}{A character string specifying a variable in the \code{input} object.}
#'             \item{\code{indices}}{An optional argument. A numeric vector specifying which indices of the variable vector to be changed.}
#'             \item{\code{values}}{An optional argument. A vector of values to be assigned.}
#'             \item{\code{counts}}{An optional argument to change the \code{counts} associated with \code{v}}.
#'         }
#'     }
#'     \subsection{Details}{
#'         As a side effect of the way that the assignment is done, if a value is assigned to a variable that does not exist, it will be created and put into the \code{input} environment, with the just the \code{data} vector without \code{counts}.
#'         Also as a side effect of the way that the assignment is done, if the \code{data} vector for a variable is of length 5 and a value is assigned to index 10, the intervening elements (6 to 9) will be \code{NA}.
#'     }
#' }
#' @exportMethod assign.input

setGeneric( "assign.input", function( input, v ,indices, values, counts = NULL ) standardGeneric("assign.input") )

setMethod( "assign.input", signature( input = "input", v = "character", indices = "numeric"),
    function( input, v, indices  = NULL, values  = NULL, counts = NULL ){
        if ( !is.null( indices ) & !is.null( values ) ){
            input@info[[ v ]]$data[ indices ] <- values
        }
        if ( !is.null( counts ) ){
            input@info[[v]]$counts <- counts
        }
    }
)

#' @rdname class.input
#' @aliases next.input
#' @section Assign the next empty (i.e. first non-\code{NaN}) element in a variable in an \code{input} object:{
#' When conducting a simulation, it is often convenient to set the next element that is stored in an \code{input} variable without having to explicitly track where the next available element is. Because the \code{input} variables are a vector filled with \code{NaN} by default (see the above methods), this amounts to using \code{assign.input} to assign the first \code{NaN} element in an \code{input} variable to some value. Because this function is intended to be used to enter data during a simulation, the \code{counter} that is associated with the variable is automatically incremented with each call to the \code{next.input} method.
#'     \subsection{Usage}{
#'         \code{next.input( input, v, value )}
#'     }
#'     \subsection{Arguments}{
#'         \describe{
#'             \item{\code{input}}{An object of class \code{input}.}
#'             \item{\code{v}}{A character string specifying a variable in the \code{input} object.}
#'             \item{\code{value}}{A value to place in the next available element in the data vector for \code{v}.}
#'         }
#'     }
#'     \subsection{Details}{
#'         Note that the new value for the associated \code{counts} will be incremented by 1. Hence, if additional changes are made to the \code{data} vector associated with \code{v}, say with the \code{assign.input}, then the \code{counts} will not be correct.
#'         Also, note that the \code{next.input} function finds the next available element in the vector by taking the index that is 1 larger than the associated \code{counts}. Hence, this will overwrite any stored information in the index regardless if it is \code{NaN}.
#'     }
#' }
#' @export next.input

setGeneric( "next.input", function( input, v, value ) standardGeneric( "next.input" ) )

setMethod( "next.input", signature( input = "input", v = "character", value = "numeric" ),
    function( input, v, value ){
        input@info[[ v ]]$counts = input@info[[ v ]]$counts + 1
        input@info[[ v ]]$data[ input@info[[ v ]]$counts ] <- value
    }
)

#' @rdname class.input
#' @aliases transform.input
#' @section Assign the next empty element in a variable in an \code{input} object by transformation:{
#' When conducing a simulation, the \code{next.input} method can be used to set the next available index in a \code{input} variable to some value. In some cases, it may be necessary for the new value to be dependent on the most recent value. An example of this is if a simulation specifies that the \code{i}th response time must be the same as the \code{i-1}th response time. See example.
#'     \subsection{Usage}{
#'         transform.input( input, v , FUN, ... )
#'     }
#'     \subsection{Arguments}{
#'         \describe{
#'             \item{\code{input}}{An object of class \code{input}.}
#'             \item{\code{v}}{A character string specifying a variable in the \code{input} object.}
#'             \item{\code{FUN}}{A function.}
#'             \item{\code{...}}{Additional arguments for \code{FUN}.}
#'         }
#'     }
#'     \subsection{Details}{
#'         The \code{transform.input} method takes the first unspecified argument for \code{FUN} as the most recent non-empty element in the variable \code{v}. By this, it is meant that the value before the one that is to be specified by the \code{transform.input} function.
#'         Further, the underlying implementation is that the \code{transform.input} function matches the arguments in \code{...} with the formal arguments of the function \code{FUN}. The first unmatched argument is assigned to be the most recent non-empty element in the \code{v} vector. Then, the \code{next.input} function is called. Hence, all of the cautions that must be applied with \code{next.input} should also be applied to \code{transform.input}.
#'     }
#' }

setGeneric( "transform.input", function( input, v, FUN, ... ) standardGeneric( "transform.input" ) )

setMethod( "transform.input", signature( input = "input", v = "character", FUN = "function" ),
    function( input, v, FUN, ... ){
        transform.input_helper( input, v, FUN, ... )
    }
)

transform.input_helper = function( input, v, FUN, ... ){
    dot_list = list( ... )
    formals = names( formals( args( FUN ) ) )
    unmatched_args = formals[ !formals %in% names( dot_list ) ]
    previous_value = input@info[[ v ]]$data[ input@info[[ v ]]$counts ]
    new_arg_list = c( previous_value, dot_list )
    names( new_arg_list )[1] = unmatched_args
    next.input( input, v, do.call( FUN, new_arg_list ) )
}

#' @rdname class.input
#' @aliases tail.input
#' @section Get the last element in a variable in an \code{input} object:{
#' To extract the last element, by which we mean the index that corresponds to the value of the associated \code{counts}, use the \code{tail.input()} function.
#'     \subsection{Usage}{
#'         \code{tail.input( input, v )}
#'     }
#'     \subsection{Arguments}{
#'         \describe{
#'             \item{\code{input}}{An object of class \code{input}.}
#'             \item{\code{v}}{A character string specifying a variable in the \code{input} object.}
#'         }
#'     }
#' }

setGeneric( "tail.input", function( input, v ) standardGeneric( "tail.input" ) )

setMethod( "tail.input", signature( input = "input", v = "character" ),
    function( input, v ){
        input@info[[ v ]]$data[ input@info[[ v ]]$counts ]
    }
)

#' @rdname class.input
#' @aliases reset.input
#' @section To reset the values stored in a variable \code{data} and \code{counts}:{
#' The \code{reset.input()} function is for resetting the values in the \code{data} to \code{NaN} and counts to zero for a variable in an \code{input} object.
#'     \subsection{Usage}{
#'         \code{reset.input( input, v )}
#'     }
#'     \subsection{Arguments}{
#'         \describe{
#'             \item{\code{input}}{An object of class \code{input}.}
#'             \item{\code{v}}{A character string specifying a variable in the \code{input} object.}
#'         }
#'     }
#' }

setGeneric( "reset.input", function( input, v ) standardGeneric( "reset.input" ) )

setMethod( "reset.input", signature( input = "input", v = "character" ),
    function( input, v ){
        count = input@info[[v]]$counts
        input@info[[v]]$data[1:count] = NaN
        input@info[[v]]$counts = 0
    }
)





