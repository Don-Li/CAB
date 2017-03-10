#### Data analysis object ####

#' \code{CAB} Analysis Objects
#'
#' \code{analysis_object} are objects for data analysis.
#'
#' In the \code{CAB} package, data is typically stored in an \code{analysis_object} of which there are multiple child classes. \code{analysis_object}s are created by running a simulation or by importing some experimental data.
#'
#' The parent class \code{analysis_object} does not really do anything. The real work is done by other classes that inherit from \code{analysis_object}.
#'
#' @slot analysis_object A \code{data.frame} that contains the data.
#' @slot meta_data A \code{list} that contains the information about the experiment or simulation.
#'
#' @rdname class.analysis_object
#' @exportClass analysis_object

setClass( "analysis_object", slots = list( analysis_object = "data.frame", meta_data = "list") )

#' @rdname class.analysis_object
#' @section {\code{UOA_analysis_object}}:{
#' The \code{UOA_analysis_object} class is made specially for the MED-PC files used at the University of Auckland.
#'     \subsection{Slots}{
#'         \describe{
#'             \item{\code{variable_arrays}}{A list that stores arrays that contains variable information, such as the number of responses or the number of reinforcement (i.e., a "C"-array).}
#'             \item{\code{general_arrays}}{A list that stores arrays that contain general information, such as the arranged inter-reinforcement intervals.}
#'             \item{\code{meta_data}}{Inerited from \code{analysis_object}. A \code{data.frame} that contains the metadata for the experiment.}
#'             \item{\code{analysis_object}}{Inherited from \code{analysis_object}. A \code{data.frame} that contains the events and the times. This will be some post-processed "X"-array.}
#'         }
#'     }
#' }
#' @exportClass UOA_analysis_object

setClass( "UOA_analysis_object", slots = list( variable_arrays = "list", general_arrays = "list" ), contains = "analysis_object" )

#' @rdname class.analysis_object
#' @section {\code{Custom analysis object}}:{
#' If the user wishes to make their own \code{analysis_object} class, the \code{set.custom_analysis_object()} function can be used to set up the class.
#'
#' To create an object of the newly set up \code{analysis_object} class, the \code{make.custom_analysis_object()} function can be used. In general, once a custom \code{analysis_object} class is defined, other functions, such as those that read in the data, will construct objects of the custom \code{analysis_object} class. Hence, the user will rarely have to use the \code{make.custom_analysis_object()} function directly.
#'     \subsection{Usage}{
#'         \code{set.custom_analysis_object( class_name, slot_names )}
#'     }
#'     \subsection{Mandatory slots}{
#'         \describe{
#'             \item{\code{analysis_object}}{Inherited from \code{analysis_object}.}
#'             \item{\code{meta_data}}{Inherited from \code{meta_data}.}
#'         }
#'     }
#'     \subsection{Arguments}{
#'         \describe{
#'             \item{\code{class_name}}{A character string giving the name of the custom \code{analysis_object} class.}
#'             \item{\code{slot_names}}{Either: 1) an unnamed list of slot names or 2) a named list, with the names specifying the slot names and the elements specifying the class of object contained in the slot. If 1), all slots will contain \code{list}. See example.}
#'         }
#'     }
#'     \subsection{Usage}{
#'         \code{make.custom_analysis_object( analysis_obj_class, ... )}
#'     }
#'     \subsection{Arguments}{
#'         \describe{
#'             \item{\code{analysis_object_class}}{A character string specifying the name of the custom \code{analysis_object} class.}
#'             \item{\code{...}}{A named set of arguments that fills up the object. The arguments should be named as the slot name to which the argument pertains to. See example.}
#'         }
#'     }
#' }
#'
#' @examples
#' # Make a custom "analysis_object"
#' # With an unnamed list:
#' set.custom_analysis_object( class_name = "test_analysis", slot_names = list( "slot1", "slot2" ) )
#' # The "slot_names" is an unnamed list, so "slot1" and "slot2" will only hold "list".
#' make.custom_analysis_object( analysis_object_class = "test_analysis", slot1 = list(1), slot2 = list(2) )
#'
#' # With a named list:
#' set.custom_analysis_object( class_name = "test_analysis", slot_names = list( slot1 = "data.frame", slot2 = "character" ) )
#' # The "slot_names" is a named list, with elements "data.frame" and "character". So, "slot1" will hold "data.frame" and "slot2" will hold "character".
#' make.custom_analysis_object( "test_analysis", slot1 = data.frame(1), slot2 = "a" )
#'
#' @export set.custom_analysis_object

set.custom_analysis_object = function( class_name, slot_names ){

    if ( is.null( names( slot_names ) ) ){
        slots = rep( list( "list" ), length( slot_names ) )
        names( slots ) = slot_names
    }

    if ( !is.null( names( slot_names ) ) ){
        slots = slot_names
    }

    x = list( Class = class_name, slots, contains = "analysis_object" )
    names( x ) = c("Class", "slots", "contains" )

    do.call( "setClass", x )
}

#' @rdname class.analysis_object
#' @export make.custom_analysis_object

make.custom_analysis_object = function( analysis_obj_class, ... ){
    new( Class = analysis_obj_class, ... )
}

#' @rdname class.analysis_object
#' @section {Analysis objects from simulations}:{
#' By default, simulations will store the data in a \code{simulation_analysis_object} object.
#'     \subsection{Usage}{
#'         \code{make.simulation_analysis_object( class_name, slot_names )}
#'     }
#'     \subsection{Slots}{
#'         \describe{
#'             \item{\code{analysis_object}}{Inherited from \code{analysis_object}. Defaults to \code{data.frame(NULL)} so that only the \code{input} slot contains simulation data.}
#'             \item{\code{meta_data}}{Inherited from \code{analysis_object}.}
#'             \item{\code{input}}{Inherited from \code{sim_input}.}
#'         }
#'     }
#' }
#'
#' @exportClass  simulation_analysis_object

setClass( "simulation_analysis_object", slots = list( input_list = "list" ), contains = list("analysis_object") )

#' @rdname class.analysis_object
#' @export make.simulation_analysis_object

make.simulation_analysis_object = function( meta_data, input, dims, data = data.frame(NULL) ){
    z = lapply( dims, function( x ) input@info[[ x ]]$data )
    names( z ) = dims
    new( "simulation_analysis_object", input_list = z, analysis_object = data, meta_data = meta_data )
}
