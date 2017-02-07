#### Data sets ####

#' @include analysis_object.R
NULL

#' The \code{dataset} class.
#'
#' In the CAB package, experimental data that is imported for analysis is stored in an \code{analysis_object} or one of its child classes (\code{\link{class.analysis_object}}. For the analysis of experimental data, each session should be stored in separate \code{analysis_object}s and these \code{analysis_object}s are in turn contained in a \code{dataset} object.
#'
#' @slot meta_data A \code{data.frame} that contains the information about each element (i.e. experimental session) in the \code{analysis_objects} slot.
#' @slot analysis_object A list, where each element is an \code{analysis_object}.
#'
#'
#'
#' @rdname class.dataset
#' @export class.dataset

class.dataset = setClass( "dataset", slots = list( meta_data = "data.frame", analysis_objects = "list" ) )

#' @rdname class.dataset
#' @section {Custom \code{dataset} class}:{
#' If the user wishes to construct their own \code{dataset} class, the \code{set.custom_dataset()} function can be used.
#'
#' To create an object of the newly set \code{dataset} child class, the \code{make.custom_dataset_object()} function can be used.
#' #'     \subsection{Usage}{
#'         \code{set.custom_dataset( class_name, slot_names )}
#'     }
#'     \subsection{Mandatory slots}{
#'         \describe{
#'             \item{\code{analysis_objects}}{Inherited from \code{analysis_object}.}
#'             \item{\code{meta_data}}{Inherited from \code{meta_data}.}
#'         }
#'     }
#'     \subsection{Arguments}{
#'         \describe{
#'             \item{\code{class_name}}{A character string giving the name of the custom \code{dataset} child class.}
#'             \item{\code{slot_names}}{Either: 1) an unnamed list of slot names or 2) a named list, with the names specifying the slot names and the elements specifying the class of object contained in the slot. If 1), all slots will contain \code{list}.}
#'         }
#'     }
#'     \subsection{Usage}{
#'         \code{make.custom_dataset_object( dataset_class, ... )}
#'     }
#'     \subsection{Arguments}{
#'         \describe{
#'             \item{\code{dataset_class}}{A character string specifying the name of the custom \code{dataset} class.}
#'             \item{\code{...}}{A named set of arguments that fills up the object. The arguments should be named as the slot name to which the argument pertains to.}
#'         }
#'     }
#' }
#'
#' @seealso
#' \code{\link{analysis_object}} For analogous examples for the usage of \code{set.custom_dataset} and \code{make.custom_dataset_object}.
#' \code{\link{import_medpc_to_UOA}}
#' @export set.custom_dataset

set.custom_dataset = function( class_name, slotnames ){

    if ( is.null( names( slot_names ) ) ){
        slots = rep( list( "list" ), length( slot_names ) )
        names( slots ) = slot_names
    }

    if ( !is.null( names( slot_names ) ) ){
        slots = slot_names
    }

    x = list( Class = class_name, slots, contains = "dataset" )
    names( x ) = c("Class", "slots", "contains" )

    do.call( "setClass", x )
}

#' @rdname class.dataset
#' @export make.custom_dataset_object

make.custom_dataset_object = function( dataset_class, ... ){
    new( dataset_class, ... )
}
