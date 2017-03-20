# #### Storing data ####
#
# #' @include analysis_object.R
# NULL
#
# #' Store data from a simulation
# #'
# #' The data from a simulation is typically stored in a \code{simulation_analysis_object} object, which is typically constructed by the \code{tidy.simulation_data} function. There are two methods for storing the data from an experiment: \code{store.csv} for storing the data in a \code{.csv} folder and \code{store.wd} for storing the \code{simulation_analysis_object} in the global environment.
# #'
# #' @usage store.csv( data, model_object, iterate, csv_name )
# #' @usage store.global( data, model_object, iterate, object_name )
# #'
# #' @param data A \code{simulation_analysis_object}.
# #' @param model_object A \code{model} object.
# #' @param iterate A numeric corresponding to which iteration of the condition list generated the data. Defaults to \code{NULL}. Either \code{iterate} or \code{csv_name}/\code{object_name} must be specified.
# #' @param object_name For \code{store.global}. A string specifying the variable label that the data will be stored as in the global environment. Defaults to \code{NULL}. See details.
# #' @param csv_name For \code{store.csv}. A character specifying the file name for the \code{.csv} file. Defaults to \code{NULL}. See details.
# #'
# #' @details
# #' When writing to a \code{.csv} file with \code{store.csv}, the \code{.csv} file will contain:
# #'
# #' 1) An \eqn{n x 2} matrix of strings with the first column listing the \code{control} objects required in the simulation and the second column containing strings that specify th parameters for the corresponding \code{control} object.
# #'
# #' 2) An \eqn{n x 2} matrix of the data in time-event format.
# #'
# #' By default, the name of the file will be "(model_object_name)_iterate_X.csv", where X is the iterate.
# #'
# #' When storing the data in the global environment with \code{store.global}, the  \code{simulation_analysis_object} is stored directly in the global environment with the name specified by \code{object_name} or the default "(model_object_name)_iterate_X" if \code{object_name} is unspecified.
# #' In general, when conducting a large scale simulation, it is not advised to use \code{store.global} because you will probably not have enough RAM to store everything that you want.
# #'
# #' @seealso
# #' \code{\link{class.analysis_object}} for \code{simulation_analysis_object}.
# #'
# #' @rdname store_data
# #' @exportMethod store.csv
# #' @aliases store.csv
#
# conditions.write_string = function( conditions ){
#     condition_params = sapply( conditions, helper.conditions.write_string )
#     data.frame( Controls = names( conditions ), Parameters = condition_params )
# }
#
# helper.conditions.write_string = function( x ){
#     c = paste( names( x ), as.character( x ), sep = ": " )
#     d = paste( c, collapse = " | " )
# }
#
# store.obj_name = function( model_object, iterate, csv = F ){
#     a = paste( class( model_object ), "iterate", iterate, sep = "_" )
#     if ( csv ) paste( a, ".csv", sep = "" )
#     else a
# }
#
# setGeneric( "store.csv", function( data, model_object, iterate = NULL, csv_name = NULL ) standardGeneric( "store.csv" ) )
#
# setMethod( "store.csv", signature( data = "simulation_analysis_object" ),
#     function( data, model_object, iterate, csv_name )
# {
#     if ( is.null( iterate ) & is.null(csv_name) ) stop( "One of 'iterate' or 'csv_name' must be specified" )
#     if ( is.null( csv_name ) ){
#         csv_name = store.obj_name( model_object, iterate, csv = T )
#     }
#     meta_datas = conditions.write_string( data@meta_data )
#     write.table( meta_datas, csv_name, row.names = F, sep = "," )
#     write.table( data@analysis_object, csv_name, row.names = F, append = T, sep = "," )
# }
# )
#
# #' @rdname store_data
# #' @exportMethod store.global
# #' @aliases store.global
#
# setGeneric( "store.global", function( data, model_object, iterate, object_name ) standardGeneric( "store.global" ) )
#
# setMethod( "store.global", signature( data = "simulation_analysis_object"),
#     function( data, model_object, iterate = NULL, object_name = NULL ){
#
#         if ( is.null( iterate ) & is.null(object_name) ) stop( "One of 'iterate' or 'object_name' must be specified" )
#         if ( is.null( object_name ) ){
#             object_name = store.obj_name( model_object, iterate )
#         }
#         assign( object_name, data, envir = globalenv() )
#     }
# )
