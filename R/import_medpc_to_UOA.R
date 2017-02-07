#### import data ####

#' @include analysis_object.R dataset.R
NULL

#' Import data from Med-PC to a \code{UOA_analysis_object}
#'
#' \code{import_medpc_to_UOA} is a function for importing data from MED-PC backup files into a list \code{UOA_analysis_object}s and an associated list of meta data.
#'
#' @param partial_file_name A string that specifies a partial file name. The \code{import_medpc_to_UOA} function searches through a directory for all files that match the (partial) file name.
#' @param variable_arrays A nested list. At the first level, a named list specifying the name of the array that is recorded in MED-PC. At the second level, the array indices that are associated with the elements of the array. Defaults to \code{NULL}.
#' @param event_arrays A nested list. At the first level, a named list specifying the name of the array that is recorded in MED-PC. At the second level, the event codes that are associated with the elements of the array. Defaults to \code{NULL}.
#' @param general_arrays A one-level list. The names of each element should correspond to the arrays that are recorded in MED-PC. Each element should be a name describing what that array is recording. Defaults to \code{NULL}.
#' @param file_path A string specifying the file path for \code{partial_file_name}. Defaults to \code{NULL}, which uses the current working directory.
#'
#' @details
#' In the event array, it is assumed that each event is recorded as a number. The leading values of the event should correspond to some event. The trailing values should correspond to the time at which an event occurred. For example, let 9000000 indicate a response and let 1320 be the number of seconds at which a response occurred. The event would then be recorded as 9001320.
#'
#' The \code{import_medpc_to_UOA} function processes the event numbers from the MED-PC backup files and splits them into an event-time data frame. This is done by figuring out the multiple of 10 on which the events are recorded with \code{floor( log( event_array[1], 10 )}. Hence, problems will arise if the first value in the MED-PC event vector is 0. The time associated with each event is obtained by the remainder of the quotient of the event number and the event code multipled by the appropriate power of 10.
#'
#' @examples
#' #Suppose that our file path is "F:/Dropbox/Don/bird_data/vi_15"
#' file_path = "F:/Dropbox/Don/bird_data/vi_15"
#'
#' #Suppose that all our files are of the name "Backup(...)"
#' partial_file_name = "Backup"
#'
#' #Suppose that MED-PC recorded the response and reinforcement counts in an array named "C" and response counts are recorded in index 0 of "C" and reinforcement counts were recorded in index 1.
#' c_name = list( resp = 0, rft = 1)
#' variable_arrays = list( C = c_name )
#'
#' #Suppose that MED-PC recorded the events in an array named "X" and responses are marked with 100000, reinforcement marked with 200000, start marked with 900000
#' x_constants = list( resp = 1, rft = 2, start = 9 )
#' event_arrays = list( X = x_constants )
#'
#' #Suppose that MED-PC recorded the arranged variable-intervals for reinforcement delivery in an array "J" and we wish to have it named "arranged_vi" in our "UOA_analysis_object".
#' general_arrays = list( J = "arranged_vi" )
#'
#' #All together
#' import_medpc_to_UOA( partial_file_name, variable_arrays, event_arrays, general_arrays, file_path )
#'
#' @seealso class.analysis_object
#'
#' @export import_medpc_to_UOA

import_medpc_to_UOA = function( partial_file_name, variable_arrays = NULL, event_arrays = NULL, general_arrays = NULL, file_path = NULL ){

    if ( !is.null( file_path ) ){
        original_wd = getwd()
        setwd( file_path )
    }
    wd_files = list.files()
    partial_file_matches = wd_files[ startsWith( wd_files, partial_file_name ) ]

    analysis_objects = lapply( partial_file_matches, mpc_read_helper, variable_arrays = variable_arrays, event_arrays = event_arrays, general_arrays = general_arrays )
    meta_data = as.data.frame( t( vapply( analysis_objects, function(x) x@meta_data, FUN.VALUE = as.list( 1:9 )) ), stringsAsFactors = F )
    setwd( original_wd )

    new( "dataset", analysis_objects = analysis_objects, meta_data = meta_data )
}

mpc_read_helper = function(partial_file_match, variable_arrays, event_arrays, general_arrays){
    slot_names = slotNames( "UOA_analysis_object" )
    new.args = vector( "list", length( slot_names ) )
    names( new.args ) = slot_names
    new.args[ "Class" ] = "UOA_analysis_object"

    data = scan( partial_file_match, what = "character", sep = "\n", skip = 1 )
    data = data.table::tstrsplit( data, split = ": " )

    meta_data = as.list( data[[2]][ 1:9 ] )
    names( meta_data ) = data[[1]][ 1:9 ]
    attributes( meta_data )$class = "data.frame"
    attributes( meta_data )$row.names = as.integer( 1 )

    arrays = c( variable_arrays, event_arrays, general_arrays )
    array_names = paste( names( arrays ), ":", sep = "" )
    indices = match( array_names, data[[1]] )
    ordering = order( indices )

    grab_indices = c( indices[ordering], length(data[[1]])+1)
    data_arrays = lapply( 1:length(indices), function(x) as.numeric( data[[2]][ ( grab_indices[x]+1 ):( grab_indices[x+1] - 1) ] ) )
    names( data_arrays ) = names( arrays )[ordering]

    variable_ar = list( data_arrays[[ names(variable_arrays) ]] )
    names( variable_ar ) = names( variable_arrays )

    new( "UOA_analysis_object",
        variable_arrays = variable_ar,
        general_arrays = data_arrays[ names( general_arrays ) ],
        analysis_object = mpc_process_event_array( data_arrays[[ names( event_arrays ) ]], event_arrays ),
        meta_data = meta_data )
}

mpc_process_event_array = function( event_array, event_arrays ){
    num_base = 10^floor( log( event_array[1], 10 ) )
    codes = event_array %/% num_base
    event_list = list( "event" = 1, "time" = 1 )
    event_list$event = names( unlist( event_arrays[[1]] ) )[ match( codes, unlist( event_arrays, use.names = F ) ) ]
    event_list$time = event_array - codes*num_base
    attributes( event_list )$class = "data.frame"
    attributes( event_list )$row.names = 1:length(event_list$event)
    event_list
}
