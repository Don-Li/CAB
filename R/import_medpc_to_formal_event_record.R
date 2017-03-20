#### import data ####

#' @include event_record.R
NULL

#' Import data from Med-PC to \code{formal_event_record}s
#'
#' \code{import_medpc_to_UOA} is a function for importing data from MED-PC backup files into a list \code{UOA_analysis_object}s and an associated list of meta data.
#'
#' @param partial_file_name A string that specifies a partial file name. The \code{import_medpc_to_UOA} function searches through a directory for all files that match the (partial) file name.
#' @param variable_arrays A nested list. At the first level, a named list specifying the name of the array that is recorded in MED-PC. At the second level, the array indices that are associated with the elements of the array. Defaults to \code{NULL}.
#' @param event_arrays A nested list. At the first level, a named list specifying the name of the array that is recorded in MED-PC. At the second level, the event codes that are associated with the elements of the array. Defaults to \code{NULL}.
#' @param general_arrays A one-level list. The names of each element should correspond to the arrays that are recorded in MED-PC. Each element should be a name describing what that array is recording. Defaults to \code{NULL}.
#' @param file_path A string specifying the file path for \code{partial_file_name}. Defaults to \code{NULL}, which uses the current working directory.
#' @param precision The number of decimal places to round the data. Defaults to \code{NULL} for no rounding. Rounding is recommended to avoid integer under/overflow.
#'
#' @details
#' In the event array, it is assumed that each event is recorded as a number. The leading values of the event should correspond to some event. The trailing values should correspond to the time at which an event occurred. For example, let 9000000 indicate a response and let 1320 be the number of seconds at which a response occurred. The event would then be recorded as 9001320.
#'
#' The \code{import_medpc_to_UOA} function processes the event numbers from the MED-PC backup files and splits them into an event-time data frame. This is done by figuring out the multiple of 10 on which the events are recorded with \code{floor( log( event_array[1], 10 )}. Hence, problems will arise if the first value in the MED-PC event vector is 0. The time associated with each event is obtained by the remainder of the quotient of the event number and the event code multipled by the appropriate power of 10.
#'
#' @examples
#'
#' variable_arrays = list( C = c("resp","rft") )
#' event_arrays = list( X = c("resp_time" = 1, "rft_time" = 2, "start" = 9 ) )
#' general_arrays = list( J = "arranged_vi" )
#' file_path = "F:/Dropbox/Don/model_data"
#'
#' data = import_medpc_to_formal_event_record( "Backup", variable_arrays, event_arrays, general_arrays, file_path = "F:\\Dropbox\\Don\\model_data", rounding = 2)
#'
#'
#'
#' @export import_medpc_to_formal_event_record

mpc_backup_reader = function( datatable, file_match, variable_arrays, event_arrays, general_arrays, rounding, row ){
    processing_arrays = c( variable_arrays, event_arrays, general_arrays )

    data = scan( file_match, what = "character" ,sep = "\n", skip = 1, strip.white = T )

    meta_data = data.table::tstrsplit( data[ 1:9 ], ": " )
    meta_data_contents = as.list( trimws( meta_data[[2]] ) )
    names( meta_data_contents ) = meta_data[[1]]
    set( datatable, i = row, j = names( meta_data_contents ), value = meta_data_contents )

    expt_data = data.table::tstrsplit( data[ -(1:9) ], ":" )
    array_header_indices = match( names(processing_arrays), expt_data[[1]] )
    array_ordering = order( array_header_indices )

    array_bound_indices = c( array_header_indices[array_ordering], length(expt_data[[1]])+1)

    data_arrays = lapply( 1:length( array_header_indices ),
        function(x){
            as.numeric( expt_data[[2]][ (array_bound_indices[x]+1 ):( array_bound_indices[x+1] - 1 ) ] )
        }
    )
    names( data_arrays ) = names( processing_arrays )[ array_ordering ]

    if ( !is.null( variable_arrays ) ){
        decompose_variable_array( variable_arrays, datatable, row, data_arrays, rounding )
    }

    if ( !is.null( general_arrays ) ){
        set_general_array( general_arrays, datatable, row, data_arrays, rounding )
    }

    if ( !is.null( event_arrays ) ){
        event_array = data_arrays[[ names( event_arrays ) ]]
    }

    set( datatable, i = row, j = "event_record", value = list( list( mpc_process_event_array( event_array, event_arrays, rounding ) ) ) )
}

decompose_variable_array = function(variable_arrays, datatable, row, data_arrays, rounding){
    lapply( 1:length(variable_arrays),
        function(x){
            if ( is.null(rounding) ){
                z = data_arrays[[ names(variable_arrays)[x] ]][ 1:length(variable_arrays[[x]]) ]
            } else{
                z = round( data_arrays[[ names(variable_arrays)[x] ]], rounding )[ 1:length(variable_arrays[[x]]) ]
            }
            set( datatable, i = row, j = variable_arrays[[x]], value = as.list(z) )
            NULL
        }
    )
}

set_general_array = function( general_arrays, datatable, row, data_arrays, rounding ){
    lapply( 1:length(general_arrays),
        function(x){
            if ( is.null( rounding ) ){
                z = data.table( data_arrays[[ names( general_arrays )[x] ]] )

            } else{
                z = data.table( round( data_arrays[[ names( general_arrays )[x] ]], rounding ) )
            }
            setnames( z, "V1", general_arrays[[x]] )
            set( datatable, i = row, j = general_arrays[[x]], value = list( list( z ) ) )
#            datatable[ i = row, general_arrays[[x]] := list( list( z ) ) ]
        }
    )
}

mpc_process_event_array = function( event_array, event_arrays, rounding ){
    number_base = 10^floor( log10( event_array[1] ) )
    event_codes = event_arrays[[1]]

    event_labels = names( event_codes )[ match( event_array %/% number_base, event_codes ) ]

    if ( is.null( rounding ) ){
        event_times = event_array %% number_base
    } else{
        event_times = round( event_array %% number_base, rounding )
    }

    event_record = data.table::data.table( time = event_times, event = event_labels )
    variables = names( event_codes )
    lengths = nrow( event_record )

    new( "formal_event_record", events = event_record, variables = variables, lengths = lengths )
}

import_medpc_to_formal_event_record = function( partial_file_name, variable_arrays = NULL, event_arrays = NULL, general_arrays = NULL, file_path = NULL, rounding = NULL ){

    if ( !is.null( file_path ) ){
        original_wd = getwd()
        setwd( file_path )
    }
    wd_files = list.files()
    partial_file_matches = wd_files[ startsWith( wd_files, partial_file_name ) ]

    datatable = data.table( dummy = rep( NA, length( wd_files ) ) )

    lapply( 1:length( wd_files ),
        function(x){
            cat( "Reading file:", x, "\n" )
            mpc_backup_reader( datatable, partial_file_matches[x], variable_arrays, event_arrays, general_arrays, rounding = rounding, row = x )
            NULL
        }
    )

    if ( !is.null( file_path ) ) setwd( original_wd )
    datatable
}
