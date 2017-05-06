#### Calculate local rates ####

#' @include event_record.R RcppExports.R
NULL

#' Compute local rates
#'
#' \code{compute.local_rates} is a method for calculating the within-session local rate of some variable. Currently, methods are defined for the classes \code{analysis_object} and its child class \code{simulation_analysis_object}.
#'
#' @param data Either an object of class \code{simulation_analysis_object}, \code{analysis_object}, or \code{dataset}.
#' @param x_event A variable with which to calculate the local rates over.
#' @param marker A variable with which to calculate the local rates within.
#' @param x_offset The duration of the \code{x_event}. Defaults to 0.
#' @param marker_offset The duration of the \code{marker_offset}. Defaults to 0.
#'
#' @details
#' \subsection{\code{x_offset} and \code{marker_offset}}{
#' If the \code{x_event} or the \code{marker} event have non-zero durations, the \code{x_offset} and \code{marker_offset} allow for the correction of the \code{x_event}s in each bin. For example, if the local response rate is defined by the reinforcement deliveries and the reinforcement delivery lasts 3 seconds, set \code{marker_offset = -3} to subtract out the reinforcement time.
#' }
#' \subsection{Exceptions}{
#' In situations where there is only one \code{marker} event, \code{compute.local_rates} returns a single \code{Inf}. This would be the case if the user were to calculate the inter-reinforcement response times, but only one reinforcement delivery occurred during a particular simulation.
#' }
#'
#' \subsection{Method for the \code{dataset} class}{
#' When \code{compute.local_rates} is called on a \code{dataset} class, it calls the appropriate \code{compute.local_rates} for each element in the \code{analysis_objects} slot
#' }
#'
#' @examples
#' #Suppose "d" is your data in an "analysis_object", "resp_time" is the response time, "rft_time" is the reinforcement time, reinforcement is delivered for 3 units of time and the operandum takes 1 unit of time to operate.
#' #Compute the local response rate accounting for the time to make a response and the time taken up by reinforcment
#' local_resp_rate = compute.local_rates( d, x_event = "resp_time", marker = "rft_time", x_event_offset = -1, marker_offset = -3 )
#'
#' @seealso
#' \code{\link{class.analysis_object}} For constructing arguments for \code{data} parameter.
#' @rdname compute.local_rates
#' @aliases local_rates
#' @aliases local_counts
#' @exportMethod compute.local_counts

setGeneric( "compute.local_counts", function( data, event_name, marker, event_offset = 0, marker_offset ) standardGeneric( "compute.local_counts" ) )

# setMethod( "compute.local_counts", signature( data = "ragged_event_record", event_name = "character", marker = "character" ),
#     function( data, event_name, marker, event_offset, marker_offset ){
#         event_times = data@events[[event_name]]
#         marker_times = data@events[[marker]]
#         if ( length( marker_times ) <= 2 ) return( Inf )
#         if ( length( event_times ) <= 1 ) return( Inf )
#         CAB_cpp_local_count_helper_ragged_event_record( event_times, marker_times, event_offset, marker_offset )
#     }
# )
#
# setMethod( "compute.local_counts", signature( data = "formal_event_record", event_name = "character", marker = "character" ),
#     function( data, event_name, marker, event_offset, marker_offset ){
#         if ( nrow( data@events ) <= 2 ) return( Inf )
#         n_markers = sum( data@events$event == marker )
#         if ( n_markers <= 1 ) return( Inf )
#         CAB_cpp_local_count_helper_formal_event_record( data = data@events, event = event_name, marker = marker, event_offset, marker_offset, n_markers = n_markers )
#     }
# )

setMethod( "compute.local_counts", signature( data = "ragged_event_record", event_name = "character", marker = "character" ),
    function( data, event_name, marker, event_offset, marker_offset ){
        event_times = data@events[[event_name]]
        marker_times = data@events[[marker]]
        if ( length( marker_times ) < 2 | length( event_times ) <= 1 ) return( list( local_times = Inf, visit_lengths = Inf ) )
        local_data = CAB_cpp_local_times_ragged_event_record( event_times, marker_times, event_offset )
        if ( !missing(marker_offset) ){
            local_data$local_times  = local_data$local_times - marker_offset
            local_data$visit_lengths = local_data$visit_lengths - marker_offset
        }
        local_data
    }
)

setMethod( "compute.local_counts", signature( data = "formal_event_record", event_name = "character", marker = "character" ),
    function( data, event_name, marker, event_offset, marker_offset ){
        if ( nrow( data@events ) < 2 ) return( list( local_times = Inf, visit_bins = Inf ) )
        n_markers = sum( data@events$event == marker )
        if ( n_markers <= 1 ) return( list( local_times = Inf, visit_bins = Inf ) )
        local_data = CAB_cpp_local_times_formal_event_record( data = data@events, event = event_name, marker = marker, event_offset, n_markers = n_markers )
        if ( !missing(marker_offset) ){
            local_data$local_times = local_data$local_times - marker_offset
            local_data$visit_lengths = local_data$visit_lengths - marker_offset
        }
        local_data
    }
)

#' @rdname compute.local_rates
#' @exportMethod compute.local_rates

setGeneric( "compute.local_rates", function( local_times, visit_lengths, n_bins, bin_size ) standardGeneric( "compute.local_rates" ) )

#####
# local_rates_helper.formal_event_record = function( data, event, marker, event_offset, marker_offset, n_bins, bin_size, n_markers ){
#     inter_marker_event_times = CAB_cpp_local_rate_helper_formal_event_record( data@events, event, marker , event_offset, marker_offset, n_markers )
#
#     int_div_local_times = inter_marker_event_times$local_times %/% bin_size
#     marker_indices = inter_marker_event_times$interior_markers - inter_marker_event_times$interior_markers[1]
#
#     bins = vapply( 1:(length(marker_indices)-1), function(x){
#         index = ( marker_indices[x]+1 ):( marker_indices[x+1] )
#         tabulate( int_div_local_times[ index ], n_bins )
#         }, FUN.VALUE = 1:n_bins )
#
#     visits = apply( bins, 2, function(x) max(which( x > 0 )) )
#     visits_per_bin = vapply( 1:n_bins, function(x) sum( visits >= x ), FUN.VALUE = 1 )
#     local_rates = rowSums(bins) / visits_per_bin
#     local_rates[ is.nan(local_rates) ] = 0
#     local_rates
# }
#
# local_rates_helper.ragged_event_record = function( event_times, marker_times, event_offset, marker_offset, n_bins, bin_size ){
#
#     if ( length( marker_times ) <= 2 ) return( Inf )
#     if ( length( event_times ) <= 1 ) return( Inf )
#
#     inter_marker_event_times = CAB_cpp_local_rate_helper_ragged_event_record( event_times, marker_times, event_offset, marker_offset )
#     int_div_local_times = inter_marker_event_times$local_times %/% bin_size
#     marker_indices = inter_marker_event_times$interior_markers
#
#     bins = vapply( 1:(length(marker_indices)-1), function(x){
#         index = ( marker_indices[x]+1 ):marker_indices[x+1]
#         tabulate( int_div_local_times[ index ], n_bins )
#         }, FUN.VALUE = 1:n_bins )
#
#     visits = apply( bins, 2, function(x) max(which( x > 0 )) )
#     visits_per_bin = vapply( 1:n_bins, function(x) sum( visits >= x ), FUN.VALUE = 1 )
#     local_rates = rowSums(bins) / visits_per_bin
#     local_rates[ is.nan(local_rates) ] = 0
#     local_rates
# }
#####
local_rate_bin_helper = function( local_times, visit_lengths, max_bins, bin_resolution ){
    local_bins = CAB_cpp_local_binning( local_times, visit_lengths, max_bins, bin_resolution )
    local_bins$local_rate = local_bins$response_bins / local_bins$visit_bins
    local_bins$local_rate[ is.nan(local_bins$local_rate) ] = 0
    local_bins$bin_names = seq( 0, max_bins ) * bin_resolution
    local_bins
}

setMethod( "compute.local_rates", signature( local_times = "numeric", visit_lengths = "numeric", n_bins = "numeric", bin_size = "numeric" ),
    function( local_times, visit_lengths, n_bins, bin_size ){
        if ( Inf %in% local_times ) return( list( visit_bins = Inf, response_bins = Inf, local_rate = rep( Inf, n_bins+1), bin_names = seq( 0, n_bins )*bin_size ) )
        local_rate_bin_helper( local_times, visit_lengths, n_bins, bin_size )
    }
)

# setMethod( "compute.local_rates", signature( data = "formal_event_record", event_name = "character", marker = "character", n_bins = "numeric", bin_size = "numeric" ),
#     function( data, event_name, marker, event_offset, marker_offset, n_bins, bin_size ){
#
#         if ( nrow( data@events ) <= 2 ) return( Inf )
#         n_markers = sum( data@events$event == marker )
#         if ( n_markers <= 1 ) return( Inf )
#         local_rates_helper.formal_event_record( data, event_name, marker, event_offset, marker_offset, n_bins, bin_size, n_markers )
#     }
# )
#
# setMethod( "compute.local_rates", signature( data = "ragged_event_record", event_name = "character", marker = "character", n_bins = "numeric", bin_size = "numeric" ),
#     function( data, event_name, marker, event_offset, marker_offset, n_bins, bin_size ){
#
#         event_times = data@events[[event_name]]
#         marker_times = data@events[[marker]]
#
#         if ( length( marker_times ) <= 2 ) return( Inf )
#         if ( length( event_times ) <= 1 ) return( Inf )
#
#         local_rates_helper.ragged_event_record( event_times, marker_times, event_offset, marker_offset, n_bins, bin_size )
#     }
# )
