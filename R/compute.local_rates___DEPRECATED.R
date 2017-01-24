# #### Calculate local rates ####
#
# #' @include analysis_object.R dataset.R
# NULL
#
# #' Compute local session rates
# #'
# #' \code{compute.local_rates} is a method for calculating the within-session local rate of some variable. Currently, methods are defined for the classes \code{analysis_object} and its child class \code{simulation_analysis_object}.
# #'
# #' @param data Either an object of class \code{simulation_analysis_object}, \code{analysis_object}, or \code{dataset}.
# #' @param x_event A variable with which to calculate the local rates over.
# #' @param marker A variable with which to calculate the local rates within.
# #' @param x_offset The duration of the \code{x_event}. Defaults to 0.
# #' @param marker_offset The duration of the \code{marker_offset}. Defaults to 0.
# #' @param domain A numeric vector of length 2 specifying the range of the inter-marker interval to bin the \code{x_event}s. Defaults to \code{"NONE"}. See details.
# #' @param resolution A numeric specifying the resolution of the bins. Defaults to \code{"NONE"}. See details.
# #' @param rate A logical specifying whether the counts of \code{x_event} should be converted into rates. Defaults to \code{TRUE}.
# #'
# #' @details
# #' \subsection{\code{x_offset} and \code{marker_offset}}{
# #' If the \code{x_event} or the \code{marker} event have non-zero durations, the \code{x_offset} and \code{marker_offset} allow for the correction of the \code{x_event}s in each bin. For example, if the local response rate is defined by the reinforcement deliveries and the reinforcement delivery lasts 3 seconds, set \code{marker_offset = -3} to subtract out the reinforcement time.
# #' }
# #' \subsection{\code{domain} and \code{resolution}}{
# #'     \describe{
# #'         \item{\code{domain = "NONE"} and \code{resolution = "NONE"}}{
# #'             The inter-marker time bins are calculated with the \code{\link{pretty}} function with the minimum and maximum \code{x_event} times since the last \code{marker} event.
# #'         }
# #'         \item{\code{domain = c(x,y)}, \code{resolution = "NONE"}}{
# #'             Where \code{c(x,y)} is a numeric vector of length 2. The inter-marker time bins are calculated with bin-size = 1.
# #'         }
# #'         \item{\code{domain = "NONE"} and \code{resolution = x}}{
# #'             Where \code{x} is a numeric. The inter-marker time bins are calculated by defining a sequence from the \code{floor{min_y}} and a little bit more than \code{ceiling{max_y}}, where \code{y} is the \code{x_event} time since the last \code{marker} event, with the bin size being \code{ resolution = x }.
# #'         }
# #'         \item{\code{domain = c(x,y)} and \code{resolution = z}}{
# #'              Where \code{c(x,y)} is a numeric vector of length 2 and \code{z} is a numeric. The inter-marker time bins are calculated with \code{seq(x, y, z )}.
# #'         }
# #'     }
# #' }
# #'
# #' @examples
# #' #Suppose "d" is your data in an "analysis_object", "resp_time" is the response time, "rft_time" is the reinforcement time, reinforcement is delivered for 3 units of time and the operandum takes 1 unit of time to operate.
# #' #Also bin the responses into bins of size 0.5 from time = 0 to time = 50
# #' #Compute the local response rate accounting for the time to make a response and the time taken up by reinforcment
# #' local_resp_rate = compute.local_rates( d, x_event = "resp_time", marker = "rft_time", x_event_offset = -1, marker_offset = -3, domain = c(0, 50), resolution = 0.5, rate = T )
# #'
# #' @seealso
# #' \code{\link{pretty}}
# #' \code{\link{seq}}
# #' \code{\link{class.analysis_object}} For constructing arguments for \code{data} parameter..
# #'
# #' @rdname compute.local_rates
# #' @aliases local_rates
# #' @exportMethod compute.local_rates
#
# compute.local_rates.analysis_object = function( data, x_event, marker, x_offset = 0, marker_offset = 0, domain = "NONE", resolution = "NONE", rate = T, compact = F ){
#     local_rate_helper( data$time[ data$event %in% marker ], data$time[ data$event %in% x_event ],  domain, resolution, rate, compact )
# }
#
# break_helper = function(domain, resolution, x_range){
#     if ( resolution == "NONE" && domain == "NONE" ){
#         return( pretty( x_range ) )
#     }
#     if ( is.numeric(domain) & is.numeric(resolution) ){
#         return( seq( domain[1], domain[2], resolution ) )
#     }
#     if ( is.numeric(domain) ){
#         return( seq( domain[1], domain[2] ) )
#     }
#     if ( is.numeric(resolution) ){
#         left = floor( x_range[1] )
#         right = ceiling( x_range[2] )
#         return( seq( from = left, to = (ceiling( right/resolution )+1)*resolution , by = resolution ) )
#     }
# }
#
# local_rate_helper = function( marker_times, event_times, x_offset, marker_offset, domain, resolution, rate, compact ){
#
#     event_between_marker = findInterval( event_times, marker_times, left.open = T )
#     marker_ends = !event_between_marker %in% c(0, length(marker_times))
#     event_times = event_times[ marker_ends ]
#     event_between_marker = event_between_marker[ marker_ends ]
#
#     iri_assignment = rle( event_between_marker )
#     start_times = rep( marker_times[ -length(marker_times)], times = iri_assignment$lengths )
#
#     reset_event_times = event_times - start_times
#     if ( ! marker_offset %in% 0 ) reset_event_times = reset_event_times + marker_offset
#     if ( ! x_offset %in% 0 ){
#         x_durations = duplicated( event_between_marker ) * x_offset
#         reset_event_times = reset_event_times + x_durations
#     }
#     if ( compact == T ) return( reset_event_times )
#
#     bins = break_helper( domain, resolution, range( reset_event_times ) )
#     tabs = table( cut( reset_event_times, bins, right = F ), exclude = NULL )
#     midpoints = (bins[-1] + bins[ -length(bins) ])/2
#
#     if ( rate ){
#         return( list( event_time_since_marker = reset_event_times, inter_marker_interval = event_between_marker ,bins = bins, midpoints = midpoints, counts = tabs, local_rate = tabs/(midpoints[2]-midpoints[1]) ) )
#     }
#     else return( list( event_time_since_marker = reset_event_times, inter_marker_interval = event_between_marker ,bins = bins, midpoints = midpoints, counts = tabs ) )
# }
#
# setGeneric( "compute.local_rates", function( data, x_event, marker, x_offset = 0, marker_offset = 0, domain = "NONE", resolution = "NONE", rate = T, compact = F ) standardGeneric( "compute.local_rates" ) )
#
# setMethod( "compute.local_rates", signature( data = "analysis_object" ),
#     function( data, x_event, marker, x_offset, marker_offset, domain, resolution, rate, compact ){
#         local_rate_helper( data@analysis_object$time[ data$event %in% marker ], data@analysis_object$time[ data$event %in% x_event ],  domain, resolution, rate, compact )
#     }
# )
#
# #' @rdname compute.local_rates
# #' @exportMethod compute.local_rates
#
# compute.local_rates.sim_analysis_object = function( data, x_event, marker, x_offset = 0, marker_offset = 0, domain = "NONE", resolution = "NONE", rate = T, compact = F ){
#     local_rate_helper( data[[ marker ]], data[[ x_event ]], x_offset, marker_offset, domain, resolution, rate, compact )
# }
#
# setMethod( "compute.local_rates", signature( data = "simulation_analysis_object" ),
#     function( data, x_event, marker, x_offset, marker_offset, domain, resolution, rate, compact ){
#         local_rate_helper( data@input_list[[ marker ]], data@input_list[[ x_event ]], x_offset, marker_offset, domain, resolution, rate, compact )
#     }
# )
#
#
#
