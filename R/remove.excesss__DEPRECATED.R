#
#
#
# setGeneric( "remove.excess.input_monitor", function( time_dimension, input_monitor_name = NULL ) standardGeneric( "remove.excess.input_monitor" ) )
#
# setMethod( "remove.excess.input_monitor", signature( input_monitor = "sim_input" ),
#     function( time_dimension, input_monitor_name ){
#         if ( is.null( input_monitor_name ) ){
#             end_time = max( input_monitor@input[[ time_dimension ]], na.rm = T )
#             lapply( names( input_monitor ), function( x ){
#                 input_monitor@input[[ x ]][ input_monitor@input[[ x ]] > end_time ] <<- NaN
#             }
#         }
#         if ( !is.null( input_monitor_name ) ){
#             txt1 = paste( "max(", input_monitor_name, "@input[[", time_dimension, "]], na.rm = T )" )
#             end_time = eval( parse( text = txt1 ) )
#             txt2 = paste( input_monitor_name, "@input[[ x ]][", input_monitor_name, "@input[[ x ]] > end_time ] <<- NaN" )
#             lapply( names( get( input_monitor_name, globalenv() ) ), function( x ){
#                 txt2
#             }
#         }
#     }
# )
