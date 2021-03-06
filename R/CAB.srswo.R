# #### simple sampling without replacement ####
#
# # #' @include RcppExports.R
# NULL
#
# # #' Grouped simple random sampling without replacement
# # #'
# # #' These functions are for repeated sampling wtihout replacement. For example, \code{CAB.srswo( size = 50, group_size = 2, n = 100 )} will produce a matrix where each column is a simple random sample without replacement of size 2 from [1,100], with 50 columns for 50 repeated samples. This is equivalent to calling \code{sample( x = 1:100, size = 2, replace = FALSE ) }, but many orders of magnitude faster. See example.
# # #'
# # #' @section \code{CAB.srswo}:{
# # #'     For simple random sampling without replacement.
# # #'     \subsection{Usage}{
# # #'         \code{CAB.srswo( size, group_size, n )}
# # #'     }
# # #'     \subsection{Arguments}{
# # #'         \describe{
# # #'             \item{\code{size}}{Number of repeated samples.}
# # #'             \item{\code{group_size}}{Number of units to be sampled at each sampling repetition.}
# # #'             \item{\code{n}}{The maximum value to be sampled. Samples are drawn from 1:n.}
# # #'         }
# # #'     }
# # #'     \subsection{Value}{
# # #'         Returns a \code{group_size} by \code{size} matrix, where each column represents a sample.
# # #'     }
# # #' }
# # #'
# # #' @section \code{CAB.srswo_2}:{
# # #'     For simplem random sampling wtihout replacement with a fixed group size of 2.
# # #'     \subsection{Usage}{
# # #'         \code{CAB.srswo_2( size, n )}
# # #'     }
# # #'     \subsection{Arguments}{
# # #'         \describe{
# # #'             \item{\code{size}}{Number of repeated samples.}
# # #'             \item{\code{n}}{The maximum value to be sampled. Samples are drawn from 1:n.}
# # #'         }
# # #'     }
# # #'     \subsection{Value}{
# # #'         Returns a \code{2} by \code{size} matrix, where each column represents a sample.
# # #'     }
# # #' }
# # #'
# # #' @section Notes:{
# # #' The underlying implementation of the \code{CAB.srswo} functions is that they generate a vector of discrete uniform values and reshapes them into a matrix where each column is a sample without replacement. The columns that have duplicated values, i.e. where sampling without replacement is not satisfied, are resampled until enough values are obtained.
# # #'
# # #' In comparison, the \code{sample} function employs a nested loop. So, if one wished to sample 2 values from 1:100 without replacement, the first value would be sampled from 1:100 and the second would be sampled from (1:100)[-first_value]. Therefore, when the \code{group_size} parameter is small, it is easy to obtain non-repeated samples, so the sampling without replacement is essentially the same as drawing a (just more than) doubly long vector of uniform random variates. When the \code{group_size} is large, this efficiency breaks down and the conditional sampling of the nested loop is more efficient.
# # #'
# # #' }
# # #'
# # #' @examples
# # #' require( microbenchmark )
# # #' # Get 100 repeated samples by drawing twice from 1:100
# # #' sample1 = CAB.srswo( 100, 2, 100 )
# # #' sample2 = CAB.srswo_2( 100, 100 )
# # #' # This is equivalent to a replicated call to sample
# # #' sample3 = replicate( 100, sample( 1:100, 2 ) )
# # #'
# # #' # Compare efficiency
# # #' microbenchmark(
# # #'     CAB.srswo( 100, 2, 100 ),
# # #'     CAB.srswo_2( 100, 100 ),
# # #'     replicate( 100, sample( 1:100, 2 ) )
# # #' )
# # #' # the CAB functions are a lot faster.
# # #' # Compare efficiency at large group_size values:
# # #' microbenchmark(
# # #'     CAB.srswo( 100, 10, 100 ),
# # #'     replicate( 100, sample( 1:100, 10 ) )
# # #' )
# # #' # CAB.srswo is still faster
# # #' microbenchmark(
# # #'     CAB.srswo( 100, 25, 100 ),
# # #'     replicate( 100, sample( 1:100, 25 ) )
# # #' )
# # #' # CAB.srswo is now slower than sample
# # #'
# # #' @rdname CAB.srswo
# # #' @aliases CAB.srswo_2
# # #' @export CAB.srswo
#
#
# CAB.srswo = function( size, group_size, n ){
#     CAB_cpp_srswo( size, group_size, n )
# }
#
# # #' @export CAB.srswo_2
# # #' @rdname CAB.srswo
#
# CAB.srswo_2 = function( size, n ){
#     CAB_cpp_srswo2( size, n )
# }
