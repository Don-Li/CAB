#### Sampling utilities ####

#' @include RcppExports.R
NULL

#' For generating random samples
#'
#' Documentation for samplers.
#'
#' @rdname CAB_samplers

#' @rdname CAB_samplers
#' @aliases CAB_srswo
#'
#' @section \code{CAB_srswo}:{
#'     Simple random sampling without replacemant and uniform selection probabilities
#'     \subsection{Usage}{
#'         \code{CAB.srswo( choose, repeats, sample_size )}
#'     }
#'     \subsection{Arguments}{
#'         \describe{
#'             \item{\code{choose}}{Numeric vector of elements to choose from.}
#'             \item{\code{repeats}}{Integer. How many repeats for the sampling.}
#'             \item{\code{sample_size}}{Integer. How many elements chosen per sample.}
#'         }
#'     }
#'     \subsection{Value}{
#'         Returns a \code{matrix} with the draws from each sample along the columns.
#'     }
#' }
#' @export CAB.srswo

CAB.srswo = function( choose, repeats, sample_size ){
    CAB_srswo( choose, repeats, sample_size )
}

#' @rdname CAB_samplers
#' @aliases mcdowell_sampling
#'
#' @section \code{mcdowell_sampling}:{
#'     Sampling used in the supplementary material of McDowell (2013).
#'     \subsection{Usage}{
#'         \code{CAB.mcdowell_sampling( fitness, fitness_weights )}
#'     }
#'     \subsection{Arguments}{
#'         \describe{
#'             \item{\code{fitness}}{Numeric vector of fitness values.}
#'             \item{\code{fitness_weights}}{Numeric vector of fitness weights.}
#'         }
#'     }
#'     \subsection{Value}{
#'         Returns a \code{matrix} with two columns and rows equal to the number of fitnss values. This matrix contains the indices of the values that are sampled. The indexing starts from zero.
#'     }
#' }
#' @export CAB.mcdowell_sampling

CAB.mcdowell_sampling = function( fitness, fitness_weights ){
    mcdowell_sampling( fitness, fitness_weights )
}
