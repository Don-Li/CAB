#### Sampling utilities ####

#' For generating random samples
#'
#' Documentation for samplers.
#'
#' @rdname CAB_samplers
#' @name CAB_samplers

#' @rdname CAB_samplers
#' @name CAB_samplers
#' @aliases CAB_srswo
#'
#' @section \code{srswo}:{
#'     Simple random sampling without replacemant and uniform selection probabilities
#'     \subsection{Usage}{
#'         \code{srswo( choose, repeats, sample_size )}
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

#' @rdname CAB_samplers
#' @name CAB_samplers
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

NULL
