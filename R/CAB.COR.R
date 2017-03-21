#### Catania's Operant Reserve stock implementation ####

#' @include event_record.R CAB.COR_helpers.R
NULL

#' Catania's Operant Reserve
#'
#' This is a stock implementation of Catania's Operant Reserve (COR; Catania, 2005). See references for a list papers on COR.
#'
#' In brief, COR is a model where the probability of responding is controlled by a construct referred to as an 'operant reserve'. The emission of responses depletes the value of the reserve. Reinforcement replenishes the reserve based on where reinforcement has occurred within the most recent inter-reinforcement interval. The rule that controls the replenishment to the reserve is the "delay of reinforcement gradient". See Catania (2005) and Berg & McDowell (2011) for more details.
#'
#' Previous implementations of COR simulated responding at each possible discrete time point. That is, the model is asked at each point in time whether or not a response occurred. Our implementation simulates inter-response times rather than times at which responses occurred by asking when the next response will occur. This allows us to save a lot of computational time.
#'
#' @seealso \link{COR_helpers} For functions associated with COR.
#'
#' @references
#' Berg, J. P., & McDowell, J. J (2011). Quantitative, steady-state properties of Catania's computational model of the operant reserve. Behavioural Processes, 87(1), 71-83. \url{https://doi.org/10.1016/j.beproc.2011.01.006}
#'
#' Catania, A. C. (2005). The operant reserve: A computer simulation in (accelerated) real time. Behavioural Processes, 69(2), 257-278. \link{https://doi.org/10.1016/j.beproc.2005.02.009}
#'
#' Li, D., Elliffe, D., & Hautus, M. J. (2017). Pre-Asymptotic Response Rates as a Function of the Delay-of-Reinforcement Gradient Summation for Catania’s Operant Reserve: A Reply to Berg & McDowell (2011). Behavioural Processes. \link{http://dx.doi.org/10.1016/j.beproc.2017.01.002}
#'
#'
#'@name CAB.COR
#'@rdname CAB.COR
#'@aliases COR
