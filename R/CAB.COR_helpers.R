#### COR helpers ####

#' Catania's Operant Reserve functions
#'
#' A list of the functions in the \code{CAB} package's implementation of Catania's Operant Reserve. See \link{CAB.COR} for more details.
#'
#' Stuff
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#' @rdname COR_helpers
#' @seealso \link{CAB.COR}


#' @rdname COR_helpers
#' @aliases DOR
#'
#' @section DOR functions:{
#'     A delay-of-reinforcement gradient. Replenishes the reserve.
#'     \subsection{\code{COR.linear_DOR}}{
#'         A linear DOR.
#'     }
#'     \subsection{Usage}{
#'         \code{COR.linear_DOR( DOR_max, DOR_scale, IRI_resp_times, reserve_value )}
#'     }
#'     \subsection{Arguments}{
#'         \describe{
#'             \item{\code{DOR_max}}{A numeric giving the maximum value of the DOR.}
#'             \item{\code{DOR_scale}}{A numeric that is proportional to the slope of the DOR.}
#'             \item{\code{IRI_resp_times}}{A numeric vector of response times in the most recent inter-reinforcement interval}
#'             \item{\code{reserve_value}}{A numeric giving the reserve value.}
#'         }
#'     }
#'     \subsection{Details}{
#'         The reserve value is defined on [0,1] for our COR implementaiton. Therefore, \code{COR.linear_DOR} will never return a value larger than 1.
#'     }
#'     \subsection{Value}{
#'         Returns the new value of the reserve after applying the DOR.
#'     }
#' }
#'
#' @export COR.linear_DOR

COR.linear_DOR = function( DOR_max, DOR_scale, IRI_resp_times, reserve_value ){
    IRI_resp_times = IRI_resp_times[ !is.nan(IRI_resp_times) ]
    time_since_food = IRI_resp_times[ length( IRI_resp_times ) ] - IRI_resp_times
    DOR_val = ( DOR_max - time_since_food/DOR_scale )
    min( sum( DOR_val * ( DOR_val > 0 ) ) + reserve_value, 1 )
}

#' @rdname COR_helpers
#' @aliases depletion
#'
#' @section Depletion functions:{
#'     A reserve depletion function. Depletes the reserve.
#'     \subsection{\code{COR.constant_depletion}}{
#'         A constant depletion function.
#'     }
#'     \subsection{Usage}{
#'         \code{COR.constant_depletion( depletion_constant, reserve_value )}
#'     }
#'     \subsection{Arguments}{
#'         \describe{
#'             \item{\code{depletion_constant}}{A numeric giving the depletion of the DOR.}
#'             \item{\code{reserve_value}}{A numeric giving the reserve value.}
#'         }
#'     }
#'     \subsection{Details}{
#'         The reserve value is defined on [0,1] for our COR implementaiton. Therefore, \code{COR.constant_depletion} will never return a value smaller than 0.
#'     }
#'     \subsection{Value}{
#'         Returns the new value of the reserve after applying the depletion function.
#'     }
#' }
#'
#' @export COR.constant_depletion

COR.constant_depletion = function( depletion_constant, reserve_value ){
    max( reserve_value - depletion_constant, 0 )
}

#' @rdname COR_helpers
#' @aliases initial_reserve
#'
#' @section Inital reserve functions:{
#'     Specify the initial reserve value.
#'     \subsection{\code{COR.initial_reserve_constant}}{
#'         A constant initial reserve value.
#'     }
#'     \subsection{Usage}{
#'         \code{COR.initial_reserve( initial_reserve_constant )}
#'     }
#'     \subsection{Arguments}{
#'         \describe{
#'             \item{\code{initial_reserve_constant}}{A numeric giving the initial reserve value.}
#'         }
#'     }
#'     \subsection{Details}{
#'         The reserve value is defined on [0,1] for our COR implementaiton. Therefore, \code{COR.initial_reserve_constant} should be in [0,1].
#'     }
#'     \subsection{Value}{
#'         Returns the initial reserve value.
#'     }
#' }
#'
#' @export COR.initial_reserve

COR.initial_reserve = function( initial_reserve_constant ){
    initial_reserve_constant
}

#' @rdname COR_helpers
#' @aliases COR.exponential_vi
#'
#' @section Reinforcement schedule functions:{
#'     Control the delivery of reinforcement
#'     \subsection{\code{COR.exponential_vi}}{
#'         A variable-interval schedule where the inter-reinforcement intervals are arranged by sampling from an Exponential(1/rate) distribution.
#'     }
#'     \subsection{Usage}{
#'         \code{COR.exponential_vi( inter_rft_interval, time )}
#'     }
#'     \subsection{Arguments}{
#'         \describe{
#'             \item{\code{inter_rft_interval}}{A numeric giving the average inter-reinforcement interval.}
#'             \item{\code{time}}{A numeric giving the time.}
#'         }
#'     }
#'     \subsection{Value}{
#'         Samples a new inter-reinforcement interval and adds that to the current \code{time}. Therefore, returns the time when the next reinforcement will be primed.
#'     }
#'     \subsection{\code{COR.shifted_exponential_vi}}{
#'         A variable-interval schedule where the inter-reinforcement intervals are arranged by sampling from a shifted Exponential(1/rate) distribution.
#'     }
#'     \subsection{Usage}{
#'         \code{COR.exponential_vi( inter_rft_interval, time )}
#'     }
#'     \subsection{Arguments}{
#'         \describe{
#'             \item{\code{inter_rft_interval}}{A numeric giving the average inter-reinforcement interval.}
#'             \item{\code{time}}{A numeric giving the time.}
#'             \item{\code{shift}}{Numeric. Shift time.}
#'         }
#'     }
#'     \subsection{Value}{
#'         Samples a new inter-reinforcement interval and adds that to the current \code{time}. Therefore, returns the time when the next reinforcement will be primed.
#'     }
#' }
#'
#' @export COR.exponential_vi
#' @export COR.shifted_exponential_vi

COR.exponential_vi = function( inter_rft_interval, time ){
    stats::rexp( 1, 1/inter_rft_interval ) + time
}

COR.shifted_exponential_vi = function( inter_rft_interval, time, shift ){
    stats::rexp( 1, 1/inter_rft_interval ) + time + shift
}


#' @rdname COR_helpers
#' @aliases COR.G_E_emission
#'
#' @section Behaviour emission functions:{
#'     Emit responses from the reserve.
#'     \subsection{\code{COR.G_E_emission}}{
#'         A function that emits responding based on the transformation from a Geometric distribution to an Exponential distribution.
#'     }
#'     \subsection{Usage}{
#'         \code{COR.G_E_emission( reserve_value, time, min_irt )}
#'     }
#'     \subsection{Arguments}{
#'         \describe{
#'             \item{\code{reserve_value}}{A numeric giving the reserve value.}
#'             \item{\code{time}}{A numeric giving the time.}
#'             \item{\code{min_irt}}{A numeric that is the minimum inter-response time.}
#'         }
#'     }
#'     \subsection{Details}{
#'         Under standard implementations of COR ( see \link{CAB.COR} ), the reserve gives the probability of responding at a given time point. Therefore, the distribution of how many time intervals to wait until the next response is a Geometric(p) distribution. We transform the Geometric distribution to an Exponential distribution so that responses can be emitted in continuous time. The distribution of each inter-response time is \deqn{ t ~ Exponential( rate = -log(1-reserve) ) + min_IRT } where \eqn{t} is the emitted inter-response time.
#'     }
#'     \subsection{Value}{
#'         Returns the time at which the next response will occur.
#'     }
#' }
#'
#' @export COR.G_E_emission

COR.G_E_emission = function( reserve_value, time, min_irt ){
    prob_to_rate = -log( 1 - reserve_value )
    stats::rexp( 1, prob_to_rate ) + min_irt + time
}

#' @rdname COR_helpers
#' @aliases rft_duration
#'
#' @section Reinforcement delivery functions:{
#'     Deliver reinforcement.
#'     \subsection{\code{COR.rft_duration}}{
#'         A function that accounts for the time associated with a reinforcement delivery.
#'     }
#'     \subsection{Usage}{
#'         \code{COR.rft_duration( time, food_duration )}
#'     }
#'     \subsection{Arguments}{
#'         \describe{
#'             \item{\code{time}}{A numeric giving the time.}
#'             \item{\code{food_duration}}{A numeric that is the minimum inter-response time.}
#'         }
#'     }
#'     \subsection{Value}{
#'         Returns the time after the reinforcement delivery
#'     }
#' }
#'
#' @export COR.rft_duration

COR.rft_duration = function( time, food_duration ){
    time + food_duration
}
