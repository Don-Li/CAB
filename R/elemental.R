#### Elemental parent class ####

#' @include e_show.R elemental_get_helpers.R
NULL

#' The \code{elemental} parent class
#'
#' In the CAB package, objects that contain particular functions for a model, such as the delay-of-reinforcement gradient in Catania's Operant Reserve (Catania, 2005; see also \code{\link{DOR}}) are referred to as \code{elemental} objects. The class \code{elemental} is the parent class from which all of the child \code{elemental} classes inherit from.
#'
#' \code{elemental} objects contain functions but without any associated parameters. To hold parameters, a \code{control} object is used, which contains both a function from an \code{elemental} object and its associated parameters. A specific model (see \code{\link{class.model}}) is a collection of \code{control} objects and some other objects to control the iteration of the model through its associated conditions.
#'
#' @slot name A character string that gives the name of the \code{elemental} object.
#' @slot type A character string containing \code{"elemental"}.
#'
#' @seealso
#' \code{\link{class.DOR}} For an implementation of the delay-of-reinforcement functions in Catania's Operant Reserve.
#'
#' \code{\link{class.dep}} For an implementation of the depletion functions in Catania's Operant Reserve.
#'
#' \code{\link{class.initial_reserve}} For an implementation of the initial_reserve functions in Catania's Operant Reserve.
#'
#' \code{\link{class.emission}} For an implementation of the emission functions in Catania's Operant Reserve.
#'
#' @references
#' Catania, A. C. (2005). The operant reserve: A computer simulation in (accelerated) real time. Behavioural Processes, 69(2), 257-278. \link{https://doi.org/10.1016/j.beproc.2005.02.009}
#'
#' @rdname class.elemental
#' @export class.elemental

class.elemental = setClass( "elemental", slots = list( name = "character", type = "character" ) )
