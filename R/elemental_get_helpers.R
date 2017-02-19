#### Elemental get helper functions ####

#' Elemental accessor helper functions
#'
#' An accessor function is a function that retrieves variables from an object. Because most of the \code{CAB} package is programmed in an object-oriented style, accessor functions are used in almost every part of the package. Hence, these accessor functions will be documented here.
#'
#'
#' \describe{
#'     \item{\code{e_get.name}}{Get the name that was assigned to the elemental object.}
#'     \item{\code{e_get.fx}}{Get the elemental function contained in the elemental object.}
#'     \item{\code{get.reserve( reserve_input )}}{Takes a list in the \code{input} object that tracks the reserve value and returns the most recently changed reserve value. Otherwise, takes a numeric.}
#'     \item{\code{get.resps( resp_input )}}{Takes the vector \code{resp_input} and returns the number of elements that are not \code{NAN}.}
#'     \item{\code{get.time( time_input )}}{Takes the vector \code{time_input} and returns a last value that is not \code{NAN}. If all values are \code{NAN}, returns 0.}
#'     \item{\code{get.IRI_responses( resp_times, rft_times )}}{Takes a list in the \code{input} object that tracks the response times within an inter-reinforcement interval. Otherwise, takes a numeric.}
#'     \item{\code{bin2int}}{Takes a matrix of binary strings with each column corresponding to each string and the rows corresponding to the digits of each binary string.}
#'     \item{\code{int2bin}}{Takes a vector of integers with an argument \code{digits} and converts each integer into a corresponding binary string with length of \code{digits}.}
#' }
#'
#' @seealso
#' \code{\link{class.input}} Many of the arguments for the helper functions come from an object of \code{input}.
#'
#' @rdname accessor_helpers
#' @export e_get.name

e_get.name = function( elementary_obj ){
    slot( elementary_obj, "name" )
}

#' @rdname accessor_helpers
#' @export e_get.fx

e_get.fx = function( elementary_obj ){
    slot( elementary_obj, class( elementary_obj ) )
}

#' @rdname accessor_helpers
#' @aliases get.reserve
#' @export get.reserve
get.reserve = function( reserve_input ){
    if ( is.list( reserve_input ) ) x = reserve_input$data[ reserve_input$counts ]
    else x = reserve_input[ length(reserve_input) ]
    if ( length(x) < 1 ) x = -1
    x
}

#' @rdname accessor_helpers
#' @export get.resps
get.resps = function( resp_input ) {
    if ( is.list( resp_input ) ) resp_input$counts
    else sum( !is.nan( resp_input ) )
}

#' @rdname accessor_helpers
#' @export get.time
get.time = function( time_input ) {
    if ( is.list( time_input ) ) x = time_input$data[ time_input$counts ]
    else x = time_input[ length(time_input) ]
    if (length(x)<1) x = -1
    x
}

#' @rdname accessor_helpers
#' @aliases get.IRI_resp_times
#' @export get.IRI_resp_times
get.IRI_resp_times = function( iri_resp_times ){
    if ( is.list( iri_resp_times ) ) iri_resp_times$data[ iri_resp_times$counts ] - iri_resp_times$data[ 1: iri_resp_times$counts ]
    else iri_resp_times
}

#' @rdname accessor_helpers
#' @aliases bin2int
#' @export bin2int
bin2int = function( binaries ){
    drop( crossprod( binaries, 2^(0:(nrow(binaries)-1)) ) )
}

#' @rdname accessor_helpers
#' @aliases int2bin
#' @export int2bin
int2bin = function( integers, digits ){
    vapply( integers, function(x) as.integer( intToBits(x)[1:digits] ), FUN.VALUE = 1:digits)
}

