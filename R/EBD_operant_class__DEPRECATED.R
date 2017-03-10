# #### EBD operant classes ####
#
# #' @include EBD_pop.R
# NULL
#
# #' Evolutionary Behaviour Dynamics Operant Class class
# #'
# #' In Evolutionary Behaviour Dynamics (McDowell, 2004), the behaviours that an organism is capable of emitting is represented as a set of integers on some domain (e.g. from 0 to 1023). This behavioural domain is divided into subsets to represent different operant classes (from the experimenter's perspective). For example, we may define [0,100] as one operant class and [101,1023] as another operant class. We may then set up an experiment where the EBD algrotihm is reinforced for a response to the first operant class of [0,100] on a variable interval schedule.
# #'
# #' The definition of the operant classes (from the organism's perspective) is contained in a \code{EBD_operant_class} object that inherits from the \code{elemental} class.. This object will contain a function that makes integers (genotypes) to operant classes.
# #'
# #' The way to put a \code{EBD_operant_class} in your model is as follows:
# #' \enumerate{
# #'     \item Make a list of vectors, each of length 2, containing the boundaries of each class.
# #'     \item Call the \code{make.EBD_operant_class} function.
# #' }
# #'
# #' @slot EBD_operant_class This will contain an operant class function that maps integers to operant classes.
# #' @slot name The name of the \code{operant_class} object. Inherited from \code{elemental}.
# #' @slot type This will be \code{elemental} because the \code{operant_class}.
# #'
# #' @rdname EBD_operant_class
# #' @export make.EBD_operant_class
# #'
# #' @examples
# #' # Define the operant classes:
# #' class_bounds = list( c(0,40), c(41, 511), c(512, 552), c(553, 1023) )
# #' # Make an "EBD_operant_class" object:
# #' int_to_class = make.EBD_operant_class( class_bounds, "pop_class" )
# #' # Test that it returns the correct class:
# #' plot( matrix( c( 0:1023, int_to_class@EBD_operant_class( 0:1023 ) ), ncol = 2 ) )
# #' abline( v = unlist( class_bounds ) )
# #'
# #' @references
# #' McDowell, J. J. (2004). A computational model of selection by consequences. Journal of the Experimental Analysis of Behavior, 81(3), 297–317. \link{https://doi.org/10.1901/jeab.2004.81-297}
#
# class.EBD_operant_class = setClass( "EBD_operant_class", slots= list( EBD_operant_class = "function" ), contains = "elemental" )
#
# #### Make an EBD_operant_class object ####
#
# make.EBD_operant_class = function( class_bounds, name ){
#     if ( !is.list(class_bounds) ) stop("Enter class_bounds as 'list'")
#     if ( !is.character( name ) ) stop( "Enter EBD_operant_class name as 'character'")
#     lower_bounds = vapply( class_bounds, function(x) x[1], FUN.VALUE = 1 )
#     o_class_fx = function( behaviour ){
#         findInterval( behaviour, lower_bounds )
#     }
#     new( "EBD_operant_class" ,EBD_operant_class = o_class_fx, name = name, type = "elemental" )
# }
