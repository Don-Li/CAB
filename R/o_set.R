#### organism set ####

#' @include model_do.R
NULL

#' Change values in a model
#'
#' In the \code{CAB} package, models are implemented in \code{CAB.model} child classes. The child classes contain slots that hold functions that are necessary for the model. The model child classes also contain a slot called \code{organisms} that contains the necessary parameters. We have two functions for repalcemen, the \code{o_set} method, and the \code{"[<-"} method. Be aware that \code{"[<-"} does not work in the same way as in base \code{R}.
#'
#' @section \code{o_set}:{
#'     For changing values of variables in the \code{organism} slot of \code{CAB.model} objects.
#'     \subsection{Usage}{
#'         \code{o_set( model, variable, value, i, j )}
#'     }
#'     \subsection{Arguments}{
#'         \describe{
#'             \item{\code{model}}{A \code{CAB.model} object.}
#'             \item{\code{variable}}{The name of a variable in the \code{organism} slot.}
#'             \item{\code{value}}{New values.}
#'             \item{\code{i}}{A \code{numeric} vector. Indices for the new values.}
#'             \item{\code{j}}{A \code{numeric} vector. Defaults to \code{NULL}. If not \code{NULL}, \code{index} becomes the rows for a matrix and \code{col} specifies the columns.}
#'         }
#'     }
#'     \subsection{Value}{
#'         Modifies the \code{CAB.model} in place.
#'     }
#' }
#'
#' @section \code{"[<-"}:{
#'     For changing values of variables in the \code{organism} slot of \code{CAB.model} objects.
#'     \subsection{Usage}{
#'         \code{"[<-"(x,v,i,j,value)}
#'     }
#'     \subsection{Arguments}{
#'         \describe{
#'             \item{\code{x}}{A \code{CAB.model} object.}
#'             \item{\code{v}}{The name of a variable in the \code{organism} slot.}
#'             \item{\code{value}}{New values.}
#'             \item{\code{i}}{Optional. A \code{numeric} vector. Indices for the new values. See notes.}
#'             \item{\code{j}}{Optional. A \code{numeric} vector. See notes. }
#'         }
#'     }
#'     \subsection{Value}{
#'         Modifies the \code{CAB.model} in place.
#'     }
#'     \subsection{Notes}{
#'         If the variable being modified is a \code{matrix}, the argument \code{i} will change the ith row if \code{j} is missing. If \code{i} and \code{j} are specified, the ijth element of the matrix will be changed.
#'         If the variable is a \code{numeric} vector, the argument \code{i} will change the ith element. Specifying \code{j} will return an error.
#'         Do not use the syntax \code{x[v,i,j] <- value}. For classes in base \code{R}, these would be equivalent, but defining custom methods for \code{"[<-"} has produced strange results. In using the \code{x[v,i,j] <- value} syntax, the assignment will occur at the wrong level and you will destroy your model object. Use the "[<-"(x,v,i,j,value) syntax for desired results.
#'         If both \code{i} and \code{j} are missing, a replacement of the variable in the \code{organism} slot is done. If the class of the new object is different from that of the object originally in the variable, then an error is thrown.
#'         MORE NOTES: The standard indexing x[i,j,whatever] now works. I have not updated the documentation for it.
#'     }
#' }
#'
#' @rdname o_set
#' @aliases CAB.repalcement
#' @seealso
#' \code{\link{model_do}} for an example of usage.
#' \code{\link{set_param}} for setting all parameters.
#'
#' @exportMethod o_set

setGeneric( "o_set", function( model, variable, value, i, j ) standardGeneric( "o_set" ) )

setMethod( "o_set", signature( model = "CAB.model", variable = "character", value = "ANY", i = "numeric", j = "numeric" ),
    function( model, variable, value, i, j ){
        reference = model@organism
        reference[[variable]][i,j] = value
    }
)

setMethod( "o_set", signature( model = "CAB.model", variable = "character", value = "ANY", i = "numeric", j = "missing" ),
    function( model, variable, value, i ){
        reference = model@organism
        reference[[variable]][i] = value
    }
)

setMethod( "o_set", signature( model = "CAB.model", variable = "character", value = "ANY", i = "missing", j = "missing" ),
    function( model, variable, value ){
        reference = model@organism
        reference[[variable]] = value
    }
)

#'
#' #' @rdname o_set
#' #' @exportMethod "[<-"
#'
#' setMethod( "[<-", signature( x = "CAB.model", i = "numeric", j = "numeric", value = "ANY" ),
#'     function( x, v, i, j, value ){
#'         x@organism[[v]][i,j] <- value
#'         x
#'     }
#' )
#'
#' setMethod( "[<-", signature( x = "CAB.model", i = "numeric", j = "missing", value = "ANY" ),
#'     function( x, v, i, j, value ){
#'         if ( is.matrix( x@organism[[v]] ) ){
#'             x@organism[[v]][i,] <- value
#'             return( x )
#'         }
#'         if ( is.numeric( x@organism[[v]] ) ){
#'             x@organism[[v]][i] <- value
#'             return( x )
#'         }
#'     }
#' )
#'
#' setMethod( "[<-", signature( x = "CAB.model", i = "missing", j = "numeric", value = "ANY" ),
#'     function( x, v, i, j, value ){
#'         x@organism[[v]][,j] <- value
#'         x
#'     }
#' )
#'
#' setMethod( "[<-", signature( x = "CAB.model", i = "missing", j = "missing", value = "ANY" ),
#'     function( x, v, value ){
#'         x@organism[[v]] <- value
#'         x
#'     }
#' )
