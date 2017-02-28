#### Reset a model object ####

#' @include model.CAB.R
NULL

#' Do this later
#'
#' Do this later
#'
#' @exportMethod reset_model

setGeneric( "reset_model", function( model, organism_params ) standardGeneric( "reset_model" ) )

setMethod( "reset_model", signature( model = "CAB.model", organism_params = "list" ),
    function( model, organism_params ){
        reset_model_helper( model, organism_params )
    }
)

reset_model_helper = function( model, organism_params ){
    list2env( x = organism_params, envir = EBD_model@organism )
}

