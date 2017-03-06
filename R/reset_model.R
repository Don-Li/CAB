#### Reset a model object ####

#' @include model.CAB.R
NULL

#' Do this later
#'
#' Do this later
#'
#' @exportMethod reset_model
#'
#' @rdname set_param
#'
#' @exportMethod set_param

setGeneric( "set_param", function( model, organism_params ) standardGeneric( "set_param" ) )

setMethod( "set_param", signature( model = "CAB.model", organism_params = "list" ),
    function( model, organism_params ){
        reset_model_helper( model, organism_params )
    }
)

reset_model_helper = function( model, organism_params ){
    list2env( x = organism_params, envir = model@organism )
    eval( model@derived_params, model@organism )
}
