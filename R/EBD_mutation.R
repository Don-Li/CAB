#### EBD mutation ####

#' @include EBD_pop.R
NULL

#' Evolutionary Behaviour Dynamics mutation
#'
#' Do this later
#'
#' @exportMethod EBD_mutation

setGeneric( "EBD_mutation", function( pop, size ,method, ... ) standardGeneric( "EBD_mutation" ) )

setMethod( "EBD_mutation", signature( pop = "EBD_pop" ),
    function( pop, size, method, ... ){
        dot_args = list( ... )
        if ( method == "w_gaussian" ){
            return( wrapped_gaussian_mutation( pop = pop, size = size, sd = dot_args$sd ) )
        }
    }
)

wrapped_gaussian_mutation = function( pop, size, sd ){
    domain = pop@info$domain
    mut_index = sample( 1:pop@info$size, size, replace = F )
    mutants = floor( rnorm( size, mean = pop@phenotype$phenotype[ mut_index ], sd = sd ) ) %% (max(domain)+1)
    list( mutants = mutants, mutant_index = mut_index )
}
