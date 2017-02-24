#### EBD parental selection ####

#' @include EBD_pop.R EBD_sample.R EBD_fitness_def.R EBD_fitness_fx.R
NULL

#' Evolutionary Behaviour Parental Selection class
#'
#' Do this later
#'
#' @exportMethod EBD_parent_sampler

setGeneric( "EBD_parent_sampler", function( pop, fitness = NULL, weights = NULL ) standardGeneric( "EBD_parent_sampler" ) )

setMethod( "EBD_parent_sampler", signature( pop = "EBD_pop", fitness = "numeric", weights = "numeric" ),
    function(pop, fitness, weights){
        EBD_get_parents( pop, fitness, weights )
    }
)

setMethod( "EBD_parent_sampler", signature( pop = "EBD_pop", fitness = "missing", weights = "missing" ),
    function(pop){
        EBD_random_parents( pop )
    }
)


EBD_get_parents = function( pop, fitness, weights ){
    size = pop@info$size
    unique_fitnesses = !duplicated( fitness )
    father_fitnesses = sample( unique(fitness), size = size, replace = T, prob = weights[unique_fitnesses] )
    father_index = match( father_fitnesses, fitness )
    fathers = pop@genotype$genotype[ ,father_index ]

    mothers = vapply( father_index, function(x){
        candidate_fitnesses = fitness[-x]
        unique_candidates = !duplicated( candidate_fitnesses )
        mother_fitness = sample( unique( candidate_fitnesses ), 1, prob = weights[-x][unique_candidates] )
        mother_index = match( mother_fitness, candidate_fitnesses )
        pop@genotype$genotype[,-x][,mother_index]
    }, FUN.VALUE = 1:10*1.0 )

    cbind( fathers, mothers )
}

EBD_random_parents = function( pop ){
    z = vapply( 1:pop@info$size, function(x){
        sample( 1:pop@info$size, 2, replace = F )
    }, FUN.VALUE = 1:2*1.0 )

    cbind( pop@genotype$genotype[, z[1,] ], pop@genotype$genotype[, z[2,] ] )
}
