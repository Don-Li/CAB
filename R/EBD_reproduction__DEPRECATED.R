# #### EBD reproduction ####
#
# #' @include EBD_pop.R EBD_sample.R EBD_fitness_def.R EBD_fitness_fx.R EBD_parent_sampler.R
# NULL
#
# #' Evolutionary Behaviour Dynamics Reproduction
# #'
# #' Do this later
# #'
# #' @exportMethod EBD_reproduction
#
# setGeneric( "EBD_reproduction", function( pop, parents, method, prob = 0.5 ) standardGeneric( "EBD_reproduction" ) )
#
# setMethod( "EBD_reproduction", signature( pop = "EBD_pop", parents = "matrix" ),
#     function(pop, parents, method, prob){
#         if ( method  == "bitwise" ){
#             return( bitwise_reproduction( pop, parents, prob ) )
#         }
#     }
# )
#
#
# bitwise_reproduction = function( pop, parents, prob ){
#     size = pop@info$size
#     selecton = rbinom( n = size*nrow(parents), size = 1, prob = prob )
#     x = parents[ (size*nrow(parents))*(1-selection) + 1:(size*nrow(parents)) ]
#     dim(x) = c( nrow(parents), size )
#     x
# }
