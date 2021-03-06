#### EBD helpers ####

#' @include RcppExports.R
NULL

#' McDowell's Evolutionary Behaviour Dynamics
#'
#' A list of functions in the \code{CAB} package's implementation of McDowell's Evolutionary Behaviour Dynamics. A list of lower level utility functions, such as for converting integers in base 10 to base 2 can be found in \link{EBD_utilities}. See \link{CAB.EBD} for more details.
#'
#' Stuff.
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
#' @rdname EBD_helpers
#' @seealso \link{CAB.EBD}


#' @rdname EBD_helpers
#' @aliases fitness_function
#'
#' @section Fitness functions:{
#'     Computes the fitness of parents.
#'     \subsection{\code{EBD.wrapped_si_fitness}}{
#'         Wrapped specific individual fitness. The fitness value of a behaviour is \eqn{max( |i-k|, m - |i-k| )}, where \eqn{i} is the base 10 value of a behaviour in the population, \eqn{k} is the integer value of the reinforced behaviour, and \eqn{m} is the maximum base 10 value that a behaviour can take. It is wrapped because it is modularised around the domain \eqn{[0,m]}.
#'     }
#'     \subsection{Usage}{
#'         \code{EBD.wrapped_si_fitness( domain, phenotypes, last_resp )}
#'     }
#'     \subsection{Arguments}{
#'         \describe{
#'             \item{\code{domain}}{A vector specifying the minimum and maximum base 10 values that a behaviour can take.}
#'             \item{\code{phenotypes}}{A numeric vector of behaviours in base 10.}
#'             \item{\code{}}{}
#'         }
#'     }
#'     \subsection{Value}{
#'         Returns a numeric vector of the same length as \code{phenotyes}. Indices will match.
#'     }
#' }
#' ############################################################################
#' @export EBD_WSI_fitness

EBD_WSI_fitness  = function( max_phenotype, phenotypes, last_resp ){
    maximum = max_phenotype+1
    abs_diff = abs( phenotypes - last_resp )
    wrap_this = abs_diff > as.integer( maximum/2 )
    abs_diff[ wrap_this ] = maximum - abs_diff[ wrap_this ]
    abs_diff
}

#' @rdname EBD_helpers
#'
#' @section Response emission functions:{
#'     Emits a response.
#'     \subsection{\code{EBD.response_emission}}{
#'         Emits a behaviour.
#'     }
#'     \subsection{Usage}{
#'         \code{EBD.response_emission( preallocated_resp_index, n_rft, time, rft_ticks, phenotypes )}
#'     }
#'     \subsection{Arguments}{
#'         \describe{
#'             \item{\code{preallocated_resp_index}}{A numeric value.}
#'             \item{\code{n_rft}}{Number of reinforcement that occurred during a session.}
#'             \item{\code{time}}{Number of time ticks.}
#'             \item{\code{rft_ticks}}{Duration of reinforcment delivery.}
#'             \item{\code{phenotypes}}{A numeric vector of behaviours in base 10}.
#'         }
#'     }
#'     \subsection{Details}{
#'         The way that EBD is implemented in \code{CAB} is that the index of the responses are preallocated at the start of the simulation. This reduces the number of function calls to a random number generator and hence improves the speed of the model. The response index is recovered from the number of time ticks that the simulation has been run, \code{time}, accounting for the time ticks that were taken up by reinforcement deliveries.
#'     }
#'     \subsection{Value}{
#'         Returns a value selected from \code{phenotyes}.
#'     }
#' }
#' ############################################################################
#' @export EBD.response_emission

EBD.response_emission = function( preallocated_resp_index, tick , phenotypes ){
    phenotypes[ preallocated_resp_index[ tick + 1 ] ]
}

#' @rdname EBD_helpers
#'
#' @section Get the operant class:{
#'     From a behaviour in base 10, get the operant class.
#'     \subsection{\code{EBD.get_oc}}{
#'         Get the operant class
#'     }
#'     \subsection{Usage}{
#'         \code{EBD.get_oc( last_resp, oc_lower_bounds )}
#'     }
#'     \subsection{Arguments}{
#'         \describe{
#'             \item{\code{last_resp}}{A numeric value.}
#'             \item{\code{oc_lower_bounds}}{Lower bounds of the operant classes.}
#'         }
#'     }
#'     \subsection{Value}{
#'         Returns the operant class that the last response belongs in.
#'     }
#' }
#' ############################################################################
#' @export EBD.get_oc

EBD.get_oc = function( last_resp, oc_lower_bounds ){
    sum( last_resp >= oc_lower_bounds )
}

#' @rdname EBD_helpers
#'
#' @section Stock reinforcement schedule:{
#'     Common reinforcement schedules.
#'     \subsection{\code{EBD.geometric_vi}}{
#'         A VI schedule with geometrically distributed arranged inter-reinforcement intervals.
#'     }
#'     \subsection{Usage}{
#'         \code{EBD.geometric_vi( inter_rft_interval, min_irt, time )}
#'     }
#'     \subsection{Arguments}{
#'         \describe{
#'             \item{\code{inter_rft_interval}}{Numeric. In real time.}
#'             \item{\code{min_irt}}{Minimum inter-response time, i.e. time ticks for the algorithm.}
#'             \item{\code{time}}{Numeric}
#'         }
#'     }
#'     \subsection{Value}{
#'         Generates a Geometrically distributed inter-reinforcement interval by doing a continuous to discrete transformation of the inter-rft-interval and adds the time to return the time at which food will be available.
#'     }
#'     \subsection{\code{EBD.shifted_geometric_vi}}{
#'         A VI schedule with shifted geometrically distributed arranged inter-reinforcement intervals.
#'     }
#'     \subsection{Usage}{
#'         \code{EBD.geometric_vi( inter_rft_interval, min_irt, time, shift )}
#'     }
#'     \subsection{Arguments}{
#'         \describe{
#'             \item{\code{inter_rft_interval}}{Numeric. In real time.}
#'             \item{\code{min_irt}}{Minimum inter-response time, i.e. time ticks for the algorithm.}
#'             \item{\code{time}}{Numeric.}
#'             \item{\code{shift}}{Numeric. In real time. }
#'         }
#'     }
#'     \subsection{Value}{
#'         Generates a Geometrically distributed inter-reinforcement interval by doing a continuous to discrete transformation of the inter-rft-interval and adds the time to return the time at which food will be available.
#'     }
#' }
#' ############################################################################
#' @export EBD.geometric_vi
#' @export EBD.shifted_geometric_vi

EBD.geometric_vi = function( inter_rft_interval, min_irt, time ){
    stats::rgeom( 1, 1-exp(-1/inter_rft_interval) )/min_irt + 1 + time
}

EBD.shifted_geometric_vi = function( inter_rft_interval, min_irt, time, shift  ){
    stats::rgeom( 1, 1-exp(-1/inter_rft_interval) )/min_irt + 1 + time + shift / min_irt
}

# #' @rdname EBD_helpers
# #'
# #' @section Parental sampling functions:{
# #'     Functions for selecting parents.
# #'     \subsection{\code{EBD.fit_parents}}{
# #'         Select parents with the following algorithm:
# #'         \enumerate{
# #'             \item Create a subpopulation comprised of behaviours with the first unique phenotype.
# #'             \item Select a father weighted by the fitness values.
# #'             \item Select a mother given that the mother cannot be the father.
# #'             \item Repeat.
# #'         }
# #'         This can be shown to be mathematically equivalent to the following algorithm used in McDowell (2013; \url{https://doi.org/10.1037/a0034244}):
# #'         \enumerate{
# #'             \item Sample a fitness value from a fitness function.
# #'             \item Loop through the population until a behaviour with the sampled fitness is found. Set this to be the father. If not, go to 1.
# #'             \item Sample a fitness value from a fitness function.
# #'             \item Loop through the population until a behaviour with the sampled fitness if found. Set this to be the mother. If not, go to 3.
# #'             \item Repeat.
# #'         }
# #'         Our sampling implementation is more efficient on average. When the behaviours have fitness values close to the mean of the fitness function, the second algorithm is marginally faster. However, when behaviours are far away, the first algorithm is significantly faster.
# #'     }
# #'     \subsection{Usage}{
# #'         \code{EBD.fit_parents( pop_size, n_bits, fitness, fitness_weights, genotypes )}
# #'     }
# #'     \subsection{Arguments}{
# #'         \describe{
# #'             \item{\code{pop_size}}{Numeric.}
# #'             \item{\code{n_bits}}{How long each genotype is.}
# #'             \item{\code{fitness}}{A vector of fitness values.}
# #'             \item{\code{fitness_weights}}{A vector of fitness weights.}
# #'             \item{\code{genotypes}}{A matrix of genotypes.}
# #'         }
# #'     }
# #'     \subsection{Value}{
# #'         Returns a list containing a matrix of father genotypes and one for mother genotypes.
# #'     }
# #'
# #'     \subsection{\code{EBD.random_parents}}{
# #'         Select parents at random.
# #'     }
# #'     \subsection{Usage}{
# #'         \code{random_parents( pop_size, genotypes )}
# #'     }
# #'     \subsection{Arguments}{
# #'         \describe{
# #'             \item{\code{pop_size}}{Numeric.}
# #'             \item{\code{genotypes}}{A matrix of genotypes.}
# #'         }
# #'     }
# #'     \subsection{Value}{
# #'         Returns a list containing a matrix of father genotypes and one for mother genotypes.
# #'     }
# #' }
# #' @export EBD.fit_parents
# #' @export EBD.random_parents


# EBD.fit_parents = function( pop_size, n_bits, fitness, fitness_weights, genotypes ){
#
#     unique_fitnesses = not_duplicated( fitness )
#     father_fitnesses = sample( fitness[unique_fitnesses], size = pop_size, replace = T, prob = fitness_weights[unique_fitnesses] )
#     father_index = match( father_fitnesses, fitness )
#
#     mother_index = vapply( father_index, function(x){
#         candidate_fitnesses = fitness[-x]
#         unique_candidates = not_duplicated( candidate_fitnesses )
#         mother_fitness = sample( candidate_fitnesses[unique_candidates], 1, prob = fitness_weights[-x][unique_candidates] )
#         mother_index = match( mother_fitness, candidate_fitnesses )
#         (1:pop_size)[-x][mother_index]
#     }, FUN.VALUE = 1 )
#
#     list( fathers = genotypes[ , father_index ], mothers = genotypes[ , mother_index ] )
# }
#
# EBD.random_parents = function( pop_size, genotypes ){
#     z = CAB.srswo_2( pop_size, pop_size )
#     list( fathers = genotypes[ ,z[1,] ], mothers = genotypes[ ,z[2,] ] )
# }

#' @rdname EBD_helpers
#'
#' @section Mutation:{
#'     Mutate behaviours in the population.
#'     \subsection{\code{EBD.w_gaussian_mutation}}{
#'         Mutate behaviours my resampling their phenotypes from a wrapped normal distribution.
#'     }
#'     \subsection{Usage}{
#'         \code{EBD.w_gaussian_mutation( tick, preallocated_mutant_change, preallocated_mutant_index, max_phenotype, phenotypes )}
#'     }
#'     \subsection{Arguments}{
#'         \describe{
#'             \item{\code{tick}}{Numeric. The number of iterations of the algorithm.}
#'             \item{\code{preallocated_mutant_change}}{Matrix giving how much each phenotype changes.}
#'             \item{\code{preallocated_mutant_index}}{Matrix giving the phenotypes that will change.}
#'             \item{\code{max_phenotype}}{Numeric. The maximum phenotype value.}
#'             \item{\code{phenotypes}}{Vector.}
#'         }
#'     }
#'     \subsection{Details}{
#'         \code{preallocated_mutant_change} is a matrix that has rows the number of iterations to run the algorithm. Each column is the amount to change each phenotype to be mutated on that iteration.
#'         \code{preallocated_mutant_index} is a matrix of corresponding dimensions. Each column is the index of the phenotype to change.
#'     }
#'     \subsection{Value}{
#'         Returns a list containing the new phenotype values and the indices to which they belong.
#'     }
#' }
#' ############################################################################
#' @export EBD.w_gaussian_mutation

EBD.w_gaussian_mutation = function( tick ,preallocated_mutant_change, preallocated_mutant_index, max_phenotype, phenotypes ){
    mut_index = preallocated_mutant_index[ tick +1, ]
    mutants = { phenotypes[ mut_index ] + preallocated_mutant_change[ tick + 1, ] } %% ( max_phenotype+1 )
    list( mutants = as.integer(mutants), mutant_index = as.integer(mut_index) )
}

#' @rdname EBD_helpers
#'
#' @section Fitness weights:{
#'     Sampling weights for sampling fitness values.
#'     \subsection{\code{EBD.geometric_fitness_weights}}{
#'         Sampling weights from a geometric distribution
#'     }
#'     \subsection{Usage}{
#'         \code{EBD.geometric_fitness_weights( fitness, parental_selection_p )}
#'     }
#'     \subsection{Arguments}{
#'         \describe{
#'             \item{\code{fitness}}{Numeric vector of fitness values}
#'             \item{\code{parental_selection_p}}{Parameter for the geometric distribution}
#'         }
#'     }
#'     \subsection{Value}{
#'         Returns a vector of fitness weights.
#'     }
#' }
#' ############################################################################
#' @export EBD.geometric_fitness_weights

EBD.geometric_fitness_weights = function( fitness, parental_selection_p ){
   dgeom( fitness, parental_selection_p )
}







