# #### Get simulation conditions from model ####
#
# #' Get the conditions from a \code{model} oject.
# #'
# #' A \code{model} object will have a number of conditions that depends on the factorial combination of all the possible parameters. The \code{get.model_conditions} function is used to get the conditions at a particular factorial combination. The main use of this function is for doing factorial simulations.
# #'
# #' \code{get.model_conditions} is useful for getting an argument for the function \code{\link{make.sim_functions}}.
# #'
# #' @param model_object A model object
# #' @param i A numeric specifying which condition to get
# #'
# #' @examples
# #' # Following from the example in ?class.model.COR
# #' my_COR
# #' get.model_conditions( my_COR, 1 )
# #' get.model_conditions( my_COR, 2 )
# #'
# #' @seealso
# #' \code{\link{class.model.COR}} For the example.
# #'
# #' \code{\link{make.sim_functions}} Where \code{get.model_conditions} is a nice way to specify the \code{conditions} argument for \code{make.sim_functions}.
# #'
# #' @export get.model_conditions
#
#
# get.model_conditions = function( model_object, i ){
#
#     control_names = names( model_object@iterator )
#     condition_list = vector( "list", length( control_names ) )
#     names( condition_list ) = control_names
#
#     for ( j in control_names ){
#         iterator_num = model_object@iterator[[ j ]][i]
#         element_numbers = model_object@enumerated_conditions[[j]][ iterator_num, ]
#         condition_list[[j]] = model_object@control_list[[ j ]][[ element_numbers[,1] ]][ element_numbers[,2], ]
#     }
#
#     condition_list
# }
