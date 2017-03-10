# #### model.run ####
#
# #' Run a model
# #'
# #' Run through all of the conditions of a simulation and store them in a list.
#
#
#
# model.run_helper = function( model_procedure, model_object ){
#     iterations = nrow( model_object@iterator )
#     data_list = vector( "list" )
#     for ( i in 1:iterations ) data_list[i] = model_procedure(i)
# }
