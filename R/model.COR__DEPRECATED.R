# #### COR model ####
#
# #' @importFrom data.table rbindlist
# #'
# #' @include elemental_get_helpers.R control.R model.R dor.R dep.R emission.R initial_reserve.R stimulus.R termination.R food_duration.R input.R
# NULL
# #' Construction of Catania's Operant Reserve
# #'
# #' Catania's Operant Reserve (COR; Catania, 2005) has been implemented in the \code{CAB} package. This is the documentation for the implementation.
# #'
# #' COR is implemented as the \code{model.COR} class. COR requires the following \code{control} classes: \code{DOR_control}, \code{emission_control}, \code{dep_control}, \code{initial_reserve_control}, \code{stimulus_control}, \code{termination_control}, \code{food_duration_control}. See \code{slot} information.
# #'
# #' In brief, COR is a model where the probability of responding is controlled by a construct referred to as the 'reserve'. The emission of responses depletes the value of the reserve. Reinforcement replenishes the reserve based on where reinforcement has occurred within the most recent inter-reinforcement interval. The rule that controls the replenishment to the reserve is the "delay of reinforcement gradient". See Catania (2005) and Berg & McDowell (2011) for more details.
# #'
# #' Previous implentations of COR simulate the model at each possible discrete time point. That is, the model is asked at each point in time whether or not a response occurred. Our implementation asks when the next response will occur. This allows us to save a lot of computational time.
# #'
# #' @slot DOR_control For the delay-of-reinforcement gradient.
# #' @slot emission_control For the function that relates the reserve value to the time at which the next response will occur.
# #' @slot dep_control For the depletion of the reserve after responding.
# #' @slot initial_reserve_control For the initial level of the reserve.
# #' @slot stimulus_control For the reinforcement schedule.
# #' @slot termination_control For the conditions that terminate the experiment.
# #' @slot food_duration_control For the food duration.
# #' @slot control_list A mandatory slot for models. Inherited from the \code{model} class. The underlying representation is an \code{environment}.
# #' @slot enumerated_conditions A mandatory slot for models. Inherited from the \code{model} class.
# #' @slot iterator A mandatory slot for models. Inherited from the \code{model} class.
# #' @slot input A mandatory slot for models. Contains all of the inputs required to run the necessary functions. Inherited from the \code{model} class.
# #'
# #' @param DOR_control A \code{DOR_control} object.
# #' @param emission_control A \code{emission_control} object.
# #' @param dep_control A \code{dep_control} object.
# #' @param initial_reserve_control A \code{initial_reserve_control} object.
# #' @param stimulus_control A \code{stimulus_control} object.
# #' @param termination_control A \code{termination_control} object.
# #' @param food_duration_control A \code{food_duration_control} object.
# #' @param input An \code{input} object.
# #'
# #' @examples
# #' # Make a 'model.COR' object
# #' # Set up inputs
# #' time_input = make.input( name = "time_input" )
# #' resp_time = make.input( name = "resp_time" )
# #' rft_time = make.input( name = "rft_time" )
# #' reserve = make.input( name = "reserve" )
# #' # Set up input object
# #' my_inputs = make.input( time_input, resp_time, rft_time, reserve )
# #'
# #' # Set up DOR and DOR_control
# #' linear_DOR = make.DOR( DOR_fx = linear_DOR_fx, name = "linear_DOR" )
# #' DOR_params = list( scale = 100, max = c(1, 0.5), "resp_time", "rft_time", "reserve" )
# #' DOR_ctrl = make.control( linear_DOR, DOR_params )
# #'
# #' # Set up dep and dep_control
# #' constant_dep = make.dep( dep_fx = constant_dep_fx, name = "constant_dep" )
# #' dep_params = list( deplete = 0.01, "reserve" )
# #' dep_ctrl = make.control( constant_dep, dep_params )
# #'
# #' # Set up initial_reserve and initial_reserve_control
# #' constant_initial = make.initial_reserve( initial_reserve_fx = constant_initial_fx, name = "constant_initial" )
# #' initial_params = list( initial = 0.75 )
# #' initial_ctrl = make.control( constant_initial, initial_params )
# #'
# #' # Set up stimulus and stimulus_control
# #' vi = make.stimulus( stimulus_fx = true_VI_fx, name = "vi" )
# #' vi_params = list( VI_interval = 2, time  = "time_input" )
# #' schedule_ctrl = make.control( vi, vi_params )
# #'
# #' # Set up emission and emission_control
# #' G_E_emission = make.emission( G_E_emission_fx, name = "G_E_emission" )
# #' emission_params = list( reserve_value = "reserve", time = "time_input", min_IRT = 1 )
# #' emission_ctrl = make.control( G_E_emission, emission_params )
# #'
# #' # Set up termination and termination_control
# #' time_termination = make.termination( termination_fx = time_termination_fx, name = "time_termination" )
# #' termination_params = list( "time", 1000 )
# #' termination_ctrl = make.control( time_termination, termination_params )
# #'
# #' # Set up food_duration and food_duration control
# #' food_duration = make.food_duration( food_duration_fx = constant_food_duration_fx, name = "constant_food_duration" )
# #' food_duration_params = list( food_dur = 3, time = "time_input" )
# #' food_duration_ctrl = make.control( food_duration, food_duration_params )
# #'
# #' # Make the cOR model
# #' my_COR = make.COR( DOR_ctrl, emission_ctrl, dep_ctrl, initial_ctrl, schedule_ctrl, termination_ctrl, food_duration_ctrl, my_inputs )
# #'
# #'
# #' @seealso
# #' \code{\link{class.control}} for \code{control} objects.
# #'
# #' \code{\link{class.DOR}} for \code{DOR} objects.
# #'
# #' \code{\link{class.dep}} for \code{dep} objects.
# #'
# #' \code{\link{class.emission}} for \code{emission} objects.
# #'
# #' \code{\link{class.initial_reserve}} for \code{initial_reserve} objects.
# #'
# #' \code{\link{class.stimulus}} for \code{stimulus} objects.
# #'
# #' \code{\link{class.termination}} for \code{termination} objects.
# #'
# #' \code{\link{class.food_duration}} for \code{food_duration} objects.
# #'
# #' \code{\link{class.input}} for \code{input} objects.
# #'
# #' \code{\link{class.model}} for the \code{model} parent class.
# #'
# #' @references
# #' Berg, J. P., & McDowell, J. J (2011). Quantitative, steady-state properties of Catania's computational model of the operant reserve. Behavioural Processes, 87(1), 71-83. \link{https://doi.org/10.1016/j.beproc.2011.01.006}
# #'
# #' Catania, A. C. (2005). The operant reserve: A computer simulation in (accelerated) real time. Behavioural Processes, 69(2), 257-278. \link{https://doi.org/10.1016/j.beproc.2005.02.009}
# #'
# #' @rdname class.model.COR
# #' @aliases model.COR
# #'
# set.model.COR = setClass( "model.COR", slots = list(
#     DOR_control = "list", emission_control = "list", dep_control = "list",
#     initial_reserve_control = "list", stimulus_control = "list", termination_control = "list",
#     food_duration_control = "list" ),
#     contains = "model" )
#
# #' @rdname class.model.COR
# #' @export make.COR
# make.COR = function( DOR_control, emission_control, dep_control, initial_reserve_control, stimulus_control, termination_control, food_duration_control, input ){
#     model_elements = list( DOR_control,
#         emission_control, dep_control,
#         initial_reserve_control, stimulus_control,
#         termination_control, food_duration_control )
#
#     slot_names = slotNames( "model.COR" )
#     parent_slot_names = slotNames( "model" )
#     elemental_classes = slot_names[ ! slot_names %in% parent_slot_names ]
#
#     control_list = lapply( model_elements, function( x ) x@params )
#     names( control_list ) = elemental_classes
#
#     enumerated_conditions = lapply( control_list, make.enumerated_conditions )
#
#     ranges = lapply( enumerated_conditions, function(x) 1:nrow(x) )
#     iterator = expand.grid(ranges)
#
#     listerinos = lapply( model_elements, get_control_fx )
#     names( listerinos ) = elemental_classes
#
#     new( "model.COR",
#         DOR_control = listerinos$DOR_control,
#         emission_control = listerinos$emission_control,
#         dep_control = listerinos$dep_control,
#         initial_reserve_control = listerinos$initial_reserve_control,
#         stimulus_control = listerinos$stimulus_control,
#         food_duration_control = listerinos$food_duration_control,
#         control_list = list2env( control_list, parent = emptyenv() ),
#         enumerated_conditions = enumerated_conditions,
#         iterator = iterator,
#         termination_control = listerinos$termination_control,
#         input = input )
# }
