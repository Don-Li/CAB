#### do.element ####

#' Call a simulation function
#'
#' Call a simulation function made by \code{\link{make.expt_functions}}.
#'
#' In the \code{CAB} package, closures are used to run simulations. These closures are created by \code{\link{make.expt_functions}}. To run one of these functions, arguments are needed for the parameters that are not currently fixed during a condition. These arguments are contained in the input monitor (see \code{\link{class.sim_input}}). The \code{\link{do.element}} function finds the necessary arguments from the input monitor and calls the specified function.
#'
#' @rdname do.sim_fx
#' @name do.sim_fx
#' @exportMethod do.sim_fx

setGeneric("do.sim_fx", function( sim_fx, input_object ) standardGeneric( "do.sim_fx" ) )

#' @rdname do.sim_fx
#'
#' @usage do.sim_fx( sim_fx, input_object )
#' @param sim_fx An \code{element_fx_closure} object made by \code{make.expt_functions}.
#' @param input_object An \code{sim_input} object.
#'
#' @examples
#' # Following from ?model.COR
#' my_COR = make.COR( DOR_ctrl, emission_ctrl, dep_ctrl, initial_ctrl, schedule_ctrl, termination_ctrl, food_duration_ctrl, my_inputs )
#' # Use the first condition
#' i = 1
#' conditions = get.model_conditions( my_COR, i )
#' expt_functions = make.sim_functions( conditions, my_COR )
#' # Extract the DOR function
#' DOR = expt_functions$DOR_control
#' # Make an input monitor
#' make.input_monitor( my_COR, 20 )
#' # Put some values in
#' access.input_monitor( "rft_time", 1, 10 )
#' access.input_monitor( "resp_time", 1:5, c(1, 4, 5, 6,10 ) )
#' access.input_monitor( "reserve", 1, 0 )
#'
#' do.sim_fx( DOR, input_monitor )
#' # 0.99 0.96 0.95 0.94 0.90
#'
#' @seealso
#' \code{\link{make.expt_functions}} For getting arguments for \code{sim_fx}

setMethod( "do.sim_fx", signature( sim_fx = "element_fx_closure", input_object = "input" ),
    function( sim_fx, input_object ){
        sim_fx@closure(input_object)
    }
)
