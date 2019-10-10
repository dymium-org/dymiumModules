# It is recommended to assign this module to a variable called: event_demography_cohabit
# for example: event_demography_cohabit <- modules::use('modules/demography/cohabit.R')
modules::import('dymiumCore')
modules::export('^^run|^util|^test')

#' Cohabit
#'
#' @param object a dymium agent class object
#' @param model a model object or a list of model objects
#' @param target a positive integers or a list of positive integers
#' @param time_steps positive integer()
#'
#' @return object
run <- function(object, model = NULL, target = NULL, time_steps = NULL) {

  # early return if `time_steps` is not the current time
  if (!dymiumCore::is_scheduled(time_steps)) {
    return(invisible(object))
  }

  # uncomment the line belows if the event doesn't require `model`
  # eg. If the event is deterministic like ageing.
  # if (!is.null(model)) {
  #   lg$warn('`model` will not be used.')
  # }

  # uncomment the line belows if the event doesn't require `target`
  # eg. If the event is to be applied to all agents.
  # if (!is.null(target)) {
  #   lg$warn('`target` will not be used.')
  # }

  # (Recommended)
  # create a reference to the main agent object for easy access eg:
  # PopObj <- assign_reference(object, Pop)

  # (Recommended)
  # create a reference to ModelContainer for easy access eg:
  # ModObj <- assign_reference(object, ModelContainer)

  # TODO: Target object
  # create a reference to TargetContainer (Not yet implemented) for easy access
  # TargetObj <- assign_reference(object, TargetContainer)

  # return the first argument (`object`) to make event functions pipe-able.
  invisible(object)
}

# private utility functions (.util_*) -------------------------------------
.util_function <- function(x) {}

TransitionEventname <-
  R6::R6Class(classname = 'TransitionEventname',
              inherit = dymiumCore::Transition,
              public = list(

              ))

# exported utility functions (util_*) -------------------------------------
util_function <- function(x) {}

