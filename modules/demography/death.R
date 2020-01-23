# It is recommended to assign this module to a variable called: event_demography_death
# for example: event_demography_death <- modules::use('modules/demography/death.R')
# default setup, you may edit the below import statments to match your requirements.
modules::import('dymiumCore')
modules::import('R6', 'R6Class')
modules::import('assertthat', 'assert_that')
modules::expose(here::here('modules/demography/logger.R')) # import lgr's logger. To use the logger use 'lg' (default logger's name).
constants <- modules::use(here::here('modules/demography/constants.R'))
helpers <- modules::use(here::here('modules/demography/helpers.R'))

modules::export('^run$|^REQUIRED_MODELS$') # default exported functions

REQUIRED_MODELS <- c("death")

#' Death
#'
#' @param world a [dymiumCore::World] object
#' @param model a model object or a list of model objects
#' @param target a positive integers or a list of positive integers
#' @param time_steps positive integer()
#'
#' @return object
run <- function(world, model = NULL, target = NULL, time_steps = NULL) {

  checkmate::assert_r6(world, classes = "World")

  # early return if `time_steps` is not the current time
  if (!dymiumCore::is_scheduled(time_steps)) {
    return(invisible(world))
  }

  lg$info('Running Death')

  Pop <- assign_reference(world, Population)
  Ind <- assign_reference(world, Individual)

  # pick models
  model <- pick_models(model, world, REQUIRED_MODELS)

  # the start of death event --------
  # ids of dying persons
  dying_persons <-
    find_dying_persons(Pop = Pop,
                             model = model$death,
                             target = target)

  n_deaths <- length(dying_persons)

  lg$info('There are {n_deaths} deaths')
  Pop$log(desc = "cnt:deaths", value = n_deaths)

  if (n_deaths > 0) {
    Pop$log(desc = "id:individuals_died", value = list(dying_persons))
    # some individual agents may not have a partner hence NAs will be returned
    # but this will be ignore by the become_widowed() function.
    partner_ids <- Ind$get_attr(x = "partner_id", ids = dying_persons)
    if (length(partner_ids) > 0) {
      become_widowed(Ind, ids = partner_ids)
    }
    die(Ind, ids = dying_persons)
    Pop$update()
  }
  invisible(world)
}

# private utility functions (.util_*) -------------------------------------
find_dying_persons = function(Pop, model, target) {

  Ind <- assign_reference(Pop, Individual)

  deathDecision <- TransitionDeath$new(x = Ind, model = model, target = target)

  Pop$log(desc = "avl:deaths", value = deathDecision$get_nrow_result())

  return(deathDecision$get_decision_maker_ids('yes'))

}

become_widowed = function(Ind, ids) {

  # filter out NAs,
  ids <- ids[!is.na(ids)]

  check_entity_ids(Ind, ids)

  Ind$remove_relationship(ids = ids, type = "partner")

  Ind$get_data(copy = FALSE) %>%
    .[get(Ind$get_id_col()) %in% ids,
      marital_status := constants$IND$MARITAL_STATUS$WIDOWED]

  add_history(entity = Ind, ids = ids, constants$EVENT$WIDOWED)

  invisible()
}

die = function(Ind, ids) {
  Ind$remove(ids = ids)
  add_history(entity = Ind,
              ids = ids,
              event = constants$EVENT$DEATH)
  invisible()
}

TransitionDeath <- R6Class(
  classname = "TransitionDeath",
  inherit = TransitionClassification,
  public = list(
    mutate = function(.data) {
      .data %>%
        .[age >= 100, age := 99]
    }
  )
)

# exported utility functions (util_*) -------------------------------------
util_function <- function(x) {}

