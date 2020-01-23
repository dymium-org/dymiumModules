# It is recommended to assign this module to a variable called: event_demography_migration
# for example: event_demography_migrate <- modules::use('modules/demography/migration.R')
# default setup, you may edit the below import statments to match your requirements.
modules::import('dymiumCore')
modules::import('data.table')
modules::import('checkmate')
modules::expose(here::here('modules/demography/logger.R')) # import lgr's logger. To use the logger use 'lg' (default logger's name).
constants <- modules::use(here::here('modules/demography/constants.R'))
helpers <- modules::use(here::here('modules/demography/helpers.R'))

modules::export('^run$|^REQUIRED_MODELS$') # default exported functions

REQUIRED_MODELS <- c("migrant_individuals", "migrant_households")

#' Migration
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

  lg$info('Running Migration')

  Pop <- assign_reference(world, Population)
  Ind <- assign_reference(world, Individual)
  Hh <- assign_reference(world, Household)

  # check model
  if (is.null(model)) {
    model <- dm_get_model(world, REQUIRED_MODELS)
  } else {
    checkmate::assert_names(names(model), type = "unique", identical.to = REQUIRED_MODELS)
  }

  # check target
  if (is.null(target)) {
    stop(lg$error("`target` must be specified. In this case, target is the number of households \\
                  to be added to the population object."))
  } else {
    checkmate::assert_count(target, positive = T, na.ok = FALSE, null.ok = FALSE)
    lg$info("{target} migrant households are joining to the population.")
  }

  # draw random migrants
  pid_col <- Ind$get_id_col()
  hid_col <- Hh$get_id_col()

  selected_migrant_hh <- .util_pick_migrants(ids = model$migrant_households[[hid_col]],
                                             weights = model$migrant_households[['weights']],
                                             n = target)

  # create migrant data
  migrants <- pop_register(
    x = Pop,
    ind_data = model$migrant_individuals[get(hid_col) %in% selected_migrant_hh],
    hh_data = model$migrant_households[get(hid_col) %in% selected_migrant_hh]
  )

  # add migrants to the population
  lg$info("There are {migrants$hh_data[, .N]} migrant households \\
          which made up of {migrants$ind_data[, .N]} individuals (avg. hhsize = {avg_hhsize})",
          avg_hhsize = round(migrants$ind_data[, .N] / migrants$hh_data[, .N], 2))
  Pop$add_population(ind_data = migrants$ind_data, hh_data = migrants$hh_data)

  # keep logs
  Pop$log(desc = "cnt:households_immigrated_as_households",
               value = migrants$hh_data[, .N])
  Pop$log(desc = "cnt:individuals_immigrated_as_households",
               value = migrants$ind_data[, .N])

  # return the first argument (`object`) to make event functions pipe-able.
  invisible(world)
}

# private utility functions (.util_*) -------------------------------------
.util_pick_migrants <- function(ids, weights, n) {
  checkmate::assert_number(n, na.ok = FALSE, lower = 0, finite = TRUE, null.ok = FALSE)
  if (missing(weights) | is.null(weights)) {
    return(dymiumCore::sample_choice(ids, size = n, replace = FALSE))
  }
  checkmate::assert_numeric(weights, lower = 0, finite = T, any.missing = FALSE, len = length(ids))
  checkmate::assert_true(sum(weights) != 0)
  dymiumCore::sample_choice(ids, size = n, prob = weights, replace = FALSE)
}

.util_generate_population_from_weights <- function(Pop, ind_data, hh_data, hh_weights) {
  pid_col <- Pop$get("Individual")$get_id_col()
  hid_col <- Pop$get("Household")$get_id_col()
  hh_data[, hh_weights := hh_weights]
}

# exported utility functions (util_*) -------------------------------------
util_function <- function(x) {}

