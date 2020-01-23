modules::import("dymiumCore")
modules::import("checkmate")
modules::import("here")
modules::export('^run$|^REQUIRED_MODELS$') # default exported functions
modules::expose(here::here('modules/demography/logger.R')) # import lgr's logger. To use the logger use 'lg' (default logger's name).
helpers <- modules::use(here::here("modules/demography/helpers.R"))
constants <- modules::use(here::here("modules/demography/constants.R"))

# the main run function ---------------------------------------------------
#' Aging
#'
#' @param world a [dymiumCore::World] object
#' @param model a model object or a list of model objects
#' @param target a positive integers or a list of positive integers
#' @param time_steps positive integer()
#'
#' @return object
#' @export
#'
#' @examples
run <- function(world, model = NULL, target = NULL, time_steps = NULL) {

  # early return if `time_steps` is not the current time
  if (!dymiumCore::is_scheduled(time_steps)) {
    return(invisible(world))
  }

  lg$info("Running Age")

  # uncomment the line belows if the event doesn't require `model`
  # eg. If the event is deterministic like ageing.
  if (!is.null(model)) {
    lg$warn('`model` will not be used.')
  }

  # uncomment the line belows if the event doesn't require `target`
  # eg. If the event is to be applied to all agents.
  if (!is.null(target)) {
    lg$warn('`target` will not be used.')
  }

  # create a reference to the main agent object for easy access
  Ind <- world$get("Individual")

  # increase age by one year
  age(Ind, year_older = 1L)

  # individuals are able to be in a relationship
  # In Australia, people that aged 15 or more can legally married and de facto
  # hence this is set to 15 year of age
  enter_adulthood(Ind, adult_age = 15L)

  # invisibly return the first argument (`object`) to make event functions pipe-able.
  invisible(world)
}

# private utility functions (.util_*) -------------------------------------
# for use inside the run function
age <- function(Ind, year_older = 1L) {
  checkmate::assert_r6(Ind, classes = "Individual")
  checkmate::assert_count(year_older)
  ind_data <- Ind$get_data(copy = FALSE)
  checkmate::assert_names(names(ind_data), must.include = "age")
  ind_data[, age := age + year_older]
  invisible()
}

enter_adulthood <- function(Ind, adult_age) {
  checkmate::assert_r6(Ind, classes = "Individual")
  ind_data <- Ind$get_data(copy = FALSE)
  checkmate::assert_names(names(ind_data), must.include = "age")

  # marital status
  if ('marital_status' %in% names(ind_data)) {
    ind_data[
      age >= adult_age & marital_status == constants$IND$MARITAL_STATUS$NOT_APPLICABLE,
      marital_status := constants$IND$MARITAL_STATUS$NEVER_MARRIED
    ]
  }

  # labour force status
  if ('labour_force_status' %in% names(ind_data)) {
    ind_data[
      age >= adult_age & labour_force_status == constants$IND$LABOUR_FORCE_STATUS$NOT_APPLICABLE,
      labour_force_status := constants$IND$LABOUR_FORCE_STATUS$UNEMPLOYED
    ]
  }

  # education
  if ('education' %in% names(ind_data)) {
    ind_data[
      age >= adult_age & education == constants$IND$EDUCATION$NOT_APPLICABLE,
      education := constants$IND$EDUCATION$YEAR_11_AND_BELOW
    ]
  }

  invisible()
}