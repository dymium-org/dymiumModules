# It is recommended to assign this module to a variable called: event_demography_birth
# for example: event_demography_birth <- modules::use('modules/demography/birth.R')
# default setup, you may edit the below import statments to match your requirements.
modules::import('dymiumCore')
modules::import('data.table')
modules::import("R6", "R6Class")
modules::import("assertthat", "assert_that")
modules::expose(here::here('modules/demography/logger.R')) # import lgr's logger. To use the logger use 'lg' (default logger's name).
constants <- modules::use(here::here('modules/demography/constants.R'))
helpers <- modules::use(here::here('modules/demography/helpers.R'))

modules::export('^^run|^util|^test') # default exported functions

REQUIRED_MODELS <- c("fertility", "birth_multiplicity", "birth_sex_ratio")

#' Birth
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

  lg$info('Running Birth')

  # get object references for cleaner code
  Pop <- assign_reference(world, Population)
  Ind <- assign_reference(world, Individual)

  # check model
  if (is.null(model)) {
    model <- dm_get_model(world, REQUIRED_MODELS)
  } else {
    checkmate::assert_names(names(model), type = "unique", identical.to = REQUIRED_MODELS)
  }

  # create a transition
  TransBirth <- TransitionBirth$new(
    x = Ind,
    model = model$fertility,
    target = target$giveBirth
  )

  all_birth_giver_ids <-
    Ind$get_data(ids = TransBirth$get_result()[['id']])

  single_birth_giver_ids <-
    TransBirth$get_decision_maker_ids("yes")

  # simulate twins birth
  TransTwinBirth <- TransitionTwinsBirth$new(
    x = Ind,
    model = model$birth_multiplicity,
    targeted_agents = single_birth_giver_ids
  )

  twins_birth_giver_ids <-
    TransTwinBirth$get_decision_maker_ids("twins")

  # the length of this vector is equal to the total number of births which is
  # represented by their mothers' id. For example, if individual with id = 1 is
  # to give birth to twins then there will be two 1 in `all_birth_giver_ids`.
  all_birth_giver_ids <-
    c(single_birth_giver_ids, twins_birth_giver_ids)

  Pop$keep_log(var = "count:births", value = length(all_birth_giver_ids))
  Pop$keep_log(var = "id:birth_givers", value = list(unique(all_birth_giver_ids)))
  Pop$keep_log(var = "occ:gave_birth", value = length(single_birth_giver_ids))
  Pop$keep_log(var = "avl:gave_birth", value = TransBirth$get_nrow_result())

  lg$info("There are {length(all_birth_giver_ids)} births from {uniqueN(all_birth_giver_ids)} birth givers.")

  if (length(all_birth_giver_ids) > 0) {

    # create newborns
    .util_create_newborns(
      Pop = Pop,
      ids = all_birth_giver_ids,
      sex_ratios = model$birth_sex_ratio
    )

    add_history(Ind, ids = all_birth_giver_ids, event = constants$EVENT$GAVE_BIRTH)
  }

  # return the first argument (`object`) to make event functions pipe-able.
  invisible(world)
}

# private utility functions (.util_*) -------------------------------------
.util_create_newborns = function(Pop, ids, sex_ratios) {

  Ind <- assign_reference(Pop, Individual)

  partner_ids <-
    Ind$get_attr(x = "partner_id", ids = ids)

  assert_that(all(Ind$get_attr(x = "sex", ids = ids) == constants$IND$SEX$FEMALE),
              msg = "Birth event is only applicable for female individual agents.")

  # generate new ids
  newborn_ids <- Ind$generate_new_ids(n = length(ids))

  # set newborns' default attributes
  attrs_keep <- c(Ind$get_id_col(), Ind$get_hid_col(),
                  "father_id", "mother_id", "age", "sex", "marital_status")

  # create newborn data
  newborn_dt <-
    Ind$get_data(ids = c(ids)) %>%
    # assign ids
    .[, Ind$get_id_col() := newborn_ids] %>%
    .[, .SD, .SDcols = attrs_keep] %>%
    # assign attributes
    .[, `:=`(
      age = 0,
      sex = sample(
        x = names(sex_ratios),
        size = .N,
        replace = TRUE,
        prob = sex_ratios
      ),
      marital_status = constants$IND$MARITAL_STATUS$NOT_APPLICABLE,
      # add parents to newborn
      father_id = partner_ids,
      mother_id = ids,
      # default values
      labour_force_status = "not applicable",
      student_status = "not attending",
      industry_of_emp = "not applicable",
      income = "not applicable",
      education = "not applicable"
    )] %>%
    # binding the newborn data with an emptied individual data make sure that
    # newborn data have the same structure and types as the existing individual data
    rbind(Ind$get_data()[0, ], ., fill = TRUE)

  # add newborns to the population
  # note: Population$add_population() update some household attributes
  #       such as household size hence you may not need to update the
  #       same attributes twice.
  Pop$add_population(ind_data = newborn_dt)
  invisible()
}

TransitionBirth <- R6Class(
  classname = "TransitionBirth",
  inherit = dymiumCore::TransitionClassification,
  public = list(
    filter = function(.data) {
      .data %>%
        helpers$FilterAgent$Ind$can_give_birth()
    }
  )
)

TransitionTwinsBirth <- R6Class(classname = "TransitionTwinsBirth",
                                inherit = TransitionClassification,
                                public = list())

# exported utility functions (util_*) -------------------------------------
util_function <- function(x) {}

