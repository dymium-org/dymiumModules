# It is recommended to assign this module to a variable called: event_demography_leavehome
# for example: event_demography_leavehome <- modules::use('modules/demography/leavehome.R')
# default setup, you may edit the below import statments to match your requirements.
modules::import('dymiumCore')
modules::import('R6', 'R6Class')
modules::import("data.table")
modules::import('checkmate')
modules::expose(here::here('modules/demography/logger.R')) # import lgr's logger. To use the logger use 'lg' (default logger's name).
modules::expose(here::here('modules/demography/transitions.R'))
constants <- modules::use(here::here('modules/demography/constants.R'))
helpers <- modules::use(here::here('modules/demography/helpers.R'))

modules::export('^run$|^REQUIRED_MODELS$') # default exported functions

REQUIRED_MODELS <- c("leavehome_male", "leavehome_female", "leavehome_hhtype", "leavehome_hf_random_join")

#' Leavehome
#'
#' @param world a [dymiumCore::World] object
#' @param model a model object or a list of model objects
#' @param target a positive integers or a list of positive integers
#' @param time_steps positive integer()
#'
#' @return a [dymiumCore::World] object.
run <- function(world, model = NULL, target = NULL, time_steps = NULL) {

  checkmate::assert_r6(world, classes = "World")

  # early return if `time_steps` is not the current time
  if (!dymiumCore::is_scheduled(time_steps)) {
    return(invisible(world))
  }

  lg$info('Running Leavehome')

  Pop <- assign_reference(world, Population)
  Ind <- assign_reference(world, Individual)

  # check model
  if (is.null(model)) {
    model <- dm_get_model(world, REQUIRED_MODELS)
  } else {
    checkmate::assert_names(names(model), type = "unique", identical.to = REQUIRED_MODELS)
  }

  if (is.null(target)) {
    target <- list(
      leave_home = NULL,
      join_non_family_hh = NULL
    )
  }

  leaverIds_ <-
    find_leavers(
      Pop = Pop,
      maleLeaveHomeDecisionModel = model$leavehome_male,
      femaleLeaveHomeDecisionModel = model$leavehome_female,
      leaveHomeTarget = target$leave_home # not using
    )

  totalLeavers <- length(leaverIds_)
  Pop$log(
    desc = "cnt:left_home",
    value = totalLeavers
  )

  lg$info("There {be} {totalLeavers} individuals leaving their parental homes.",
          be = ifelse(totalLeavers > 1, "are", "is"))

  if (totalLeavers > 0) {

    Pop$log(
      desc = "id:demography-left_home",
      value = list(leaverIds_)
    )

    add_history(
      entity = Ind,
      ids = leaverIds_,
      event = constants$EVENT$LEFT_HOME
    )

    hhTypeDecision_dt <-
      choose_hhtype_to_join(
        Pop = Pop,
        hhTypeDecisionModel = model$leavehome_hhtype,
        leaverIds_ = leaverIds_
      )

    move_out(
      Pop = Pop,
      model = model$leavehome_hf_random_join,
      hhTypeDecision_dt = hhTypeDecision_dt
    )

  }

  # return the first argument (`object`) to make event functions pipe-able.
  invisible(world)
}

# private utility functions (.util_*) -------------------------------------
find_leavers = function(Pop, maleLeaveHomeDecisionModel, femaleLeaveHomeDecisionModel, leaveHomeTarget) {

  Ind <- assign_reference(Pop, Individual)

  # select potential leavers
  potential_leavers_dt <-
    helpers$FilterAgent$Ind$can_leave_parentalhome(Pop) %>%
    .[, .SD, .SDcols = c(Ind$get_id_col(), "sex")]

  # divide by gender
  potential_male_leaver_ids <- potential_leavers_dt[sex == constants$IND$SEX$MALE, pid]
  potential_female_leaver_ids <- potential_leavers_dt[sex == constants$IND$SEX$FEMALE, pid]

  Pop$log(
    desc = "avl:individuals_male_left_parental_homes",
    value = length(potential_male_leaver_ids)
  )

  Pop$log(
    desc = "avl:individuals_female_left_parental_homes",
    value = length(potential_female_leaver_ids)
  )

  Pop$log(
    desc = "avl:demography-left_home",
    value = potential_leavers_dt[, .N]
  )

  #' since potential movers are selected and divided into two groups by gender
  #' we can use `targeted_agents` argument to apply the model to those agents only
  #' hence our `TransitionLeaveHome` does not need to have a customised `filter` function
  #' implemented and can be reused for both genders.

  # Males
  TransLeavehomeMales <-
    TransitionLeaveHome$new(
      x = Ind,
      model = maleLeaveHomeDecisionModel,
      targeted_agents = potential_male_leaver_ids
    )

  # Females
  TransLeavehomeFemales <-
    TransitionLeaveHome$new(
      x = Ind,
      model = femaleLeaveHomeDecisionModel,
      targeted_agents = potential_female_leaver_ids
    )

  male_leavers_ids <-
    TransLeavehomeMales$get_decision_maker_ids('yes')

  female_leavers_ids <-
    TransLeavehomeFemales$get_decision_maker_ids('yes')

  Pop$log(
    desc = "cnt:individuals_male_left_parental_homes",
    value = length(male_leavers_ids)
  )

  Pop$log(
    desc = "cnt:individuals_female_left_parental_homes",
    value = length(female_leavers_ids)
  )

  return(c(male_leavers_ids, female_leavers_ids))
}

choose_hhtype_to_join = function(Pop, hhTypeDecisionModel, leaverIds_) {
  IndObj <- assign_reference(Pop, Individual)
  TransGroupHousehold <- TransitionGroupHousehold$new(x = IndObj,
                                                      model = hhTypeDecisionModel,
                                                      targeted_agents = leaverIds_)
  hhTypeDecision_dt <-
    TransGroupHousehold$get_result() %>%
    data.table::setnames(., old = "id", new = "leaverId")
  invisible(hhTypeDecision_dt)
}

join_new_lone_hh = function(Pop, self_ids) {
  if (length(self_ids) != 0) {
    loneMovers_dt <- data.table(ind_id = self_ids, hh_id = NA_integer_)
    household_formation(Pop = Pop,
                        mapping = loneMovers_dt,
                        type = "new")
    add_history(entity = Pop$get("Individual"),
                ids = self_ids,
                event = constants$EVENT$CREATE_NEW_HOUSEHOLD)
  }
  invisible()
}

join_existing_hh = function(Pop, model, self_ids) {
  if (length(self_ids) != 0) {
    groupMovers_dt <- data.table(ind_id = self_ids, hh_id = NA_integer_)
    household_formation(Pop, model, mapping = groupMovers_dt, type = "randomjoin")
    add_history(entity = Pop$get("Individual"),
                ids = self_ids,
                event = constants$EVENT$JOINED_EXISTING_HOUSEHOLD)
  }
  invisible()
}

join_hh = function(Pop, model, hhTypeDecision_dt) {
  join_new_lone_hh(Pop = Pop,
                      self_ids = hhTypeDecision_dt[response == "lone", leaverId])
  join_existing_hh(Pop = Pop,
                       model = model,
                       self_ids = hhTypeDecision_dt[response == "group", leaverId])
  invisible()
}

move_out = function(Pop, model, hhTypeDecision_dt) {
  Pop$leave_household(ind_ids = hhTypeDecision_dt[, leaverId])
  join_hh(Pop = Pop,
               model = model,
               hhTypeDecision_dt = hhTypeDecision_dt)
  invisible()
}

TransitionLeaveHome <- R6Class(
  classname = "TransitionLeaveHome",
  inherit = TransitionClassification,
  public = list(
    mutate = function(.data) {
      Ind <- private$.AgtObj
      .data %>%
        helpers$DeriveVar$IND$hhadult(x = ., Ind) %>%
        helpers$DeriveVar$IND$mrcurr(x = ., Ind) %>%
        helpers$DeriveVar$IND$has_children(x = ., Ind) %>%
        helpers$DeriveVar$IND$has_resident_children(x = ., Ind) %>%
        helpers$DeriveVar$IND$n_resident_children(x = ., Ind) %>%
        helpers$DeriveVar$IND$age_youngest_resident_child(x = ., Ind) %>%
        helpers$DeriveVar$IND$age_youngest_child(x = ., Ind) %>%
        helpers$DeriveVar$IND$age5(x = ., Ind) %>%
        helpers$DeriveVar$IND$hhsize(x = ., Ind) %>%
        helpers$DeriveVar$IND$n_children(x = ., Ind)
    }
  )
)

# exported utility functions (util_*) -------------------------------------
util_function <- function(x) {}

