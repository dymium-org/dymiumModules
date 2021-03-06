# It is recommended to assign this module to a variable called: event_demography_breakup
# for example: event_demography_breakup <- modules::use('modules/demography/breakup.R')
# default setup, you may edit the below import statments to match your requirements.
modules::import('dymiumCore')
modules::import('R6', 'R6Class')
modules::import('checkmate')
modules::expose(here::here('modules/demography/logger.R')) # import lgr's logger. To use the logger use 'lg' (default logger's name).
modules::expose(here::here('modules/demography/transitions.R'))
constants <- modules::use(here::here('modules/demography/constants.R'))
helpers <- modules::use(here::here('modules/demography/helpers.R'))
household_formation <- modules::use(here::here('modules/demography/household_formation.R'))

modules::export('^run$|^REQUIRED_MODELS$') # default exported functions

REQUIRED_MODELS <-
  c("breakup",
    "breakup_child_custody",
    "breakup_hhtype",
    "breakup_hf_random_join")

#' Breakup
#'
#' @param world a [dymiumCore::World] object
#' @param model a model object or a list of model objects
#' @param target a positive integers or a list of positive integers
#' @param time_steps positive integer()
#'
#' @return world
run <- function(world, model = NULL, target = NULL, time_steps = NULL) {

  checkmate::assert_r6(world, classes = "World")

  # early return if `time_steps` is not the current time
  if (!dymiumCore::is_scheduled(time_steps)) {
    return(invisible(world))
  }

  lg$info('Running Breakup')

  Pop <- world$get("Population")
  Hh <- world$get("Household")
  Ind <- world$get("Individual")

  # check model
  model <- pick_models(model, world, REQUIRED_MODELS)

  # determine the ids of those whom will under go the event, male initiated event
  TransBreakup <- TransitionBreakup$new(
    x = Ind,
    model = model$breakup,
    target = target
  )

  # form a list of initiators and their partners
  if (TransBreakup$get_result()[response == 'yes', .N] > 0) {
    breakingup_persons <-
      list(
        partner_x = TransBreakup$get_result()[response == 'yes', id],
        partner_y = Ind$get_data(ids = TransBreakup$get_result()[response == 'yes', id])[, (partner_id)]
      )  
    stopifnot(length(breakingup_persons$partner_x) == length(breakingup_persons$partner_y))
  } else {
    breakingup_persons <- list(partner_x = integer(0), partner_y = integer(0))  
  }
  
  # if both of persons in a same-sex relationship undergo TransitionBreakup and
  # they both decide to breakup both of their ids in be in breakingup_persons
  # in partner_x and partner_y which means there will be duplications of their ids
  # this will raise errors later on the process. Hence we must remove one of the
  # partner from partner_x and partner_y
  if (length(breakingup_persons$partner_x[breakingup_persons$partner_x %in%
                                          breakingup_persons$partner_y]) != 0) {
    dups <-
      breakingup_persons$partner_x[
        breakingup_persons$partner_x %in%
          breakingup_persons$partner_y]
    dups_x <-
      Ind$get_data() %>%
      .[get(Ind$get_id_col()) %in% dups,
        .SD, .SDcols = c(Ind$get_id_col(), "partner_id")] %>%
      .[, .(pid, partner_id)] %>%
      .[, purrr::map2_int(pid, partner_id, ~ sort(c(.x, .y))[1])] %>%
      unique()
    dups_y <- dups[!dups %in% dups_x]

    breakingup_persons$partner_x <-
      breakingup_persons$partner_x[!breakingup_persons$partner_x %in% dups_x]
    breakingup_persons$partner_y <-
      breakingup_persons$partner_y[!breakingup_persons$partner_y %in% dups_y]
  }

  Pop$log(
    desc = "cnt:breakups",
    value = length(breakingup_persons$partner_x)
  )

  Pop$log(
    desc = "avl:breakups",
    value = TransBreakup$get_nrow_result()
  )

  Pop$log(
    desc = "id:individuals_brokenup",
    value = list(append(breakingup_persons$partner_x, breakingup_persons$partner_y))
  )

  n_breakups <- length(breakingup_persons$partner_x)

  lg$info("There {be} {n_breakups} {couple} who broke up",
          be = ifelse(n_breakups > 1, "are", "is"),
          couple = ifelse(n_breakups > 1, "couples", "couple"))

  # apply side-effects
  if (n_breakups > 0) {

    # determine child custody
    child_custody_decision <-
      TransitionChildCustody$new(
        x = Ind,
        model = model$breakup_child_custody,
        targeted_agents = breakingup_persons$partner_x
      )

    custody_to_partner_x <-
      child_custody_decision$get_result()[['response']] == 'yes'

    # those who get child custody get to stay in the current household
    stayers <-
      c(breakingup_persons$partner_x[custody_to_partner_x],
        breakingup_persons$partner_y[!custody_to_partner_x])
    movers <-
      c(breakingup_persons$partner_x[!custody_to_partner_x],
        breakingup_persons$partner_y[custody_to_partner_x])

    # remove relationship
    # husband and wife remove each other from partner_id
    Ind$remove_relationship(ids = breakingup_persons$partner_x, type = "partner")
    add_history(
      entity = Ind,
      ids = c(breakingup_persons$partner_x, breakingup_persons$partner_y),
      event = constants$EVENT$BREAKUP
    )
    # movers remove self from household
    Pop$leave_household(ind_ids = movers)

    # movers decide to join an existing household or create a new lone person
    # household
    TransHhtype <- TransitionGroupHousehold$new(
      x = Ind,
      model = model$breakup_hhtype,
      targeted_agents = movers
    )
    
    lone_mover_ids <-
      TransHhtype$get_decision_maker_ids("lone")
    group_mover_ids <-
      TransHhtype$get_decision_maker_ids("group")
    
    # Lone movers --------------
    if (length(lone_mover_ids) > 0) {
      household_formation$join_new_lone_household(Pop, ids = lone_mover_ids)
    }
    
    # Group movers --------------
    if (length(group_mover_ids) > 0) {
      household_formation$random_join_group_household(
        Pop = Pop,
        ids = group_mover_ids,
        model = model$breakup_hf_random_join
      )
    }
  }
    
  
  # return the first argument (`object`) to make event functions pipe-able.
  invisible(world)
}

# private utility functions (.util_*) -------------------------------------
TransitionBreakup <- R6Class(
  classname = "TransitionBreakup",
  inherit = TransitionClassification,
  public = list(
    filter = function(.data) {
      .data %>%
        helpers$FilterAgent$Ind$can_breakup(.) %>%
        .[sex == constants$IND$SEX$MALE,]
    },
    mutate = function(.data) {
      Ind <- private$.AgtObj
      .data %>%
        helpers$DeriveVar$IND$hhadult(x = ., Ind) %>%
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

