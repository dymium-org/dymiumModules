# It is recommended to assign this module to a variable called: event_demog_marriage
# for example: event_demog_marriage <- modules::use('modules/demography/marriage.R')
modules::import('dymiumCore')
modules::import("here")
modules::import("data.table")
modules::import("checkmate")
modules::import("purrr", "flatten_int")
modules::import("R6", "R6Class")
modules::export('^run$|^REQUIRED_MODELS$') # default exported functions
modules::expose(here::here("modules/demography/logger.R"))
helpers <- modules::use(here::here("modules/demography/helpers.R"))
constants <- modules::use(here::here("modules/demography/constants.R"))

REQUIRED_MODELS <- c("marriage_cohab_male",
                     "marriage_no_cohab_male",
                     "marriage_no_cohab_female")

#' Marriage
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

  lg$info("Running Marriage")

  # get references of the relavant entities in this event
  Pop <- world$get("Population")
  Ind <- world$get("Individual")
  Hh <- world$get("Household")

  # check the model argument
  model <- pick_models(model, world, REQUIRED_MODELS)

  ## cohabitation prior to marriage -------
  TransMarriageCohabited <-
    TransitionMarriageCohabited$new(
      x = Ind,
      model = model$marriage_cohab_male,
      target = target$marriage_cohab
    )

  cohabiting_male_to_marry_ids <-
    TransMarriageCohabited$get_result()[response == "yes", id]

  Pop$log(
    desc = "cnt:marriages_from_cohabitation",
    value = length(cohabiting_male_to_marry_ids))

  if (length(cohabiting_male_to_marry_ids) > 0) {

    cohabiting_female_to_marry_ids <-
      Ind$get_partner(ids = cohabiting_male_to_marry_ids)

    # update marital status
    Ind$get_data(copy = FALSE) %>%
      .[get(Ind$get_id_col()) %in% c(cohabiting_male_to_marry_ids,
                                     cohabiting_female_to_marry_ids),
        marital_status := constants$IND$MARITAL_STATUS$MARRIED]

    checkmate::assert_integerish(
      x = c(cohabiting_male_to_marry_ids, cohabiting_female_to_marry_ids),
      len = length(cohabiting_male_to_marry_ids) * 2,
      lower = 1,
      unique = T
    )

    add_history(
      entity = Ind,
      ids = c(cohabiting_male_to_marry_ids, cohabiting_female_to_marry_ids),
      event = constants$EVENT$MARRIAGE_FROM_COHABITATION
    )

    n_marriage_cohab <- length(cohabiting_male_to_marry_ids)
  } else {
    n_marriage_cohab <- 0
  }

  ## no cohabitation prior to marriage -------

  # decide whether to find a partner to marry with
  TransMarriageNoCohabitationMale <-
    TransitionMarriageNoCohabitationMale$new(
      x = Ind,
      model = model$marriage_no_cohab_male,
      target = target$marriage_no_cohab_male
    )

  TransMarriageNoCohabitationFemale <-
    TransitionMarriageNoCohabitationFemale$new(
      x = Ind,
      model = model$marriage_no_cohab_female,
      target = target$marriage_no_cohab_female
    )

  active_male_ids <- TransMarriageNoCohabitationMale$get_result()[response == "yes", id]
  active_female_ids <- TransMarriageNoCohabitationFemale$get_result()[response == "yes", id]

  # log
  lg$info("{length(active_male_ids)} males and {length(active_female_ids)} \\
          are entering the marriage market (ratio={ratio}:1).",
          ratio = round(length(active_male_ids) / length(active_female_ids), 2))
  Pop$log(
    desc = "cnt:individuals_male_entered_marriage_market",
    value = length(active_male_ids))
  Pop$log(
    desc = "cnt:individuals_female_entered_marriage_market",
    value = length(active_female_ids))

  ## update individuals and their households -------
  if (length(active_male_ids) > 0 && length(active_female_ids) > 0) {

    # find a match
    MarriageMarket <-
      OptimalMarriageMarket$new(agentset_A = Ind$get_data(ids = active_male_ids),
                                agentset_B = Ind$get_data(ids = active_female_ids))

    matches <-
      MarriageMarket$simulate(method = "one-to-one") %>%
      # remove no match
      data.table:::na.omit.data.table(.) %>%
      # remove potential incest matches
      .[!Ind$living_together(id_A, id_B), ]

    Pop$log(desc = "cnt:marriages_no_cohabitation",
                    value = nrow(matches))

    # update partner relationship
    Ind$add_relationship(ids = matches[["id_A"]],
                         target_ids = matches[["id_B"]],
                         type = "partner")

    # update marital status
    Ind$get_data(copy = FALSE) %>%
      .[get(Ind$get_id_col()) %in% matches[, c(id_A, id_B)],
        marital_status := constants$IND$MARITAL_STATUS$MARRIED]

    # move-out together or move-in with partner?
    # - if the male partner is living with his parent then move out to create a new household
    # - if the male partner is not living with his parent then move in
    # Note that, any resident children will be moved with their parent
    move_out_decision_flag <-
      matches[,
              .(living_with_father = Ind$living_together(self_ids = id_A,
                                                         target_ids = Ind$get_attr(x = "father_id",
                                                                                   ids = id_A)),
                living_with_mother = Ind$living_together(self_ids = id_A,
                                                         target_ids = Ind$get_attr(x = "mother_id",
                                                                                   ids = id_A)))] %>%
      rowSums(na.rm = T) %>%
      {. == 0}

    # create new households that are emptied (no members yet)
    if (sum(move_out_decision_flag) != 0) {
      Hh$add_new_agents(n = sum(move_out_decision_flag))  
      
      # assign new emptied household ids to those that are moving out
      matches[move_out_decision_flag, hid := Hh$get_new_agent_ids()]
    }
    
    # assign male household ids to those that are moving in
    matches[!move_out_decision_flag, hid := Ind$get_household_ids(id_A)]

    # get resident children
    matches[, `:=`(
      id_A_resident_children = Ind$get_resident_children(id_A),
      id_B_resident_children = Ind$get_resident_children(id_B)
    )]

    resident_children <-
      rbind(matches[, .(hid, resident_children = id_A_resident_children)],
            matches[, .(hid, resident_children = id_B_resident_children)]) %>%
      .[, lapply(.SD, unlist), by = hid] %>%
      data.table:::na.omit.data.table()

    # now get moving!
    Pop$leave_household(ind_ids = matches[move_out_decision_flag, ][["id_A"]])
    Pop$join_household(ind_ids = matches[move_out_decision_flag, ][["id_A"]],
                       hh_ids = matches[move_out_decision_flag, ][["hid"]])
    Pop$leave_household(ind_ids = matches[["id_B"]])
    Pop$join_household(ind_ids = matches[["id_B"]],
                       hh_ids = matches[["hid"]])
    Pop$leave_household(ind_ids = resident_children[["resident_children"]])
    Pop$join_household(ind_ids = resident_children[["resident_children"]],
                       hh_ids = resident_children[["hid"]])

    add_history(entity = Pop$get("Individual"),
                ids = matches[, c(id_A, id_B)],
                event = constants$EVENT$MARRIAGE)

    n_marriages_no_cohab <- nrow(matches)

  } else {
    n_marriages_no_cohab <- 0
  }

  n_marriages <- n_marriage_cohab + n_marriages_no_cohab
  lg$info("There were {n_marriages} marriages occured \\
          (priorly cohabited: {n_marriage_cohab}, \\
          did not cohabited: {n_marriages_no_cohab})")
  Pop$log(
    desc = "cnt:marriages",
    value = n_marriages)

  invisible(world)
}

# private utility functions (.util_*) -------------------------------------


# Marriage Market classes -------------------------------------------------
StochasticMarriageMarket <- R6::R6Class(
  classname = "StochasticMarriageMarket",
  inherit = MatchingMarketStochastic,
  public = list(
    matching_score_A = function(matching_problem = self$matching_problem, idx_A, idx_B) {
      scores <- 1 / (1 + abs(
        matching_problem$agentset_A[["age"]][idx_A] -
          matching_problem$agentset_B[["age"]][idx_B]
      ))
    },
    matching_score_B = function(matching_problem = self$matching_problem, idx_B, idx_A) {
      scores <- 1 / (1 + abs(
        matching_problem$agentset_B[["age"]][idx_B] -
          matching_problem$agentset_A[["age"]][idx_A]
      ))
    }
  )
)

OptimalMarriageMarket <- R6::R6Class(
  classname = "OptimalMarriageMarket",
  inherit = MatchingMarketOptimal,
  public = list(
    matching_score_A = function(matching_problem, idx_A, idx_B) {
      outer(X = matching_problem$agentset_B[["age"]][idx_B], # reviewers, rows
            Y = matching_problem$agentset_A[["age"]][idx_A], # proposers, columns
            function(x, y) {
              1 / (1 + abs(x - y))
            })
    },
    matching_score_B = function(matching_problem, idx_B, idx_A) {
      outer(X = matching_problem$agentset_A[["age"]][idx_A], # reviewers, rows
            Y = matching_problem$agentset_B[["age"]][idx_B], # proposers, columns
            function(x, y) {
              1 / (1 + abs(x - y))
            })
    }
  )
)

# TransitionMarriageNotCohabited ------------------------------------------
# filter by targeted agent to avoid creating two classes with almost
# the same functionality: Male and Female
TransitionMarriageNoCohabitationMale <- R6Class(
  classname = "TransitionMarriageNoCohabitationMale",
  inherit = TransitionClassification,
  public = list(
    filter = function(.data) {
      .data %>%
        helpers$FilterAgent$Ind$can_marry(.) %>%
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

TransitionMarriageNoCohabitationFemale <- R6Class(
  classname = "TransitionMarriageNoCohabitationFemale",
  inherit = TransitionClassification,
  public = list(
    filter = function(.data) {
      .data %>%
        helpers$FilterAgent$Ind$can_marry(.) %>%
        .[sex == constants$IND$SEX$FEMALE,]
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

# TransitionMarriageCohabited ---------------------------------------------
TransitionMarriageCohabited <- R6Class(
  classname = "TransitionMarriageCohabited",
  inherit = TransitionClassification,
  public = list(
    filter = function(.data) {
      .data %>%
        helpers$FilterAgent$Ind$can_marry_from_cohabitation(.) %>%
        .[sex == constants$IND$SEX$MALE]
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