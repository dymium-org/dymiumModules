# It is recommended to assign this module to a variable called: event_demography_cohabit
# for example: event_demography_cohabit2 <- modules::use('modules/demography/cohabit2.R')
# default setup, you may edit the below import statments to match your requirements.
modules::import('dymiumCore')
modules::import("here")
modules::import("data.table")
modules::import("checkmate")
modules::import("R6", "R6Class")
modules::expose(here::here('modules/demography/logger.R')) # import lgr's logger. To use the logger use 'lg' (default logger's name).
constants <- modules::use(here::here('modules/demography/constants.R'))
helpers <- modules::use(here::here('modules/demography/helpers.R'))

modules::export('^run$|^REQUIRED_MODELS$') # default exported functions

REQUIRED_MODELS <-
  c("cohabitation_male",
    "cohabitation_female")

#' Cohabit
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

  lg$info('Running Cohabit2')

  # get references of the relavant entities in this event
  Pop <- world$get("Population")
  Ind <- world$get("Individual")
  Hh <- world$get("Household")

  # check model
  model <- pick_models(model, world, REQUIRED_MODELS)

  # select people to cohabit
  TransCohabitationMale <-
    TransitionCohabitationMale$new(Ind, model$cohabitation_male, target)
  TransCohabitationFemale <-
    TransitionCohabitationFemale$new(Ind, model$cohabitation_female, target)

  # get ids of active individuals (those that have decided to mingle!)
  active_males <- TransCohabitationMale$get_result()[response == "yes", id]
  active_females <- TransCohabitationFemale$get_result()[response == "yes", id]

  lg$info("{length(active_males)} males and {length(active_females)} \\
          females enter the cohabitation market (ratio = {ratio}:1).",
          ratio = round(length(active_males) / length(active_females), 2))

  # log the number of people entered the cohabitation market
  Pop$log(
    desc = "cnt:individuals_male_entered_cohabitation_market",
    value = length(active_males))
  Pop$log(
    desc = "cnt:individuals_female_entered_cohabitation_market",
    value = length(active_females))

  # if there are people to be mathed
  if (length(active_males) > 0 && length(active_females) > 0) {
    Pop$log(
      desc = "id:individuals_entered_cohabitation_market",
      value = length(active_males) + length(active_females))

    cohabitationMarket <-
      OptimalCohabitationMarket$new(agentset_A = Ind$get_data(ids = active_males),
                                    agentset_B = Ind$get_data(ids = active_females))

    # simulate returns a data.table with two columns: id_A and id_B
    matches <-
      cohabitationMarket$simulate(method = "one-to-one") %>%
      # remove all the rows that didn't match
      data.table:::na.omit.data.table(., cols = c("id_A", "id_B")) %>%
      # there is a change that two individuals from the same household could be matched
      # are they are likely to be related in some way. Hence, we can remove them to
      # avoid problems
      .[!is_incest(Pop, id_A, id_B), ]

    # matches[, table(c(id_A, id_B))] %>% sort()

    add_history(
      entity = Ind,
      ids = matches[, c(id_A, id_B)],
      event = constants$EVENT$COHABITATION
    )

    # update partner id of both partners
    Ind$add_relationship(ids = matches[["id_A"]],
                         target_ids = matches[["id_B"]],
                         type = "partner")

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
    n_couples <- nrow(matches)
  } else {
    n_couples <- 0
  }

  lg$info("{n_couples} newly cohabited couples were formed.")
  Pop$log(desc = "cnt:cohabitations", value = n_couples)

  return(invisible(world))
}

# private utility functions -------------------------------------
is_incest = function(Pop, self_ids, partner_ids) {
  # ideally this should check for relationships between two
  # people if they are siblings or in a child-parent relationship
  # but to simplify this we are just going to if they are from the same
  # household in the current simulation time
  Ind <- Pop$get("Individual")
  dymiumCore::assert_entity_ids(Ind, ids = self_ids)
  dymiumCore::assert_entity_ids(Ind, ids = partner_ids)
  stopifnot(length(self_ids) == length(partner_ids))
  self_hids = Ind$get_household_ids(ids = self_ids)
  partner_hids = Ind$get_household_ids(ids = partner_ids)
  Pop$log(desc = "warn:removed_incest_cohabitations",
                  value = sum(self_hids == partner_hids))
  return(self_hids == partner_hids)
}

# Transition classes -----------------
TransitionCohabitationMale <- R6::R6Class(
  classname = "TransitionCohabitationMale",
  inherit = TransitionClassification,
  public = list(
    filter = function(.data) {
      .data %>%
        helpers$FilterAgent$Ind$can_cohabit(.) %>%
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

#' @export
TransitionCohabitationFemale <- R6::R6Class(
  classname = "TransitionCohabitationFemale",
  inherit = TransitionClassification,
  public = list(
    filter = function(.data) {
      .data %>%
        helpers$FilterAgent$Ind$can_cohabit(.) %>%
        .[sex == constants$IND$SEX$FEMALE]
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

TransitionCohabitationHHJoin <- R6Class(
  classname = "TransitionCohabitationHHJoin",
  inherit = TransitionClassification
)


# Market classes -----------------------------------------------------
# StochasticMarriageMarket
StochasticCohabitationMarket <- R6::R6Class(
  classname = "StochasticCohabitationMarket",
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

# OptimalMarriageMarket
OptimalCohabitationMarket <- R6::R6Class(
  classname = "OptimalCohabitationMarket",
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