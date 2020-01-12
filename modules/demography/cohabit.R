# It is recommended to assign this module to a variable called: event_demography_cohabit
# for example: event_demography_cohabit <- modules::use('modules/demography/cohabit.R')
# default setup, you may edit the below import statments to match your requirements.
modules::import('dymiumCore')
modules::import("here")
modules::import("data.table")
modules::import("checkmate")
modules::import("purrr", "flatten_int")
modules::import("R6", "R6Class")
modules::expose(here::here('modules/demography/logger.R')) # import lgr's logger. To use the logger use 'lg' (default logger's name).
constants <- modules::use(here::here('modules/demography/constants.R'))
helpers <- modules::use(here::here('modules/demography/helpers.R'))

modules::export('^run$|^REQUIRED_MODELS$') # default exported functions

REQUIRED_MODELS <-
  c("cohabitation_male",
    "cohabitation_female",
    "malePartnerAgeRuleToCreateNewHousehold"
  )

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

  lg$info('Running Cohabit')

  # get object references for cleaner code
  Pop <- assign_reference(world, Population)
  Ind <- assign_reference(world, Individual)
  Hh <- assign_reference(world, Household)

  # check model
  if (is.null(model)) {
    model <- dm_get_model(world, REQUIRED_MODELS)
  } else {
    checkmate::assert_names(names(model), type = "unique", identical.to = REQUIRED_MODELS)
  }

  cohabitationPool_ls <-
    select_cohabitants(
      Pop,
      maleCohabitDecisionModel = model$cohabitation_male,
      femaleCohabitDecisionModel = model$cohabitation_female,
      maleCohabitDecisionTarget_int = target,
      femaleCohabitDecisionTarget_int = target
    )

  n_males_looking_to_cohabit <- length(cohabitationPool_ls$partnerX_)
  n_females_looking_to_cohabit <- length(cohabitationPool_ls$partnerY_)

  lg$info("{n_males_looking_to_cohabit} males and {n_females_looking_to_cohabit} \\
          females are entering the cohabitation market.")

  Pop$keep_log(
    var = "occ:individuals_male_entered_cohabitation_market",
    value = n_males_looking_to_cohabit)
  Pop$keep_log(
    var = "occ:individuals_female_entered_cohabitation_market",
    value = n_females_looking_to_cohabit)

  if (n_males_looking_to_cohabit > 0 && n_females_looking_to_cohabit > 0) {
    Pop$keep_log(
      var = "id:individuals_entered_cohabitation_market",
      value = list(flatten_int(cohabitationPool_ls)))

    matches_dt <-
      find_partner(
        partnerX_dt = Ind$get_data(ids = cohabitationPool_ls$partnerX_),
        partnerY_dt = Ind$get_data(ids = cohabitationPool_ls$partnerY_)
      )

    matches_dt <-
      remove_incest(
        Pop = Pop,
        partnerX_ = matches_dt[, partnerX],
        partnerY_ = matches_dt[, partnerY]
      )

    add_history(
      entity = Ind,
      ids = matches_dt[, c(partnerX, partnerY)],
      event = constants$EVENT$COHABITATION
    )

    totalCohabitationCouples <- nrow(matches_dt)
    Pop$keep_log(var = "id:individuals_cohabited",
                    value = list(c(matches_dt[, partnerX], matches_dt[, partnerY])))

    update_partner(Ind, matches_dt = matches_dt)
    update_household(Pop, model, matches_dt = matches_dt)
  } else {
    totalCohabitationCouples <- 0
  }

  lg$info("{totalCohabitationCouples} newly cohabited couples were formed.")
  Pop$keep_log(var = "occ:cohabitations", value = totalCohabitationCouples)

  invisible(world)
}

# private utility functions -------------------------------------
select_cohabitants = function(Pop,
                                           maleCohabitDecisionModel,
                                           femaleCohabitDecisionModel,
                                           maleCohabitDecisionTarget_int,
                                           femaleCohabitDecisionTarget_int) {
        Ind <- assign_reference(Pop, Individual)
        maleCohabitDecision <-
          TransitionCohabitationMale$new(x = Ind,
                                         model = maleCohabitDecisionModel,
                                         target = maleCohabitDecisionTarget_int)

        male_ids <-
          maleCohabitDecision$get_decision_maker_ids('yes')

        femaleCohabitDecision <-
          TransitionCohabitationFemale$new(x = Ind,
                                           model = femaleCohabitDecisionModel,
                                           target = femaleCohabitDecisionTarget_int)

        female_ids <-
          femaleCohabitDecision$get_decision_maker_ids('yes')

        # keep the lower value
        Pop$keep_log(
          var = "avl:cohabitations",
          value = ifelse(
            test = femaleCohabitDecision$get_nrow_result() <= maleCohabitDecision$get_nrow_result(),
            yes = femaleCohabitDecision$get_nrow_result(),
            no = maleCohabitDecision$get_nrow_result())
        )

        cohabitationPool_ls <-
          list(partnerX_ = male_ids, partnerY_ = female_ids)

        return(cohabitationPool_ls)
}

find_partner =
  function(partnerX_dt, partnerY_dt) {
    cohabitationMarket <-
      OptimalCohabitationMarket$new(agentset_A = partnerX_dt,
                                    agentset_B = partnerY_dt)
    matches_dt <- cohabitationMarket$simulate(method = "one-to-one")

    # how many individuals of each gender couldn't find a match?
    individuals_without_matches <-
      matches_dt[rowSums(is.na(matches_dt)) != 0] %>%
      .[, c(id_A, id_B)] %>%
      .[!is.na(.)]
    id_col <- cohabitationMarket$matching_problem$id_col_A
    count_table_by_gender <-
      table(c(partnerX_dt[get(id_col) %in% individuals_without_matches, sex],
              partnerY_dt[get(id_col) %in% individuals_without_matches, sex]))

    if (length(count_table_by_gender) != 0) {
      n_single_male <- ifelse(constants$IND$SEX$MALE %in% names(count_table_by_gender),
                              count_table_by_gender[[constants$IND$SEX$MALE]],
                              0)
      n_single_female <- ifelse(constants$IND$SEX$FEMALE %in% names(count_table_by_gender),
                                count_table_by_gender[[constants$IND$SEX$FEMALE]],
                                0)
      lg$info("There {ifelse(total_single == 1, 'was', 'were')} \\
            {total_single} individuals who couldn't find their matches \\
            (male: {n_single_male}, female: {n_single_female}).",
              total_single = sum(count_table_by_gender))
    }

    # clean and return the match results
    matches_dt <- stats::na.omit(matches_dt) # remove rows with NA

    data.table::setnames(matches_dt,
                         old = c("id_A", "id_B"),
                         new = c("partnerX", "partnerY"))

    matches_dt
  }

remove_incest = function(Pop, partnerX_, partnerY_) {
  Ind <- assign_reference(Pop, Individual)
  # ideally this should check for relationships between two
  # people if they are siblings or in a child-parent relationship
  # but to simplify this we are just going to if they are from the same
  # household in the current simulation time
  partner_x_hids = Ind$get_household_ids(ids = partnerX_)
  partner_y_hids = Ind$get_household_ids(ids = partnerY_)
  Pop$keep_log(var = "warn:removed_incest_cohabitations",
                  value = sum(partner_x_hids == partner_y_hids))
  data.table(partnerX = partnerX_,
             partnerY = partnerY_)[partner_x_hids != partner_y_hids]
}

update_partner = function(Ind, matches_dt) {
  Ind$add_relationship(ids = matches_dt[, partnerX],
                              target_ids = matches_dt[, partnerY],
                              type = "partner")
  invisible()
}

join_new_household = function(Pop, matches_dt) {
  Hh <- assign_reference(Pop, Household)
  Hh$add_new_agents(n = nrow(matches_dt))
  matches_dt[, newHid := Hh$get_new_agent_ids()]
  Pop$leave_household(matches_dt[, partnerX])
  Pop$leave_household(matches_dt[, partnerY])
  Pop$join_household(
    ind_ids = matches_dt[, partnerX],
    hh_ids = matches_dt[, newHid])
  Pop$join_household(
    ind_ids = matches_dt[, partnerY],
    hh_ids = matches_dt[, newHid])
  invisible()
}

move_to_partner_household = function(Pop, selfIds_, partnerHid_) {
  Pop$leave_household(ind_ids = selfIds_)
  Pop$join_household(ind_ids = selfIds_, hh_ids = partnerHid_)
  invisible()
}

# returns data.table(id = integer(), children_ids = list(integer(), ...))
get_dependent_children_dt = function(Pop, self_ids) {

  Ind <- assign_reference(Pop, Individual)

  parentChild_dt <-
    data.table::data.table(id = self_ids) %>%
    .[, children_ids := Ind$get_children(id)] %>%
    .[, lapply(.SD, unlist), by = id] %>%
    data.table::setnames(., old = "children_ids", "child_id") %>%
    .[Ind$living_together(self_ids = id, target_ids = child_id), ] %>%
    # exempting children who are in the cohabitation market
    .[!child_id %in% self_ids] %>%
    data.table::setnames(., old = c("id", "child_id"), new = c("selfId", "childrenId"))
  return(parentChild_dt)
}

filter_dependent_children_and_assign_new_hid = function(Pop, depChildren_dt, selfIds_) {

  Ind <- assign_reference(Pop, Individual)
  self_dependent_children <-
    depChildren_dt[selfId %in% selfIds_]

  # the if condition is to avoid Ind$get_household_ids() from throwing
  # an error that selfId of an empty data.table doesn't exists
  if (nrow(depChildren_dt[selfId %in% selfIds_]) != 0) {
    self_dependent_children[, newHid := Ind$get_household_ids(ids = selfId)]
  }

  return(self_dependent_children)
}

move_dependent_children = function(Pop, parentChild_dt) {
  if (nrow(parentChild_dt) > 0) {
    stopifnot(all(c("selfId", "newHid", "childrenId") %in% names(parentChild_dt)))
    Pop$keep_log(var = "warn:marriage_movedDependentChildren", value = nrow(parentChild_dt))
    Pop$keep_log(var = "id:marriage_movedDependentChildren", value = list(parentChild_dt[, childrenId]))
    Pop$leave_household(ind_ids = parentChild_dt$childrenId)
    Pop$join_household(
      ind_ids = parentChild_dt$childrenId,
      hh_ids = parentChild_dt$newHid
    )
    add_history(
      entity = Pop$get("Individual"),
      ids = parentChild_dt$childrenId,
      event = constants$EVENT$MOVED_WITH_PARENT
    )
  }
  return()
}

update_household = function(Pop, model, matches_dt) {

  Ind <- assign_reference(Pop, Individual)

  # male age > 'x' create new household
  agePartnerX_ <- Ind$get_attr(x = "age", ids = matches_dt[, partnerX])

  matchesCreateNewHh_dt <-
    matches_dt[
      agePartnerX_ <= model$malePartnerAgeRuleToCreateNewHousehold$age]
  matchesJoinHh_dt <-
    matches_dt[
      !agePartnerX_ <= model$malePartnerAgeRuleToCreateNewHousehold$age]

  stopifnot(
    sum(c(nrow(matchesCreateNewHh_dt), nrow(matchesJoinHh_dt))) == nrow(matches_dt)
  )
  depChildren_dt <-
    get_dependent_children_dt(
      Pop = Pop,
      self_ids = matches_dt[, flatten_int(list(partnerX, partnerY))]
    )

  if (nrow(matchesCreateNewHh_dt) > 0) {
    join_new_household(
      Pop = Pop,
      matches_dt = matchesCreateNewHh_dt
    )

    partnerXYWithDepChildren_dt <-
      filter_dependent_children_and_assign_new_hid(
        Pop = Pop,
        depChildren_dt = depChildren_dt,
        selfIds_ = matchesCreateNewHh_dt[, flatten_int(list(partnerX, partnerY))]
      )

    move_dependent_children(
      Pop = Pop,
      parentChild_dt = partnerXYWithDepChildren_dt
    )
  }

  # else join household of partnerX
  if (nrow(matchesJoinHh_dt) > 0) {

    move_to_partner_household(
      Pop = Pop,
      selfIds_ = matchesJoinHh_dt[, partnerY],
      partnerHid_ = Ind$get_household_ids(ids = matchesJoinHh_dt[, partnerX])
    )

    partnerYWithDepchildren_dt <-
      filter_dependent_children_and_assign_new_hid(
        Pop = Pop,
        depChildren_dt = depChildren_dt,
        selfIds_ = matchesJoinHh_dt[, partnerY]
      )

    move_dependent_children(
      Pop = Pop,
      parentChild_dt = partnerYWithDepchildren_dt
    )

  }

  return()
}

# Transition classes -----------------
TransitionCohabitationMale <- R6::R6Class(
  classname = "TransitionCohabitationMale",
  inherit = TransitionClassification,
  public = list(
    filter = function(.data) {
      helpers$FilterAgent$Ind$can_cohabit(.data) %>%
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