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
                     "marriage_no_cohab_female",
                     "husbandAgeRuleToCreateNewHousehold")

OPTIONAL_TARGETS <- c(
  "couple_marriage_decision_target", 
  "non_couple_marriage_decision_target"
)

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

  lg$info("Running Marriage event")

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

  ## cohabiting marriages -------
  marryingCouples_ls <- select_marrying_couples(
    Pop = Pop,
    couple_marriage_decision_model = model$marriage_cohab_male,
    couple_marriage_decision_target = target$couple_marriage_decision_target
  )

  totalMarriageFromCohabitation <- length(marryingCouples_ls$partnerX)
  Pop$keep_log(
    var = "occ:marriages_from_cohabitation",
    value = totalMarriageFromCohabitation)

  if (totalMarriageFromCohabitation > 0) {
    Pop$keep_log(
      var = "id:individuals_married_from_cohabitation",
      value = list(c(marryingCouples_ls$partnerX, marryingCouples_ls$partnerY)))
    become_married(
      Pop = Pop,
      ids = c(marryingCouples_ls$partnerX, marryingCouples_ls$partnerY)
    )
    add_history(
      entity = Ind,
      ids = purrr::flatten_int(marryingCouples_ls),
      event = constants$EVENT$MARRIAGE
    )
  }

  ## non-cohabiting marriages -------
  marriagePool_ls <-
    select_marrying_persons(
      Pop = Pop,
      maleMarriageDecisionModel = model$marriage_no_cohab_male,
      femaleMarriageDecisionModel = model$marriage_no_cohab_female,
      non_couple_marriage_decision_target = target$non_couple_marriage_decision_target
    )

  n_males_looking_to_marry <- length(marriagePool_ls$partnerX)
  n_females_looking_to_marry <- length(marriagePool_ls$partnerY)

  lg$info("{n_males_looking_to_marry} males and {n_females_looking_to_marry} \\
          are entering the marriage market.")

  Pop$keep_log(
    var = "occ:individuals_male_entered_marriage_market",
    value = n_males_looking_to_marry)
  Pop$keep_log(
    var = "occ:individuals_female_entered_marriage_market",
    value = n_females_looking_to_marry)

  ## update individuals and their households -------
  if (n_males_looking_to_marry > 0 && n_females_looking_to_marry > 0) {
    Pop$keep_log(
      var = "id:individuals_entered_marriage_market",
      value = list(flatten_int(marriagePool_ls)))

    matches_dt <-
      find_partner(
        partnerXdt = Pop$ind$get_data(ids = marriagePool_ls$partnerX),
        partnerYdt = Pop$ind$get_data(ids = marriagePool_ls$partnerY)
      )

    matches_dt <-
      remove_incest_matches(
        Pop = Pop,
        partnerX = matches_dt[, partnerX],
        partnerY = matches_dt[, partnerY]
      )

    totalMarriageNoCohabitation <- nrow(matches_dt)
    Pop$keep_log(var = "occ:marriages_no_cohabitation",
                    value = totalMarriageNoCohabitation)
    Pop$keep_log(var = "id:individuals_married_no_prior_cohabitation",
                    value = list(matches_dt[, c(partnerX,partnerY)]))

    become_married(Pop, ids = matches_dt[, c(partnerX, partnerY)])
    update_partner(Pop, matches_dt = matches_dt)
    add_history(entity = Pop$get("Individual"),
                ids = matches_dt[, c(partnerX, partnerY)],
                event = constants$EVENT$MARRIAGE)
    update_household(Pop, model$husbandAgeRuleToCreateNewHousehold, matches_dt = matches_dt)
  } else {
    totalMarriageNoCohabitation <- 0
  }

  total_marriages <- totalMarriageFromCohabitation + totalMarriageNoCohabitation
  lg$info("There were {total_marriages} marriages occured \\
          (priorly cohabited: {totalMarriageFromCohabitation}, \\
          did not cohabited: {totalMarriageNoCohabitation})")
  Pop$keep_log(
    var = "occ:marriages",
    value = totalMarriageFromCohabitation + totalMarriageNoCohabitation)

  invisible(world)
}

# private utility functions (.util_*) -------------------------------------
select_marrying_couples = function(Pop,
                                   couple_marriage_decision_model,
                                   couple_marriage_decision_target) {

  Ind <- assign_reference(Pop, Individual)

  TransMarriageCohabited <-
    TransitionMarriageCohabited$new(
      x = Ind,
      model = couple_marriage_decision_model,
      target = couple_marriage_decision_target
    )

  male_ids <-
    TransMarriageCohabited$get_decision_maker_ids('yes')

  female_ids <-
    Ind$get_attr(x = "partner_id", ids = male_ids)

  # ids of same sex partners can appear twice, in `male_ids` and `female_ids`. Hence,
  # those probamatic rows must be filtered out to make sure each id should only
  # appear in either of the vectors but not both.
  couple_ids_dt <-
    data.table(male_id = male_ids, female_id = female_ids) %>%
    # create a 'z' column where both ids of the couple are orderly concatenated into a
    # string
    .[, z := purrr::map2_chr(male_id, female_id, ~ paste(sort(c(.x, .y)), collapse = "-"))] %>%
    # filter rows with duplicated 'z' values. This means that those rows are same-sex
    # couples
    unique(., by = "z") %>%
    # remove the 'z' column
    .[, z := NULL]

  checkmate::assert_integerish(couple_ids_dt[, unlist(male_id, female_id)],
                               lower = 1, any.missing = FALSE, unique = TRUE)

  Pop$keep_log(
    var = "avl:marriages_from_cohabitation",
    value = TransMarriageCohabited$get_nrow_result())

  return(list(partnerX = couple_ids_dt[["male_id"]],
              partnerY = couple_ids_dt[["female_id"]]))
}

select_marrying_persons = function(Pop,
                                   maleMarriageDecisionModel,
                                   femaleMarriageDecisionModel,
                                   non_couple_marriage_decision_target) {

  Ind <- assign_reference(Pop, Individual)

  maleToMarryDecision <-
    TransitionMarriageNoCohabitationMale$new(
      x = Ind,
      model = maleMarriageDecisionModel,
      target = non_couple_marriage_decision_target
    )

  # debugging plot
  if (FALSE) {
    maleToMarryDecision$get_result()[Ind$get_data()[, .(pid, age, marital_status)], , on = .(id = pid), nomatch = 0] %>%
      .[, response := as.factor(response)] %>%
      tabplot::tableplot(dat = ., select = c(age, marital_status, response))
  }

  maleId_ <-
    maleToMarryDecision$get_decision_maker_ids('yes')

  femaleToMarryDecision <-
    TransitionMarriageNoCohabitationFemale$new(
      x = Ind,
      model = femaleMarriageDecisionModel,
      target = non_couple_marriage_decision_target
    )

  # debugging plot
  if (FALSE) {
    femaleToMarryDecision$get_result()[Ind$get_data()[, .(pid, age, marital_status)], , on = .(id = pid), nomatch = 0] %>%
      .[, response := as.factor(response)] %>%
      tabplot::tableplot(dat = ., select = c(age, marital_status, response))
  }

  femaleId_ <-
    femaleToMarryDecision$get_decision_maker_ids('yes')

  # keep the lower count
  Pop$keep_log(
    var = "avl:marriages_no_cohabitation",
    value = ifelse(
      test = femaleToMarryDecision$get_nrow_result() <= maleToMarryDecision$get_nrow_result(),
      yes = femaleToMarryDecision$get_nrow_result(),
      no = maleToMarryDecision$get_nrow_result())
  )

  marriagePool_ls <-
    list(partnerX = maleId_, partnerY = femaleId_)

  return(marriagePool_ls)
}

# partnerX is male so partnerY is female
find_partner = function(partnerXdt, partnerYdt) {
  marriageMarket <-
    OptimalMarriageMarket$new(agentset_A = partnerXdt,
                              agentset_B = partnerYdt)
  matches_dt <- marriageMarket$simulate(method = "one-to-one")

  # how many individuals of each gender couldn't find a match?
  individuals_without_matches <-
    matches_dt[rowSums(is.na(matches_dt)) != 0] %>%
    .[, c(id_A, id_B)] %>%
    .[!is.na(.)]
  id_col <- marriageMarket$matching_problem$id_col_A
  count_table_by_gender <-
    table(c(partnerXdt[get(id_col) %in% individuals_without_matches, sex],
            partnerYdt[get(id_col) %in% individuals_without_matches, sex]))
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

  return(matches_dt)
}

remove_incest_matches = function(Pop, partnerX, partnerY) {
  # ideally this should check for relationships between two
  # people if they are siblings or in a child-parent relationship
  # but to simplify this we are just going to if they are from the same
  # household in the current simulation time
  hidpartnerX = Pop$get(Individual)$get_household_ids(ids = partnerX)
  hidpartnerY = Pop$get(Individual)$get_household_ids(ids = partnerY)
  Pop$keep_log(var = "warn:removed_incest_marriages",
                  value = sum(hidpartnerX == hidpartnerY))
  return(data.table(partnerX = partnerX, partnerY = partnerY)[hidpartnerX != hidpartnerY])
}

become_married = function(Pop, ids){
  checkmate::assert_integerish(ids, lower = 1, any.missing = FALSE, null.ok = FALSE, unique = TRUE)
  Pop$get(Individual)$get_data(copy = FALSE)[pid %in% ids,
                                marital_status := constants$IND$MARITAL_STATUS$MARRIED]
  return()
}

update_partner = function(Pop, matches_dt) {
  Pop$get(Individual)$add_relationship(ids = matches_dt[, partnerX],
                              target_ids = matches_dt[, partnerY],
                              type = "partner")

  return()
}

join_new_household = function(Pop, matches_dt) {
  Pop$get(Household)$add_new_agents(n = nrow(matches_dt))
  matches_dt[, newHid := Pop$get(Household)$get_new_agent_ids()]

  Pop$leave_household(ind_ids = matches_dt[, partnerX])
  Pop$leave_household(ind_ids = matches_dt[, partnerY])

  Pop$join_household(
    ind_ids = matches_dt[, partnerX],
    hh_ids = matches_dt[, newHid])
  Pop$join_household(
    ind_ids = matches_dt[, partnerY],
    hh_ids = matches_dt[, newHid])

  return()
}

join_partner_household = function(Pop, ids, partner_hids) {
  Pop$leave_household(ind_ids = ids)
  Pop$join_household(ind_ids = ids, hh_ids = partner_hids)
  add_history(
    entity = Pop$get("Individual"),
    ids = ids,
    event = constants$EVENT$JOINED_PARTNER_HOUSEHOLD
  )
  return()
}

# returns data.table(id = integer(), children_ids = list(integer(), ...))
# only for those with dependent children
get_dependent_children = function(Pop, ids) {
  data.table::data.table(id = ids) %>%
    .[, children_ids := Pop$ind$get_children(id)] %>%
    .[, lapply(.SD, unlist), by = id] %>%
    data.table::setnames(., old = "children_ids", "child_id") %>%
    .[Pop$get(Individual)$living_together(self_ids = id, target_ids = child_id), ] %>%
    # exempting children who are in the cohabitation market
    .[!child_id %in% ids] %>%
    # group children in to list column corresponding to their parents' entry
    .[, .(children_ids = list(child_id)), by = id] %>%
    # arrange the data.table in the same order as the input `ids`
    .[data.table(id = ids), , on = .(id)] %>%
    .[!purrr::map_lgl(children_ids, is.null), ]
}

move_dependent_children = function(Pop,
                                   parent_children_dt) {
  if (nrow(parent_children_dt) == 0) {
    return(invisible())
  }
  # impute parent's new household id -------
  parent_children_dt <-
    parent_children_dt[, new_hid := Pop$ind$get_household_ids(ids = id)] %>%
    # unnest the list column `children_ids`
    .[, lapply(.SD, unlist), id] %>%
    data.table::setnames(old = "children_ids", "child_id")
  checkmate::assert_names(names(parent_children_dt), identical.to = c("id", "child_id", "new_hid"))
  Pop$keep_log(var = "warn:marriage_movedDependentChildren", value = nrow(parent_children_dt))
  Pop$keep_log(var = "id:marriage_movedDependentChildren", value = list(parent_children_dt[["child_id"]]))
  # move to parent's new household -------
  Pop$leave_household(ind_ids = parent_children_dt[["child_id"]])
  Pop$join_household(ind_ids = parent_children_dt[["child_id"]],
                        hh_ids = parent_children_dt[["new_hid"]])
  add_history(
    entity = Pop$get("Individual"),
    ids = parent_children_dt[["child_id"]],
    event = constants$EVENT$MOVED_WITH_PARENT
  )
  invisible()
}


update_household = function(Pop, model, matches_dt) {
  # husband age > 'x' create new household
  agepartnerX <- Pop$ind$get_attr(x = "age", ids = matches_dt[, partnerX])

  # classify household formation type -----
  matches_new_hh <-
    matches_dt[
      agepartnerX <= model]

  matches_join_hh <-
    matches_dt[
      !agepartnerX <= model]

  stopifnot(
    sum(c(nrow(matches_new_hh), nrow(matches_join_hh))) == nrow(matches_dt)
  )

  # identify dependent children -------
  # **before moving household**
  parent_children_dt <-
    get_dependent_children(
      Pop = Pop,
      ids = matches_dt[, c(partnerX, partnerY)]
    )

  # household formation -------
  if (nrow(matches_new_hh) > 0) {
    join_new_household(Pop = Pop,
                                     matches_dt = matches_new_hh)
  }

  if (nrow(matches_join_hh) > 0) {
    join_partner_household(
      Pop = Pop,
      ids = matches_join_hh[, partnerY],
      partner_hids = Pop$ind$get_household_ids(ids = matches_join_hh[, partnerX])
    )
  }

  # move dependent children to new households -------
  move_dependent_children(Pop = Pop,
                                        parent_children_dt = parent_children_dt)

  return()
}


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

# exported utility functions (util_*) -------------------------------------