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

REQUIRED_MODELS <-
  c(
    "leavehome_male",
    "leavehome_female",
    "leavehome_hhtype",
    "leavehome_hf_random_join"
  )

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
  
  Pop <- world$get("Population")
  Hh <- world$get("Household")
  Ind <- world$get("Individual")
  
  # check model
  model <- pick_models(model, world, REQUIRED_MODELS)
  
  # mainly those that are adults and still living with parents
  potential_leavers_dt <-
    helpers$FilterAgent$Ind$can_leave_parentalhome(Ind) %>%
    .[, .(id = list(list(get(Ind$get_id_col())))), by = sex]
  
  TransLeaveHomeMales <-
    TransitionLeaveHome$new(Ind,
                            model = model$leavehome_male,
                            targeted_agent = potential_leavers_dt[sex == constants$IND$SEX$MALE, 
                                                                  unlist(id)])
  
  TransLeaveHomeFemales <-
    TransitionLeaveHome$new(Ind,
                            model = model$leavehome_male,
                            targeted_agent = potential_leavers_dt[sex == constants$IND$SEX$FEMALE,
                                                                  unlist(id)])
  
  leaver_ids <-
    c(TransLeaveHomeFemales$get_result()[response == "yes", id],
      TransLeaveHomeMales$get_result()[response == "yes", id])
  
  number_of_leavers <- length(leaver_ids)
  
  Pop$log(
    desc = "avl:individuals_male_left_parental_homes",
    value = TransLeaveHomeMales$get_nrow_result()
  )
  
  Pop$log(
    desc = "avl:individuals_female_left_parental_homes",
    value = TransLeaveHomeFemales$get_nrow_result()
  )
  
  Pop$log(
    desc = "avl:demography-left_home",
    value = TransLeaveHomeFemales$get_nrow_result() + TransLeaveHomeMales$get_nrow_result()
  )
  
  Pop$log(
    desc = "cnt:left_home",
    value = number_of_leavers
  )
  
  lg$info("There {be} {number_of_leavers} individuals leaving their parental homes.",
          be = ifelse(number_of_leavers > 1, "are", "is"))
  
  if (number_of_leavers > 0) {
    
    # all leavers are leaving their current households
    Pop$leave_household(ind_ids = leaver_ids)
    
    add_history(
      entity = Ind,
      ids = leaver_ids,
      event = constants$EVENT$LEFT_HOME
    )
    
    # simulate leavers' household formation preference
    TransHhtype <-
      TransitionClassification$new(Ind,
                                   model = model$leavehome_hhtype,
                                   targeted_agents = leaver_ids)
    
    lone_leaver_ids <- TransHhtype$get_result()[response == "lone", id]
    group_leaver_ids <- TransHhtype$get_result()[response == "group", id]
    
    # lone household ------------
    if (length(lone_leaver_ids) > 0) {
      # create new emptied households
      Hh$add(n = length(lone_leaver_ids))
      # get the new emptied households' ids
      new_hids <- Hh$get_new_agent_ids()
      # add to history
      add_history(
        entity = Ind,
        ids = lone_leaver_ids,
        event = constants$EVENT$CREATE_NEW_HOUSEHOLD
      )
      # leavers join their new lone person households
      Pop$join_household(ind_ids = lone_leaver_ids, hh_ids = new_hids)
    }
    
    # group household ----------
    if (length(group_leaver_ids) > 0) {
      group_leavers_hhsize_pref <-
        sample(names(model$leavehome_hf_random_join),
               length(group_leaver_ids),
               replace = TRUE,
               prob = model$leavehome_hf_random_join) %>%
        as.integer()
      
      # make sure that hhsize is up-to-date.
      Pop$update_hhsize()
      # create place holders and information vectors
      hids <- Hh$get_ids()
      hhsize <- Hh$get_attr(x = "hhsize")
      hids_to_join <- vector(mode = "integer", length = length(group_leaver_ids))
      
      # simulate household selection
      for (.group_leaver_idx in seq_along(group_leaver_ids)) {
        # get the hhsize pref of the current chooser
        .hhsize_pref <- group_leavers_hhsize_pref[.group_leaver_idx]
        # draw one of the households with size equals to the prefered size
        .hid_to_join <- sample(x = hids[which(hhsize == .hhsize_pref)], size = 1)
        # store the chosen household id
        hids_to_join[.group_leaver_idx] <- .hid_to_join
        # increase the size of the chosen household by one
        hhsize[which(hids_to_join[.group_leaver_idx] == .hid_to_join)] <- .hhsize_pref + 1L
      }
      
      # add to history
      add_history(entity = Ind,
                  ids = group_leaver_ids,
                  event = constants$EVENT$JOINED_EXISTING_HOUSEHOLD)
      
      # group leavers join their chosen households
      Pop$join_household(ind_ids = group_leaver_ids, hh_ids = hids_to_join)
    }
    
  }
  
  invisible(world)
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

