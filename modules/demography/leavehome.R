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
household_formation <- modules::use(here::here('modules/demography/household_formation.R'))

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
  
  # create a placeholder
  leaver_ids <- c()
  n_avl_male_leavers <- 0
  n_avl_female_leavers <- 0
  
  # these if statements are to make sure that 
  # > potential_leavers_dt[sex == constants$IND$SEX$MALE, unlist(id)]
  # won't return NULL and make Transition thinks that all agents are 
  # to be considered, when no agents are eligible to leave parental home.
  if (nrow(potential_leavers_dt[sex == constants$IND$SEX$MALE, ]) != 0) {
    TransLeaveHomeMales <-
      TransitionLeaveHome$new(Ind,
                              model = model$leavehome_male,
                              targeted_agent = potential_leavers_dt[sex == constants$IND$SEX$MALE,
                                                                    unlist(id)])
    leaver_ids <-
      c(leaver_ids,
        TransLeaveHomeMales$get_result()[response == "yes", id])
    
    n_avl_male_leavers <- TransLeaveHomeMales$get_nrow_result()
  } 
  
  if (nrow(potential_leavers_dt[sex == constants$IND$SEX$FEMALE, ]) != 0) {
    TransLeaveHomeFemales <-
      TransitionLeaveHome$new(Ind,
                              model = model$leavehome_male,
                              targeted_agent = potential_leavers_dt[sex == constants$IND$SEX$FEMALE,
                                                                    unlist(id)])
    leaver_ids <-
      c(leaver_ids,
        TransLeaveHomeFemales$get_result()[response == "yes", id])
    
    n_avl_female_leavers <- TransLeaveHomeFemales$get_nrow_result()
  }
  
  number_of_leavers <- length(leaver_ids)
  
  Pop$log(
    desc = "avl:individuals_male_left_parental_homes",
    value = n_avl_male_leavers
  )
  
  Pop$log(
    desc = "avl:individuals_female_left_parental_homes",
    value = n_avl_female_leavers
  )
  
  Pop$log(
    desc = "avl:left_home",
    value = n_avl_male_leavers + n_avl_female_leavers
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
      household_formation$join_new_lone_household(Pop, ids = lone_leaver_ids)
    }
    
    # group household ----------
    if (length(group_leaver_ids) > 0) {
      household_formation$random_join_group_household(
        Pop = Pop,
        ids = group_leaver_ids,
        model = model$leavehome_hf_random_join
      )
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

