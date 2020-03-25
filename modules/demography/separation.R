# It is recommended to assign this module to a variable called: event_demography_separation
# for example: event_demography_separation <- modules::use('modules/demography/separation.R')
# default setup, you may edit the below import statments to match your requirements.
modules::import('dymiumCore')
modules::import('checkmate')
modules::import('R6', 'R6Class')
modules::expose(here::here('modules/demography/logger.R')) # import lgr's logger. To use the logger use 'lg' (default logger's name).
modules::expose(here::here('modules/demography/transitions.R'))
constants <- modules::use(here::here('modules/demography/constants.R'))
helpers <- modules::use(here::here('modules/demography/helpers.R'))
household_formation <- modules::use(here::here('modules/demography/household_formation.R'))

modules::export('^run$|^REQUIRED_MODELS$') # default exported functions

REQUIRED_MODELS <- c("separate_male", 
                     "separate_child_custody", 
                     "separate_hhtype",
                     "separate_hf_random_join")

#' Separation
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

  lg$info('Running Separation')

  # get object references for cleaner code
  Pop <- world$get("Population")
  Ind <- world$get("Individual")
  Hh <- world$get("Household")

  # check model
  if (is.null(model)) {
    model <- dm_get_model(world, REQUIRED_MODELS)
  } else {
    checkmate::assert_names(names(model), type = "unique", identical.to = REQUIRED_MODELS)
  }

  # note that this is a male initiated event
  TransSeparate <- TransitionSeparate$new(
    x = Ind,
    model = model$separate_male,
    target = target
  )
  
  # form a list of separators and their partners
  if (TransSeparate$get_result()[response == 'yes', .N] != 0) {
    separator_ids <- TransSeparate$get_result()[response == 'yes', id]
    partner_ids <- Pop$ind$get_data(ids = separator_ids)[, (partner_id)]
    stopifnot(length(separator_ids) == length(partner_ids))
    separating_couples_list <- list(partner_x = separator_ids, partner_y = partner_ids)  
  } else {
    separating_couples_list <- list(partner_x = integer(0), partner_y = integer(0))  
  }

  # if both partners in a same-sex relationship undergo TransitionBreakup and
  # they both decide to breakup both of their ids will be in `breakingup_persons`
  # in `partner_x` and `partner_y` columns which means there will be duplications
  # of their ids this will raise errors later on the process. Hence we must remove
  # one of the partner from partner_x and partner_y
  if (length(separating_couples_list$partner_x[separating_couples_list$partner_x %in% separating_couples_list$partner_y]) != 0) {
    dups <-
      separating_couples_list$partner_x[separating_couples_list$partner_x %in% separating_couples_list$partner_y]
    pid_col <- Ind$get_id_col()
    dups_x <-
      Ind$get_data() %>%
      .[get(pid_col) %in% dups, .SD, .SDcols = c(pid_col, "partner_id")] %>%
      .[, purrr::map2_int(.x = get(pid_col), .y = partner_id, .f = ~ sort(c(.x, .y))[1])] %>%
      unique()
    dups_y <- dups[!dups %in% dups_x]
    separating_couples_list$partner_x <-
      separating_couples_list$partner_x[!separating_couples_list$partner_x %in% dups_x]
    separating_couples_list$partner_y <-
      separating_couples_list$partner_y[!separating_couples_list$partner_y %in% dups_y]
  }

  lg$info("#seperating couples: {length(separating_couples_list$partner_x)}")
  
  Pop$log(
    desc = "cnt:separations",
    value = length(separating_couples_list$partner_x)
  )
  Pop$log(
    desc = "avl:separations",
    value = TransSeparate$get_nrow_result()
  )
  Pop$log(
    desc = "id:individuals_separated",
    value = list(append(separating_couples_list$partner_x, separating_couples_list$partner_y))
  )

  # apply side-effects
  if (length(separating_couples_list$partner_x) > 0) {

    if (!all(Ind$get_attr("marital_status", unlist(separating_couples_list)) == 
             constants$IND$MARITAL_STATUS$MARRIED)) {
      lg$warn("Not all separating individuals are married!")
    }
    
    # update marital status to 'separated'
    Ind$get_data(copy = FALSE) %>%
      .[get(Ind$get_id_col()) %in% unlist(separating_couples_list),
        `:=`(marital_status = constants$IND$MARITAL_STATUS$SEPARATED)]
    
    add_history(
      entity = Ind,
      ids = c(separating_couples_list$partner_x, separating_couples_list$partner_y),
      event = constants$EVENT$SEPARATION
    )

    # 1) determine child custody - for those partners who have kids  ---------
    custody_to_partner_x <-
      TransitionChildCustody$new(
        x = Ind,
        model = model$separate_child_custody,
        targeted_agents = separating_couples_list$partner_x
      )$get_result()[['response']] == 'yes'

    # those who get child custody get to stay in the current household
    stayers <-
      c(separating_couples_list$partner_x[custody_to_partner_x], separating_couples_list$partner_y[!custody_to_partner_x])
    movers <-
      c(separating_couples_list$partner_x[!custody_to_partner_x], separating_couples_list$partner_y[custody_to_partner_x])

    # 2) remove relationship -----------
    # 2.1) husband and wife remove each other from partner_id
    # note that `remove_relationship` removes the partner_id fields of both of the
    # partners
    Pop$ind$remove_relationship(ids = separating_couples_list$partner_x, type = "partner")
    # 2.2) movers remove self from household
    Pop$leave_household(ind_ids = movers)

    # 3) movers join new households ------------
    TransHhType <- TransitionGroupHousehold$new(
      x = Ind,
      model = model$separate_hhtype, # choicelist
      targeted_agents = movers)
    
    lone_mover_ids <-
      TransHhType$get_result()[response == "lone", id]
    group_mover_ids <-
      TransHhType$get_result()[response == "group", id]
    
    # Lone movers
    if (length(lone_mover_ids) > 0) {
      household_formation$join_new_lone_household(Pop, ids = lone_mover_ids)
    }
    
    # Group movers
    if (length(group_mover_ids) > 0) {
      household_formation$random_join_group_household(
        Pop = Pop,
        ids = group_mover_ids,
        model = model$separate_hf_random_join
      )
    }
  }
  
  invisible(world)
}

TransitionSeparate <- R6Class(
  classname = "TransitionSeparate",
  inherit = TransitionClassification,
  public = list(
    filter = function(.data) {
      .data %>%
        helpers$FilterAgent$Ind$can_separate(.) %>%
        # take the divorcing probabilities from the male partners
        # since I didn't estimate a joint-attribute model for the melborune implementation
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

