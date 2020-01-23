# It is recommended to assign this module to a variable called: event_demography_divorce
# for example: event_demography_divorce <- modules::use('modules/demography/divorce.R')
# default setup, you may edit the below import statments to match your requirements.
modules::import('dymiumCore')
modules::import('data.table')
modules::import('R6', 'R6Class')
modules::import('checkmate')
modules::expose(here::here('modules/demography/logger.R')) # import lgr's logger. To use the logger use 'lg' (default logger's name).
constants <- modules::use(here::here('modules/demography/constants.R'))
helpers <- modules::use(here::here('modules/demography/helpers.R'))

modules::export('^run$|^REQUIRED_MODELS$') # default exported functions

REQUIRED_MODELS <- c("divorce_male", "divorce_female")

#' Divorce
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

  lg$info("Running Divorce")

  Pop <- assign_reference(world, Population)
  Ind <- assign_reference(world, Individual)

  # check model
  if (is.null(model)) {
    model <- dm_get_model(world, REQUIRED_MODELS)
  } else {
    checkmate::assert_names(names(model), type = "unique", identical.to = REQUIRED_MODELS)
  }

  TransDivorceMale <- TransitionDivorceMale$new(
    x = Ind,
    model = model$divorce_male,
    target = target
  )

  TransDivorceFemale <- TransitionDivorceFemale$new(
    x = Ind,
    model = model$divorce_female,
    target = target
  )

  divorcer_ids <-
    c(TransDivorceMale$get_decision_maker_ids('yes'),
      TransDivorceFemale$get_decision_maker_ids('yes'))

  # TODO: find the former partner and set to divorce

  Pop$log(
    desc = "cnt:divorces",
    value = length(divorcer_ids)
  )
  Pop$log(
    desc = "avl:divorces",
    value = TransDivorceMale$get_nrow_result() + TransDivorceFemale$get_nrow_result()
  )
  Pop$log(
    desc = "id:individuals_divorced",
    value = list(divorcer_ids)
  )

  lg$info("#individuals to divorce: {.n_divorcers}",
          .n_divorcers = length(divorcer_ids))

  # apply side-effects
  if (length(divorcer_ids) > 0) {

    # change marital status
    become_divorced(Pop = Pop, ids = divorcer_ids)

    add_history(entity = Ind,
                ids = divorcer_ids,
                event = constants$EVENT$DIVORCE)

  }

  # return the first argument (`object`) to make event functions pipe-able.
  invisible(world)
}

# private utility functions (.util_*) -------------------------------------
become_divorced <- function(Pop, ids) {

  Ind <- Pop$get("Individual")
  ind_data <- Ind$get_data()
  id_col <- Ind$get_id_col()
  self_idx <- Ind$get_idx(ids)

  if (!all(Ind$get_attr("marital_status", ids) ==
          constants$IND$MARITAL_STATUS$SEPARATED)) {
    stop("Not all divorcing individuals are separated!")
  }

  # change marital status to 'divorced'
  data.table::set(x = Ind$get_data(copy = FALSE), 
                  i = self_idx, 
                  j = "marital_status", 
                  value = constants$IND$MARITAL_STATUS$DIVORCED)

  # `.past_partner_id` only exists in remove_relationship(type = "partner") 
  # has been called. Meaning if you run divorce before separation there will
  # not be a `.past_partner_id` column in your individual agents' attribute data
  if (".past_partner_id" %in% names(ind_data)) {
    
    # only keep the records of individuals that most recent partners are matched
    partner_ids <- ind_data[self_idx, .past_partner_id] %>%
      .[!is.na(.)] %>%
      .[Ind$is_alive(.)] # partner must be alived
    
    partner_idx <- Ind$get_idx(partner_ids)
    
    cols <- c(id_col, ".past_partner_id")
    
    self_rel <- ind_data[self_idx, .SD, .SDcol = cols] %>%
      setnames(old = cols, new = c("self", "partner"))
    
    partner_rel <- ind_data[partner_idx, .SD, .SDcol = cols] %>%
      setnames(old = cols, new = c("partner", "self"))
    
    partner_ids <- merge(partner_rel, self_rel, by = "self") %>%
      .[partner.x == partner.y, partner.x]
    
    # past partner also become divorced if his/her marital status is still 'separated'
    # and their most recent partner was the initiating individual
    Ind$get_data(copy = FALSE) %>%
      .[get(id_col) %in% partner_ids &  marital_status == constants$IND$MARITAL_STATUS$SEPARATED,
        marital_status := constants$IND$MARITAL_STATUS$DIVORCED]  
    
    add_history(entity = Ind,
                ids = partner_ids,
                event = constants$EVENT$DIVORCE)
  }
  
  return(invisible())
}

TransitionDivorceMale <- R6Class(
  classname = "TransitionDivorce",
  inherit = TransitionClassification,
  public = list(
    filter = function(.data) {
      .data %>%
        helpers$FilterAgent$Ind$can_divorce(.) %>%
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

TransitionDivorceFemale <- R6Class(
  classname = "TransitionDivorce",
  inherit = TransitionClassification,
  public = list(
    filter = function(.data) {
      .data %>%
        helpers$FilterAgent$Ind$can_divorce(.) %>%
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

