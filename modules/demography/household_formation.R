modules::import('dymiumCore')
modules::import("data.table")
modules::import("checkmate")
constants <- modules::use(here::here('modules/demography/constants.R'))

modules::export("random_join_group_household", "join_new_lone_household")

#' Individual joins group household
#'
#' @param Pop a [dymiumCore::Population] object.
#' @param ids ids of individuals
#' @param model a named list or named vector that its names are household sizes
#'  and its values their relative selection weights. 
#'
#' @return NULL
random_join_group_household <- function(Pop, ids, model) {
  
  checkmate::assert_r6(Pop, classes = "Population")
  Hh <- Pop$get("Household")
  Ind <- Pop$get("Individual")
  
  dymiumCore::assert_entity_ids(x = Ind, ids = ids, informative = TRUE)
  checkmate::assert(
    checkmate::check_list(model, types = "numeric", any.missing = FALSE, names = "unique"),
    checkmate::check_numeric(model, finite = T, any.missing = FALSE, names = "unique"), 
    combine = "or"
  )
  
  # make sure that hhsize is up-to-date and has no emptied households.
  Pop$remove_emptied_households(update_hhsize = TRUE)
  
  # choose preferred household size to join
  group_leavers_hhsize_pref <-
    sample(names(model),
           length(ids),
           replace = TRUE,
           prob = model) %>%
    as.integer()
  
  # create place holders and information vectors
  hids <- Hh$get_ids()
  hhsize <- Hh$get_attr(x = "hhsize")
  hids_to_join <- vector(mode = "integer", length = length(ids))

  # simulate household selection
  for (.group_mover_idx in seq_along(ids)) {
    # get the hhsize pref of the current chooser
    .hhsize_pref <- group_leavers_hhsize_pref[.group_mover_idx]
    # draw one of the households with size equals to the prefered size
    # Note that, if the preferred household size cannot be satisfied then the agent
    # randomly join one of the existing households
    .potential_hids <- hids[which(hhsize == .hhsize_pref)]
    if (length(.potential_hids) != 0) {
      .hid_to_join <- dymiumCore::sample_choice(x = .potential_hids, size = 1L)
    } else {
      .hid_to_join <- dymiumCore::sample_choice(x = hids, size = 1)
      .hhsize_pref <- hhsize[which(hids == .hid_to_join)] # over write agent's prefered hhsize
    }
    # store the chosen household id
    hids_to_join[.group_mover_idx] <- .hid_to_join
    # increase the size of the chosen household by one
    hhsize[which(hids_to_join[.group_mover_idx] == .hid_to_join)] <- .hhsize_pref + 1L
  }
  
  # add to history
  add_history(entity = Ind,
              ids = ids,
              event = constants$EVENT$JOINED_EXISTING_HOUSEHOLD)
  
  # group leavers join their chosen households
  Pop$join_household(ind_ids = ids, hh_ids = hids_to_join)
  
  invisible()
}

#' Individual create and join new lone household
#'
#' @param Pop a dymiumCore::Population object
#' @param ids ids of individual agents 
#'
#' @return NULL
join_new_lone_household <- function(Pop, ids) {
  checkmate::assert_r6(x = Pop, classes = "Population")
  
  Hh <- Pop$get("Household")
  Ind <- Pop$get("Individual")
  
  # create new emptied households
  Hh$add(n = length(ids))
  # get the new emptied households' ids
  new_hids <- Hh$get_new_agent_ids()
  # add to history
  add_history(
    entity = Ind,
    ids = ids,
    event = constants$EVENT$CREATE_NEW_HOUSEHOLD
  )
  # leavers join their new lone person households
  Pop$join_household(ind_ids = ids, hh_ids = new_hids)
  
  invisible()
}