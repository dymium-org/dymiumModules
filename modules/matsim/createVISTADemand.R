# It is recommended to assign this module to a variable called: event_matsim_fusevista
# for example: event_matsim_createVISTADemand <- modules::use('modules/matsim/createVISTADemand.R')
# default setup, you may edit the below import statments to match your requirements.
modules::import('dymiumCore')
modules::import('StatMatch')
modules::import('data.table')
modules::import('checkmate')
modules::import('fs')
modules::expose(here::here('modules/matsim/logger.R')) # import lgr's logger. To use the logger use 'lg' (default logger's name).
constants <- modules::use(here::here('modules/matsim/constants.R'))
helpers <- modules::use(here::here('modules/matsim/helpers.R'))

modules::export('^run$|^REQUIRED_MODELS$') # default exported functions

REQUIRED_MODELS <-
  c("vista_persons",
    "vista_trips")

#' createVISTADemand
#'
#' @param world a dymium agent class object
#' @param model a model object or a list of model objects
#' @param target a positive integers or a list of positive integers
#' @param time_steps positive integer()
#'
#' @return world
run <- function(world, model = NULL, target = NULL, time_steps = NULL) {

  # early return if `time_steps` is not the current time
  if (!dymiumCore::is_scheduled(time_steps)) {
    return(invisible(world))
  }

  lg$info('Running createVISTADemand')

  # check model
  model <- pick_models(model, world, REQUIRED_MODELS)

  # uncomment the line belows if the event doesn't require `model`
  # eg. If the event is deterministic like ageing.
  # if (!is.null(model)) {
  #   lg$warn('`model` will not be used.')
  # }

  # uncomment the line belows if the event doesn't require `target`
  # eg. If the event is to be applied to all agents.
  if (!is.null(target)) {
    lg$warn('`target` will not be used.')
  }

  # create a reference to the main agent objects for easy access
  Ind <- world$get("Individual")
  Hh <- world$get("Household")

  # create travel demand ----------------------------------------------------

  # fuse activity pattern to Individual agents.
  create_matsim_plan(trips = fuse_vista(Ind, world$models),
                     outdir = file.path(get_active_scenario()$scenario_dir, "inputs/matsim"))

  # return the first argument (`world`) to make event functions pipe-able.
  invisible(world)
}

fuse_vista <- function(Ind, models) {
  .start_time <- Sys.time()
  lg$info("Fusing VISTA person with dymium individuals.")

  # StatMatch only works with data.frames
  don <- models$vista_persons$get() %>%
    .[ADPERSWGT > 0] %>%
    as.data.frame()

  rec <- Ind$get_data() %>%
    as.data.frame()

  # estimate fusion
  fused_gower_result <- StatMatch::RANDwNND.hotdeck(data.rec = rec,
                                                    data.don = don,
                                                    don.class = c('sex'),
                                                    match.vars = 'age',
                                                    dist.fun = 'Gower',
                                                    cut.don = 'min',
                                                    weight.don = 'ADPERSWGT')

  lg$info('Summary of minimum gower distance fusion result')
  print(summary(fused_gower_result$sum.dist[, 1]))

  # build fused persons
  pid_col <- Ind$get_id_col()
  fused_persons <- create.fused(
    data.rec = rec,
    data.don = don,
    mtc.ids = fused_gower_result$mtc.ids,
    z.vars = "PERSID"
  ) %>%
    data.table::setDT(.) %>%
    .[, .SD, .SDcols = c('PERSID', pid_col)] %>%
    # make PERSID unique but still retains the original column for merging
    .[, .(VISTA_PERSID = PERSID,
          PERSID = paste0(pid_col, get(pid_col), PERSID))]

  lg$info("Activity patterns from {data.table::uniqueN(fused_persons[['VISTA_PERSID']])} unique \\
          person records from VISTA 2009 being used to represent the travel demand \\
          of dymium individuals")

  trips <-
    merge(fused_persons, models$vista_trips$get(), by.x = 'VISTA_PERSID', by.y = 'PERSID', all.x = TRUE, allow.cartesian = T) %>%
    # some people didn't make any trips so we need to filter them out
    .[!is.na(TRIPID)]

  # save some stats
  Ind$log(desc = "cnt:number_of_trips", value = nrow(trips))
  mode_counts <- table(trips$Mode_Group)
  for (i in seq_along(mode_counts)) {
    clean_mode_name <- gsub(" ", "_", tolower(names(mode_counts)[i]))
    Ind$log(desc = paste0("cnt:ModeOfTransport-", clean_mode_name), value = mode_counts[i])
  }

  lg$info("{nrow(trips)} trips have been generated.")
  lg$info("Finished in ", format(Sys.time() - .start_time))
  invisible(trips)
}

create_matsim_plan <-
  function(trips, outdir = here::here()) {
    checkmate::expect_data_table(trips, min.rows = 1, col.names = 'strict')
    lg$info("Start generating a MATSim plan file from {nrow(trips)} trips")
    .start_time <- Sys.time()
    vec <- rep(NA, 2000000)
    vec_i <- 1

    add_to_vec <- function(x) {
      vec[vec_i] <<- x
      vec_i[1] <<- vec_i + 1L
    }

    add_activity <- function(type, coord_x, coord_y, end_time) {
      add_to_vec(paste0("<activity type=\"", type, "\" x=\"", coord_x, "\" y=\"", coord_y, "\" end_time=\"", end_time, "\" > </activity>"))
    }

    add_last_activity <- function(type, coord_x, coord_y) {
      add_to_vec(paste0("<activity type=\"", type, "\" x=\"", coord_x, "\" y=\"", coord_y, "\" > </activity>"))
    }

    add_leg <- function(mode) {
      add_to_vec(paste0("<leg mode=\"", mode, "\" > </leg>"))
    }

    add_line <- function(x) {
      add_to_vec(x)
    }

    setorder(trips, PERSID, TRIPID)
    all_PERSID <- trips[, unique(PERSID)]
    # utils::head(trips[, .(VISTA_PERSID, PERSID, TRIPID)], 50)
    # browser()

    add_line("<!DOCTYPE population SYSTEM \"http://www.matsim.org/files/dtd/population_v6.dtd\">")
    add_line("<population>")
    # for (id in all_PERSID[1:500]) {
    for (id in all_PERSID) {
      indices_of_id <- trips[PERSID == id, which = TRUE]
      length_indices_of_id <- length(indices_of_id)
      add_line(paste0("<person id=\"", id, "\">"))
      add_line("<plan selected=\"yes\">")
      for (i in seq_along(indices_of_id)) {
        current_index <- indices_of_id[i]
        if (i == 1) {
          add_activity(type = trips[['ORIGPURP1']][current_index], coord_x = trips[['ORIG_X']][current_index], coord_y = trips[['ORIG_Y']][current_index], end_time = trips[['START_TIME']][current_index])
        }
        if (i == length_indices_of_id) {
          add_leg(mode = trips[current_index, Mode_Group])
          add_last_activity(type = trips[['DESTPURP1']][current_index], coord_x = trips[['DEST_X']][current_index],coord_y = trips[['DEST_Y']][current_index])
        } else {
          add_leg(mode = trips[current_index, Mode_Group])
          add_activity(type = trips[['DESTPURP1']][current_index], coord_x = trips[['DEST_X']][current_index], coord_y = trips[['DEST_Y']][current_index], end_time = trips[['DEP_TIME']][current_index])
        }
      }
      add_line("</plan>")
      add_line("</person>")
    }
    add_line("</population>")
    x <- paste0(vec[!is.na(vec)], collapse = "")
    # for validation the xml with a MATSim DTD file
    myxml <- xml2::read_xml(x, options = "DTDVALID")
    # check if the output directory exists
    if (!fs::dir_exists(outdir)) {
      fs::dir_create(outdir)
    }
    # write the plan file to the output directory
    xml2::write_xml(myxml, file.path(outdir, "plans-dymium.xml"), options = c("format", "as_xml"))
    lg$info("Saved `plan-dymium.xml` at: '{outdir}'")
    lg$info("Finished in ", format(Sys.time() - .start_time))
  }
