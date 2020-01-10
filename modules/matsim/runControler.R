# It is recommended to assign this module to a variable called: event_matsim_runcontroler
# for example: event_matsim_runcontroler <- modules::use('modules/matsim/runControler.R')
# default setup, you may edit the below import statments to match your requirements.
modules::import('dymiumCore')
modules::import('here', 'here')
modules::import('rJava', '.jinit', '.jnew', '.jcall')
# modules::import('rJava', '.jnew', '.jinit')
modules::expose(here::here('modules/matsim/logger.R')) # import lgr's logger. To use the logger use 'lg' (default logger's name).
constants <- modules::use(here::here('modules/matsim/constants.R'))
helpers <- modules::use(here::here('modules/matsim/helpers.R'))

modules::export('^run$|^REQUIRED_MODELS$') # default exported functions

REQUIRED_MODELS <-
  c("config",
    "lastIteration")

#' runControler
#'
#' This function calls a matsim.jar executable file.
#'
#' @param object a dymium agent class object
#' @param model a matsin config file
#' @param target a positive integers or a list of positive integers
#' @param time_steps positive integer()
#'
#' @return object
run <- function(world, model = NULL, target = NULL, time_steps = NULL) {

  # early return if `time_steps` is not the current time
  if (!dymiumCore::is_scheduled(time_steps)) {
    return(invisible(world))
  }

  lg$info('Running runControler')

  # run matsim
  .start_time <- Sys.time()
  execute_matsim(model, world)
  lg$info("Finished in ", format(Sys.time() - .start_time))

  # return the first argument (`object`) to make event functions pipe-able.
  invisible(world)
}

# private utility functions (.util_*) -------------------------------------
execute_matsim = function(model, world) {
  # check model params
  checkmate::check_names(names(model), must.include = c('config', 'lastIteration'))
  checkmate::assert_file_exists(model$config, access = 'wr')
  checkmate::assert_integerish(model$lastIteration, lower = 0, len = 1, null.ok = FALSE)

  .config <- model$config
  .lastIteration <- model$lastIteration

  # create output directory
  outdir <- file.path(active_scenario()$output_dir,
                      paste0('iter-', world$get_time()), 'matsim')
  base::dir.create(outdir, recursive = T)

  # modify the config file
  cf <- helpers$MatsimConfig$new(.config)
  cf$set_controler(outputDirectory = outdir,
                   firstIteration = 0,
                   lastIteration = .lastIteration)

  # save the modified config file
  cf_dymium <- paste0(tools::file_path_sans_ext(.config), "-dymium.xml")
  xml2::write_xml(cf$config, cf_dymium)

  # .call_matsim_system(cf_dymium)
  .call_matsim_rjava(cf_dymium)

}

#' @param outdir path where the matsim's output will be saved to.
.call_matsim_rjava = function(config) {

  # load the jar file
  my_jclasspath <- tryCatch(
    {rJava::.jclassPath()},
    error = function(cond) {
      lg$error(paste(cond, collapse = ", "))
      return(FALSE)
    }
  )

  if (!any(grepl('matsim', my_jclasspath))) {
    .jinit(.matsim_setting$path_to_matsim_jar, parameters = .matsim_setting$max_memory)
  }

  # construct a controler object with the modified config file loaded
  matsimControler <- .jnew('org.matsim.run.Controler', config)

  # call run method
  .jcall(matsimControler, "V", "run") # "V" is for void

  invisible()
}

# call MATSim.jar directly
.call_matsim_system = function(config) {

  browser()

  # call matsim run controler
  system(glue::glue("java {.matsim_setting$max_memory} -cp \\
                    \"{.matsim_setting$path_to_matsim_jar}\" \\
                    org.matsim.run.Controler \\
                    \"{config}\""))

  invisible()
}

.matsim_setting <-
  list(
    max_memory = "-Xmx8048m",
    path_to_matsim_jar = here::here('modules/matsim/matsim/matsim-0.10.1.jar')
  )

# exported utility functions (util_*) -------------------------------------
util_function <- function(x) {}