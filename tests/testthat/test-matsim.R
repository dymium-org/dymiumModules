test_that("matsim module", {
  
  testthat::skip_on_ci()
  
  lgr::threshold("error")
  # load event functions
  event_matsim_createVISTADemand <- modules::use(here::here('modules/matsim/createVISTADemand.R'))
  event_matsim_runcontroler <- modules::use(here::here('modules/matsim/runControler.R'))

  # create world
  world <- World$new()

  # add agents
  world$add(x = Population$new(ind_data = dymiumCore::toy_individuals,
                               hh_data = dymiumCore::toy_households,
                               pid_col = c("pid", "partner_id", "father_id", "mother_id"),
                               hid_col = "hid"))

  # matsim module's models and settings
  matsim_runControler_model <-
    list(
      matsim_config = here::here("scenarios/matsim-integration/inputs/matsim/config.xml"),
      matsim_config_params = list(controler = list(lastIteration = 5))
      # max_memory = "-Xmx2048m" # 2GBs of RAM
    )

  # add trip data for data fusion
  world$add(
    x = readRDS(here::here("scenarios/matsim-integration/inputs/models/vista_persons.rds")),
    name = "vista_persons"
  )
  world$add(
    x = readRDS(here::here("scenarios/matsim-integration/inputs/models/vista_trips.rds")),
    name = "vista_trips"
  )

  # set a tmp scenario to save matsim output
  create_scenario(name = "matsim", active = T, .basedir = tempdir())

  # run trip fusion and matsim
  world %>%
    event_matsim_createVISTADemand$run(.) %>%
    event_matsim_runcontroler$run(.,
                                  model = matsim_runControler_model,
                                  use_rJava = TRUE)

})
