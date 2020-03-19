test_that("demography all events", {

  testthat::skip_on_ci()
  
  lgr::threshold("error")
  # load demography event functions
  demography <- modules::use(file.path("..", "..", "modules", "demography"))
  # demography <- modules::use(file.path("..", "..", "00_pkg_src", "dymiumModules", "modules", "demography"))

  # create world
  world <- World$new()

  # add agents
  world$add(x = Population$new(ind_data = dymiumCore::toy_individuals,
                               hh_data = dymiumCore::toy_households,
                               pid_col = c("pid", "partner_id", "father_id", "mother_id"),
                               hid_col = "hid"))

  # add models
  models <- list(fertility = list(yes = 0.05, no = 0.95),
                 birth_multiplicity = list("single" = 0.97, "twins" = 0.03),
                 birth_sex_ratio = list(male = 0.51, female = 0.49),
                 death = list(yes = 0.1, no = 0.9),
                 marriage_cohab_male = list(yes = 0.1, no = 0.9),
                 marriage_no_cohab_male = list(yes = 0.1, no = 0.9),
                 marriage_no_cohab_female = list(yes = 0.1, no = 0.9),
                 separate_male = list(yes = 0.1, no = 0.9),
                 separate_child_custody = list(male = 0.2, female = 0.8),
                 separate_hhtype = list(lone = 0.5, group = 0.5),
                 separate_hf_random_join = list("1" = 0.4, "2" = 0.3, "3" = 0.2, "4" = 0.1),
                 divorce_male = list(yes = 0.5, no = 0.9),
                 divorce_female = list(yes = 0.5, no = 0.9),
                 cohabitation_male = list(yes = 0.1, no = 0.9),
                 cohabitation_female = list(yes = 0.1, no = 0.9),
                 breakup = list(yes = 0.1, no = 0.9),
                 breakup_child_custody = list(male = 0.2, female = 0.8),
                 breakup_hhtype = list(lone = 0.5, group = 0.5),
                 breakup_hf_random_join = list("1" = 0.4, "2" = 0.3, "3" = 0.2, "4" = 0.1),
                 leavehome_male = list(yes = 0.3, no = 0.7),
                 leavehome_female = list(yes = 0.2, no = 0.8),
                 leavehome_hhtype = list(lone = 0.2, group = 0.8),
                 leavehome_hf_random_join = list("1" = 0.5, "2" = 0.3, "3" = 0.1, "4" = 0.1),
                 migrant_individuals = dymiumCore::toy_individuals,
                 migrant_households = dymiumCore::toy_households)

  for (i in seq_along(models)) {
    world$add(models[[i]], name = names(models)[i])
  }

  # create a pipeline of events
  DemographyPipe <- dymiumCore::Pipeline$new(
    . %>%
      demography$age$run(.) %>%
      demography$birth$run(.) %>%
      demography$death$run(.) %>%
      demography$cohabit$run(.) %>%
      demography$marriage$run(.) %>%
      demography$divorce$run(.) %>%
      demography$breakup$run(.) %>%
      demography$separation$run(.) %>%
      demography$leavehome$run(.) %>%
      demography$migration$run(., target = 10)
  )

  for (i in 1:20) {
    message("iteration: ", i)
    world$start_iter(time_step = i, unit = "year") %>%
      DemographyPipe$run(., shuffle = T)
  }

  expect_true(dymiumCore::validate_linkages(world))

})

