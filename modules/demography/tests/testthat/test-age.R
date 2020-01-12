# note: 'Run tests' in RStudio may not work
# import the module
library(dymiumCore)
dymiumCore:::lg$set_threshold(level = "warn")
event_demog_age <- modules::use(here::here("modules/demography/age.R"))

test_that("non-exported functions shouldn't be available", {
  expect_true(is.null(event_demog_age$foo))
})

test_that("age event works", {

  # get sample population
  create_toy_population()

  # record the sum of age variable before running the ageing event
  sum_age_before <- event_demog_age$util_sum_age(pop)

  # run the ageing event
  pop %>%
    event_demog_age$run(.)

  # record after
  sum_age_after <- event_demog_age$util_sum_age(pop)

  # sum after must be greater than sum before
  expect_true(sum_age_after > sum_age_before)

  # or more specifically, all people should have their ages increase by one year
  expect_true(sum_age_after == sum_age_before + nrow(pop$ind$get_data(copy = FALSE)))
})

test_that("age many times works", {

  # get sample population
  dymiumCore::create_toy_world()

  # record the sum of age variable before running the ageing event
  sum_age_before <- world$get("Individual")$get_attr("age") %>% sum()

  years <- 1:10
  for (year in years) {
    world$start_iter(time_step = year, unit = "year") %>%
      event_demog_age$run(., time_steps = c(1,3,10))
  }

  # record after
  sum_age_after <- world$get("Individual")$get_attr("age") %>% sum()

  # all people got three years older, since age was ran in time 1, 3 and 10 only
  expect_true(sum_age_after == sum_age_before + nrow(world$get("Individual")$get_data(copy = FALSE)) * 3)
})

