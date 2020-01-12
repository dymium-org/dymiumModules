# note: The 'Run tests' button in RStudio may not work (Please let me know if you can make it work!)
#       to run this test use # test_file('modules/demography/tests/testthat/test-breakup.R')
# import the module
library(dymiumCore)
# set logger's threshold to 'warn' to mute info level loggings
dymiumCore:::lg$set_threshold(level = 'warn')
event_demography_breakup <- modules::use(here::here('modules/demography/breakup.R'))
helpers <- modules::use(here::here('modules/demography/helpers.R'))

# write your on tests using testthat::test_that(...)
test_that('event works', {
  # for example
  dymiumCore::create_toy_world(small = TRUE, with_model = T)
  
  n_singles_before <- world$AgentContainer$pop %>%
    helpers$FilterAgent$Ind$is_single() %>%
    nrow()
  
  # run the event for 10 iterations
  years <- 1:10
  for (year in years) {
    world$start_iter(year, "year") %>% 
      event_demography_breakup$run(object = .)   
  }
  
  n_singles_after <- world$AgentContainer$pop %>%
    helpers$FilterAgent$Ind$is_single() %>%
    nrow()
  
  expect_true(n_singles_after > n_singles_before)
  
})
