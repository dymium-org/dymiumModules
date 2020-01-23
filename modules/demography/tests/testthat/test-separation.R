# note: The 'Run tests' button in RStudio may not work (Please let me know if you can make it work!)
#       to run this test use # test_file('modules/demography/tests/testthat/test-separation.R')
# import the module
library(dymiumCore)
# set logger's threshold to 'warn' to mute info level loggings
dymiumCore:::lg$set_threshold(level = 'warn')
event_demography_separation <- modules::use(here::here('modules/demography/separation.R'))
helpers <- modules::use(here::here('modules/demography/helpers.R'))

# write your on tests using testthat::test_that(...)
test_that('event works', {
  
  dymiumCore::create_toy_world(small = TRUE, with_model = T)
  
  n_separated_before <- world$AgentContainer$pop %>%
    helpers$FilterAgent$Ind$is_separated() %>%
    nrow()
  
  # run the event for 10 iterations
  years <- 1:10
  for (year in years) {
    world$start_iter(year, "year") %>% 
      event_demography_separation$run(object = .)   
  }
  
  n_separated_after <- world$AgentContainer$pop %>%
    helpers$FilterAgent$Ind$is_separated() %>%
    nrow()
  
  expect_true(n_separated_after > n_separated_before)
  
})
