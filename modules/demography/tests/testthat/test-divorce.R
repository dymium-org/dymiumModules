# note: The 'Run tests' button in RStudio may not work (Please let me know if you can make it work!)
#       to run this test use # test_file('modules/demography/tests/testthat/test-divorce.R')
# import the module
library(dymiumCore)
# set logger's threshold to 'warn' to mute info level loggings
dymiumCore:::lg$set_threshold(level = 'warn')
event_demography_divorce <- modules::use(here::here('modules/demography/divorce.R'))
helpers <- modules::use(here::here("modules/demography/helpers.R"))

# write your on tests using testthat::test_that(...)
test_that("event works", {
  # get sample world
  dymiumCore::create_toy_world(small = TRUE, with_model = T)
  
  n_divorced_before <- world$AgentContainer$pop %>% 
    helpers$FilterAgent$Ind$is_married() %>% 
    nrow()
  # n_divorced_before
  
  # run the ageing event
  years <- 1:10
  for (year in years) {
    # world$start_iter(year, "year") %>% 
    world %>%
      event_demography_divorce$run(.)   
  }
  
  n_divorced_after <- world$AgentContainer$pop %>% 
    helpers$FilterAgent$Ind$is_married() %>% 
    nrow()
  # n_divorced_after
  
  expect_true(n_married_after > n_married_before)
})
