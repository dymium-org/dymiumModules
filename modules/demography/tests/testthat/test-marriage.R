# note: 'Run tests' in RStudio may not work
# import the module 
library(dymiumCore)
dymiumCore:::lg$set_threshold(level = "warn")
event_demog_marriage <- modules::use('modules/demography/marriage.R')
helpers <- modules::use(here::here('modules/demography/helpers.R'))

test_that("event works", {
  # get sample world
  dymiumCore::create_toy_world(small = TRUE, with_model = T)
  
  n_married_before <- world$AgentContainer$pop %>% 
    helpers$FilterAgent$Ind$is_married() %>% 
    nrow()
  
  # run the ageing event
  years <- 1:10
  for (year in years) {
    world$start_iter(year, "year") %>% 
      event_demog_marriage$run(object = .)   
  }
  
  n_married_after <- world$AgentContainer$pop %>% 
    helpers$FilterAgent$Ind$is_married() %>% 
    nrow()
  
  expect_true(n_married_after > n_married_before)
})

