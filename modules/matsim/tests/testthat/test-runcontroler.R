# note: The 'Run tests' button in RStudio may not work (Please let me know if you can make it work!)
#       to run this test use # test_file('modules/matsim/tests/testthat/test-runcontroler.R')
# import the module
library(dymiumCore)
library(here)
# set logger's threshold to 'warn' to mute info level loggings
dymiumCore:::lg$set_threshold(level = 'warn')
event_matsim_runcontroler <- modules::use(here::here('modules/matsim/runControler.R'))

# write your on tests using testthat::test_that(...)
test_that('event works', {
  # for example
  create_toy_world()

  for (i in 1:10) {
    world$start_iter(time_step = i, unit = 'year') %>%
      event_matsim_runcontroler$run(
        world = .,
        model = list(
          config = here::here("modules/matsim/matsim/examples/equil/config.xml"),
          lastIteration = 5
        ),
        use_rJava = TRUE
      )
  }
  
})
