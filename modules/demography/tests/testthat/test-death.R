# note: The 'Run tests' button in RStudio may not work (Please let me know if you can make it work!)
#       to run this test use # test_file('modules/demography/tests/testthat/test-death.R')
# import the module
library(dymiumCore)
# set logger's threshold to 'warn' to mute info level loggings
dymiumCore:::lg$set_threshold(level = 'warn')
event_demography_death <- modules::use(here::here('modules/demography/death.R'))

# write your on tests using testthat::test_that(...)
test_that('event works', {
  # for example
  expect_true(1 == 1)
})
