# note: The 'Run tests' button in RStudio may not work (Please let me know if you can make it work!)
#       to run this test use # test_file('modules/matsim/tests/testthat/test-helpers.R')
# import the module
library(dymiumCore)
# set logger's threshold to 'warn' to mute info level loggings
dymiumCore:::lg$set_threshold(level = 'warn')
modules::expose(here::here('modules/matsim/helpers.R'))

# write your on tests using testthat::test_that(...)
test_that('event works', {
  cf <- MatsimConfig$new("modules/matsim/matsim/examples/equil/config.xml")
  cf$set_controler(outputDirectory = "output/matsim")
  n <- cf$get_controler()
  expect_equal(object = xml2::xml_find_first(n, "param[@name='outputDirectory']") %>% xml_attr(attr = "value"), 
               expected = "output/matsim")
})
