library(dymiumCore)
dymiumCore:::lg$set_threshold(level = 'warn')
helpers <- modules::use(here::here('modules/demography/helpers.R'))

# write your on tests using testthat::test_that(...)
test_that('helper works', {
  # for example
  dymiumCore::create_toy_world(small = FALSE, with_model = T)
  helpers$DeriveVar$IND$n_children(world$AgentContainer$pop$ind$get_data(), world) %>%
    helpers$DeriveVar$IND$hhadult(., world) %>%
    helpers$DeriveVar$IND$mrcurr(., world) %>%
    helpers$DeriveVar$IND$n_children(., world) %>%
    helpers$DeriveVar$IND$n_resident_children(., world) %>%
    helpers$DeriveVar$IND$has_children(., world) %>%
    helpers$DeriveVar$IND$has_resident_children(., world) %>%
    helpers$DeriveVar$IND$age5(., world) %>%
    helpers$DeriveVar$IND$age_youngest_child(., world) %>%
    helpers$DeriveVar$IND$age_youngest_resident_child(., world) %>%
    helpers$DeriveVar$IND$age_oldest_child(., world) %>%
    helpers$DeriveVar$IND$age_oldest_resident_child(., world) %>%
    helpers$DeriveVar$IND$hhsize(., world) %>%
    checkmate::expect_data_table(x = ., nrows = world$AgentContainer$pop$ind$data$nrow()) -> new_ind_data
  checkmate::expect_names(
    x = names(new_ind_data),
    must.include = c(
      "hhadult",
      "mrcurr",
      "n_children",
      "n_resident_children",
      "has_children",
      "has_resident_children",
      "age5",
      "age_youngest_child",
      "age_youngest_resident_child",
      "age_oldest_child",
      "age_oldest_resident_child",
      "hhsize"
    )
  )
})
