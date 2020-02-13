test_that(desc = "age", {
  
  world <- dymiumCore::World$new()
  
  world$add(dymiumCore::Individual$new(.data = dymiumCore::toy_individuals, id_col = "pid"))
  
  event_age <- modules::use(here::here("modules/demography/age.R"))
  
  sum_age_before <- world$entities$Individual$get_attr("age") %>% sum()
  
  world %>%
    event_age$run(.)
  
  sum_age_after <- world$entities$Individual$get_attr("age") %>% sum()
  
  expect_gt(sum_age_after, sum_age_before)
  
})
