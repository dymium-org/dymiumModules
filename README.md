
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dymiumModules

<!-- badges: start -->

<!-- badges: end -->

A repository for sharing dymium modules.

# Create a new module

``` r
library("dymiumCore")
create_new_module(name = "demography", 
                  event = c("age", "birth", "death", "marriage", "cohabit",
                            "separate", "divorce", "breakup", "leavehome", 
                            "migrate", "emigrate"), 
                  path = "modules")
```

Completed modules
- 'Demography' module (private)
- 'Socionomic' module (private)
- 'MATSim' module (private)

In-development
- 'Firmography' module (private)
- 'Market interaction' module (private)
