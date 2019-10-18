
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

# Modules

## Completed modules
- 'Demography' module (private by asiripanich)
- 'Socioeconomic' module (private by asiripanich)
- 'MATSim' module (private by asiripanich)

## Modules in development
- 'Firmography' module (private by asiripanich)
- 'Market interaction' module (private by asiripanich)
