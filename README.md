
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dymiumModules

<!-- badges: start -->

<!-- badges: end -->

A repository for sharing modules developed with dymiumCore. To share
your module in this repository please send in your module as a pull
request.

# Create a new module

``` r
library("dymiumCore")
use_module(name = "mymodule")
use_event("event1", "mymodule")
use_event("event2", "mymodule", with_comments = TRUE) # with recommendations
use_event("event3", "mymodule", with_comments = FALSE) # without recommendations
```

# Module directory

  - (demography)\[<https://github.com/dymium-org/dymiumModules/tree/master/modules/demography>\]
