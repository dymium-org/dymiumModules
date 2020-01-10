
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

# Download a module from this repository using R

To download a module it is recommended that you use
`dymiumCore::download_module()`.

Before downloading a module, make sure you setup and use an RStudio
project. The download module will be saved to the ‘modules’ folder (this
will be created if it is not already existed) to the root of your
working directory.

``` r
download_module(name = 'test')
```

The `name` argument is the name of the module in this repository that
you would like to download. You may try to download the ‘test’ module
which contains dummy events. There are other arguments that you can
specify in `download_module, please see the documentation by
calling`?download\_module\`.

# Module directory

  - [demography](https://github.com/dymium-org/dymiumModules/tree/master/modules/demography):
    demographic processes for simulating population and household
    dynamics.
  - [matsim](https://github.com/dymium-org/dymiumModules/tree/master/modules/matsim):
    an agent-based traffic simulator.
  - [test](https://github.com/dymium-org/dymiumModules/tree/master/modules/test):
    a module for testing purposes.
