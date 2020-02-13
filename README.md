
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dymiumModules

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/dymium-org/dymiumModules.svg?branch=master)](https://travis-ci.org/dymium-org/dymiumModules)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/dymium-org/dymiumModules?branch=master&svg=true)](https://ci.appveyor.com/project/dymium-org/dymiumModules)
<!-- badges: end -->

A repository for sharing modules developed with dymiumCore. To share
your module in this repository please send in your module as a pull
request.

# Module directory

  - [demography](https://github.com/dymium-org/dymiumModules/tree/master/modules/demography):
    demographic processes for simulating population and household
    dynamics.
  - [matsim](https://github.com/dymium-org/dymiumModules/tree/master/modules/matsim):
    an agent-based traffic simulator.
  - [test](https://github.com/dymium-org/dymiumModules/tree/master/modules/test):
    regression testing for dymiumCore.

# Create a new module

**dymiumCore** provides many helper functions to help you create your
first module. The `use_module(name = "mymodule")` function creates a
folder called `modules` under your active R project directory. Inside
the `modules` folder you will find another folder called `mymodule`
which is the name of your new module and inside that you will find
`logger.R`, `helpers.R`, `constants.R` and a `testthat` folder.

``` r
library("dymiumCore")
use_module(name = "mymodule")
```

To add new events to your newly create module you can use the
`use_event()` function as in the example below.

``` r
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
specify in `download_module()`, please see the documentation by calling
`?download_module`.
