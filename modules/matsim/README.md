
  - [MATSim module documentation](#matsim-module-documentation)
  - [Release note](#release-note)
      - [Version 0.1.2](#version-0.1.2)
      - [Version 0.1.1](#version-0.1.1)
      - [Version 0.1.0](#version-0.1.0)
  - [Requiements](#requiements)
      - [R packages](#r-packages)
      - [MATSim](#matsim)
  - [Events](#events)
      - [Run controler](#run-controler)
      - [Create a MATSim plan](#create-a-matsim-plan)
  - [Helper functions](#helper-functions)
  - [Known issues](#known-issues)
      - [MATSim](#matsim-1)
      - [Setup Java](#setup-java)
      - [rJava failed to load\!
        Ughhhhh\!](#rjava-failed-to-load-ughhhhh)

<!-- README.md is generated from README.Rmd. Please edit that file -->

# MATSim module documentation

This module provides the following functionalities:

  - `runControl.R`: run MATSim using a MATSim config file.
  - `createVISTADemand.R`: assign daily activity patterns to a
    population and write it as a MATSim plan\_v4 file.
  - `helpers.R`: reader and edit matsim’s config, create a MATSim
    network\_v2 file from shpfiles, assign random geographical locations
    to activities.

# Release note

## Version 0.1.2

  - `runControler` has new requirements in `model`. It can now set the
    path to a matsim.jar file and the maximum amount of memory for
    MATSim to use using `matsim_path_to_jar` and `matsim_max_memory`,
    respectively. `matsim_config_params` accepts a named list which will
    be used to modify a given `matsim_config`. See its documentation
    section for more details.

## Version 0.1.1

  - add `use_rJava` argument to `runControler` this allows the event to
    be configured how it should run MATSim’s controler.

## Version 0.1.0

  - Allows basic calls to MATSim APIs from R.
  - Adds functions for preparing MATSim inputs.

# Requiements

## R packages

  - R CRAN packages: data.table, modules, StatMatch, checkmate, here,
    rJava, xml2, sf, glue, furrr, fs
  - R github packages: dymium-org/dymiumCore,

## MATSim

  - You will need to install Java. It is recommended to use Java 8 to
    run MATSim (see related issues here:
    <https://github.com/matsim-org/matsim-code-examples/issues/176>,
    <https://github.com/matsim-org/matsim-code-examples/issues/16>).

  - The matsim module doesn’t shipped with a MATSim java executable file
    that is required to make this module works. The user must download
    MATSim from [MATSim website](https://www.matsim.org/downloads/). I
    recommend to use the latest stable release (version 0.10.0 as of
    2020-01-25) with this module. Once, you have downloaded MATSim you
    must extract the matsim zip file into your matsim module folder
    (usually under the root folder of your active R project
    `modules/matsim`). Then rename the extracted file as ‘matsim’. Then
    you are good to go\! :)

# Events

## Run controler

### Description

Run MATSim using a MATSim config file.

<details>

dymium always makes a copy of the original MATSim config file and saves
the copy in the same location with a suffix `-dymium`, incase it needs
to modify some settings. If you are using dymium’s scenario, your matsim
output will be automatically saved inside the `outputs` folder of your
active scenario.

</details>

### Usage

``` r
event_matsim_runcontroler <- modules::use(here::here("modules/matsim/runControler.R"))
#> Masking (modules:sf):
#>   `%>%` from: modules:dymiumCore
```

    #> run:
    #> function(world, model = NULL, target = NULL, time_steps = NULL, use_rJava = TRUE)
    #> 
    #> 
    #> REQUIRED_MODELS:
    #>  chr [1:4] "matsim_config" "matsim_config_params" "matsim_path_to_jar" ...

### Parameters

  - **world**: a World object.
  - **model**: a named list that contains path to a MATSim config file.
      - *matsim\_config*: a path to a matsim config file
      - *matsim\_config\_params*: a named list that is used to modify
        the given `matsim_config`. From the example below, `controler`
        is a module name and `firstIteration` and `lastIteration` are
        param names that belong to the `controler` module. We set
        `firstIteration` to 0 and `lastIteration` to 10. To learn more
        about what parameters can be set please see a MATSim
        documentation.
      - *matsim\_path\_to\_jar*: (optional, with a default value of
        “here::here(‘modules/matsim/matsim/matsim-0.10.1.jar’)”) path
        to a MATSim.jar file.
      - *matsim\_max\_memory*: (optional, with a default value of
        “-Xmx1024m” which is 1GB) max amount of RAM for MATSim to use.

<!-- end list -->

``` r
model <- list(
  matsim_config = "path/to/config.xml",
  matsim_config_params = list(
    controler = list(
      firstIteration = 0,
      lastIteration = 10
  )),
  matsim_path_to_jar = "path/to/matsim.jar",
  matsim_max_memory = "-Xmx1024m" # 1024mbs or 1gb which is also the default value.
)
```

  - **target**: NULL
  - **time\_steps**: a integer vector that contains the time steps in
    which this event should be run.
  - **use\_rJava**: a logical value default as `TRUE`, this determine
    the method that the function uses to run MATSim. By default, the
    function will attempt to use rJava to start MATSim’s controler. If
    FALSE, then it will call MATSim using a commandline command. Note
    that, calling MATSim from the commandline only possible on a UNIX
    machine if you are running this through a Windows machine then you
    must use rJava to call MATSim. An error will be raised if this is
    set to FALSE on a Windows machine.

### Example

The following example runs an example MATSim scenario for five
iterations.

``` r
event_matsim_runcontroler <-
  modules::use(here::here('modules/matsim/runControler.R'))

create_toy_world()

for (i in 1:1) {
  world$start_iter(time_step = i, unit = 'year') %>%
    event_matsim_runcontroler$run(model = list(
      matsim_config = here::here("modules/matsim/matsim/examples/equil/config.xml"),
      matsim_config_params = list(controler = list(
        firstIteration = 0,
        lastIteration = 5
      ))
    ))
}
```

## Create a MATSim plan

### Description

This event function assigns daily travel activity patterns to Dymium
individuals using a statistical matching technique. It uses a travel
survey in this case the 2009 VISTA household travel survey of Victoria
state, Australia
([link](https://transport.vic.gov.au/about/data-and-research/vista/vista-data-and-publications)).
It then parses the fused travel activities to an xml file that follows
the format of [MATSim’s
population\_V6](http://www.matsim.org/files/dtd/population_v6.dtd) and
save it to your matsim input folder.

### Note

<details>

The implementation of this function is very specific to the 2009 VISTA
data hence it’s most likely not going to work if you replace the vista
data with other data. However, this can be used as a template for
implementing other travel surveys or can be replaced this step entirely
with a proper activity-based model if available.

</details>

### Usage

``` r
event_matsim_createVISTADemand <- modules::use('modules/matsim/createVISTADemand.R')
#> Masking (modules:sf):
#>   `%>%` from: modules:dymiumCore
```

    #> run:
    #> function(world, model = NULL, target = NULL, time_steps = NULL)
    #> 
    #> 
    #> REQUIRED_MODELS:
    #>  chr [1:2] "vista_persons" "vista_trips"

### Parameters

  - world: a World object.
  - model: a named list.
      - *vista\_persons*: a data.frame that contains a vista person file
      - *vista\_trips*: a data.frame that contains a vista trip file

<!-- end list -->

``` r
model <- list(vista_persons = data.frame(),
              vista_trips = data.frame())
```

  - target: NULL
  - time\_steps: a integer vector that contains the time steps in which
    this event should be run.

# Helper functions

  - MatsimConfig: an R6 class that can modify a MATSim config file
  - create\_population\_v6: create a population xml file that complies
    with MATSim’s Population V6.
  - assign\_location: assign a random locations within a shapefile
  - shp2xml: convert a network shapfile to a MATSim compatible xml
    network file

See the `helpers.R` script for more details.

Additionally, there are some functions for binding MATSim’s histogram
plots into a single animated histogram by iteration number in `plots.R`

# Known issues

## MATSim

The MATSim melbourne scenario has the following unresolved issues

  - strategy setting ‘SubtourModeChoice’ doesn’t work. If this setting
    is used, the error will be raised about modeParam ‘pt’ and ‘walk’
    not being valid.

MATSim works best with Java 8. If you are running newer versions of Java
there is a high chance that some dependencies might be missing one of
which is the JAXB libraries as discussed
[here](https://github.com/matsim-org/matsim-code-examples/issues/176).

## Setup Java

(2020-01-12) Java JDK 11.0.1 must be installed in order for the `rJava`
pacakge to work. This issue has been discussed
[here](https://github.com/rstudio/rstudio/issues/2254) and
[here](https://github.com/s-u/rJava/issues/151). This is only required
if you would like to run the MATSim module using rJava as implemented in
`.run_matsim_rjava` function inside `runControler.R`. Otherwise, you can
use `.run_matsim_system()` which basically invoke your `matsim.jar` from
the OS command, this option requires you to make sure that Java8+ is
installed and is callable using the OS command.

To install the required version of Java click on this
[link](https://www.oracle.com/technetwork/java/javase/downloads/java-archive-javase11-5116896.html)
and download **Java SE Development Kit 11.0.1** that works on your
machine.

You will need to run `R CMD javareconf` on terminal to make sure R
detects your new java installation. Make sure that all R sessions are
close before running this command.

## rJava failed to load\! Ughhhhh\!

If you get the following error..

``` r
> library(rJava)
# Error: package or namespace load failed for 'rJava':
#  .onLoad failed in loadNamespace() for 'rJava', details:
#   call: dyn.load(file, DLLpath = DLLpath, ...)
#   error: unable to load shared object '/Users/kevin/Library/R/3.4/library/rJava/libs/rJava.so':
#   dlopen(/Users/kevin/Library/R/3.4/library/rJava/libs/rJava.so, 6): Library not loaded: @rpath/libjvm.dylib
#   Referenced from: /Users/kevin/Library/R/3.4/library/rJava/libs/rJava.so
#   Reason: image not found
```

It may be a good idea to run the following command in your Terminal.

    R CMD javareconf

Then quit RStudio and launch it again to try `library(rJava)`. If the
error still persist then Google search is your best friend\!
