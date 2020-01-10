
  - [MATSim module documentation](#matsim-module-documentation)
  - [Release note](#release-note)
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
    2020-01-10) with this module. Once, you have downloaded MATSim you
    must extract the matsim zip file into your matsim module folder
    (usually under the root folder of your active R project
    `modules/matsim`). Then rename the extracted file as ‘matsim’. Then
    you are good to go\! :)

# Events

## Run controler

### Usage

``` r
event_matsim_runcontroler <- modules::use("modules/matsim/runControler.R")
event_matsim_runcontroler$run(world, model = NULL, target = NULL, time_steps = NULL)
```

### Parameters

  - object: a \[World\] object.
  - model: a named list that contains path to a MATSim config file.

<!-- end list -->

``` r
# config: path to a matsim config file
# lastIteration: a numeric value that denotes the number of iterations for matsim to run
model <- list(config = "path/to/config.xml",
              lastIteration = 10)
```

  - target: NULL
  - time\_steps: a integer vector that contains the time steps in which
    this event should be run.

### Description

This script contains the main function which is
`event_matsim_runcontroler$run(object, model, target, time_steps)`. The
function calls a MATSim configuration file that contains all the
settings necessary to run a MATSim scenario. By default, dymium modifies
the output directory where MATSim results will be saved to
`/<path-to-your-dymium-RStudio-project>/output/matsim`. This is done by
creating a modified version (with a `-dymium` suffix) of the supplied
config file and saved inside the same directory.

To modify a MATSim config at runtime you may use the suppiled
MatsimConfig class in `helpers.R`. This class allows you to modify
values inside a MATSim config file using its public methods.

Some MATSim settings can be modified in this script such as the path to
a MATSim.jar executable file and the maximum amount of memory for MATSim
to utilise. They are stored as `.matsim_setting`. To modify these
options open the script and find the lines that are similar to the code
in the chuck below.

### Note

Some settings need to be specified such as the maxmimum memory MATSim is
allowed to use and the path to the MATSim java executable file. These
settings can be changed inside the event script that can be found at
`modules/matsim/runControler.R`.

``` r
.matsim_setting <- 
  list(
    max_memory = "-Xmx8048m", # maximum amount of memory
    path_to_matsim_jar = here::here('modules/matsim/matsim/matsim-0.11.0-SNAPSHOT.jar') # path to your MATSim.jar
  )
```

### Example

The following example runs an example MATSim scenario for five
iterations.

``` r
event_matsim_runcontroler <-
  modules::use(here::here('modules/matsim/runControler.R'))

create_toy_world()

for (i in 1:10) {
  world$start_iter(time_step = i, unit = 'year') %>%
    event_matsim_runcontroler$run(model = list(
      config = here::here("modules/matsim/matsim/examples/equil/config.xml"),
      lastIteration = 5
    ))
}
```

## Create a MATSim plan

### Usage

``` r
event_matsim_createVISTADemand <- modules::use('modules/matsim/createVISTADemand.R')
event_matsim_createVISTADemand$run(world, model = NULL, target = NULL, time_steps = NULL)
```

### Parameters

  - object: a World object.
  - model: a named list.

<!-- end list -->

``` r
# vista_persons: a data.frame that contains a vista person file 
# vista_trips: a data.frame that contains a vista trip file
model <- list(vista_persons = data.frame(),
              vista_trips = data.frame())
```

  - target: NULL
  - time\_steps: a integer vector that contains the time steps in which
    this event should be run.

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

The implementation of this function is very specific to the 2009 VISTA
data hence it most likely not going to work if you replace the vista
data with other data. However, this can be used as a template for
implementing other travel surveys or can be replaced this step entirely
with a proper activity-based model if available.

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

## Setup Java

As of 2020-01-10, Java JDK 11.0.1 must be installed in order for the
`rJava` pacakge to work. This issue has been discussed
[here](https://github.com/rstudio/rstudio/issues/2254). This is only
required if you would like to run the MATSim module using rJava as
implemented in `.run_matsim` function inside `runControler.R`.
Otherwise, you can use `.run_matsim2` which basically invoke your
`matsim.jar` from the OS command, this option requires you to make sure
that Java8+ is installed and is callable using the OS command.

To install the required version of Java click on this
[link](https://www.oracle.com/technetwork/java/javase/downloads/java-archive-javase11-5116896.html)
and download **Java SE Development Kit 11.0.1** that works on your
machine.

You may need to run `R CMD javareconf` on terminal to make sure R
detects your new java installation. Make sure that all R sessions are
close before running this command.

## rJava failed to load\! Ughhhhh\!

If you get the follow error from

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

Then quite RStudio and launch in again to try `library(rJava)`. If the
error still presist then Google search is your best friend\!
