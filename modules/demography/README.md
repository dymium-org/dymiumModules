
  - [demography module documentation](#demography-module-documentation)
  - [Release note](#release-note)
      - [Version 0.1.0](#version-0.1.0)
  - [Requirements](#requirements)
      - [R packages](#r-packages)
  - [Events](#events)
      - [Aging](#aging)
      - [Birth](#birth)
      - [Death](#death)
      - [Marriage](#marriage)
      - [Separation](#separation)
      - [Divorce](#divorce)
      - [Cohabitation](#cohabitation)
      - [Breakup](#breakup)
      - [Leave parental home](#leave-parental-home)
      - [Migration](#migration)
  - [Known issues](#known-issues)

<!-- README.md is generated from README.Rmd. Please edit that file -->

# demography module documentation

The demography module provides event functions for simutenouesly
simulating the dynamics of evolving urban population and their
households at the individual-based level. These events are responsible
for household formation (marriage, cohabitation, leaving parental home),
household dissolutions (separation, divorce, break up), household
expansion (birth).

# Release note

## Version 0.1.0

First release\!

# Requirements

## R packages

R packages: dymiumCore, data.table

# Events

## Aging

### Usage

``` r
event_demography_age <- modules::use("modules/demography/age.R")
event_demography_age$run(world, model = NULL, target = NULL, time_steps = NULL)
```

### Params

  - object: a \[World\] object.
  - model: NULL, this event doesn’t require this argument.
  - target: NULL, this event doesn’t require this argument.
  - time\_steps: a integer vector that contains the time steps in which
    this event should be run.

### Description

Increase the age of all individual agents by 1 which is equipvalent to
one year.

### Note

If there are any attributes that should be updated depending on the age
of agent then it can be implemented inside this event. For example,
currently there are three rules that are active. These rules update
marital status, education, and labour force status of the agent once it
has reached some certain age thresholds. All agents aged 16 and above
can be in a relationship (a marriage or a cohabitation) hence once they
turn 16 their marital status will be changed from “not applicable” to
“never married” which allows them to be considered when the marriage
event and the cohabitation event are being simulated.

### Example

``` r
create_toy_world()

world %>%
  event_demography_age$run(.)
```

## Birth

### Usage

### Params

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

### Note

### Example

For female individual agents.

## Death

### Usage

### Params

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

### Note

### Example

## Marriage

### Usage

### Params

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

### Note

### Example

Marriages can be divided into two types: marriage with priorly cohabited
and marriage without cohabitation. The first type only applies to
cohabiting couples who want to get married. The second type of marriage
can occur to any non-cohabiting individuals.

### Required Models

  - marriage\_cohab\_male: a binary choice model
  - marriage\_no\_cohab\_male: a binary choice model
  - marriage\_no\_cohab\_female: a binary choice model

## Separation

### Usage

### Params

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

### Note

### Example

Separation is the first step before couples can be officially divorced.
This module assume that no couples will recoupled once they have decided
to separate.

## Divorce

### Usage

### Params

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

### Note

### Example

When divorce is triggered for a separted individual, his/her ex-partner
will also under go divorce **if** the marital status of his/her
ex-partner is still ‘separated’.

## Cohabitation

### Usage

### Params

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

### Note

### Example

## Breakup

### Usage

### Params

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

### Note

### Example

## Leave parental home

### Usage

### Params

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

### Note

### Example

## Migration

### Usage

### Params

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

### Note

### Example

All operations are done within your R environment with no external
dependencies. To use any event function please consider using the
following commands instead of using `source("path/to/event/script")`.

``` r
event_demography_age <- modules::use("modules/demography/age.R")
```

# Known issues
