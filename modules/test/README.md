
  - [test module documentation](#test-module-documentation)
  - [Release notes](#release-notes)
      - [Version 0.1](#version-0.1)
  - [Requirements](#requirements)
  - [Events](#events)
      - [Event name 1](#event-name-1)
      - [Event name 2](#event-name-2)
  - [Known issues](#known-issues)

# test module documentation

  - Authors: Your name
  - Email: <your@email.com>
  - Current version: 0.0.2

This module provides the following functionalities…

# Release notes

## Version 0.1

This is the first version of this module.

# Requirements

This module requires the following packages … and external dependecies
such as …

# Events

## Event name 1

### Usage

``` r
event_test_eventname <- modules::use("modules/test/eventname.R")
event_test_eventname$run(world, model = NULL, target = NULL, time_steps = NULL)
```

### Params

  - world: a World object.
  - model: a named list.

<!-- end list -->

``` r
# For example
model <- list(x1 = list(yes = 0.5, no = 0.5),
              x2 = data.table(sex = c("male", "female"), prob = c(0.2, 0.1)))
```

  - target: a named list.
  - time\_steps: a numeric vector that indicates the time steps in the
    simulation clock that this event should be run.

### Description

This event does..

### Note

Here are some assumptions and settings that you should know about this
event..

### Example

``` r
1 + 1
```

## Event name 2

### Usage

``` r
event_test_eventname <- modules::use("modules/test/eventname.R")
event_test_eventname$run(world, model = NULL, target = NULL, time_steps = NULL)
```

### Params

  - world: a World object.
  - model: a named list.

<!-- end list -->

``` r
# For example
model <- list(x1 = list(yes = 0.5, no = 0.5),
              x2 = data.table(sex = c("male", "female"), prob = c(0.2, 0.1)))
```

  - target: a named list.
  - time\_steps: a numeric vector that indicates the time steps in the
    simulation clock that this event should be run.

### Description

This event does..

### Note

Here are some assumptions and settings that you should know about this
event..

### Example

``` r
1 + 1
```

# Known issues

Here are some known issues
