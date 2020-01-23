
  - [demography module documentation](#demography-module-documentation)
  - [Release note](#release-note)
      - [version 0.1.1](#version-0.1.1)
      - [version 0.1.0](#version-0.1.0)
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

The demography module provides event functions for simulating the
dynamics of evolving urban population and their households at an
individual-based level. These events are responsible for household
formation (marriage, cohabitation, leaving parental home), household
dissolutions (separation, divorce, break up), household expansion
(birth).

Note that, there are hard-coded rules that can be modified, such as the
age range in which females can bare a child. See `constants.R`.

# Release note

### version 0.1.1

  - Updated outdated event `modules` options. Event scipts should only
    export `run` and `REQUIRED_MODELS`.

### version 0.1.0

  - First release\!

# Requirements

## R packages

R packages: dymiumCore, data.table, modules, here, checkmate, R6.

# Events

## Aging

### Description

Increase age of all `Individual` agents by 1, which is equipvalent to
one year.

<details>

If there are any attributes that should be updated depending on the age
of agent then it can be implemented inside this event. For example,
currently there are three rules that are active. These rules update
marital status, education, and labour force status of the agent once it
has reached some certain age thresholds. All agents aged 16 and above
can be in a relationship (a marriage or a cohabitation) hence once they
turn 16 their marital status will be changed from “not applicable” to
“never married” which allows them to be considered when the marriage
event and the cohabitation event are being simulated.

</details>

### Usage

``` r
event_demography_age <- modules::use(here::here("modules/demography/age.R"))
```

    #> run:
    #> function(world, model = NULL, target = NULL, time_steps = NULL)

### Params

  - **object**: a World object.
  - **model**: NULL, this event doesn’t require this argument.
  - **target**: NULL, this event doesn’t require this argument.
  - **time\_steps**: an integer vector that contains the time steps in
    which this event should be run.

### Example

``` r
create_toy_world()
#> [17:22:52] WARN  dymiumCore self$initialise_data: Creating `hhsize` as it is not provided with `hh_data`.

event_demography_age <- modules::use(here::here("modules/demography/age.R"))

# summary of 'age' attribute before ageing
summary(world$entities$Individual$get_attr(x = "age"))
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>    0.00   22.00   40.00   38.78   55.00   90.00

world %>%
  event_demography_age$run(.)
#> [17:22:52] INFO  demography event_demography_age$run: Running Age

# summary of 'age' attribute after ageing 
summary(world$entities$Individual$get_attr(x = "age"))
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>    1.00   23.00   41.00   39.78   56.00   91.00
```

## Birth

### Description

The birth event adds newborns to the population by simulating the chance
for fertile individual agents to give birth.

<details>

Newborns inherit some attributes from their mothers such as household id
and dwelling id. Their mother id and father id fields will be set at the
time of their birth and a new unique id will be assigned to them. If
your `Individual` agents have more attributes than the basic ones in
`toy_individuals` then you should modify the `birth` event script to set
the default values for those extra attributes. See `create_newborns()`
inside the event script.

</details>

### Usage

``` r
event_demography_birth <- modules::use(here::here("modules/demography/birth.R"))
```

    #> run:
    #> function(world, model = NULL, target = NULL, time_steps = NULL)
    #> 
    #> 
    #> REQUIRED_MODELS:
    #>  chr [1:3] "fertility" "birth_multiplicity" "birth_sex_ratio"

### Params

  - **object**: a \[World\] object.
  - **model**: a named list.
      - **fertility**: a binary model to determine whether an eligible
        female would to have a child or not.
      - **birth\_multiplicity**: chance of giving birth to more than one
        baby. Note that, the current code is only allow for twins but
        you may modify this to accommodate cases of triplets and more.
      - **birth\_sex\_ratio**: the chance of giving birth to a female
        baby vs a male baby.

<!-- end list -->

``` r
model <- list(fertility = list(yes = 0.05, no = 0.95),
              birth_multiplicity = list("single" = 0.97, "twins" = 0.03),
              birth_sex_ratio = list(male = 0.51, female = 0.49))
```

  - target: default as `NULL` or it can be a named list which determines
    the number of individual agents to under go the fertility event. For
    example, if a list `list(yes = 100)` it will garantee that there are
    100 individual agents that will give birth.
  - time\_steps: an integer vector that contains the time steps in which
    this event should be run.

### Example

``` r
create_toy_world()
#> [17:22:53] WARN  dymiumCore self$initialise_data: Creating `hhsize` as it is not provided with `hh_data`.

event_demography_birth <- modules::use(here::here("modules/demography/birth.R"))

# Total number of individuals before a run of the birth event
world$entities$Individual$n()
#> [1] 373

world %>%
  event_demography_birth$run(., model = model)
#> [17:22:53] INFO  demography event_demography_birth$run: Running Birth
#> [17:22:53] INFO  demography event_demography_birth$run: There are 2 births from 2 birth givers.

# Total number of individuals after a run of the birth event
world$entities$Individual$n()
#> [1] 375
```

## Death

### Usage

``` r
event_demography_birth <- modules::use(here::here("modules/demography/birth.R"))
```

``` r
event_demography_birth
#> run:
#> function(world, model = NULL, target = NULL, time_steps = NULL)
#> 
#> 
#> REQUIRED_MODELS:
#>  chr [1:3] "fertility" "birth_multiplicity" "birth_sex_ratio"
```

### Params

  - **world**: a `World` object.
  - **model**: a named list.
      - **death**: a binary model.

<!-- end list -->

``` r
model <- list(death = list(yes = 0.1, no = 0.9))
```

  - **target**: default as `NULL` or a named list.
  - **time\_steps**: an integer vector that contains the time steps in
    which this event should be run.

### Description

Removes dying individual agents from the individual database and also
updates any affected attributes (such as marital status, household size)
the agents that are related to the dying agents.

<details>

The marital status of those individual agents whom their partner has
died will be labelled as “widowed”. To retrive the data of all agents
that have been removed you through the death event use the
`get_removed_data()` method.

</details>

### Example

``` r
create_toy_world()
#> [17:22:53] WARN  dymiumCore self$initialise_data: Creating `hhsize` as it is not provided with `hh_data`.

event_demography_death <- modules::use(here::here("modules/demography/death.R"))

# Total number of individuals before a run of the death event
world$entities$Individual$n()
#> [1] 373
# Distribution of marital status before a run of the death event
table(world$entities$Individual$get_attr("marital_status"))
#> 
#>       divorced        married  never married not applicable      separated 
#>             22            174            108             53              6 
#>        widowed 
#>             10

world %>%
  event_demography_death$run(., model = model)
#> [17:22:53] INFO  demography event_demography_death$run: Running Death
#> [17:22:53] INFO  demography event_demography_death$run: There are 40 deaths

# Total number of individuals after a run of the death event
world$entities$Individual$n()
#> [1] 333
# Distribution of marital status after a run of the death event
table(world$entities$Individual$get_attr("marital_status"))
#> 
#>       divorced        married  never married not applicable      separated 
#>             16            143             92             49              6 
#>        widowed 
#>             27
```

## Marriage

### Description

This event forms marriage relationships between cohabiting couples and
any two opposite-sex individuals that are not in a relationship. After
the marriage is formed, the newly wedded couple then decide whether to
form a new household or to merge their households into one. When forming
a new household, regardless of their household formation decision, any
related individuals (i.e. dependent children) to both individuals will
also follow to the new household.

<details>

As you can see there are four models that shall be supplied to determine
the likelihood in each stage of the marriage event. The first stage is
to marry cohabiting couples based on the likelihood produces by the
`marriage_cohab_male` model. The second stage is to marry eligible,
single individual agents. The probabilities for the individual agents to
enter the marital market come from the Monte Carlo simulation result
performed using the `marriage_no_cohab_male` and
`marriage_no_cohab_female` models on the individual agents’ attributes.
Then all participating individual agents are paired based on a given
rule. The rule can be as simple as all agents prefer to match with an
agent that has the minimum age difference to theirs. See
`StochasticMarriageMarket` and `OptimalMarriageMarket` for the matching
strategies available in `modules/demography/marriage.R`. Note that, if
there are more agents of one gender than other in the marital market
then the number of maximum matches will be equal to the number of
individual agents with the lesser number. Those who are not matched will
remain single after the event has ended. The current implementation
doesn’t include marriages between same-sex couples. After the matching
step, all newly formed couples will decide whether they will form a new
household (both agents leave their current households) or for the wife
and her related individuals to join her husband’s household. The current
implementation applies a very simple rule which is likely to be wrong
and should be replaced if there is a better assumption or model. The
current rule is that for all newly wedded couples if their male partner
aged more than the number given in `husbandAgeRuleToCreateNewHousehold`
they will create a new household.

TLDR;

  - Simulate marriages for comhabiting couples and single individuals.
  - Aged 16 and above to be able to marry.
  - Doesn’t consider marriages between same-sex couples.
  - The matching strategy for pairing individuals can be configured,
    either optimal or stochastic.

</details>

### Usage

``` r
event_demography_marriage <- modules::use(here::here("modules/demography/marriage.R"))
```

    #> run:
    #> function(world, model = NULL, target = NULL, time_steps = NULL)
    #> 
    #> 
    #> REQUIRED_MODELS:
    #>  chr [1:3] "marriage_cohab_male" "marriage_no_cohab_male" ...

### Params

  - **object**: a \[World\] object.
  - **model**: a named list.
      - **marriage\_cohab\_male**: a binary model that determines
        whether a cohabitating couple will get married. This is based on
        attributes of the male partner.
      - **marriage\_no\_cohab\_male**: a binary model that determines
        the chance for eligible males to enter the marital market.
      - **marriage\_no\_cohab\_female**: a binary model that determines
        the chance for eligible females to enter the marital market.

<!-- end list -->

``` r
model <- list(
  marriage_cohab_male = list(yes = 0.1, no = 0.9),
  marriage_no_cohab_male = list(yes = 0.1, no = 0.9),
  marriage_no_cohab_female = list(yes = 0.1, no = 0.9)
)
```

  - **target**: a named list
  - **time\_steps**: an integer vector that contains the time steps in
    which this event should be run.

### Example

``` r
create_toy_world()
#> [17:22:53] WARN  dymiumCore self$initialise_data: Creating `hhsize` as it is not provided with `hh_data`.

event_demography_marriage <- modules::use(here::here("modules/demography/marriage.R"))

# Distribution of marital status before a run of the marriage event
table(world$entities$Individual$get_attr("marital_status"))
#> 
#>       divorced        married  never married not applicable      separated 
#>             22            174            108             53              6 
#>        widowed 
#>             10

world %>%
  event_demography_marriage$run(., model = model)
#> [17:22:53] INFO  demography event_demography_marriage$run: Running Marriage
#> [17:22:54] INFO  demography event_demography_marriage$run: 7 males and 6 are entering the marriage market (ratio=1.17:1).
#> [17:22:54] INFO  demography event_demography_marriage$run: There were 7 marriages occured (priorly cohabited: 1, did not cohabited: 6)

# Distribution of marital status before a run of the marriage event
table(world$entities$Individual$get_attr("marital_status"))
#> 
#>       divorced        married  never married not applicable      separated 
#>             20            188             96             53              6 
#>        widowed 
#>             10
```

## Separation

### Description

Separation is the stage before a couple is officially divorced. In the
Australian context, all married couples that would like to get divorced
must register their separtion one year prior to becoming legally
divorced. Hence, this event simulates that process.

<details>

Separation is the first step before couples can be officially divorced.
This module assume that no couples will recoupled once they have decided
to separate.

</details>

### Usage

``` r
event_demography_separation <- modules::use(here::here("modules/demography/separation.R"))
```

    #> run:
    #> function(world, model = NULL, target = NULL, time_steps = NULL)
    #> 
    #> 
    #> REQUIRED_MODELS:
    #>  chr [1:4] "separate_male" "separate_child_custody" "separate_hhtype" ...

### Params

  - **object**: a World object.
  - **model**: a named list.

<!-- end list -->

``` r
model <- 
  list(
    separate_male = list(yes = 0.1, no = 0.9),
    separate_child_custody = list(male = 0.2, female = 0.8),
    separate_hhtype = list(lone = 0.5, group = 0.5),
    separate_hf_random_join = list("1" = 0.4, "2" = 0.3, "3" = 0.2, "4" = 0.1)
  )
```

  - **target**: NULL
  - time\_steps: an integer vector that contains the time steps in which
    this event should be run.

### Example

``` r
create_toy_world()
#> [17:22:54] WARN  dymiumCore self$initialise_data: Creating `hhsize` as it is not provided with `hh_data`.

event_demography_separation <- modules::use(here::here("modules/demography/separation.R"))

# Distribution of marital status before a run of the marriage event
table(world$entities$Individual$get_attr("marital_status"))
#> 
#>       divorced        married  never married not applicable      separated 
#>             22            174            108             53              6 
#>        widowed 
#>             10

world %>%
  event_demography_separation$run(., model = model)
#> [17:22:54] INFO  demography event_demography_separation$run: Running Separation
#> [17:22:54] INFO  demography event_demography_separation$run: #seperating couples: 6

# Distribution of marital status before a run of the marriage event
table(world$entities$Individual$get_attr("marital_status"))
#> 
#>       divorced        married  never married not applicable      separated 
#>             22            162            108             53             18 
#>        widowed 
#>             10
```

## Divorce

### Description

When divorce is triggered for a separted individual, his/her ex-partner
will also under go divorce **if** the marital status of his/her
ex-partner is still ‘separated’.

<details>

</details>

### Usage

``` r
event_demography_divorce <- modules::use(here::here("modules/demography/divorce.R"))
```

    #> run:
    #> function(world, model = NULL, target = NULL, time_steps = NULL)
    #> 
    #> 
    #> REQUIRED_MODELS:
    #>  chr [1:2] "divorce_male" "divorce_female"

### Params

  - **object**: a World object.
  - **model**: a named list.

<!-- end list -->

``` r
model <- list(
  divorce_male = list(yes = 0.5, no = 0.9),
  divorce_female = list(yes = 0.5, no = 0.9)
)
```

  - **target**: NULL
  - **time\_steps**: an integer vector that contains the time steps in
    which this event should be run.

### Example

``` r
create_toy_world()
#> [17:22:54] WARN  dymiumCore self$initialise_data: Creating `hhsize` as it is not provided with `hh_data`.

event_demography_divorce <- modules::use(here::here("modules/demography/divorce.R"))

# Distribution of marital status before a run of the marriage event
table(world$entities$Individual$get_attr("marital_status"))
#> 
#>       divorced        married  never married not applicable      separated 
#>             22            174            108             53              6 
#>        widowed 
#>             10

world %>%
  event_demography_divorce$run(., model = model)
#> [17:22:54] INFO  demography event_demography_divorce$run: Running Divorce
#> [17:22:55] INFO  demography event_demography_divorce$run: #individuals to divorce: 2

# Distribution of marital status before a run of the marriage event
table(world$entities$Individual$get_attr("marital_status"))
#> 
#>       divorced        married  never married not applicable      separated 
#>             24            174            108             53              4 
#>        widowed 
#>             10
```

## Cohabitation

### Description

Form cohabitation relationships. Everything is the same with marriage in
term of the steps.

<details>

</details>

### Usage

``` r
event_demography_cohabit <- modules::use(here::here("modules/demography/cohabit.R"))
```

    #> run:
    #> function(world, model = NULL, target = NULL, time_steps = NULL)
    #> 
    #> 
    #> REQUIRED_MODELS:
    #>  chr [1:2] "cohabitation_male" "cohabitation_female"

### Params

  - object: a \[World\] object.
  - model: a named list.

<!-- end list -->

``` r
model <- list(
  cohabitation_male = list(yes = 0.1, no = 0.9),
  cohabitation_female = list(yes = 0.1, no = 0.9)
)
```

  - target: a named list.
  - time\_steps: an integer vector that contains the time steps in which
    this event should be run.

### Example

``` r
create_toy_world()
#> [17:22:55] WARN  dymiumCore self$initialise_data: Creating `hhsize` as it is not provided with `hh_data`.

event_demography_cohabit <- modules::use(here::here("modules/demography/cohabit.R"))

# Distribution of marital status before a run of the marriage event
table(world$entities$Individual$get_attr("marital_status"))
#> 
#>       divorced        married  never married not applicable      separated 
#>             22            174            108             53              6 
#>        widowed 
#>             10

world %>%
  event_demography_cohabit$run(., model = model)
#> [17:22:55] INFO  demography event_demography_cohabit$run: Running Cohabit2
#> [17:22:55] INFO  demography event_demography_cohabit$run: 2 males and 4 females enter the cohabitation market (ratio = 0.5:1).
#> [17:22:55] INFO  demography event_demography_cohabit$run: 2 newly cohabited couples were formed.

# Distribution of marital status before a run of the marriage event
table(world$entities$Individual$get_attr("marital_status"))
#> 
#>       divorced        married  never married not applicable      separated 
#>             22            174            108             53              6 
#>        widowed 
#>             10
```

## Breakup

### Description

Breakup ends cohabitation relationships.

### Usage

``` r
event_demography_breakup <- modules::use(here::here("modules/demography/breakup.R"))
```

    #> run:
    #> function(world, model = NULL, target = NULL, time_steps = NULL)
    #> 
    #> 
    #> REQUIRED_MODELS:
    #>  chr [1:4] "breakup" "breakup_child_custody" "breakup_hhtype" ...

### Params

  - object: a \[World\] object.
  - model: a named list.

<!-- end list -->

``` r
model <- 
  list(
    breakup = list(yes = 0.1, no = 0.9),
    breakup_child_custody = list(male = 0.2, female = 0.8),
    breakup_hhtype = list(lone = 0.5, group = 0.5),
    breakup_hf_random_join = list("1" = 0.4, "2" = 0.3, "3" = 0.2, "4" = 0.1)
  )
```

  - target: a named list.
  - time\_steps: an integer vector that contains the time steps in which
    this event should be run.

### Example

``` r
create_toy_world()
#> [17:22:55] WARN  dymiumCore self$initialise_data: Creating `hhsize` as it is not provided with `hh_data`.

event_demography_breakup <- modules::use(here::here("modules/demography/breakup.R"))

# Total number of individuals with partner before
table(is.na(world$entities$Individual$get_attr("partner_id")))
#> 
#> FALSE  TRUE 
#>   196   177

world %>%
  event_demography_breakup$run(., model = model)
#> [17:22:55] INFO  demography event_demography_breakup$run: Running Breakup
#> [17:22:55] INFO  demography event_demography_breakup$run: There is 1 couple who broke up

# Total number of individuals with partner after
table(is.na(world$entities$Individual$get_attr("partner_id")))
#> 
#> FALSE  TRUE 
#>   194   179
```

## Leave parental home

### Description

This simulates young adults leaving their parental home.

<details>

This should not include the proportion of people whom leave home to get
married or to live with their partner as it has been accounted for in
the marriage event and the cohabitation event.

</details>

### Usage

``` r
event_demography_leavehome <- modules::use(here::here("modules/demography/leavehome.R"))
```

    #> run:
    #> function(world, model = NULL, target = NULL, time_steps = NULL)
    #> 
    #> 
    #> REQUIRED_MODELS:
    #>  chr [1:4] "leavehome_male" "leavehome_female" "leavehome_hhtype" ...

### Params

  - **object**: a \[World\] object.
  - **model**: a named list.
      - **leavehome\_male**: a binary decision for male individuals
        whether to leave home or not.
      - **leavehome\_female**: a binary decision for female individuals
        whether to leave home or not.
      - **leavehome\_hhtype**: a binary choice whether to join a group
        household or to form a single household.
      - **leavehome\_hf\_random\_join**: a discrete distribution of the
        household size. For example, if this is `list(1 = 0.5, 2
        = 0.3, 3 = 0.1, 4 = 0.1)` there is a 50% chance that an
        individual leaving their parental home will find a single
        household to join and a 30% chance to join a two-person
        household and so on.

<!-- end list -->

``` r
model <- list(leavehome_male = list(yes = 0.3, no = 0.7),
              leavehome_female = list(yes = 0.2, no = 0.8),
              leavehome_hhtype = list(lone = 0.2, group = 0.8),
              leavehome_hf_random_join = list("1" = 0.5, "2" = 0.3, "3" = 0.1, "4" = 0.1))
```

  - **target**: `NULL` or a named list.
  - **time\_steps**: an integer vector that contains the time steps in
    which this event should be run.

### Example

``` r
create_toy_world()
#> [17:22:56] WARN  dymiumCore self$initialise_data: Creating `hhsize` as it is not provided with `hh_data`.

event_demography_leavehome <- modules::use(here::here("modules/demography/leavehome.R"))

# Total number of households before
world$entities$Household$n()
#> [1] 144

world %>%
  event_demography_leavehome$run(., model = model)
#> [17:22:56] INFO  demography event_demography_leavehome$run: Running Leavehome
#> [17:22:56] INFO  demography event_demography_leavehome$run: There are 10 individuals leaving their parental homes.

# Total number of households after
world$entities$Household$n()
#> [1] 145
```

## Migration

### Description

Add a migrant population to the main population.

<details>

The migrant population will be assigned new unique ids once it is added.
Note that all migrants should have all the same attribute columns (as
well as their types) as the main population it is being added to.

</details>

### Usage

``` r
event_demography_migration <- modules::use(here::here("modules/demography/migration.R"))
```

    #> run:
    #> function(world, model = NULL, target = NULL, time_steps = NULL)
    #> 
    #> 
    #> REQUIRED_MODELS:
    #>  chr [1:2] "migrant_individuals" "migrant_households"

### Params

  - object: a \[World\] object.
  - model: a named list.
  - migrant\_individuals: a data.table that contains migrant individual
    agents’ attribute data.
  - migrant\_households: a data.table that contains migrant household
    agents’ attribute data.

<!-- end list -->

``` r
model <- list(
  migrant_individuals = dymiumCore::toy_individuals, 
  migrant_households = dymiumCore::toy_households
)
```

  - target: an integer which indicates how many households should be
    added.

<!-- end list -->

``` r
target = 100
```

  - time\_steps: an integer vector that contains the time steps in which
    this event should be run.

### Note

### Example

``` r
create_toy_world()
#> [17:22:56] WARN  dymiumCore self$initialise_data: Creating `hhsize` as it is not provided with `hh_data`.

event_demography_migration <- modules::use(here::here("modules/demography/migration.R"))

# Total number of households and individuals before
world$entities$Household$n()
#> [1] 144
world$entities$Individual$n()
#> [1] 373

world %>%
  event_demography_migration$run(., model = model, target = target)
#> [17:22:56] INFO  demography event_demography_migration$run: Running Migration
#> [17:22:56] INFO  demography event_demography_migration$run: 100 migrant households are joining to the population.
#> [17:22:56] INFO  demography event_demography_migration$run: There are 100 migrant households which made up of 253 individuals (avg. hhsize = 2.53)

# Total number of households and individuals after
world$entities$Household$n()
#> [1] 244
world$entities$Individual$n()
#> [1] 626
```

# Known issues
