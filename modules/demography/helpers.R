# setup modules
modules::import("dymiumCore")
modules::import("data.table")
modules::import("checkmate")
modules::import("here")
modules::expose(here::here("modules/demography/constants.R"))
# derive agent characteristics ---------------------------------------------
# these functions should accept data as argument and add the extra column that it
# meant to create to the original data then return that. This means we can use them in pipe.
# However, this strategy only works when one agent's dataset is needed to compute
# the variable.
modules::export("DeriveVar")
DeriveVar <- list(
  IND = list(
    mrcurr = function(x, Ind)  {
      stopifnot(is.data.table(x))
      pid_col <- Ind$get_id_col()
      stopifnot(pid_col %in% names(x))

      .marital_status <- Ind$get_attr("marital_status", ids = x[[pid_col]])
      .partner_id <- Ind$get_attr("partner_id", ids = x[[pid_col]])
      x[, mrcurr := ifelse(!is.na(.partner_id) &
                             .marital_status != IND$MARITAL_STATUS$MARRIED,
                           yes = IND$MARITAL_STATUS$DE_FACTO,
                           no = marital_status)]
    },

    mrs = function(x, Ind) {
      stopifnot(is.data.table(x))
      pid_col <- Ind$get_id_col()
      stopifnot(pid_col %in% names(x))
      DeriveVar$IND$mrcurr(x, Ind)
      x[, mrs := ifelse(
        !mrcurr %in% c(IND$MARITAL_STATUS$MARRIED, IND$MARITAL_STATUS$DE_FACTO),
        "not in relationship",
        as.character(mrcurr)
      )]
    },

    age5 = function(x, IndObj)  {
      stopifnot(is.data.table(x))
      stopifnot("age" %in% names(x))
      pid_col <- IndObj$get_id_col()
      stopifnot(pid_col %in% names(x))
      .age <- IndObj$get_attr("age", ids = x[[pid_col]])
      .age[.age > 100] <- 100
      x[, age5 := cut(.age, breaks = seq(0, 100, 5), include.lowest = T, right = FALSE)]
    },

    n_children = function(x, IndObj) {
      stopifnot(is.data.table(x))
      pid_col <- IndObj$get_id_col()
      stopifnot(pid_col %in% names(x))
      x[, n_children := sapply(IndObj$get_children(get(pid_col)), function(x){sum(!is.na(x))})]
    },

    n_resident_children = function(x, IndObj) {
      stopifnot(is.data.table(x))
      pid_col <- IndObj$get_id_col()
      stopifnot(pid_col %in% names(x))
      x[, n_resident_children := sapply(IndObj$get_resident_children(get(pid_col)), function(x){sum(!is.na(x))})]
    },

    hhadult = function(x, IndObj) {
      stopifnot(is.data.table(x))
      pid_col <- IndObj$get_id_col()
      hid_col <- IndObj$get_hid_col()
      stopifnot(pid_col %in% names(x))
      adult_age <- 18L
      x[, hhadult := sum(age >= adult_age), by = c(hid_col)]
    },

    has_children = function(x, IndObj) {
      stopifnot(is.data.table(x))
      pid_col <- IndObj$get_id_col()
      stopifnot(pid_col %in% names(x))
      x[, has_children := sapply(IndObj$get_children(get(pid_col)), function(x){!is.na(sum(x))})]
    },

    has_resident_children = function(x, IndObj) {
      stopifnot(is.data.table(x))
      pid_col <- IndObj$get_id_col()
      stopifnot(pid_col %in% names(x))
      x[, has_resident_children := sapply(IndObj$get_resident_children(get(pid_col)), function(x){!is.na(sum(x))})]
    },

    age_youngest_resident_child = function(x, IndObj) {
      stopifnot(is.data.table(x))
      pid_col <- IndObj$get_id_col()
      stopifnot(pid_col %in% names(x))
      # create a minimal copy to avoid changing the main data `x`
      child_age_dt <- get_child_age(x, IndObj)
      # find the min age of the resident children of each individual
      child_age_dt <-
        child_age_dt[child_age_dt[, .I[which.min(child_age)], by = c(pid_col)]$V1, .SD, .SDcols = c(pid_col, "child_age")] %>%
        data.table::setnames(x = ., old = "child_age", "age_youngest_resident_child")
      # merge to the main data
      nrow_of_x_before_merge <- nrow(x)
      x <- merge(x, child_age_dt, by = pid_col, all.x = T)
      #' all NAs to -1 otherwise caret::train models will exclude these records
      #' when using the predict method on them
      x[is.na(age_youngest_resident_child), age_youngest_resident_child := -1]
      stopifnot(nrow(x) == nrow_of_x_before_merge)
      x
    },

    age_youngest_child = function(x, IndObj) {
      stopifnot(is.data.table(x))
      pid_col <- IndObj$get_id_col()
      stopifnot(pid_col %in% names(x))
      # create a minimal copy to avoid changing the main data `x`
      child_age_dt <- get_child_age(x, IndObj)
      # find the min age of the resident children of each individual
      child_age_dt <-
        child_age_dt[child_age_dt[, .I[which.min(child_age)], by = c(pid_col)]$V1, .SD, .SDcols = c(pid_col, "child_age")] %>%
        data.table::setnames(x = ., old = "child_age", "age_youngest_child")
      # merge to the main data
      nrow_of_x_before_merge <- nrow(x)
      x <- merge(x, child_age_dt, by = pid_col, all.x = T)
      #' all NAs to -1 otherwise caret::train models will exclude these records
      #' when using the predict method on them
      x[is.na(age_youngest_child), age_youngest_child := -1]
      stopifnot(nrow(x) == nrow_of_x_before_merge)
      x
    },

    age_oldest_child = function(x, IndObj) {
      stopifnot(is.data.table(x))
      pid_col <- IndObj$get_id_col()
      stopifnot(pid_col %in% names(x))
      # create a minimal copy to avoid changing the main data `x`
      child_age_dt <- get_child_age(x, IndObj)
      # find the min age of the resident children of each individual
      child_age_dt <-
        child_age_dt[child_age_dt[, .I[which.max(child_age)], by = c(pid_col)]$V1, .SD, .SDcols = c(pid_col, "child_age")] %>%
        data.table::setnames(x = ., old = "child_age", "age_oldest_child")
      # merge to the main data
      nrow_of_x_before_merge <- nrow(x)
      x <- merge(x, child_age_dt, by = pid_col, all.x = T)
      #' all NAs to -1 otherwise caret::train models will exclude these records
      #' when using the predict method on them
      x[is.na(age_oldest_child), age_oldest_child := -1]
      stopifnot(nrow(x) == nrow_of_x_before_merge)
      x
    },

    age_oldest_resident_child = function(x, IndObj) {
      stopifnot(is.data.table(x))
      pid_col <- IndObj$get_id_col()
      stopifnot(pid_col %in% names(x))
      # create a minimal copy to avoid changing the main data `x`
      child_age_dt <- get_child_age(x, IndObj)
      # find the max age of the resident children of each individual
      child_age_dt <-
        child_age_dt[child_age_dt[, .I[which.max(child_age)], by = c(pid_col)]$V1, .SD, .SDcols = c(pid_col, "child_age")] %>%
        data.table::setnames(x = ., old = "child_age", "age_oldest_resident_child")
      # merge to the main data
      nrow_of_x_before_merge <- nrow(x)
      x <- merge(x, child_age_dt, by = pid_col, all.x = T)
      #' all NAs to -1 otherwise caret::train models will exclude these records
      #' when using the predict method on them
      x[is.na(age_oldest_resident_child), age_oldest_resident_child := -1]
      stopifnot(nrow(x) == nrow_of_x_before_merge)
      x
    },

    hhsize = function(x, IndObj) {
      stopifnot(is.data.table(x))
      pid_col <- IndObj$get_id_col()
      hid_col <- IndObj$get_hid_col()
      stopifnot(pid_col %in% names(x))
      stopifnot(hid_col %in% names(x))
      hhsize <- IndObj$get_data() %>%
        .[, .(hhsize = .N), by = hid_col]
      merge(x, hhsize, by = hid_col, all.x = T, sort = FALSE)
    }
  )
)


# agent filter ------------------------------------------------------------
modules::export("FilterAgent")
FilterAgent <- list(
  Ind = list(
    can_marry = function(x) {
      get_individual_data(x) %>%
        .[age %between% c(RULES$MARRIAGE$AGE_LOWER_BOUND,
                          RULES$MARRIAGE$AGE_UPPER_BOUND) &
            marital_status != IND$MARITAL_STATUS$MARRIED &
            is.na(partner_id)]
    },

    can_marry_from_cohabitation = function(x) {
      get_individual_data(x) %>%
        .[age %between% c(RULES$MARRIAGE$AGE_LOWER_BOUND,
                          RULES$MARRIAGE$AGE_UPPER_BOUND) &
            marital_status != IND$MARITAL_STATUS$MARRIED & !is.na(partner_id)]
    },

    can_cohabit = function(x) {
      get_individual_data(x) %>%
        .[age %between% c(RULES$COHABITATION$AGE_LOWER_BOUND,
                          RULES$COHABITATION$AGE_UPPER_BOUND) &
            marital_status != IND$MARITAL_STATUS$MARRIED & is.na(partner_id)]
    },

    can_divorce = function(x) {
      get_individual_data(x) %>%
        FilterAgent$Ind$is_separated(.) %>%
        .[age %between% c(RULES$DIVORCE$AGE_LOWER_BOUND,
                          RULES$DIVORCE$AGE_UPPER_BOUND)]
    },

    can_breakup = function(x) {
      get_individual_data(x) %>%
        FilterAgent$Ind$is_cohabiting(.) %>%
        .[age %between% c(RULES$DIVORCE$AGE_LOWER_BOUND,
                          RULES$DIVORCE$AGE_UPPER_BOUND)]
    },

    can_separate = function(x) {
      get_individual_data(x) %>%
        FilterAgent$Ind$is_married(.) %>%
        .[age %between% c(RULES$DIVORCE$AGE_LOWER_BOUND,
                          RULES$DIVORCE$AGE_UPPER_BOUND)]
    },

    can_give_birth = function(x) {
      get_individual_data(x) %>%
        .[sex == IND$SEX$FEMALE &
            age %between% c(RULES$GIVE_BIRTH$AGE_LOWER_BOUND,
                            RULES$GIVE_BIRTH$AGE_UPPER_BOUND)]
    },

    can_leave_parentalhome = function(x) {
      IndObj <- assign_reference(x, Individual)
      pid_col <- IndObj$get_id_col()
      get_individual_data(x) %>%
        #' conditions - within the age range rule, have no partner, have at least
        #' one identifiable parent and has no children
        .[age %between% c(
          RULES$LEAVE_HOME$AGE_LOWER_BOUND,
          RULES$LEAVE_HOME$AGE_UPPER_BOUND
        ) &
          is.na(partner_id) &
          (!is.na(mother_id) | !is.na(father_id)) &
          !IndObj$have_relationship(ids = get(IndObj$get_id_col()), type = "children")] %>%
        #' only consider those living with parents
        #' lwm - living with mother, lmf - living with father
        .[, lwm := IndObj$living_together(get(pid_col), mother_id)] %>%
        .[, lwf := IndObj$living_together(get(pid_col), father_id)] %>%
        .[any(lwm, lwf) == TRUE,] %>%
        .[, c("lwm", "lwf") := NULL]
    },

    is_single = function(x) {
      get_individual_data(x) %>%
        .[marital_status != IND$MARITAL_STATUS$MARRIED & is.na(partner_id)]
    },

    is_married = function(x) {
      get_individual_data(x) %>%
        .[!is.na(partner_id) & marital_status == IND$MARITAL_STATUS$MARRIED]
    },

    is_cohabiting = function(x) {
      get_individual_data(x) %>%
        .[marital_status != IND$MARITAL_STATUS$MARRIED & !is.na(partner_id)]
    },

    is_divorced = function(x) {
      get_individual_data(x) %>%
        .[marital_status == IND$MARITAL_STATUS$DIVORCED]
    },

    is_separated = function(x) {
      get_individual_data(x) %>%
        .[marital_status == IND$MARITAL_STATUS$SEPARATED]
    },

    is_in_relationship = function(x) {
      get_individual_data(x) %>%
        .[!is.na(partner_id)]
    },

    is_male = function(x) {
      get_individual_data(x) %>%
        .[sex == IND$SEX$MALE]
    },

    is_female = function(x) {
      get_individual_data(x) %>%
        .[sex == IND$SEX$FEMALE]
    },

    is_living_with_parents = function(x, IndObj) {
      # serves as a check and assignment
      pid_col <- IndObj$get_id_col()
      get_individual_data(x) %>%
        #' only consider those living with parents
        #' lwm - living with mother, lmf - living with father
        .[, lwm := IndObj$living_together(get(pid_col), mother_id)] %>%
        .[, lwf := IndObj$living_together(get(pid_col), father_id)] %>%
        .[any(lwm, lwf) == TRUE,] %>%
        .[, c("lwm", "lwf") := NULL]
    }
  )
)

# helpers of helpers :) ---------------------------------------------------
# if x is data.frame then early return, otherwise extract individual data from x
modules::export("get_individual_data")
get_individual_data <- function(x) {
  if (is.data.frame(x))
    return(x)
  assign_reference(x, Individual)$get_data()
}

get_child_age <- function(x, IndObj, resident_child = FALSE) {
  checkmate::assert_data_table(x, null.ok = FALSE)
  checkmate::assert_r6(IndObj, classes = "Individual")
  pid_col <- IndObj$get_id_col()
  stopifnot(pid_col %in% names(x))

  # create a minimal copy to avoid changing the main data `x`
  if (resident_child == TRUE) {
    x_new <- copy(x)[, .SD, .SDcols = c(pid_col)] %>%
      .[, children := IndObj$get_resident_children(get(pid_col))]
  } else {
    x_new <- copy(x)[, .SD, .SDcols = c(pid_col)] %>%
      .[, children := IndObj$get_children(get(pid_col))]
  }

  x_new %>%
    .[, lapply(.SD, unlist), by = c(pid_col)] %>%
    # merge age attribute
    .[, child_age := NA_integer_] %>%
    .[!is.na(children), child_age := IndObj$get_attr(x = "age", ids = children)]
}
