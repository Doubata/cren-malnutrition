
#' Validate Arguments for \code{get_value_at()}
#'
#' @description
#' Internal gatekeeper for \code{\link{get_value_at}}. Collects **all**
#' argument violations via a \pkg{checkmate} assertion collection and reports
#' them in a single error message.
#'
#' @param df         Data frame to validate against \code{col_schema}.
#' @param id_col     Column name for grouping IDs — must be numeric.
#' @param target_col Column name for extracted values — must be numeric or character.
#' @param row_index  Single strictly positive integer.
#' @param ids        Vector of length ≥ 1.
#'
#' @return \code{NULL} invisibly on success; throws a combined error on failure.
#'
#' @seealso \code{\link{get_value_at}}, \code{.validate_df()},
#'   \code{\link[checkmate]{makeAssertCollection}}
#'
#' @keywords internal

.validate_value_at_args <- function(df, id_col, target_col, row_index, ids){
  
  coll <- makeAssertCollection()
  
  # 1. Dataframe & Structure Checks
  col_schema <- list(
    id     = list(value = id_col,     type = "numeric",               required = TRUE),
    target = list(value = target_col, type = "numeric_or_character",  required = TRUE)
  )
  .validate_df(df, col_schema, coll)
  
  # 2. Check if row_index is a single positive integer
  assert_count(row_index, positive = TRUE, add = coll)
  
  # 3. Check ids
  assert_vector(ids, min.len = 1, add = coll)
  
  # 4. Report all errors at once if any occurred
  reportAssertions(coll)
}



#' Validate Arguments for \code{get_series_str()}
#'
#' @description
#' Internal gatekeeper for \code{\link{get_series_str}}. Collects **all**
#' argument violations via a \pkg{checkmate} assertion collection and reports
#' them in a single error message.
#'
#' @param df         Data frame to validate against \code{col_schema}.
#' @param id_col     Column name for grouping IDs — must be numeric.
#' @param target_col Column name for encoded values — must be character.
#' @param attr_col   Optional column name for attributes — must be numeric if provided.
#' @param ids        Vector of length ≥ 1.
#'
#' @return \code{NULL} invisibly on success; throws a combined error on failure.
#'
#' @seealso \code{\link{get_series_str}}, \code{.validate_df()},
#'   \code{\link[checkmate]{makeAssertCollection}}
#'
#' @keywords internal

.validate_series_str_args <- function(df, id_col, target_col, attr_col, ids){
  
  coll <- makeAssertCollection()
  
  # 1. df structure & column types (delegated to shared helper)
  col_schema <- list(
    weight  = list(value = id_col,     type = "numeric",    required = TRUE),
    target  = list(value = target_col, type = "character",  required = TRUE),
    attr    = list(value = attr_col,   type = "numeric",    required = FALSE)
  )
  .validate_df(df, col_schema, coll)
  
  # 2. Non-empty vector (type coercion handled upstream in get_series_str)
  assert_vector(ids, min.len = 1, add = coll)
  
  # 4. Report all errors at once if any occurred
  reportAssertions(coll)
}



#' Validate Arguments for \code{get_matching_series_str()}
#'
#' @description
#' Internal gatekeeper for \code{\link{get_matching_series_str}}. Collects
#' **all** argument violations via a \pkg{checkmate} assertion collection and
#' reports them in a single error message.
#'
#' @param df         Data frame to validate against \code{col_schema}.
#' @param id_col     Column name for grouping IDs — must be numeric.
#' @param target_col Column name for encoded values — must be character.
#' @param ref_col    Column name for the reference series — must be character.
#' @param attr_col   Optional column name for attributes — must be numeric if provided.
#' @param run_order  Single strictly positive integer.
#' @param ids        Vector of length ≥ 1.
#'
#' @return \code{NULL} invisibly on success; throws a combined error on failure.
#'
#' @seealso \code{\link{get_matching_series_str}}, \code{.validate_df()},
#'   \code{\link[checkmate]{makeAssertCollection}}
#'
#' @keywords internal

.validate_matching_series_str_args <- function(df, id_col, target_col, ref_col, attr_col = NULL, run_order, ids){
  
  coll <- makeAssertCollection()

  # 1. df structure & column types (delegated to shared helper)
  col_schema <- list(
    weight  = list(value = id_col,     type = "numeric",    required = TRUE),
    target  = list(value = target_col, type = "character",  required = TRUE),
    ref     = list(value = ref_col,    type = "character",  required = TRUE),
    attr    = list(value = attr_col,   type = "numeric",    required = FALSE) # optional
  )
  .validate_df(df, col_schema, coll)
  
  # 2. Single strictly positive integer
  assert_count(run_order, positive = TRUE, add = coll)
  
  # 3. Non-empty vector (type coercion handled upstream)
  assert_vector(ids, min.len = 1, add = coll)
  
  # 4. Report all collected violations at once
  reportAssertions(coll)
}



#' Validate Arguments for \code{summarise_event()}
#'
#' @description
#' Internal gatekeeper for \code{\link{summarise_event}}. Collects **all**
#' argument violations via a \pkg{checkmate} assertion collection and reports
#' them in a single error message.
#'
#' @param df          Data frame to validate — must have at least one row.
#' @param id_col      Column name for grouping IDs — must be numeric.
#' @param phase_col   Column name for the phase series — must be character.
#' @param event       The event value to match — must be a single character scalar.
#' @param target_cols Character vector of length ≥ 1 of column names to search.
#' @param ids         Vector of length ≥ 1.
#' @param run_order   Either \code{NULL} or a single strictly positive integer.
#' @param type        Must be one of \code{c("any", "global", "proportion",
#'                    "density", "consecutive", "first", "last")}.
#'
#' @return \code{NULL} invisibly on success; throws a combined error on failure.
#'
#' @note
#' \code{"density"} was added to the \code{type} choices in v1.1.0 when
#' \code{get_event_prop(type = "point_wise")} was deprecated and folded into
#' \code{\link{summarise_event}}. The full supersession map is:
#' \itemize{
#'   \item \code{get_prop_flag()}                      → \code{type = "proportion"}
#'   \item \code{get_event_prop(type = "row_wise")}    → \code{type = "proportion"}
#'   \item \code{get_event_prop(type = "point_wise")}  → \code{type = "density"}
#' }
#'
#' @seealso \code{\link{summarise_event}}, \code{.validate_df()},
#'   \code{\link[checkmate]{makeAssertCollection}}
#'
#' @keywords internal
.check_summarise_event_args <- function(df, id_col, phase_col, event,
                                        target_cols, ids, run_order, type) {
  
  coll <- makeAssertCollection()
  
  # 1. Data frame must exist and have at least one row
  assert_data_frame(df, min.rows = 1, add = coll)
  
  # 2. df structure & column types (delegated to shared helper)
  col_schema <- list(
    id    = list(value = id_col,    type = "numeric",   required = TRUE),
    phase = list(value = phase_col, type = "character", required = TRUE)
  )
  .validate_df(df, col_schema, coll)
  
  # 3. target_cols: non-empty character vector of existing column names
  assert_character(target_cols, min.len = 1, add = coll)
  assert_subset(target_cols, choices = names(df), empty.ok = FALSE, add = coll)
  
  # 4. event: single character scalar — not a column name
  assert_character(event, len = 1, add = coll)
  
  # 5. run_order: optional — validate only when provided
  if (!is.null(run_order)) {
    assert_count(run_order, positive = TRUE, add = coll)
  }
  
  # 6. ids: non-empty vector (type coercion handled upstream)
  assert_vector(ids, min.len = 1, add = coll)
  
  # 7. type: full set including "density" added in v1.1.0
  assert_subset(type,
                choices   = c("any", "global", "proportion", "density",
                              "consecutive", "first", "last"),
                empty.ok  = FALSE,
                add       = coll)
  
  # 8. Report all collected violations at once
  reportAssertions(coll)
}



#' Validate Arguments for \code{get_series_length_at()}
#'
#' @description
#' Internal gatekeeper for \code{\link{get_series_length_at}}. Collects
#' **all** argument violations via a \pkg{checkmate} assertion collection and
#' reports them in a single error message.
#'
#' @param df         Data frame to validate — must have at least one row.
#' @param id_col     Column name for grouping IDs — must be numeric.
#' @param target_col Column name for the series to encode — must be character.
#' @param run_order  Single strictly positive integer.
#' @param ids        Vector of length ≥ 1.
#'
#' @return \code{NULL} invisibly on success; throws a combined error on failure.
#'
#' @seealso \code{\link{get_series_length_at}}, \code{.validate_df()},
#'   \code{\link[checkmate]{makeAssertCollection}}
#'
#' @keywords internal
.check_get_series_length_at_args <- function(df, id_col, target_col,
                                             run_order, ids) {
  coll <- makeAssertCollection()
  
  # 1. Data frame must exist and have at least one row
  assert_data_frame(df, min.rows = 1, add = coll)
  
  # 2. df structure & column types (delegated to shared helper)
  col_schema <- list(
    id     = list(value = id_col,     type = "numeric",   required = TRUE),
    target = list(value = target_col, type = "character", required = TRUE)
  )
  .validate_df(df, col_schema, coll)
  
  # 3. Single strictly positive integer
  assert_count(run_order, positive = TRUE, add = coll)
  
  # 4. Non-empty vector (type coercion handled upstream)
  assert_vector(ids, min.len = 1, add = coll)
  
  # 5. Report all collected violations at once
  reportAssertions(coll)
}



#' Validate Arguments for \code{count_by_id()}
#'
#' @description
#' Internal gatekeeper for \code{\link{count_by_id}}. Collects **all**
#' argument violations via a \pkg{checkmate} assertion collection and reports
#' them in a single error message.
#'
#' @param df         Data frame to validate — must have at least one row.
#' @param id_col     Column name for grouping IDs — must be numeric.
#' @param target_col Column name for counted values — must be character.
#' @param attr_col   Optional column name for attributes — must be character if provided.
#' @param ids        Vector of length ≥ 1.
#' @param criteria   Optional atomic vector of length ≥ 1 to filter counts.
#'                   Defaults to \code{NULL} (no filtering).
#'
#' @return \code{NULL} invisibly on success; throws a combined error on failure.
#'
#' @seealso \code{\link{count_by_id}}, \code{.validate_df()},
#'   \code{\link[checkmate]{makeAssertCollection}}
#'
#' @keywords internal

.check_count_by_id_args <- function(df, id_col, target_col, attr_col = NULL, ids, criteria = NULL){
  
  coll <- makeAssertCollection()

  # 1. df structure & column types (delegated to shared helper)
  col_schema <- list(
    weight  = list(value = id_col,     type = "numeric",   required = TRUE),
    target  = list(value = target_col, type = "character", required = TRUE),
    attr    = list(value = attr_col,   type = "character", required = FALSE)
  )
  .validate_df(df, col_schema, coll)
  
  # 2. Non-empty vector (type coercion handled upstream)
  assert_vector(ids, min.len = 1, add = coll)
  
  # 3. When provided, criteria must be a non-empty atomic vector (no lists)
  assert_atomic_vector(criteria, min.len = 1, add = coll)
  
  # 4. Report all collected violations at once
  reportAssertions(coll)
}



#' Validate Arguments for \code{detect_state_transition()}
#'
#' @description
#' Internal gatekeeper for \code{\link{detect_state_transition}}. Collects
#' **all** argument violations via a \pkg{checkmate} assertion collection
#' and reports them in a single error message.
#'
#' @param df         Data frame to validate — must have at least one row.
#' @param id_col     Column name for grouping IDs — must be numeric.
#' @param phase_col  Column name for the phase series — must be character.
#' @param target_col Column name for state values — must be character.
#' @param from       Non-empty character vector of source states.
#' @param to         Either \code{NULL} or a non-empty character vector of
#'                   target states.
#' @param attr_col   Either \code{NULL} or a single character column name —
#'                   must be character if provided.
#' @param ids        Either \code{NULL} or a non-empty vector of IDs.
#' @param run_order  Either \code{NULL} or a single strictly positive integer.
#' @param verbose    Logical scalar.
#'
#' @return \code{NULL} invisibly on success; throws a combined error on failure.
#'
#' @note
#' \code{from}, \code{to}, \code{ids}, and \code{run_order} are all optional
#' or conditionally validated — each is skipped when \code{NULL} except
#' \code{from}, which is always required. \code{verbose} is validated as a
#' logical scalar via \code{\link[checkmate]{assert_flag}()}.
#'
#' @seealso \code{\link{detect_state_transition}}, \code{.validate_df()},
#'   \code{\link[checkmate]{makeAssertCollection}}
#'
#' @keywords internal
.check_detect_state_transition_args <- function(df, id_col, phase_col,
                                                target_col, from, to,
                                                attr_col, ids,
                                                run_order, verbose) {
  coll <- makeAssertCollection()
  
  # 1. Data frame must exist and have at least one row
  assert_data_frame(df, min.rows = 1, add = coll)
  
  # 2. df structure & column types (delegated to shared helper)
  col_schema <- list(
    id     = list(value = id_col,     type = "numeric",   required = TRUE),
    phase  = list(value = phase_col,  type = "character", required = TRUE),
    target = list(value = target_col, type = "numeric_or_character", required = TRUE),
    attr   = list(value = attr_col,   type = "character", required = FALSE)  # optional
  )
  .validate_df(df, col_schema, coll)
  
  # 3. from: always required — non-empty character vector of source states
  assert_character(from, min.len = 1, add = coll)
  
  # 4. to: optional — when provided must be a non-empty character vector
  if (!is.null(to)) {
    assert_character(to, min.len = 1, add = coll)
  }
  
  # 5. ids: optional — when provided must be a non-empty vector
  if (!is.null(ids)) {
    assert_vector(ids, min.len = 1, add = coll)
  }
  
  # 6. run_order: optional — when provided must be a single positive integer
  if (!is.null(run_order)) {
    assert_count(run_order, positive = TRUE, add = coll)
  }
  
  # 7. verbose: must be a single logical flag
  assert_flag(verbose, add = coll)
  
  # 8. Report all collected violations at once
  reportAssertions(coll)
}



#' Validate Arguments for \code{assign_who_f75x()}
#'
#' @description
#' Internal gatekeeper for \code{\link{assign_who_f75x}}. Collects **all**
#' argument violations via a \pkg{checkmate} assertion collection and reports
#' them in a single error message.
#'
#' @param df           Data frame to validate — must have at least one row.
#' @param weight_col   Column name for patient weight — must be numeric.
#' @param food_col     Column name for food type — must be character.
#' @param oedema_col   Column name for oedema grade — must be numeric.
#' @param target_col   Column name for the input value to look up — must be numeric.
#' @param type_food    Must be a non-empty subset of \code{c("F75", "F75F")}.
#' @param measure_type Must be a non-empty subset of \code{c("frequency", "quantity")}.
#'
#' @return \code{NULL} invisibly on success; throws a combined error on failure.
#'
#' @note
#' Unlike sibling validators, this gatekeeper validates two enumerated choice
#' arguments (\code{type_food}, \code{measure_type}) via
#' \code{\link[checkmate]{assert_subset}()}, and delegates column-level checks
#' to \code{.validate_df()} rather than \code{.validate_df()}.
#'
#' @seealso \code{\link{assign_who_f75x}}, \code{.validate_df()},
#'   \code{\link[checkmate]{makeAssertCollection}}
#'
#' @keywords internal
.check_assign_who_f75x_args <- function(df, weight_col, food_col, oedema_col,
                                        target_col, type_food, measure_type) {
  
  coll <- makeAssertCollection()
  
  # 1. Data frame must exist and have at least one row
  assert_data_frame(df, min.rows = 1, add = coll)
  
  # 2. df structure & column types (delegated to shared helper)
  col_schema <- list(
    weight = list(value = weight_col, type = "numeric",   required = TRUE),
    food   = list(value = food_col,   type = "character", required = TRUE),
    oedema = list(value = oedema_col, type = "numeric",   required = TRUE),
    target = list(value = target_col, type = "numeric",   required = TRUE)
  )
  .validate_df(df, col_schema, coll)
  
  # 3. Enumerated choices — empty vectors are rejected
  assert_subset(type_food,    choices = c("F75", "F75F"),             empty.ok = FALSE, add = coll)
  assert_subset(measure_type, choices = c("frequency", "quantity"),   empty.ok = FALSE, add = coll)
  
  # 4. Report all collected violations at once
  reportAssertions(coll)
}



#' Validate Arguments for \code{validate_f75x_adh()}
#'
#' @description
#' Internal gatekeeper for \code{\link{validate_f75x_adh}}. Collects **all**
#' argument violations via a \pkg{checkmate} assertion collection and reports
#' them in a single error message.
#'
#' @param df         Data frame to validate — must have at least one row.
#' @param weight_col Column name for patient weight — must be numeric.
#' @param food_col   Column name for food type — must be character.
#' @param oedema_col Column name for oedema grade — must be numeric.
#' @param qty_col    Column name for prescribed quantity — must be numeric.
#' @param meal_cols  Vector of meal intake column names — must be numeric.  
#' @param type_food  Must be a non-empty subset of \code{c("F75", "F75F")}.
#'
#' @return \code{NULL} invisibly on success; throws a combined error on failure.
#'
#' @note
#' Mirrors \code{.check_args_assign_who_f75x()} in structure but omits the
#' \code{measure_type} choice check, as adherence validation does not require
#' a frequency/quantity mode switch.
#'
#' @seealso \code{\link{validate_f75x_adh}}, \code{.validate_df()},
#'   \code{\link[checkmate]{makeAssertCollection}}
#'
#' @keywords internal
.check_validate_f75x_adh_args <- function(df, weight_col, food_col,
                                          oedema_col, qty_col, meal_cols, type_food) {
  coll <- makeAssertCollection()
  
  # 1. Data frame must exist and have at least one row
  assert_data_frame(df, min.rows = 1, add = coll)
  
  # 2. df structure & column types (delegated to shared helper)
  col_schema <- list(
    weight = list(value = weight_col, type = "numeric",   required = TRUE),
    food   = list(value = food_col,   type = "character", required = TRUE),
    oedema = list(value = oedema_col, type = "numeric",   required = TRUE),
    qty    = list(value = qty_col,    type = "numeric",   required = TRUE)
  )
  
  for (m in meal_cols) {
    col_schema[[m]] <- list(value = m, type = "character", required = TRUE)
  }
  
  .validate_df(df, col_schema, coll)
  
  # 3. Enumerated choice — empty vector is rejected
  assert_subset(type_food, choices = c("F75", "F75F"), empty.ok = FALSE, add = coll)
  
  # 4. Report all collected violations at once
  reportAssertions(coll)
}



#' Validate Arguments for \code{validate_f75x_qty()}
#'
#' @description
#' Internal gatekeeper for \code{\link{validate_f75x_qty}}. Collects **all**
#' argument violations via a \pkg{checkmate} assertion collection and reports
#' them in a single error message.
#'
#' @param df         Data frame to validate — must have at least one row.
#' @param weight_col Column name for patient weight — must be numeric.
#' @param food_col   Column name for food type — must be character.
#' @param oedema_col Column name for oedema grade — must be numeric.
#' @param freq_col   Column name for feeding frequency (meals per day) — must be numeric.
#' @param qty_col    Column name for the quantity consumed — must be numeric.
#' @param type_food  Must be a non-empty subset of \code{c("F75", "F75F")}.
#'
#' @return \code{NULL} invisibly on success; throws a combined error on failure.
#'
#' @note
#' This helper specifically ensures that both \code{freq_col} and \code{qty_col} 
#' are present and valid, as both are required to calculate/validate total 
#' nutritional intake quantities.
#'
#' @seealso \code{\link{validate_f75x_qty}}, \code{.validate_df()},
#'   \code{\link[checkmate]{makeAssertCollection}}
#'
#' @keywords internal
.check_validate_f75x_qty_args <- function(df, weight_col, food_col, oedema_col,
                                          freq_col, qty_col, type_food) {
  coll <- makeAssertCollection()
  
  # 1. Data frame must exist and have at least one row
  assert_data_frame(df, min.rows = 1, add = coll)
  
  # 2. df structure & column types (delegated to shared helper)
  col_schema <- list(
    weight = list(value = weight_col, type = "numeric",   required = TRUE),
    food   = list(value = food_col,   type = "character", required = TRUE),
    oedema = list(value = oedema_col, type = "numeric",   required = TRUE),
    freq   = list(value = freq_col,   type = "numeric",   required = TRUE),
    qty    = list(value = qty_col,    type = "numeric",   required = TRUE)
  )
  .validate_df(df, col_schema, coll)
  
  # 3. Enumerated choice — empty vector is rejected
  assert_subset(type_food, choices = c("F75", "F75F"), empty.ok = FALSE, add = coll)
  
  # 4. Report all collected violations at once
  reportAssertions(coll)
}



#' Validate Arguments for \code{assign_who_f100()}
#'
#' @description
#' Internal gatekeeper for \code{\link{assign_who_f100}}. Collects **all**
#' argument violations via a \pkg{checkmate} assertion collection and reports
#' them in a single error message.
#'
#' @param df            Data frame to validate — must have at least one row.
#' @param weight_col    Column name for patient weight — must be numeric.
#' @param food_col      Column name for food type — must be character.
#' @param target_col    Column name for the input value to look up — must be numeric.
#' @param measure_type  Character string; must be either \code{"frequency"} or \code{"quantity"}.
#'
#' @return \code{NULL} invisibly on success; throws a combined error on failure.
#'
#' @note
#' This helper is simpler than the F75 counterpart as it omits oedema and 
#' food type subset checks, focusing strictly on the requirements for 
#' F100 lookup logic.
#'
#' @seealso \code{\link{assign_who_f100}}, \code{.validate_df()},
#'   \code{\link[checkmate]{makeAssertCollection}}
#'
#' @keywords internal
.check_assign_who_f100_args <- function(df, weight_col, food_col, target_col, measure_type) {
  
  coll <- makeAssertCollection()
  
  # 1. Structure Checks
  # Data frame must exist and have at least one row
  assert_data_frame(df, min.rows = 1, add = coll)
  
  # 2. Main Dataframe: Individual Argument & Data Checks
  # Delegated to shared helper to verify column existence and types
  col_schema <- list(
    weight  = list(value = weight_col,  type = "numeric",   required = TRUE),
    food    = list(value = food_col,    type = "character", required = TRUE),
    target  = list(value = target_col,  type = "numeric",   required = TRUE)
  )
  .validate_df(df, col_schema, coll)
  
  # 3. Choices
  # Ensures measure_type is valid and not empty
  assert_subset(measure_type, choices = c("frequency", "quantity"), empty.ok = FALSE, add = coll)
  
  # 4. Report all collected violations at once
  reportAssertions(coll)
}



#' Validate Arguments for \code{validate_f100_qty()}
#'
#' @description
#' Internal gatekeeper for \code{\link{validate_f100_qty}}. Collects **all**
#' argument violations via a \pkg{checkmate} assertion collection and reports
#' them in a single error message.
#'
#' @param df         Data frame to validate — must have at least one row.
#' @param weight_col Column name for patient weight — must be numeric.
#' @param food_col   Column name for food type — must be character.
#' @param freq_col   Column name for prescribed feeding frequency — must be numeric.
#' @param qty_col    Column name for prescribed quantity per feed — must be numeric.
#'
#' @return \code{NULL} invisibly on success; throws a combined error on failure.
#'
#' @note
#' The body references \code{measure_type} and an \code{assert_subset()} check
#' that are not present in the function signature — this appears to be a
#' copy-paste residual from \code{.check_args_assign_who_f75x()} and should
#' be removed, as \code{validate_f100_qty()} does not use a
#' \code{measure_type} argument.
#'
#' @seealso \code{\link{validate_f100_qty}}, \code{.validate_df()},
#'   \code{\link[checkmate]{makeAssertCollection}}
#'
#' @keywords internal
.check_validate_f100_qty_args <- function(df, weight_col, food_col, freq_col, qty_col) {
  coll <- makeAssertCollection()
  
  # 1. Data frame must exist and have at least one row
  assert_data_frame(df, min.rows = 1, add = coll)
  
  # 2. df structure & column types (delegated to shared helper)
  col_schema <- list(
    weight = list(value = weight_col, type = "numeric",   required = TRUE),
    food   = list(value = food_col,   type = "character", required = TRUE),
    freq   = list(value = freq_col,   type = "numeric",   required = TRUE),
    qty    = list(value = qty_col,    type = "numeric",   required = TRUE)
  )
  .validate_df(df, col_schema, coll)
  
  # 3. Report all collected violations at once
  reportAssertions(coll)
}



#' Validate Arguments for \code{assign_who_f100d_freq()}
#'
#' @description
#' Internal gatekeeper for \code{\link{assign_who_f100d_freq}}. Collects
#' **all** argument violations via a \pkg{checkmate} assertion collection and
#' reports them in a single error message.
#'
#' @param df       Data frame to validate — must have at least one row.
#' @param food_col Column name for food type — must be character.
#'
#' @return \code{NULL} invisibly on success; throws a combined error on failure.
#'
#' @note
#' The most minimal validator in the WHO feeding family — only \code{df} and
#' a single character column are checked, reflecting the fixed-value simplicity
#' of \code{\link{assign_who_f100d_freq}}.
#'
#' @seealso \code{\link{assign_who_f100d_freq}}, \code{.validate_df()},
#'   \code{\link[checkmate]{makeAssertCollection}}
#'
#' @keywords internal
.check_assign_who_f100d_freq_args <- function(df, food_col) {
  
  coll <- makeAssertCollection()
  
  # 1. Data frame must exist and have at least one row
  assert_data_frame(df, min.rows = 1, add = coll)
  
  # 2. Single character column check (delegated to shared helper)
  col_schema <- list(
    food = list(value = food_col, type = "character", required = TRUE)
  )
  .validate_df(df, col_schema, coll)
  
  # 3. Report all collected violations at once
  reportAssertions(coll)
}



#' Validate Arguments for \code{assign_who_f100d_qty()}
#'
#' @description
#' Internal gatekeeper for \code{\link{assign_who_f100d_qty}}. Collects
#' **all** argument violations via a \pkg{checkmate} assertion collection and
#' reports them in a single error message.
#'
#' @param df         Data frame to validate — must have at least one row.
#' @param weight_col Column name for patient weight — must be numeric.
#' @param food_col   Column name for food type — must be character.
#' @param bf_col     Column name for breastfeeding status — declared as numeric
#'                   in the schema, though the parent function matches against
#'                   character values (\code{"oui"}). Verify type consistency
#'                   with the actual data.
#'
#' @return \code{NULL} invisibly on success; throws a combined error on failure.
#'
#' @note
#' 
#'
#' @seealso \code{\link{assign_who_f100d_qty}}, \code{.validate_df()},
#'   \code{\link[checkmate]{makeAssertCollection}}
#'
#' @keywords internal
.check_assign_who_f100d_args <- function(df, weight_col, food_col, bf_col) {
  
  coll <- makeAssertCollection()
  
  # 1. Data frame must exist and have at least one row
  assert_data_frame(df, min.rows = 1, add = coll)
  
  # 2. df structure & column types (delegated to shared helper)
  col_schema <- list(
    weight = list(value = weight_col, type = "numeric",   required = TRUE),
    food   = list(value = food_col,   type = "character", required = TRUE),
    bf     = list(value = bf_col,     type = "character", required = TRUE)
  )
  .validate_df(df, col_schema, coll)
  
  # 3. Report all collected violations at once
  reportAssertions(coll)
}



#' Validate Arguments for \code{assign_who_rutf()}
#'
#' @description
#' Internal gatekeeper for \code{\link{assign_who_rutf}}. Collects **all**
#' argument violations via a \pkg{checkmate} assertion collection and reports
#' them in a single error message.
#'
#' @param df         Data frame to validate — must have at least one row.
#' @param weight_col Column name for patient weight — must be numeric.
#' @param rutf_col   Column name for RUTF quantity — must be numeric.
#'
#' @return \code{NULL} invisibly on success; throws a combined error on failure.
#'
#' @note
#' The most minimal validator in the WHO assignment family alongside
#' \code{.check_assign_who_f100d_freq_args()} — only two numeric columns are
#' required, reflecting the weight-only lookup logic of
#' \code{\link{assign_who_rutf}}.
#'
#' @seealso \code{\link{assign_who_rutf}}, \code{.validate_df()},
#'   \code{\link[checkmate]{makeAssertCollection}}
#'
#' @keywords internal
.check_assign_who_rutf_args <- function(df, weight_col, rutf_col) {
  
  coll <- makeAssertCollection()
  
  # 1. Data frame must exist and have at least one row
  assert_data_frame(df, min.rows = 1, add = coll)
  
  # 2. df structure & column types (delegated to shared helper)
  col_schema <- list(
    weight = list(value = weight_col, type = "numeric", required = TRUE),
    rutf   = list(value = rutf_col,   type = "numeric", required = TRUE)
  )
  .validate_df(df, col_schema, coll)
  
  # 3. Report all collected violations at once
  reportAssertions(coll)
}



#' Validate Arguments for \code{assign_who_whz()}
#'
#' @description
#' Internal gatekeeper for \code{\link{assign_who_whz}}. Collects
#' **all** argument violations via a \pkg{checkmate} assertion collection and
#' reports them in a single error message.
#'
#' @param df         Data frame to validate — must have at least one row.
#' @param weight_col Column name for patient weight — must be numeric.
#' @param height_col Column name for patient height — must be numeric.
#' @param sex_col    Column name for patient sex — must be character.
#' @param age_col    Column name for patient age in months — must be numeric.
#'
#' @return \code{NULL} invisibly on success; throws a combined error on failure.
#'
#' @note
#' The most column-rich validator in the WHO assignment family — four columns
#' are required, reflecting the four-way sex × age-band stratification in
#' \code{\link{assign_who_whz}}.
#'
#' @seealso \code{\link{assign_who_whz}}, \code{.validate_df()},
#'   \code{\link[checkmate]{makeAssertCollection}}
#'
#' @keywords internal
.check_assign_who_whz_args <- function(df, weight_col, height_col,
                                              sex_col, age_col) {
  coll <- makeAssertCollection()
  
  # 1. Data frame must exist and have at least one row
  assert_data_frame(df, min.rows = 1, add = coll)
  
  # 2. df structure & column types (delegated to shared helper)
  col_schema <- list(
    weight = list(value = weight_col, type = "numeric",   required = TRUE),
    height = list(value = height_col, type = "numeric",   required = TRUE),
    sex    = list(value = sex_col,    type = "character", required = TRUE),
    age    = list(value = age_col,    type = "numeric",   required = TRUE)
  )
  .validate_df(df, col_schema, coll)
  
  # 3. Report all collected violations at once
  reportAssertions(coll)
}






