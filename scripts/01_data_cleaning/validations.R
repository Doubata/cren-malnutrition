#' Validate Arguments for \code{get_event_prop()}
#'
#' @description
#' Internal gatekeeper for \code{\link{get_event_prop}}. Collects **all**
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
#' @param type        Must be a non-empty subset of
#'                    \code{c("row_wise", "point_wise")}.
#'
#' @return \code{NULL} invisibly on success; throws a combined error on failure.
#'
#' @note
#' Mirrors \code{.check_summarise_event_args()} in structure with two
#' differences: \code{type} choices are restricted to
#' \code{c("row_wise", "point_wise")} reflecting the proportional modes of
#' \code{\link{get_event_prop}}, and \code{target_cols} is validated both as
#' a character vector and as a subset of \code{names(df)} to ensure every
#' named column exists.
#'
#' @seealso \code{\link{get_event_prop}}, \code{.validate_df()},
#'   \code{\link[checkmate]{makeAssertCollection}}
#'
#' @keywords internal
.check_get_event_prop_args <- function(df, id_col, phase_col, event,
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
  
  # 4. event must be a single character scalar (not a vector or column name)
  assert_character(event, len = 1, add = coll)
  
  # 5. run_order is optional — validate only when provided
  if (!is.null(run_order)) {
    assert_count(run_order, positive = TRUE, add = coll)
  }
  
  # 6. Non-empty vector (type coercion handled upstream)
  assert_vector(ids, min.len = 1, add = coll)
  
  # 7. Enumerated choices — empty vector is rejected
  assert_subset(type, choices = c("row_wise", "point_wise"),
                empty.ok = FALSE, add = coll)
  
  # 8. Report all collected violations at once
  reportAssertions(coll)
}



#' Validate Arguments for \code{get_prop_flag()}
#'
#' @description
#' Internal gatekeeper for \code{\link{get_prop_flag}}. Collects **all**
#' argument violations via a \pkg{checkmate} assertion collection and reports
#' them in a single error message.
#'
#' @param df         Data frame to validate against \code{col_schema}.
#' @param id_col     Column name for grouping IDs — must be numeric.
#' @param phase_col  Column name for the phase series — must be character.
#' @param target_col Column name for evaluated values — must be character.
#' @param flag       The flag value to match — must be a single character scalar.
#' @param run_order  Single strictly positive integer.
#' @param ids        Vector of length ≥ 1.
#'
#' @return \code{NULL} invisibly on success; throws a combined error on failure.
#'
#' @note
#' Corrected from the original: the schema key for \code{id_col} was
#' mistakenly labelled \code{"weight"} and \code{flag} was checked with
#' \code{assert_character(min.len = 1)} — an invalid argument.
#' Both are fixed here: the key is renamed \code{"id"} and \code{flag} is
#' validated as a length-1 character scalar via
#' \code{assert_character(flag, len = 1)}.
#'
#' @seealso \code{\link{get_prop_flag}}, \code{.validate_df()},
#'   \code{\link[checkmate]{makeAssertCollection}}
#'
#' @keywords internal
.check_get_prop_flag_args <- function(df, id_col, phase_col, target_col,
                                      flag, run_order, ids) {
  
  coll <- makeAssertCollection()
  
  # 1. df structure & column types (delegated to shared helper)
  col_schema <- list(
    id     = list(value = id_col,     type = "numeric",   required = TRUE),  # fixed: was "weight"
    phase  = list(value = phase_col,  type = "character", required = TRUE),
    target = list(value = target_col, type = "character", required = TRUE)
  )
  .validate_df(df, col_schema, coll)
  
  # 2. flag must be a single character scalar (not a vector)
  assert_character(flag, len = 1, add = coll)   # fixed: min.len is not a valid argument
  
  # 3. Single strictly positive integer
  assert_count(run_order, positive = TRUE, add = coll)
  
  # 4. Non-empty vector (type coercion handled upstream)
  assert_vector(ids, min.len = 1, add = coll)
  
  # 5. Report all collected violations at once
  reportAssertions(coll)
}
