
#' Validate a Single Data Frame Column
#'
#' @description
#' Internal atomic validator. For a single column mapping, checks in order:
#' \enumerate{
#'   \item Handles \code{NULL} — pushes an error if the column is required,
#'         silently skips if optional.
#'   \item Asserts \code{col_name} is a non-empty string (the column name).
#'   \item Asserts the column exists in \code{df}.
#'   \item Dispatches a type check based on \code{expected_type}.
#' }
#' All failures are pushed to \code{coll} rather than thrown immediately,
#' so every violation is collected before reporting.
#'
#' @param df            A data frame to validate.
#' @param col_name      Character scalar. The name of the column to validate,
#'                      or \code{NULL} if the argument was not provided.
#' @param expected_type Character scalar. One of \code{"numeric"},
#'                      \code{"character"}, \code{"logical"}, \code{"date"},
#'                      \code{"factor"}, \code{"choice"}, or
#'                      \code{"numeric_or_character"}.
#' @param is_required   Logical. Whether the column is mandatory.
#' @param lkp_name      Character scalar. Human-readable label used in error
#'                      messages (e.g. \code{"id_col"}).
#' @param coll          A \pkg{checkmate} assertion collection created by
#'                      \code{\link[checkmate]{makeAssertCollection}()}.
#'
#' @return \code{NULL} invisibly. All violations are pushed to \code{coll}.
#'
#' @details
#' Type dispatch is handled via \code{\link{switch}()}:
#' \itemize{
#'   \item \code{"numeric"}              → \code{assert_numeric()}
#'   \item \code{"character"}            → \code{assert_character()}
#'   \item \code{"logical"}              → \code{assert_logical()}
#'   \item \code{"date"}                 → \code{assert_date()}
#'   \item \code{"factor"}               → \code{assert_factor()}
#'   \item \code{"choice"}               → \code{assert_subset()} against
#'         \code{req$choices}
#'   \item \code{"numeric_or_character"} → \code{assert(check_numeric(),
#'         check_character(), combine = "or")}
#'   \item Unknown type → descriptive warning pushed to \code{coll}.
#' }
#'
#' @note
#' This function is the atomic building block called by \code{.validate_df()}
#' for each column entry in the schema. It should never be called directly
#' by end users or top-level gatekeepers — always go through
#' \code{.validate_df()}.
#'
#' @seealso \code{.validate_df()},
#'   \code{\link[checkmate]{makeAssertCollection}}
#'
#' @keywords internal
.check_column <- function(df, col_name, expected_type, is_required,
                          lkp_name, coll) {
  
  # --- 1. NULL Handling ----------------------------------------------------
  # Return early: required columns push an error, optional ones are skipped.
  # Fixed: replaced `next` (only valid in loops) with early return.
  if (is.null(col_name)) {
    if (is_required) {
      coll$push(paste0("Variable '", lkp_name, "' is mandatory and cannot be NULL."))
    }
    return(invisible(NULL))
  }
  
  # --- 2. Column Name Validity ---------------------------------------------
  # Ensure the column name argument is a non-empty string before proceeding.
  assert_string(col_name, min.chars = 1, .var.name = lkp_name, add = coll)
  
  # --- 3. Column Existence -------------------------------------------------
  # Only proceed to type check if col_name is a valid string.
  if (test_string(col_name, min.chars = 1)) {
    assert_names(names(df), must.include = col_name, add = coll)
    
    # --- 4. Type Dispatch --------------------------------------------------
    # Only type-check if the column actually exists in df.
    if (col_name %in% names(df)) {
      col_data <- df[[col_name]]
      
      switch(expected_type,
             "numeric"   = assert_numeric(col_data,
                                          .var.name = col_name, add = coll),
             "character" = assert_character(col_data,
                                            .var.name = col_name, add = coll),
             "logical"   = assert_logical(col_data,
                                          .var.name = col_name, add = coll),
             "date"      = assert_date(col_data,
                                       .var.name = col_name, add = coll),
             "factor"    = assert_factor(col_data,
                                         .var.name = col_name, add = coll),
             "choice"    = assert_subset(col_data, choices = req$choices,
                                         empty.ok = FALSE,
                                         .var.name = col_name, add = coll),
             # Hybrid: accepts either numeric or character
             "numeric_or_character" = {
               assert(check_numeric(col_data),
                      check_character(col_data),
                      combine    = "or",
                      .var.name  = col_name,
                      add        = coll)
             },
             # Fallback: unknown type — push a descriptive warning
             {
               coll$push(sprintf(
                 "[%s] Unknown type '%s' for column '%s'. Skipping type check.",
                 lkp_name, expected_type, col_name   # fixed: col_key not in scope
               ))
             }
      )
    }
  }
  
  return(invisible(NULL))
}



#' Validate a Data Frame Against a Column Schema
#'
#' @description
#' Shared validation helper used by all gatekeepers in this package. Asserts
#' that \code{data} is a non-empty data frame, then iterates over
#' \code{col_schema} and delegates each column's existence and type check to
#' \code{.check_column()}. All violations are collected before reporting,
#' so callers receive a single consolidated error message.
#'
#' @param data       A data frame to validate.
#' @param col_schema A named list where each element describes one column
#'                   mapping and must contain:
#'   \itemize{
#'     \item \code{value}    — character scalar, the actual column name in
#'           \code{data} (e.g. \code{"weight_kg"}).
#'     \item \code{type}     — expected column type; one of \code{"numeric"},
#'           \code{"character"}, \code{"logical"}, \code{"date"},
#'           \code{"factor"}, \code{"choice"}, or
#'           \code{"numeric_or_character"}.
#'     \item \code{required} — logical, whether the column is mandatory.
#'   }
#' @param coll       A \pkg{checkmate} assertion collection created by
#'                   \code{\link[checkmate]{makeAssertCollection}()}.
#'
#' @return \code{NULL} invisibly. All violations are pushed to \code{coll}.
#'
#' @details
#' Processing pipeline:
#' \enumerate{
#'   \item Asserts \code{data} is a data frame with at least one row via
#'         \code{\link[checkmate]{assert_data_frame}()}. Returns early if
#'         this check fails to avoid misleading downstream column errors.
#'   \item Iterates over \code{names(col_schema)}, extracting the
#'         \code{value}, \code{type}, and \code{required} fields for each
#'         entry.
#'   \item Delegates each column to \code{.check_column()} which handles
#'         \code{NULL}, existence, and type checks.
#' }
#'
#' @examples
#' # Typical usage inside a gatekeeper:
#' coll <- makeAssertCollection()
#' col_schema <- list(
#'   id     = list(value = "patient_id", type = "numeric",   required = TRUE),
#'   status = list(value = "status_col", type = "character", required = TRUE),
#'   flag   = list(value = NULL,         type = "character", required = FALSE)
#' )
#' .validate_df(df, col_schema, coll)
#' reportAssertions(coll)
#'
#' @seealso \code{.check_column()},
#'   \code{\link[checkmate]{makeAssertCollection}},
#'   \code{\link[checkmate]{assert_data_frame}}
#'
#' @keywords internal
.validate_df <- function(data, col_schema, coll) {
  
  # --- 1. Data Frame Check -------------------------------------------------
  # Fixed: was assert_data_frame(df, ...) — parameter is named `data`.
  assert_data_frame(data, min.rows = 1, add = coll)
  
  # Return early if data is not a valid data frame — avoids misleading
  # "column not found" errors that would cascade from a non-existent df.
  if (!test_data_frame(data)) return(invisible(NULL))
  
  # --- 2. Column Schema Iteration ------------------------------------------
  for (col_key in names(col_schema)) {
    col_req <- col_schema[[col_key]]
    
    # Fixed: original checked data[[col_key]] (schema key as column name).
    # Correct check: does the declared column name exist in data?
    if (!is.null(col_req$value) && !(col_req$value %in% names(data))) {
      if (col_req$required) {
        coll$push(sprintf(
          "[%s] Required column '%s' is missing from the data frame.",
          col_key, col_req$value
        ))
      }
      next
    }
    
    # Delegate existence + type check to the atomic column validator
    .check_column(data, col_req$value, col_req$type,
                  col_req$required, col_req$value, coll)
  }
  
  return(invisible(NULL))
}



#' Validate WHO F75 Standard Lookup Table
#'
#' @description
#' Validates the structure and column types of a WHO F75 feeding lookup table
#' (used for both standard and severe oedema variants). Pushes all violations
#' to \code{coll} for consolidated reporting by the calling gatekeeper.
#'
#' @param df   A data frame to validate. Expected columns: \code{weight_kg},
#'             \code{ml_per_2h}, \code{ml_per_3h}, \code{ml_per_4h},
#'             \code{ml_per_day}, \code{min_ml_per_day} — all numeric.
#' @param coll A \pkg{checkmate} assertion collection created by
#'             \code{\link[checkmate]{makeAssertCollection}()}.
#'
#' @return \code{NULL} invisibly. All violations are pushed to \code{coll}.
#'
#' @note
#' Shared by both \code{f75_lookup_df} and \code{f75_oed3_lookup_df} in
#' \code{\link{assign_who_f75x}} and \code{\link{validate_f75x_qty}} —
#' the two tables have identical schemas but different data ranges.
#'
#' @seealso \code{\link{assign_who_f75x}}, \code{\link{validate_f75x_qty}},
#'   \code{\link{validate_f75x_adh}}, \code{.validate_df()}
#'
#' @keywords internal
.validate_who_f75_std <- function(df, coll) {
  .validate_df(df, list(
    weight_kg      = list(value = "weight_kg",      type = "numeric", required = TRUE),
    ml_per_2h      = list(value = "ml_per_2h",      type = "numeric", required = TRUE),
    ml_per_3h      = list(value = "ml_per_3h",      type = "numeric", required = TRUE),
    ml_per_4h      = list(value = "ml_per_4h",      type = "numeric", required = TRUE),
    ml_per_day     = list(value = "ml_per_day",     type = "numeric", required = TRUE),
    min_ml_per_day = list(value = "min_ml_per_day", type = "numeric", required = TRUE)
  ), coll)
}



#' Validate WHO F100 Standard Lookup Table
#'
#' @description
#' Validates the structure and column types of the WHO F100 feeding lookup
#' table. Pushes all violations to \code{coll} for consolidated reporting
#' by the calling gatekeeper.
#'
#' @param df   A data frame to validate. Expected columns: \code{weight_kg},
#'             \code{max_ml_per_day}, \code{min_ml_per_day} — all numeric.
#' @param coll A \pkg{checkmate} assertion collection created by
#'             \code{\link[checkmate]{makeAssertCollection}()}.
#'
#' @return \code{NULL} invisibly. All violations are pushed to \code{coll}.
#'
#' @seealso \code{\link{validate_f100_qty}}, \code{.validate_df()}
#'
#' @keywords internal
.validate_who_f100_std <- function(df, coll) {
  .validate_df(df, list(
    weight_kg      = list(value = "weight_kg",      type = "numeric", required = TRUE),
    max_ml_per_day = list(value = "max_ml_per_day", type = "numeric", required = TRUE),
    min_ml_per_day = list(value = "min_ml_per_day", type = "numeric", required = TRUE)
  ), coll)
}



#' Validate WHO F100d Breastfed Lookup Table
#'
#' @description
#' Validates the structure and column types of the WHO F100d lookup table for
#' breastfed patients. Pushes all violations to \code{coll} for consolidated
#' reporting by the calling gatekeeper.
#'
#' @param df   A data frame to validate. Expected columns:
#'             \code{weight_kg_lower}, \code{weight_kg_upper},
#'             \code{ml_per_feed} — all numeric.
#' @param coll A \pkg{checkmate} assertion collection created by
#'             \code{\link[checkmate]{makeAssertCollection}()}.
#'
#' @return \code{NULL} invisibly. All violations are pushed to \code{coll}.
#'
#' @note
#' Uses weight band intervals (\code{weight_kg_lower} / \code{weight_kg_upper})
#' rather than single breakpoints — matched via \code{.lookup_who_f100d_idx()}
#' rather than \code{.lookup_who_idx()}.
#'
#' @seealso \code{\link{assign_who_f100d_qty}}, \code{.validate_df()}
#'
#' @keywords internal
.validate_who_f100d_bf <- function(df, coll) {
  .validate_df(df, list(
    weight_kg_lower = list(value = "weight_kg_lower", type = "numeric", required = TRUE),
    weight_kg_upper = list(value = "weight_kg_upper", type = "numeric", required = TRUE),
    ml_per_feed     = list(value = "ml_per_feed",     type = "numeric", required = TRUE)
  ), coll)
}



#' Validate WHO F100d Non-Breastfed Lookup Table
#'
#' @description
#' Validates the structure and column types of the WHO F100d lookup table for
#' non-breastfed patients. Pushes all violations to \code{coll} for
#' consolidated reporting by the calling gatekeeper.
#'
#' @param df   A data frame to validate. Expected columns:
#'             \code{weight_kg_lower}, \code{weight_kg_upper},
#'             \code{ml_severe_phase}, \code{ml_reha_phase} — all numeric.
#' @param coll A \pkg{checkmate} assertion collection created by
#'             \code{\link[checkmate]{makeAssertCollection}()}.
#'
#' @return \code{NULL} invisibly. All violations are pushed to \code{coll}.
#'
#' @note
#' The two phase columns reflect the two-phase lookup in
#' \code{\link{assign_who_f100d_qty}}: rows 1–7 map to \code{ml_severe_phase}
#' (acute phase); rows > 7 map to \code{ml_reha_phase} (rehabilitation phase).
#'
#' @seealso \code{\link{assign_who_f100d_qty}}, \code{.validate_df()}
#'
#' @keywords internal
.validate_who_f100d_nbf <- function(df, coll) {
  .validate_df(df, list(
    weight_kg_lower = list(value = "weight_kg_lower", type = "numeric", required = TRUE),
    weight_kg_upper = list(value = "weight_kg_upper", type = "numeric", required = TRUE),
    ml_severe_phase = list(value = "ml_severe_phase", type = "numeric", required = TRUE),
    ml_reha_phase   = list(value = "ml_reha_phase",   type = "numeric", required = TRUE)
  ), coll)
}



#' Validate WHO RUTF Standard Lookup Table
#'
#' @description
#' Validates the structure and column types of the WHO RUTF standard lookup
#' table. Pushes all violations to \code{coll} for consolidated reporting
#' by the calling gatekeeper.
#'
#' @param df   A data frame to validate. Expected columns: \code{weight_kg},
#'             \code{sachets_per_day} — all numeric.
#' @param coll A \pkg{checkmate} assertion collection created by
#'             \code{\link[checkmate]{makeAssertCollection}()}.
#'
#' @return \code{NULL} invisibly. All violations are pushed to \code{coll}.
#'
#' @note
#' The most minimal WHO lookup validator — only two columns are required,
#' reflecting the weight-only lookup logic of \code{\link{assign_who_rutf}}.
#'
#' @seealso \code{\link{assign_who_rutf}}, \code{.validate_df()}
#'
#' @keywords internal
.validate_who_rutf_std <- function(df, coll) {
  .validate_df(df, list(
    weight_kg       = list(value = "weight_kg",       type = "numeric", required = TRUE),
    sachets_per_day = list(value = "sachets_per_day", type = "numeric", required = TRUE)
  ), coll)
}



#' Validate a WHO Weight-for-Height Z-Score Lookup Table
#'
#' @description
#' Validates the structure and column types of any WHO weight-for-height
#' z-score lookup table. Shared across all four sex × age-band variants
#' (boys/girls, 0–24 months / 25–60 months). Pushes all violations to
#' \code{coll} for consolidated reporting by the calling gatekeeper.
#'
#' @param df   A data frame to validate. Expected columns: \code{Height},
#'             \code{SD4neg}, \code{SD3neg}, \code{SD2neg}, \code{SD1neg}, \code{SD0}
#'             — all numeric.
#' @param coll A \pkg{checkmate} assertion collection created by
#'             \code{\link[checkmate]{makeAssertCollection}()}.
#'
#' @return \code{NULL} invisibly. All violations are pushed to \code{coll}.
#'
#' @note
#' Shared by all four WFH lookup tables passed to
#' \code{\link{assign_who_wfh_zscore}}:
#' \code{wfh_boys02_z_lkp_df}, \code{wfh_boys25_z_lkp_df},
#' \code{wfh_girls02_z_lkp_df}, and \code{wfh_girls25_z_lkp_df}.
#' The four tables have identical schemas but different height ranges and
#' reference values.
#'
#' @seealso \code{\link{assign_who_wfh_zscore}}, \code{.validate_df()}
#'
#' @keywords internal
.validate_who_wfh_std <- function(df, coll) {
  .validate_df(df, list(
    Height = list(value = "Height", type = "numeric", required = TRUE),
    SD4neg = list(value = "SD4neg", type = "numeric", required = TRUE),
    SD3neg = list(value = "SD3neg", type = "numeric", required = TRUE),
    SD2neg = list(value = "SD2neg", type = "numeric", required = TRUE),
    SD1neg = list(value = "SD1neg", type = "numeric", required = TRUE),
    SD0    = list(value = "SD0",    type = "numeric", required = TRUE)
  ), coll)
}



#' Validate All WHO Standard Lookup Tables
#'
#' @description
#' Convenience wrapper that calls all per-table validators at once.
#' Useful for pre-validating the full set of WHO reference tables at
#' pipeline start. All violations across every table are collected and
#' reported in a single consolidated error message.
#'
#' @param WHO_F75_std        WHO F75 standard lookup table.
#' @param WHO_F75_oed3_std   WHO F75 severe oedema lookup table.
#' @param WHO_F100_std       WHO F100 standard lookup table.
#' @param WHO_F100d_bf       WHO F100d breastfed lookup table.
#' @param WHO_F100d_nbf      WHO F100d non-breastfed lookup table.
#' @param WHO_RUTF_std       WHO RUTF standard lookup table.
#' @param WHO_WFH_boys02_std WHO WFH boys 0–24 months lookup table.
#' @param WHO_WFH_boys25_std WHO WFH boys 25–60 months lookup table.
#' @param WHO_WFH_girls02_std WHO WFH girls 0–24 months lookup table.
#' @param WHO_WFH_girls25_std WHO WFH girls 25–60 months lookup table.
#'
#' @return \code{NULL} invisibly on success; throws a consolidated error
#'   on failure.
#'
#' @details
#' Each table is validated by its dedicated atomic validator:
#' \code{.validate_who_f75_std()}, \code{.validate_who_f100_std()},
#' \code{.validate_who_f100d_bf()}, \code{.validate_who_f100d_nbf()},
#' \code{.validate_who_rutf_std()}, and \code{.validate_who_wfh_std()}.
#' Schema definitions live exclusively in those atomic validators —
#' this function contains no duplicated schema logic.
#'
#' @seealso
#' \code{.validate_who_f75_std()}, \code{.validate_who_f100_std()},
#' \code{.validate_who_f100d_bf()}, \code{.validate_who_f100d_nbf()},
#' \code{.validate_who_rutf_std()}, \code{.validate_who_wfh_std()}
#'
#' @export
validate_who_lookup_tables <- function(WHO_F75_std,
                                       WHO_F75_oed3_std,
                                       WHO_F100_std,
                                       WHO_F100d_bf,
                                       WHO_F100d_nbf,
                                       WHO_RUTF_std,
                                       WHO_WFH_boys02_std,
                                       WHO_WFH_boys25_std,
                                       WHO_WFH_girls02_std,
                                       WHO_WFH_girls25_std) {
  
  coll <- makeAssertCollection()
  
  .validate_who_f75_std(WHO_F75_std,      coll)  # standard
  .validate_who_f75_std(WHO_F75_oed3_std, coll)  # severe oedema — same schema
  .validate_who_f100_std(WHO_F100_std,    coll)
  .validate_who_f100d_bf(WHO_F100d_bf,    coll)
  .validate_who_f100d_nbf(WHO_F100d_nbf,  coll)
  .validate_who_rutf_std(WHO_RUTF_std,    coll)
  .validate_who_wfh_std(WHO_WFH_boys02_std,  coll)  # boys  0-24m
  .validate_who_wfh_std(WHO_WFH_boys25_std,  coll)  # boys  25-60m
  .validate_who_wfh_std(WHO_WFH_girls02_std, coll)  # girls 0-24m
  .validate_who_wfh_std(WHO_WFH_girls25_std, coll)  # girls 25-60m
  
  reportAssertions(coll)
  
  return(invisible(NULL))
}

