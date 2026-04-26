
#' Validate F75 Feeding Adherence Against WHO Recommendations
#'
#' @description
#' For each row in \code{df}, computes the patient's actual daily intake by
#' aggregating meal-level quantities, then compares it against the WHO F75
#' recommended daily range derived from the patient's weight. Returns a
#' categorical adherence label per row: \code{"within"}, \code{"more"}, or
#' \code{"less"}. Patients with severe oedema (grade ≥ 3) are routed to a
#' dedicated lookup table (\code{f75_oed3_lkp_df}); all others use the
#' standard table (\code{f75_lkp_df}).
#'
#' @param df                 A data frame containing patient-level data.
#' @param weight_col         Column name (quoted or unquoted) for patient weight in kg.
#' @param food_col           Column name (quoted or unquoted) for the food type given.
#' @param oedema_col         Column name (quoted or unquoted) for oedema grade
#'                           (numeric; grade ≥ 3 = severe).
#' @param qty_col            Column name (quoted or unquoted) for the prescribed
#'                           quantity per feed (ml/feed).
#' @param meal_cols          Character vector of column names (unquoted)
#'                           representing individual meal intake records, aggregated
#'                           by \code{.get_daily_intake_batch()} into a daily total.
#' @param ...                Reserved for future use or passed to internal validators.
#' @param type_food          Character vector of food type codes to process.
#'                           Defaults to \code{c("F75", "F75F")}. Rows whose
#'                           \code{food_col} value is not in \code{type_food}
#'                           are left as \code{NA}.
#' @param f75_lkp_df         WHO F75 lookup table for non-severe oedema patients.
#'                           Must contain columns \code{weight_kg},
#'                           \code{ml_per_day}, and \code{min_ml_per_day}.
#' @param f75_oed3_lkp_df    WHO F75 lookup table for severe oedema (grade ≥ 3)
#'                           patients. Same column structure as \code{f75_lkp_df}.
#'
#' @return
#' An unnamed character vector the same length as \code{nrow(df)}, with values:
#' \itemize{
#'   \item \code{"within"} — daily intake is within the WHO recommended range.
#'   \item \code{"more"}   — daily intake exceeds the WHO maximum.
#'   \item \code{"less"}   — daily intake falls below the WHO minimum.
#'   \item \code{NA}       — \code{food_col} not in \code{type_food}, weight
#'                           outside the lookup table range, or oedema grade missing.
#' }
#'
#' @details
#' Processing pipeline:
#' \enumerate{
#'   \item \strong{Symbol capture} — column name arguments accept both quoted
#'         strings and bare (unquoted) names via \code{\link{substitute}()}.
#'   \item \strong{Validation} — delegates to
#'         \code{.check_args_validate_who_f75x_adh()}.
#'   \item \strong{Food filter} — restricts processing to rows where
#'         \code{food_col \%in\% type_food}.
#'   \item \strong{Oedema split} — partitions filtered rows into severe oedema
#'         (grade ≥ 3) and moderate/no oedema groups, each routed to its
#'         corresponding lookup table.
#'   \item \strong{Daily intake} — \code{.get_daily_intake_batch()} aggregates
#'         \code{meal_cols} values (coerced to character to handle factors)
#'         with \code{qty_col} into a total daily intake per patient.
#'   \item \strong{Weight lookup} — \code{.lookup_who_idx()} maps patient
#'         weights to the nearest ceiling row; rows with out-of-range weights
#'         (\code{w_idx = NA}) are left as \code{NA} in the output.
#'   \item \strong{Adherence classification} — compares daily intake against
#'         \code{ml_per_day} (max) and \code{min_ml_per_day} (min) from the
#'         lookup table, defaulting to \code{"within"} then overriding with
#'         \code{"more"} or \code{"less"} where appropriate.
#' }
#'
#' @examples
#' # Classify adherence for all patients in df
#' validate_f75x_adh(
#'   df,
#'   weight_col = "weight_kg", food_col = "food_type",
#'   oedema_col = "oedema_grade",
#'   qty_col    = "qty_prescribed",
#'   meal_cols  = c("meal_1", "meal_2", "meal_3"),
#'   f75_lkp_df      = who_f75,
#'   f75_oed3_lkp_df = who_f75_oed3
#' )
#' #> [1] "within" "less" "more" NA ...
#'
#' @seealso
#' \code{\link{assign_who_f75x}}, \code{.check_validate_f75x_adh_args()},
#' \code{.get_daily_intake_batch()}, \code{.lookup_who_idx()}
#'
#' @export
validate_f75x_adh <- function(df, weight_col, food_col, oedema_col,
                              qty_col, meal_cols, ...,
                              type_food = c("F75", "F75F"),
                              f75_lkp_df, f75_oed3_lkp_df) {
  
  # --- 1. Symbol Capture ---------------------------------------------------
  # Allows callers to pass bare column names (unquoted) or quoted strings.
  if (is.symbol(substitute(weight_col))) weight_col <- deparse(substitute(weight_col))
  if (is.symbol(substitute(food_col)))   food_col   <- deparse(substitute(food_col))
  if (is.symbol(substitute(oedema_col))) oedema_col <- deparse(substitute(oedema_col))
  if (is.symbol(substitute(qty_col)))    qty_col    <- deparse(substitute(qty_col))
  
  # --- 2. Validation -------------------------------------------------------
  .check_validate_f75x_adh_args(df, weight_col, food_col, oedema_col, qty_col, meal_cols, type_food)
  
  # --- 3. Vector Extraction ------------------------------------------------
  # Pre-extract columns for performance
  weight  <- df[[weight_col]]
  food    <- df[[food_col]]
  oedema  <- df[[oedema_col]]
  qty     <- df[[qty_col]]
  
  # --- 4. Initialisation ---------------------------------------------------
  results <- rep(NA_character_, nrow(df))
  
  # --- 5. Food Filter ------------------------------------------------------
  # Only process rows with a recognised F75 food type
  valid_food <- food %in% type_food
  if (length(valid_food) == 0) return(results)
  
  # --- 6. Oedema Split -----------------------------------------------------
  # Severe oedema: grade >= 3 → routed to the dedicated lookup table
  severe       <- !is.na(oedema) & oedema >= 3
  moderate_oedema <- valid_food & !severe
  severe_oedema   <- valid_food &  severe
  
  # --- 7. Internal Adherence Helper ----------------------------------------
  process_group <- function(mask, lkp_df) {
    if (length(mask) == 0) return(NULL)
    
    idx <- which(mask)
    
    # Coerce meal columns to character to prevent factor level mismatches
    intake_matrix <- df[idx, meal_cols, drop = FALSE]
    intake_matrix <- sapply(intake_matrix, as.character)
    if (!is.matrix(intake_matrix))
      intake_matrix <- matrix(intake_matrix, nrow = length(idx))
    
    # Aggregate meal records into a total daily intake per patient
    daily_intakes <- .get_daily_intake_batch(qty[idx], intake_matrix)
    
    # Map patient weights to the ceiling row in the lookup table
    w_idx   <- .lookup_who_idx(weight[idx], lkp_df[["weight_kg"]])
    valid_w <- !is.na(w_idx)   # rows with out-of-range weights are left NA
    
    res <- rep(NA_character_, length(idx))
    
    if (any(valid_w)) {
      curr_intakes <- daily_intakes[valid_w]
      curr_max     <- lkp_df[["ml_per_day"]][w_idx[valid_w]]
      curr_min     <- lkp_df[["min_ml_per_day"]][w_idx[valid_w]]
      
      # Default to "within", then override at the boundaries
      sub_res <- rep("within", length(curr_intakes))
      sub_res[curr_intakes > curr_max] <- "more"
      sub_res[curr_intakes < curr_min] <- "less"
      
      res[valid_w] <- sub_res
    }
    return(res)
  }
  
  # --- 8. Assign Results ---------------------------------------------------
  if (length(moderate_oedema) > 0)
    results[moderate_oedema] <- process_group(moderate_oedema, f75_lkp_df)
  if (length(severe_oedema) > 0)
    results[severe_oedema]   <- process_group(severe_oedema,   f75_oed3_lkp_df)
  
  return(results)
}
