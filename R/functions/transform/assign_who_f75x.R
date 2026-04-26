
#' @family WHO assignment
#' Assign WHO F75 Feeding Frequency or Quantity per Patient
#'
#' @description
#' For each row in \code{df}, looks up the WHO F75 therapeutic feeding table
#' and returns either the recommended feeding **frequency** (feeds/day) for a
#' given quantity, or the recommended **quantity** (ml/feed) for a given
#' frequency — depending on \code{measure_type}. Patients with severe oedema
#' (grade ≥ 3) are routed to a dedicated lookup table (\code{f75_oed3_lkp_df});
#' all others use the standard table (\code{f75_lkp_df}).
#'
#' @param df               A data frame containing patient-level data.
#' @param weight_col       Column name (quoted or unquoted) for patient weight in kg.
#' @param food_col         Column name (quoted or unquoted) for the food type given.
#' @param oedema_col       Column name (quoted or unquoted) for oedema grade
#'                         (numeric; grade ≥ 3 = severe).
#' @param target_col       Column name (quoted or unquoted) for the input value
#'                         to look up — either a quantity (ml) or a frequency
#'                         (feeds/day), depending on \code{measure_type}.
#' @param ...              Reserved for future use or passed to internal validators.
#' @param type_food        Character vector of food type codes to process.
#'                         Defaults to \code{c("F75", "F75F")}. Rows whose
#'                         \code{food_col} value is not in \code{type_food} are
#'                         left as \code{NA}.
#' @param measure_type     One of \code{"frequency"} or \code{"quantity"}:
#'   \itemize{
#'     \item \code{"frequency"} — \code{target_col} holds a quantity (ml/feed);
#'           returns the matching WHO feeding frequency (12, 8, or 6 feeds/day).
#'     \item \code{"quantity"} — \code{target_col} holds a frequency
#'           (12, 8, or 6); returns the matching WHO quantity (ml/feed).
#'   }
#' @param f75_lkp_df        WHO F75 lookup table for non-severe oedema patients.
#'                             Must contain columns \code{weight_kg},
#'                             \code{ml_per_2h}, \code{ml_per_3h}, \code{ml_per_4h}.
#' @param f75_oed3_lkp_df   WHO F75 lookup table for severe oedema (grade ≥ 3)
#'                             patients. Same column structure as \code{f75_lkp_df}.
#'
#' @return
#' An unnamed numeric vector the same length as \code{nrow(df)}. Each element
#' is the looked-up frequency (feeds/day) or quantity (ml/feed) for that row.
#' Returns \code{NA_real_} for rows where:
#' \itemize{
#'   \item \code{food_col} is not in \code{type_food}.
#'   \item Patient weight falls outside the lookup table range.
#'   \item The input value in \code{target_col} does not match any table entry.
#' }
#'
#' @details
#' Processing pipeline:
#' \enumerate{
#'   \item \strong{Symbol capture} — column name arguments accept both quoted
#'         strings and bare (unquoted) names via \code{\link{substitute}()}.
#'   \item \strong{Validation} — delegates to \code{.check_args_assign_who_f75x()}.
#'   \item \strong{Food filter} — restricts processing to rows where
#'         \code{food_col \%in\% type_food}.
#'   \item \strong{Oedema split} — partitions filtered rows into two groups:
#'         severe oedema (grade ≥ 3) and moderate/no oedema.
#'   \item \strong{Lookup} — for each group, \code{.lookup_who_idx()} maps
#'         patient weights to the nearest ceiling row in the lookup table,
#'         then builds a value matrix from the \code{ml_per_2h},
#'         \code{ml_per_3h}, \code{ml_per_4h} columns (corresponding to
#'         12, 8, and 6 feeds/day respectively).
#'   \item \strong{Frequency mode} — uses matrix algebra (\code{max.col()})
#'         to find which column matches the input quantity and returns the
#'         corresponding frequency.
#'   \item \strong{Quantity mode} — matches the input frequency against
#'         \code{c(12, 8, 6)} and extracts the corresponding ml value via
#'         matrix row/column indexing.
#' }
#'
#' @examples
#' # Frequency mode: given a quantity, find the WHO recommended frequency
#' assign_who_f75x(df, weight_col = "weight_kg", food_col = "food_type",
#'                 oedema_col = "oedema_grade", target_col = "ml_given",
#'                 measure_type = "frequency",
#'                 f75_lkp_df = who_f75, f75_oed3_lkp_df = who_f75_oed3)
#'
#' # Quantity mode: given a frequency, find the WHO recommended quantity
#' assign_who_f75x(df, weight_col = "weight_kg", food_col = "food_type",
#'                 oedema_col = "oedema_grade", target_col = "freq_given",
#'                 measure_type = "quantity",
#'                 f75_lkp_df = who_f75, f75_oed3_lkp_df = who_f75_oed3)
#'
#' @family WHO assignment
#'
#' @seealso
#' \code{.check_assign_who_f75x_args()}, \code{.lookup_who_idx()},
#' \code{\link{get_series_str}}
#'
#' @export
assign_who_f75x <- function(df, weight_col, food_col, oedema_col, target_col, ...,
                            type_food    = c("F75", "F75F"),
                            measure_type = c("frequency", "quantity"),
                            f75_lkp_df, f75_oed3_lkp_df) {
  
  # --- 1. Symbol Capture ---------------------------------------------------
  # Allows callers to pass bare column names (unquoted) or quoted strings.
  if (is.symbol(substitute(weight_col)))   weight_col   <- deparse(substitute(weight_col))
  if (is.symbol(substitute(food_col)))     food_col     <- deparse(substitute(food_col))
  if (is.symbol(substitute(oedema_col)))   oedema_col   <- deparse(substitute(oedema_col))
  if (is.symbol(substitute(target_col)))   target_col   <- deparse(substitute(target_col))
  if (is.symbol(substitute(type_food)))    type_food    <- deparse(substitute(type_food))
  if (is.symbol(substitute(measure_type))) measure_type <- deparse(substitute(measure_type))
  
  # --- 2. Validation -------------------------------------------------------
  .check_assign_who_f75x_args(df, weight_col, food_col, oedema_col, target_col, type_food, measure_type)
  
  # --- 3. Vector Extraction ------------------------------------------------
  # Pre-extract columns for performance
  weight  <- df[[weight_col]]
  food    <- df[[food_col]]
  oedema  <- df[[oedema_col]]
  target  <- df[[target_col]]

  # --- 4. Initialisation ---------------------------------------------------
  # Standard WHO feeding frequencies (feeds/day) matching ml_per_2h/3h/4h columns
  freq    <- c(12, 8, 6)
  results <- rep(NA_real_, nrow(df))
  
  # --- 5. Food Filter ------------------------------------------------------
  # Only process rows with a recognised F75 food type
  valid_food <- food %in% type_food
  if (length(valid_food) == 0) return(results)
  
  # --- 6. Oedema Split -----------------------------------------------------
  # Severe oedema: grade >= 3 → routed to the dedicated lookup table
  severe <- !is.na(oedema) & oedema >= 3
  severe_oedema     <- valid_food & severe
  moderate_oedema   <- valid_food & !severe
  
  # --- 7. Internal Lookup Helper -------------------------------------------
  process_group <- function(mask, lkp_df) {
    if (length(mask) == 0) return(NULL)
    
    idx <- which(mask)
    
    # Map patient weights to the ceiling row in the lookup table
    w_idx <- .lookup_who_idx(weight[idx], lkp_df[["weight_kg"]])
    
    # Build a [n × 3] value matrix: columns = x12, x8, x6 feeds/day
    val_matrix <- cbind(lkp_df[["ml_per_2h"]][w_idx],
                        lkp_df[["ml_per_3h"]][w_idx],
                        lkp_df[["ml_per_4h"]][w_idx])
    
    input_vals <- target[idx]
    
    if (measure_type == "frequency") {
      # Input is a quantity → find which column matches → return frequency
      matches <- max.col(val_matrix == input_vals, ties.method = "first")
      matches[rowSums(val_matrix == input_vals, na.rm = TRUE) == 0] <- NA_real_
      return(freq[matches])
      
    } else {
      # Input is a frequency → match to freq vector → return quantity (ml)
      freq_idx <- match(input_vals, freq)
      return(val_matrix[cbind(seq_along(idx), freq_idx)])
    }
  }
  
  # --- 8. Assign Results ---------------------------------------------------
  if (length(moderate_oedema) > 0)
    results[moderate_oedema] <- process_group(moderate_oedema, f75_lkp_df)
  if (length(severe_oedema) > 0)
    results[severe_oedema]   <- process_group(severe_oedema,   f75_oed3_lkp_df)
  
  return(unname(results))
}
