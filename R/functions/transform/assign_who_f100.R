
#' @family WHO assignment
#' Assign WHO F100 Feeding Frequency or Quantity per Patient
#'
#' @description
#' For each row in \code{df}, looks up the WHO F100 therapeutic feeding table
#' and returns either the recommended feeding **frequency** (feeds/day) or the 
#' recommended **quantity** (ml/feed) range — depending on \code{measure_type}.
#' Unlike F75, F100 calculations are generally based on a fixed frequency and 
#' a volume range (min/max) based on weight.
#'
#' @param df                A data frame containing patient-level data.
#' @param weight_col        Column name (quoted or unquoted) for patient weight in kg.
#' @param food_col          Column name (quoted or unquoted) for the food type given.
#' @param target_col        Column name (quoted or unquoted) for the input value 
#'                          to look up — either a quantity (ml) or a frequency 
#'                          (feeds/day), depending on \code{measure_type}.
#' @param ...               Reserved for future use or passed to internal validators.
#' @param measure_type      One of \code{"frequency"} or \code{"quantity"}:
#'   \itemize{
#'     \item \code{"frequency"} — Returns the standard WHO F100 feeding 
#'           frequency (typically 6 feeds/day).
#'     \item \code{"quantity"} — \code{target_col} holds a frequency; returns the 
#'           matching WHO quantity range (ml/feed) based on the weight-defined 
#'           minimum and maximum.
#'   }
#' @param f100_lkp_df       WHO F100 lookup table. Must contain columns 
#'                          \code{weight_kg}, \code{min_ml_per_day}, and 
#'                          \code{max_ml_per_day}.
#'
#' @return
#' An unnamed numeric vector the same length as \code{nrow(df)}. Each element
#' is the looked-up frequency (feeds/day) or quantity (ml/feed) for that row. 
#' Returns \code{NA_real_} for rows where:
#' \itemize{
#'    \item \code{food_col} is not "F100".
#'    \item Patient weight falls outside the lookup table range.
#' }
#'
#' @details
#' Processing pipeline:
#' \enumerate{
#'    \item \strong{Symbol capture} — column name arguments accept both quoted
#'          strings and bare (unquoted) names via \code{\link{substitute}()}.
#'    \item \strong{Validation} — delegates to \code{.check_args_assign_who_f100()}.
#'    \item \strong{Food filter} — restricts processing to rows where 
#'          \code{food_col == "F100"}.
#'    \item \strong{Lookup mapping} — \code{.lookup_who_idx()} maps patient weights 
#'          to the nearest ceiling row in the \code{f100_lkp_df}.
#'    \item \strong{Frequency mode} — returns a fixed frequency of 6 feeds per day 
#'          for all valid F100 rows.
#'    \item \strong{Quantity mode} — calculates the volume by comparing the 
#'          \code{target_col} against the weight-based \code{min_ml_per_day} 
#'          and \code{max_ml_per_day}. If the target is at or below the minimum, 
#'          the minimum is used; otherwise, the maximum is returned.
#' }
#'
#' @examples
#' # Quantity mode: find the recommended F100 quantity range for a patient
#' assign_who_f100(df, weight_col = "weight_kg", food_col = "food_type",
#'                  target_col = "freq_given", measure_type = "quantity",
#'                  f100_lkp_df = who_f100_table)
#'
#' @family WHO assignment
#'
#' @seealso
#' \code{.check_assign_who_f100_args()}, \code{.lookup_who_idx()}, 
#' \code{assign_who_f75x}
#'
#' @export
assign_who_f100 <- function(df, weight_col, food_col, target_col, ...,
                            measure_type = c("frequency", "quantity"),
                            f100_lkp_df) {
  
  # --- 1. Symbol Capture ---------------------------------------------------
  # Allows callers to pass bare column names (unquoted) or quoted strings.
  if (is.symbol(substitute(weight_col)))   weight_col   <- deparse(substitute(weight_col))
  if (is.symbol(substitute(food_col)))     food_col     <- deparse(substitute(food_col))
  if (is.symbol(substitute(target_col)))   target_col   <- deparse(substitute(target_col))
  if (is.symbol(substitute(measure_type))) measure_type <- deparse(substitute(measure_type))
  
  # --- 2. Validation -------------------------------------------------------
  # Internal gatekeeper to check data frame integrity and column types
  .check_assign_who_f100_args(df, weight_col, food_col, target_col, measure_type)
  
  # --- 3. Vector Extraction ------------------------------------------------
  # Pre-extract columns for performance
  weight  <- df[[weight_col]]
  food    <- df[[food_col]]
  target  <- df[[target_col]]
  
  
  # --- 4. Initialisation ---------------------------------------------------
  results <- rep(NA_real_, nrow(df))
  
  # --- 5. Food Filter ------------------------------------------------------
  # Identify rows where food type is specifically F100
  is_f100 <- !is.na(food) & food == "F100"

  # Early exit if no valid rows found
  if (length(is_f100) == 0) return(results)
  
  idx_f100 <- which(is_f100)
  
  # --- 6. Lookup Mapping ---------------------------------------------------
  # Map patient weights to the corresponding row index in the F100 table
  w_idx <- .lookup_who_idx(weight[idx_f100], f100_lkp_df[["weight_kg"]])
  target_vals <- target[idx_f100]
  
  # --- 7. Assign Results ---------------------------------------------------
  results[idx_f100] <- if (measure_type == "frequency") {
    
    # Frequency mode: For F100, the standard WHO recommendation is 6 feeds/day
    rep(6, length(idx_f100))
    
  } else { 
    
    # Quantity mode: Calculate ml based on min/max daily volume range
    current_mins <- f100_lkp_df[["min_ml_per_day"]][w_idx]
    current_maxs <- f100_lkp_df[["max_ml_per_day"]][w_idx]
    
    # Logic: If target <= min, return min. If target > min, return max.
    # This handles the transition from F75 to F100 quantity requirements.
    qty <- pmax(target_vals, current_mins, na.rm = TRUE)
    ifelse(qty > current_mins, current_maxs, qty)
  }
  
  return(unname(results))
}