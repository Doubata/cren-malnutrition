
#' @family WHO assignment
#' Assign WHO F100d Recommended Feeding Frequency
#'
#' @description
#' Assigns the WHO-recommended feeding frequency of **8 feeds/day** to all
#' rows where \code{food_col} equals \code{"F100d"}. All other rows are left
#' as \code{NA}.
#'
#' @param df       A data frame containing patient-level data.
#' @param food_col Column name (quoted or unquoted) for the food type given.
#'                 Only rows where \code{food_col == "F100d"} are assigned a value.
#'
#' @return
#' A numeric vector the same length as \code{nrow(df)}: \code{8} for F100d
#' rows, \code{NA} for all others. Returns a full \code{NA} vector if no
#' F100d rows are found.
#'
#' @note
#' Unlike \code{\link{assign_who_f75x}}, this function requires no weight or
#' lookup table — the WHO F100d frequency is a single fixed value (8) applied
#' uniformly regardless of patient weight or oedema status.
#'
#' @examples
#' df <- data.frame(food_type = c("F75", "F100d", "F100d", "F100"))
#'
#' assign_who_f100d_freq(df, food_col = "food_type")
#' #> [1] NA  8  8 NA
#'
#' @family WHO assignment
#'
#' @seealso
#' \code{\link{assign_who_f75x}}, \code{\link{validate_f100_qty}},
#' \code{.check_assign_who_f100d_freq_args()}
#'
#' @export
assign_who_f100d_freq <- function(df, food_col) {
  
  # --- 1. Symbol Capture ---------------------------------------------------
  # Allows callers to pass bare column names (unquoted) or quoted strings.
  if (is.symbol(substitute(food_col)))     food_col     <- deparse(substitute(food_col))
  
  # --- 2. Symbol Capture ---------------------------------------------------
  # Allows callers to pass bare column names (unquoted) or quoted strings.
  if (is.symbol(substitute(food_col))) food_col <- deparse(substitute(food_col))
  
  # --- 3. Validation -------------------------------------------------------
  .check_assign_who_f100d_freq_args(df, food_col)
  
  # --- 4. Initialisation ---------------------------------------------------
  results <- rep(NA, nrow(df))
  
  # --- 5. Food Filter & Assignment -----------------------------------------
  # WHO F100d protocol: fixed frequency of 8 feeds/day for all F100d rows
  is_f100d <- !is.na(df[[food_col]]) & df[[food_col]] == "F100d"
  if (!any(is_f100d)) return(results)
  
  results[is_f100d] <- 8
  
  return(results)
}
