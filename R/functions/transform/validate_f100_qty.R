
#' Validate F100 Prescribed Quantity Against WHO Recommendations
#'
#' @description
#' For each F100 row in \code{df}, computes the prescribed daily intake as
#' \code{freq_col × qty_col} and compares it against the WHO F100 recommended
#' daily range derived from the patient's weight. Returns a categorical label
#' per row: \code{"within"}, \code{"more"}, or \code{"less"}.
#'
#' @param df             A data frame containing patient-level data.
#' @param weight_col     Column name (quoted or unquoted) for patient weight in kg.
#' @param food_col       Column name (quoted or unquoted) for the food type given.
#'                       Only rows where \code{food_col == "F100"} are processed.
#' @param freq_col       Column name (quoted or unquoted) for the prescribed
#'                       feeding frequency (feeds/day).
#' @param qty_col        Column name (quoted or unquoted) for the prescribed
#'                       quantity per feed (ml/feed).
#' @param ...            Reserved for future use or passed to internal validators.
#' @param f100_lkp_df WHO F100 lookup table. Must contain columns
#'                       \code{weight_kg}, \code{max_ml_per_day}, and
#'                       \code{min_ml_per_day}.
#'
#' @return
#' An unnamed character vector the same length as \code{nrow(df)}, with values:
#' \itemize{
#'   \item \code{"within"} — prescribed daily intake is within the WHO range.
#'   \item \code{"more"}   — prescribed daily intake exceeds the WHO maximum.
#'   \item \code{"less"}   — prescribed daily intake falls below the WHO minimum.
#'   \item \code{NA}       — \code{food_col} is not \code{"F100"}, or weight is
#'                           outside the lookup table range.
#' }
#' Returns \code{character(0)} if no F100 rows are found in \code{df}.
#'
#' @details
#' Processing pipeline:
#' \enumerate{
#'   \item \strong{Symbol capture} — column name arguments accept both quoted
#'         strings and bare (unquoted) names via \code{\link{substitute}()}.
#'   \item \strong{Validation} — delegates to
#'         \code{.check_args_validate_who_f100_qty()}.
#'   \item \strong{Food filter} — restricts processing to non-\code{NA} rows
#'         where \code{food_col == "F100"}.
#'   \item \strong{Daily intake} — computed as \code{freq_col × qty_col},
#'         representing total prescribed ml per day.
#'   \item \strong{Weight lookup} — \code{.lookup_who_idx()} maps patient
#'         weights to the nearest ceiling row in \code{f100_lkp_df};
#'         out-of-range weights produce \code{NA} in the output.
#'   \item \strong{Categorisation} — defaults to \code{"within"}, then overrides
#'         with \code{"more"} or \code{"less"} using vectorised logical indexing,
#'         ignoring \code{NA} entries in the limit matrix.
#' }
#'
#' @note
#' Simpler than its F75 counterparts (\code{\link{validate_f75x_qty}},
#' \code{\link{validate_f75x_adh}}) — no oedema split, no secondary lookup
#' table, and a single fixed food code (\code{"F100"}) rather than a
#' configurable \code{type_food} vector.
#'
#' @examples
#' validate_f100_qty(
#'   df,
#'   weight_col = "weight_kg", food_col  = "food_type",
#'   freq_col   = "freq_prescribed", qty_col = "qty_prescribed",
#'   f100_lkp_df = who_f100
#' )
#' #> [1] "within" "less" "more" NA ...
#'
#' @seealso
#' \code{\link{validate_f75x_qty}}, \code{\link{validate_f75x_adh}},
#' \code{.check_validate_f100_qty_args()}, \code{.lookup_who_idx()}
#'
#' @export
validate_f100_qty <- function(df, weight_col, food_col, freq_col, qty_col, ...,
                              f100_lkp_df) {
  
  # --- 1. Symbol Capture ---------------------------------------------------
  # Allows callers to pass bare column names (unquoted) or quoted strings.
  if (is.symbol(substitute(weight_col))) weight_col <- deparse(substitute(weight_col))
  if (is.symbol(substitute(food_col)))   food_col   <- deparse(substitute(food_col))
  if (is.symbol(substitute(freq_col)))   freq_col   <- deparse(substitute(freq_col))
  if (is.symbol(substitute(qty_col)))    qty_col    <- deparse(substitute(qty_col))
  
  # --- 2. Validation -------------------------------------------------------
  .check_validate_f100_qty_args(df, weight_col, food_col, freq_col, qty_col)
  
  # --- 3. Vector Extraction ------------------------------------------------
  # Pre-extract columns for performance
  weight  <- df[[weight_col]]
  food    <- df[[food_col]]
  freq    <- df[[freq_col]]
  qty     <- df[[qty_col]]
  
  # --- 4. Initialisation ---------------------------------------------------
  results <- rep(NA_character_, nrow(df))
  
  # --- 5. Food Filter ------------------------------------------------------
  # Restrict to non-NA rows where food type is exactly "F100"
  is_f100  <- !is.na(food) & food == "F100"
  idx_f100 <- which(is_f100)
  
  if (length(idx_f100) == 0) return(character(0))
  
  # --- 6. Weight Lookup ----------------------------------------------------
  # Map patient weights to the ceiling row in the lookup table
  w_idx <- .lookup_who_idx(weight[idx_f100],
                           f100_lkp_df[["weight_kg"]])
  
  # Build a [n × 2] limit matrix: max and min recommended daily ml
  limit_matrix <- cbind(
    max = f100_lkp_df[["max_ml_per_day"]][w_idx],
    min = f100_lkp_df[["min_ml_per_day"]][w_idx]
  )
  
  # --- 7. Daily Intake -----------------------------------------------------
  # Prescribed daily intake = frequency × quantity per feed
  qty_intakes <- freq[idx_f100] * qty[idx_f100]
  
  # --- 8. Categorisation ---------------------------------------------------
  # Default to "within", then override at boundaries (NA-safe comparisons)
  res <- rep("within", length(qty_intakes))
  res[qty_intakes > limit_matrix[, "max"] & !is.na(limit_matrix[, "max"])] <- "more"
  res[qty_intakes < limit_matrix[, "min"] & !is.na(limit_matrix[, "min"])] <- "less"
  
  # Rows with out-of-range weights have no valid lookup → NA
  res[is.na(w_idx)] <- NA_character_
  
  # --- 9. Assign Results ---------------------------------------------------
  results[idx_f100] <- res
  
  return(results)
}
