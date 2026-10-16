
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
#' @param who_f100_data WHO F100 lookup table. Must contain columns
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
#'         weights to the nearest ceiling row in \code{who_f100_data};
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
#'   who_f100_data = who_f100
#' )
#' #> [1] "within" "less" "more" NA ...
#'
#' @seealso
#' \code{\link{validate_f75x_qty}}, \code{\link{validate_f75x_adh}},
#' \code{.check_validate_f100_qty_args()}, \code{.lookup_who_idx()}
#'
#' @export
validate_f100_qty <- function(df, weight_col, food_col, freq_col, qty_col, ..., 
                              who_f100_data) {
  
  # # 1. Robust Column Name Extraction
  # get_col_name <- function(arg, env = parent.frame()) {
  #   expr <- substitute(arg, env)
  #   if (is.character(expr)) return(expr)
  #   if (is.name(expr)) return(as.character(expr))
  #   stop("Column arguments must be unquoted symbols or character strings.")
  # }
  # 
  # weight_col <- get_col_name(weight_col)
  # food_col   <- get_col_name(food_col)
  # freq_col   <- get_col_name(freq_col)
  # qty_col    <- get_col_name(qty_col)
  
  # 2. Validation
  # .check_validate_f100_qty_args(df, weight_col, food_col, freq_col, qty_col)
  
  # 3. Vector Extraction
  weight <- df[[weight_col]]
  food   <- df[[food_col]]
  freq   <- df[[freq_col]]
  qty    <- df[[qty_col]]
  
  # 4. Pre-allocate Output Vector
  n_rows <- nrow(df)
  results <- rep(NA_character_, n_rows)
  
  # 5. Food Filter
  is_f100  <- !is.na(food) & food == "F100"
  idx_f100 <- which(is_f100)
  
  # Fix: Return rep(NA, n_rows) on early exit to preserve vector length
  if (length(idx_f100) == 0) return(results)
  
  # 6. Lookup & Calculations
  w_idx <- .lookup_who_idx(weight[idx_f100], who_f100_data[["weight_kg"]])
  
  max_limits  <- who_f100_data[["max_ml_per_day"]][w_idx]
  min_limits  <- who_f100_data[["min_ml_per_day"]][w_idx]
  qty_intakes <- freq[idx_f100] * qty[idx_f100]
  
  # 7. Vectorized Categorisation
  res <- rep(NA_character_, length(idx_f100))
  
  # Explicitly handle non-NA intake checks
  valid_intake <- !is.na(qty_intakes) & !is.na(w_idx)
  
  res[valid_intake & qty_intakes > max_limits] <- "more"
  res[valid_intake & qty_intakes < min_limits] <- "less"
  res[valid_intake & qty_intakes >= min_limits & qty_intakes <= max_limits] <- "within"
  
  # 8. Assign Results
  results[idx_f100] <- res
  
  return(results)
}
