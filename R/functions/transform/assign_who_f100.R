
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
#' @param who_f100_data       WHO F100 lookup table. Must contain columns 
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
#'          to the nearest ceiling row in the \code{who_f100_data}.
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
#'                  who_f100_data = who_f100_table)
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
                            who_f100_data) {
  
  # # 1. Match default choices robustly
  # measure_type <- match.arg(measure_type)
  # 
  # # 2. Robust Column Name Extraction (Handles symbols, strings, and NSE)
  # get_col_name <- function(arg, env = parent.frame()) {
  #   expr <- substitute(arg, env)
  #   if (is.character(expr)) return(expr)
  #   if (is.name(expr)) return(as.character(expr))
  #   stop("Column arguments must be unquoted symbols or character strings.")
  # }
  # 
  # weight_col <- get_col_name(weight_col)
  # food_col   <- get_col_name(food_col)
  # target_col <- get_col_name(target_col)
  
  # 3. Validation Gatekeeper
  # .check_assign_who_f100_args(df, weight_col, food_col, target_col, measure_type)
  
  # 4. Extract Vectors
  weight <- df[[weight_col]]
  food   <- df[[food_col]]
  target <- df[[target_col]]
  
  # 5. Filter for F100
  is_f100 <- !is.na(food) & food == "F100"
  
  # Early exit if no matching rows
  if (!any(is_f100)) {
    return(rep(NA_real_, nrow(df)))
  }
  
  idx_f100 <- which(is_f100)
  
  # 6. Lookup Indices
  w_idx <- .lookup_who_idx(weight[idx_f100], who_f100_data[["weight_kg"]])
  
  # 7. Assignment Logic
  results <- rep(NA_real_, nrow(df))
  
  if (measure_type == "frequency") {
    results[idx_f100] <- 6
  } else {
    current_mins <- who_f100_data[["min_ml_per_day"]][w_idx]
    current_maxs <- who_f100_data[["max_ml_per_day"]][w_idx]
    target_vals  <- target[idx_f100]
    
    # Logic: If target <= min -> return min, otherwise return max
    results[idx_f100] <- ifelse(target_vals > current_mins, current_maxs, current_mins)
  }
  
  return(unname(results))
}