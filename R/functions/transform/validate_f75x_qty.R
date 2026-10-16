
#' Validate F75 Prescribed Quantity Against WHO Recommendations
#'
#' @description
#' For each row in \code{data}, computes the patient's prescribed daily intake
#' as \code{freq_col × qty_col} and compares it against the WHO F75 recommended
#' daily range derived from the patient's weight. Returns a categorical label
#' per row: \code{"within"}, \code{"more"}, or \code{"less"}. Patients with
#' severe oedema (grade ≥ 3) are routed to \code{who_f75_oed3_data}; all
#' others use \code{who_f75_data}.
#'
#' @param data                 A data frame containing patient-level data.
#' @param weight_col         Column name (quoted or unquoted) for patient weight in kg.
#' @param food_col           Column name (quoted or unquoted) for the food type given.
#' @param oedema_col         Column name (quoted or unquoted) for oedema grade
#'                           (numeric; grade ≥ 3 = severe).
#' @param freq_col           Column name (quoted or unquoted) for the prescribed
#'                           feeding frequency (feeds/day).
#' @param qty_col            Column name (quoted or unquoted) for the prescribed
#'                           quantity per feed (ml/feed).
#' @param ...                Reserved for future use or passed to internal validators.
#' @param type_food          Character vector of food type codes to process.
#'                           Defaults to \code{c("F75", "F75F")}. Rows whose
#'                           \code{food_col} is not in \code{type_food} are
#'                           left as \code{NA}.
#' @param who_f75_data      WHO F75 lookup table for non-severe oedema patients.
#'                           Must contain columns \code{who_weight_col},
#'                           \code{ml_per_day}, and \code{min_ml_per_day}.
#' @param who_f75_oed3_data WHO F75 lookup table for severe oedema (grade ≥ 3)
#'                           patients. Same column structure as \code{who_f75_data}.
#'
#' @return
#' An unnamed character vector the same length as \code{nrow(data)}, with values:
#' \itemize{
#'   \item \code{"within"} — prescribed daily intake is within the WHO range.
#'   \item \code{"more"}   — prescribed daily intake exceeds the WHO maximum.
#'   \item \code{"less"}   — prescribed daily intake falls below the WHO minimum.
#'   \item \code{NA}       — \code{food_col} not in \code{type_food}, or weight
#'                           outside the lookup table range.
#' }
#'
#' @details
#' Processing pipeline:
#' \enumerate{
#'   \item \strong{Symbol capture} — column name arguments accept both quoted
#'         strings and bare (unquoted) names via \code{\link{substitute}()}.
#'   \item \strong{Validation} — delegates to
#'         \code{.check_args_validate_who_f75x_qty()}.
#'   \item \strong{Food filter} — restricts processing to rows where
#'         \code{food_col \%in\% type_food}.
#'   \item \strong{Oedema split} — partitions filtered rows into severe oedema
#'         (grade ≥ 3) and moderate/no oedema groups.
#'   \item \strong{Daily intake} — computed as \code{freq_col × qty_col},
#'         representing total prescribed ml per day.
#'   \item \strong{Weight lookup} — \code{.lookup_who_idx()} maps patient
#'         weights to the nearest ceiling row in the lookup table; out-of-range
#'         weights produce \code{NA} in the output.
#'   \item \strong{Categorisation} — defaults to \code{"within"}, then overrides
#'         with \code{"more"} or \code{"less"} using vectorised logical indexing,
#'         ignoring \code{NA} entries in the limit matrix.
#' }
#'
#' @note
#' Unlike \code{\link{validate_f75x_adh}}, which aggregates actual meal-level
#' intake records, this function validates the \emph{prescribed} regimen only
#' (\code{freq × qty}). No meal columns are required.
#'
#' @examples
#' validate_f75x_qty(
#'   data,
#'   weight_col = "weight_kg", food_col   = "food_type",
#'   oedema_col = "oedema_grade", freq_col = "freq_prescribed",
#'   qty_col    = "qty_prescribed",
#'   who_f75_data      = who_f75,
#'   who_f75_oed3_data = who_f75_oed3
#' )
#' #> [1] "within" "less" "more" NA ...
#'
#' @seealso
#' \code{\link{validate_f75x_adh}}, \code{\link{assign_who_f75x}},
#' \code{.check_validate_f75x_qty_args()}, \code{.lookup_who_idx()}
#'
#' @export
validate_f75x_qty <- function(data, weight_col, food_col, oedema_col,
                              freq_col, qty_col, ...,
                              type_food = c("F75", "F75F"),
                              who_f75_data, who_f75_oed3_data) {
  
  # --- 1. Symbol Capture ---------------------------------------------------
  # Allows callers to pass bare column names (unquoted) or quoted strings.
  if (is.symbol(substitute(weight_col))) weight_col <- deparse(substitute(weight_col))
  if (is.symbol(substitute(food_col)))   food_col   <- deparse(substitute(food_col))
  if (is.symbol(substitute(oedema_col))) oedema_col <- deparse(substitute(oedema_col))
  if (is.symbol(substitute(freq_col)))   freq_col   <- deparse(substitute(freq_col))
  if (is.symbol(substitute(qty_col)))    qty_col    <- deparse(substitute(qty_col))
  
  # --- 2. Validation -------------------------------------------------------
  .check_validate_f75x_qty_args(data, weight_col, food_col, oedema_col,
                                freq_col, qty_col, type_food)
  
  # --- 3. Vector Extraction ------------------------------------------------
  # Pre-extract columns for performance
  weight  <- data[[weight_col]]
  food    <- data[[food_col]]
  oedema  <- data[[oedema_col]]
  freq    <- data[[freq_col]]
  qty     <- data[[qty_col]]
  
  # --- 4. Initialisation ---------------------------------------------------
  results <- rep(NA_character_, nrow(data))
  
  # --- 5. Food Filter ------------------------------------------------------
  # Only process rows with a recognised F75 food type
  valid_food <- food %in% type_food
  if (length(valid_food) == 0) return(results)
  
  # --- 6. Oedema Split -----------------------------------------------------
  # Severe oedema: grade >= 3 → routed to the dedicated lookup table
  severe <- !is.na(oedema) & oedema >= 3
  severe_oedema     <- valid_food & severe
  moderate_oedema   <- valid_food & !severe
  
  # --- 7. Internal Categorisation Helper -----------------------------------
  process_group <- function(mask, lkp_data) {
    if (length(mask) == 0) return(character(0))
    
    idx <- which(mask)
    
    # Map patient weights to the ceiling row in the lookup table
    w_idx <- .lookup_who_idx(weight[idx], lkp_data[["weight_kg"]])
    
    # Build a [n × 2] limit matrix: max and min recommended daily ml
    limit_matrix <- cbind(
      max = lkp_data[["ml_per_day"]][w_idx],
      min = lkp_data[["min_ml_per_day"]][w_idx]
    )
    
    # Prescribed daily intake = frequency × quantity per feed
    input_vals <- freq[idx] * qty[idx]
    
    # Default to "within", then override at boundaries (NA-safe comparisons)
    res <- rep("within", length(input_vals))
    res[input_vals > limit_matrix[, "max"] & !is.na(limit_matrix[, "max"])] <- "more"
    res[input_vals < limit_matrix[, "min"] & !is.na(limit_matrix[, "min"])] <- "less"
    
    # Rows with out-of-range weights have no valid lookup → NA
    res[is.na(w_idx)] <- NA_character_
    
    return(res)
  }
  
  # --- 8. Assign Results ---------------------------------------------------
  if (length(moderate_oedema) > 0)
    results[moderate_oedema] <- process_group(moderate_oedema, who_f75_data)
  if (length(severe_oedema) > 0)
    results[severe_oedema]   <- process_group(severe_oedema,   who_f75_oed3_data)
  
  return(results)
}

