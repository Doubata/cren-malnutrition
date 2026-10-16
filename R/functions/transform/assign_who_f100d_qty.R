
#' @family WHO assignment
#' Assign WHO F100d Recommended Quantity per Feed
#'
#' @description
#' For each F100d row in \code{df}, looks up the WHO-recommended quantity per
#' feed (ml/feed) based on patient weight and breastfeeding status. Breastfed
#' patients and non-breastfed patients are routed to separate lookup tables and
#' phase logic. All other rows are left as \code{NA}.
#'
#' @param df                  A data frame containing patient-level data.
#' @param weight_col          Column name for patient weight in kg.
#' @param food_col            Column name for food type. Only rows where
#'                            \code{food_col == "F100d"} are processed.
#' @param bf_col              Column name for breastfeeding status. Rows with value
#'                            \code{"oui"} or \code{NA} are treated as breastfed;
#'                            all other values as not breastfed.
#' @param ...                 Reserved for future use or passed to internal validators.
#' @param who_f100d_bf_data     WHO F100d lookup table for breastfed patients. Must
#'                            contain columns \code{weight_kg_lower},
#'                            \code{weight_kg_upper}, and \code{ml_per_feed}.
#' @param who_f100d_nbf_data    F100d lookup table for non-breastfed patients. Must
#'                            contain columns \code{weight_kg_lower},
#'                            \code{weight_kg_upper}, \code{ml_severe_phase}, and
#'                            \code{ml_reha_phase}.
#'
#' @return
#' An unnamed numeric vector the same length as \code{nrow(df)}: the
#' WHO-recommended ml/feed for F100d rows, \code{NA} for all others. Returns
#' a full \code{NA} vector if no F100d rows are found.
#'
#' @details
#' Processing pipeline:
#' \enumerate{
#'   \item \strong{Validation} — delegates to \code{.check_assign_who_f100d_args()}.
#'   \item \strong{Food filter} — restricts processing to non-\code{NA} rows
#'         where \code{food_col == "F100d"}.
#'   \item \strong{Breastfeeding split} — partitions F100d rows into two groups:
#'     \itemize{
#'       \item \strong{Breastfed} (\code{bf_col \%in\% c("oui", NA)}) —
#'             weight is matched via \code{.lookup_who_f100d_idx()} against
#'             \code{f100d_bf_df} weight bounds; \code{ml_per_feed} is returned directly.
#'       \item \strong{Not breastfed} (all other values) — weight is matched via
#'             \code{.lookup_who_f100d_idx()} against \code{f100d_nbf_df}
#'             weight bounds, then further split by phase:
#'             \itemize{
#'               \item Indices 1–7 → \strong{acute/severe phase}:
#'                     \code{ml_severe_phase}.
#'               \item Indices > 7 → \strong{rehabilitation phase}:
#'                     \code{ml_reha_phase}.
#'             }
#'     }
#' }
#'
#' @note
#' Unlike the F75 family, there is no oedema split. The key stratification
#' here is breastfeeding status, with an additional acute vs. rehabilitation
#' phase distinction for non-breastfed patients based on their lookup table
#' row index.
#'
#' @examples
#' assign_who_f100d_qty(
#'   df,
#'   weight_col = "weight_kg", food_col = "food_type",
#'   bf_col     = "breastfeeding",
#'   who_f100d_nbf_data  = who_f100d_bf,
#'   who_f100d_nbf_data = who_f100d_nbf
#' )
#' #> [1] NA 45 60 NA ...
#'
#' @family WHO assignment
#'
#' @seealso
#' \code{\link{assign_who_f100d_freq}}, \code{\link{validate_f100_qty}},
#' \code{.check_assign_who_f100d_args()}, \code{.lookup_who_f100d_idx()},
#' \code{.lookup_who_f100d_idx()}
#'
#' @export
assign_who_f100d_qty <- function(df, weight_col, food_col, bf_col, ...,
                                 who_f100d_bf_data, who_f100d_nbf_data) {
  
  # --- 1. Safe Symbol/String Resolution ----------------------------------
  weight_col <- if (is.name(substitute(weight_col))) deparse(substitute(weight_col)) else as.character(weight_col)
  food_col   <- if (is.name(substitute(food_col)))   deparse(substitute(food_col))   else as.character(food_col)
  bf_col     <- if (is.name(substitute(bf_col)))     deparse(substitute(bf_col))     else as.character(bf_col)
  
  # --- 2. Validation -------------------------------------------------------
  .check_assign_who_f100d_args(df, weight_col, food_col, bf_col)
  
  # --- 3. Vector Extraction ------------------------------------------------
  weight <- df[[weight_col]]
  food   <- df[[food_col]]
  bf     <- df[[bf_col]]
  
  # --- 4. Initialisation ---------------------------------------------------
  results <- rep(NA_real_, nrow(df))
  
  # --- 5. Food Filter ------------------------------------------------------
  is_f100d <- !is.na(food) & food == "F100d"
  if (!any(is_f100d)) return(results)
  
  # --- 6. Breastfeeding Split ----------------------------------------------
  # Treats "oui" or NA as breastfed
  is_bf_val <- is.na(bf) | bf == "oui"
  is_bf     <- is_f100d & is_bf_val
  is_not_bf <- is_f100d & !is_bf_val
  
  # --- 7. Breastfed Group --------------------------------------------------
  if (any(is_bf)) {
    weights_bf <- weight[is_bf]
    w_idx <- .lookup_who_f100d_idx(
      weights_bf,
      who_f100d_bf_data[["weight_kg_lower"]],
      who_f100d_bf_data[["weight_kg_upper"]]
    )
    results[is_bf] <- who_f100d_bf_data[["ml_per_feed"]][w_idx]
  }
  
  # --- 8. Non-Breastfed Group ----------------------------------------------
  if (any(is_not_bf)) {
    weights_nbf <- weight[is_not_bf]
    w_idx <- .lookup_who_f100d_idx(
      weights_nbf,
      who_f100d_nbf_data[["weight_kg_lower"]],
      who_f100d_nbf_data[["weight_kg_upper"]]
    )
    
    vals <- rep(NA_real_, length(w_idx))
    valid_idx <- !is.na(w_idx)
    
    is_acute <- valid_idx & w_idx <= 7
    is_reha  <- valid_idx & w_idx > 7
    
    vals[is_acute] <- who_f100d_nbf_data[["ml_severe_phase"]][w_idx[is_acute]]
    vals[is_reha]  <- who_f100d_nbf_data[["ml_reha_phase"]][w_idx[is_reha]]
    
    results[is_not_bf] <- vals
  }
  
  return(unname(results))
}
