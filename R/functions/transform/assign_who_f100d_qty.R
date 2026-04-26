
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
#' @param f100d_bf_lkp_df     WHO F100d lookup table for breastfed patients. Must
#'                            contain columns \code{weight_kg_lower},
#'                            \code{weight_kg_upper}, and \code{ml_per_feed}.
#' @param f100d_nbf_lkp_df    F100d lookup table for non-breastfed patients. Must
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
#'   f100d_bf_df  = who_f100d_bf,
#'   f100d_nbf_df = who_f100d_nbf
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
                                 f100d_bf_lkp_df, f100d_nbf_lkp_df) {
  
  # --- 1. Symbol Capture ---------------------------------------------------
  # Allows callers to pass bare column names (unquoted) or quoted strings.
  if (is.symbol(substitute(weight_col)))   weight_col   <- deparse(substitute(weight_col))
  if (is.symbol(substitute(food_col)))     food_col     <- deparse(substitute(food_col))
  if (is.symbol(substitute(bf_col)))       bf_col       <- deparse(substitute(bf_col))
  
  # --- 2. Validation -------------------------------------------------------
  .check_assign_who_f100d_args(df, weight_col, food_col, bf_col)
  
  # --- 3. Vector Extraction ------------------------------------------------
  # Pre-extract columns for performance
  weight  <- df[[weight_col]]
  food    <- df[[food_col]]
  bf      <- df[[bf_col]]

  # --- 4. Initialisation ---------------------------------------------------
  results <- rep(NA, nrow(df))
  
  # --- 5. Food Filter ------------------------------------------------------
  # Restrict to non-NA rows where food type is exactly "F100d"
  is_f100d <- !is.na(food) & food == "F100d"
  if (!any(is_f100d)) return(results)
  
  # --- 6. Breastfeeding Split ----------------------------------------------
  # "oui" or NA in bf_col → treated as breastfed (conservative assumption)
  is_bf     <- is_f100d &  (df[[bf_col]] %in% c("oui", NA))
  is_not_bf <- is_f100d & !(df[[bf_col]] %in% c("oui", NA))
  
  # --- 7. Breastfed Group --------------------------------------------------
  # Single lookup: weight band → ml_per_feed (no phase distinction)
  if (any(is_bf)) {
    weights_bf <- weight[is_bf]
    
    l_idx <- .lookup_who_f100d_idx(weights_bf,
                                   f100d_bf_lkp_df[["weight_kg_lower"]],
                                   f100d_bf_lkp_df[["weight_kg_upper"]])
    
    results[is_bf] <- f100d_bf_lkp_df[["ml_per_feed"]][l_idx]
  }
  
  # --- 8. Non-Breastfed Group ----------------------------------------------
  # Two-phase lookup: indices 1-7 → acute/severe phase; > 7 → rehabilitation
  if (any(is_not_bf)) {
    weights_nbf <- weight[is_not_bf]
    l_idx <- .lookup_who_f100d_idx(weights_nbf,
                                       f100d_nbf_lkp_df[["weight_kg_lower"]],
                                       f100d_nbf_lkp_df[["weight_kg_upper"]])
    
    is_acute <- !is.na(l_idx) & l_idx <= 7   # rows 1-7 = acute/severe phase
    
    vals <- rep(NA_real_, length(l_idx))
    vals[ is_acute] <- f100d_nbf_lkp_df[["ml_severe_phase"]][l_idx[is_acute]]
    vals[!is_acute] <- f100d_nbf_lkp_df[["ml_reha_phase"]][l_idx[!is_acute]]
    
    results[is_not_bf] <- vals
  }
  
  return(unname(results))
}
