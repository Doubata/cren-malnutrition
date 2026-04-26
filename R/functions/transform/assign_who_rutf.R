
#' @family WHO assignment
#' Assign WHO Recommended RUTF Sachets per Day
#'
#' @description
#' For each row in \code{df} where a positive RUTF quantity is recorded,
#' looks up the WHO-recommended number of RUTF sachets per day based on
#' patient weight. Rows with missing or zero RUTF values are left as \code{NA}.
#'
#' @param df             A data frame containing patient-level data.
#' @param weight_col     Column name (quoted or unquoted) for patient weight in kg.
#' @param rutf_col       Column name (quoted or unquoted) for the recorded RUTF
#'                       quantity. Rows where this column is \code{NA} or \code{<= 0}
#'                       are skipped.
#' @param ...            Reserved for future use or passed to internal validators.
#' @param rutf_lkp_df    WHO RUTF lookup table. Must contain columns
#'                       \code{weight_kg} and \code{sachets_per_day}.
#'
#' @return
#' An unnamed numeric vector the same length as \code{nrow(df)}: the
#' WHO-recommended sachets/day for valid RUTF rows, \code{NA} for all others.
#' Returns a full \code{NA_real_} vector if no valid RUTF rows are found.
#'
#' @details
#' Processing pipeline:
#' \enumerate{
#'   \item \strong{Symbol capture} — column name arguments accept both quoted
#'         strings and bare (unquoted) names via \code{\link{substitute}()}.
#'   \item \strong{Validation} — delegates to \code{.check_assign_who_rutf_args()}.
#'   \item \strong{RUTF filter} — restricts processing to non-\code{NA} rows
#'         where \code{rutf_col > 0}.
#'   \item \strong{Weight lookup} — \code{.lookup_who_idx()} maps patient
#'         weights to the nearest ceiling row in \code{rutf_lkp_df};
#'         unmatched weights (\code{l_idx = NA}) are left as \code{NA} in
#'         the output.
#'   \item \strong{Assignment} — \code{sachets_per_day} is extracted for rows
#'         with a valid lookup match and written back to their original
#'         positions in \code{results}.
#' }
#'
#' @note
#' Unlike the F75/F100 family, there is no oedema split, breastfeeding split,
#' or food type filter — RUTF assignment depends solely on patient weight and
#' a positive RUTF intake record.
#'
#' @examples
#' assign_who_rutf(
#'   df,
#'   weight_col     = "weight_kg",
#'   rutf_col       = "rutf_qty",
#'   rutf_lkp_df    = who_rutf
#' )
#' #> [1] 2 NA 3 NA ...
#'
#' @family WHO assignment
#'
#' @seealso
#' \code{\link{assign_who_f100d_qty}}, \code{\link{assign_who_f75x}},
#' \code{.check_assign_who_rutf_args()}, \code{.lookup_who_idx()}
#'
#' @export
assign_who_rutf <- function(df, weight_col, rutf_col, ...,
                            rutf_lkp_df) {
  
  # --- 1. Symbol Capture ---------------------------------------------------
  # Allows callers to pass bare column names (unquoted) or quoted strings.
  if (is.symbol(substitute(weight_col))) weight_col <- deparse(substitute(weight_col))
  if (is.symbol(substitute(rutf_col)))   rutf_col   <- deparse(substitute(rutf_col))
  
  # --- 2. Validation -------------------------------------------------------
  .check_assign_who_rutf_args(df, weight_col, rutf_col)
  
  # --- 3. Vector Extraction ------------------------------------------------
  # Pre-extract columns for performance
  weight  <- df[[weight_col]]
  rutf    <- df[[rutf_col]]
  
  # --- 4. Initialisation ---------------------------------------------------
  results <- rep(NA_real_, nrow(df))
  
  # --- 5. RUTF Filter ------------------------------------------------------
  # Only process rows with a non-missing, positive RUTF quantity
  is_plumpy <- !is.na(rutf) & (rutf > 0)
  if (!any(is_plumpy)) return(results)
  
  # --- 6. Weight Lookup ----------------------------------------------------
  weights_plumpy <- weight[is_plumpy]
  l_idx          <- .lookup_who_idx(weights_plumpy, rutf_lkp_df[["weight_kg"]])
  
  # --- 7. Assignment -------------------------------------------------------
  # Only write sachets_per_day where a lookup match was found
  vals            <- rep(NA_real_, length(l_idx))
  has_match       <- !is.na(l_idx)
  vals[has_match] <- rutf_lkp_df[["sachets_per_day"]][has_match]
  
  results[is_plumpy] <- vals
  
  return(unname(results))
}
