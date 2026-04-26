
#' Compute the Proportion of Event Occurrences per ID
#'
#' @description
#' For each requested ID, computes the proportion of \code{event} occurrences
#' across one or more \code{target_cols}, optionally restricted to the
#' \code{run_order}-th consecutive run in \code{phase_col}. Two proportion
#' modes are available: \code{"row_wise"} treats each row as the unit of
#' analysis; \code{"point_wise"} treats each individual cell as the unit.
#' Output order matches the input order of \code{ids}.
#'
#' @param df          A data frame containing patient-level data.
#' @param id_col      Column name (quoted or unquoted) for the grouping ID.
#' @param phase_col   Column name (quoted or unquoted) for the phase series
#'                    used to locate the run window. Only used when
#'                    \code{run_order} is not \code{NULL}.
#' @param event       The value whose proportion is computed. Coerced to
#'                    character before comparison. Pass as a value
#'                    (e.g. \code{"adverse"}), not a column name.
#' @param target_cols Character vector of column names to search for
#'                    \code{event}.
#' @param ids         Vector of length ≥ 1. IDs to look up in \code{id_col}.
#' @param run_order   Optional positive integer. When provided, restricts
#'                    analysis to the \code{run_order}-th consecutive run in
#'                    \code{phase_col}. Defaults to \code{NULL} (no restriction).
#' @param type        One of \code{"row_wise"} or \code{"point_wise"}:
#'   \itemize{
#'     \item \code{"row_wise"}   — proportion of rows where \code{event}
#'           appears in at least one of \code{target_cols}:
#'           \eqn{n_{\text{hit rows}} / n_{\text{valid rows}}}.
#'     \item \code{"point_wise"} — proportion of non-\code{NA} cells
#'           containing \code{event} across all \code{target_cols}:
#'           \eqn{n_{\text{event cells}} / n_{\text{non-NA cells}}}.
#'   }
#'
#' @return
#' An unnamed numeric vector the same length as \code{ids}, with values in
#' \code{[0, 1]}. Returns \code{NA_real_} for IDs not found in \code{df},
#' where \code{run_order} exceeds the available runs, or where the denominator
#' is zero (no valid rows or non-\code{NA} cells).
#'
#' @details
#' Processing pipeline:
#' \enumerate{
#'   \item \strong{Symbol capture} — \code{id_col}, \code{phase_col}, and
#'         \code{event} accept both quoted strings and bare (unquoted) names.
#'         \code{target_cols} is not symbol-captured as it is a character vector.
#'   \item \strong{Validation} — delegates to \code{.check_get_event_prop_args()}.
#'   \item \strong{Pre-split} — \code{df} is split once by \code{id_col}
#'         before iterating, avoiding repeated full-frame subsetting per ID.
#'   \item \strong{Run filter} — when \code{run_order} is provided,
#'         \code{.get_series_idx()} restricts rows to the matching phase window.
#'   \item \strong{Matrix evaluation} — \code{target_cols} are coerced to a
#'         matrix for vectorised \code{== event} comparison.
#'   \item \strong{Proportion} — computed according to \code{type}, with
#'         zero-denominator guard returning \code{NA_real_}.
#' }
#'
#' @note
#' The distinction between \code{"row_wise"} and \code{"point_wise"} matters
#' most when \code{target_cols} has more than one column:
#' \code{"row_wise"} is insensitive to how many columns per row contain the
#' event (a row with 3 hits counts the same as a row with 1 hit), while
#' \code{"point_wise"} reflects cell density and grows with the number of
#' columns containing the event per row.
#'
#' @examples
#' df <- data.frame(
#'   id    = c("A","A","A","A","B","B","B","B"),
#'   phase = c("P1","P1","P2","P2","P1","P1","P2","P2"),
#'   col1  = c("E","X","E","X","E","E","X","X"),
#'   col2  = c("X","X","E","X","X","E","X","E")
#' )
#'
#' # row_wise: proportion of rows with at least one "E"
#' get_event_prop(df, id_col = "id", phase_col = "phase",
#'                event = "E", target_cols = c("col1", "col2"),
#'                ids = c("A", "B"), type = "row_wise")
#' #> [1] 0.75 0.75
#'
#' # point_wise: proportion of cells containing "E"
#' get_event_prop(df, id_col = "id", phase_col = "phase",
#'                event = "E", target_cols = c("col1", "col2"),
#'                ids = c("A", "B"), type = "point_wise")
#' #> [1] 0.375 0.375
#'
#' # Restrict to 1st phase run
#' get_event_prop(df, id_col = "id", phase_col = "phase",
#'                event = "E", target_cols = c("col1", "col2"),
#'                ids = c("A", "B"), run_order = 1, type = "row_wise")
#' #> [1] 0.5 1.0
#'
#' # Unknown ID returns NA
#' get_event_prop(df, id_col = "id", phase_col = "phase",
#'                event = "E", target_cols = c("col1", "col2"),
#'                ids = c("A", "Z"), type = "row_wise")
#' #> [1] 0.75 NA
#'
#' @seealso
#' \code{\link{summarise_event}}, \code{\link{get_prop_flag}},
#' \code{.get_series_idx()}, \code{.check_get_event_prop_args()}
#'
#' @export
get_event_prop <- function(df, id_col, phase_col, event, target_cols, ids,
                           run_order = NULL,
                           type = c("row_wise", "point_wise")) {
  
  # --- 1. Symbol Capture ---------------------------------------------------
  # Applied to column name arguments and event value only.
  # target_cols is a character vector — symbol capture does not apply.
  if (is.symbol(substitute(id_col)))    id_col    <- deparse(substitute(id_col))
  if (is.symbol(substitute(phase_col))) phase_col <- deparse(substitute(phase_col))
  if (is.symbol(substitute(event)))     event     <- deparse(substitute(event))
  
  type <- match.arg(type)
  
  # --- 2. Validation -------------------------------------------------------
  .check_get_event_prop_args(df, id_col, phase_col, event, target_cols,
                             ids, run_order, type)
  
  # --- 3. Pre-split by ID --------------------------------------------------
  # Split once before the loop to avoid repeated full-frame subsetting per ID
  columns  <- unique(c(phase_col, target_cols))
  split_df <- split(df[, columns, drop = FALSE], as.character(df[[id_col]]))
  
  # Coerce event once outside the loop
  event_chr <- as.character(event)
  
  # --- 4. Compute Proportion per ID ----------------------------------------
  results <- vapply(ids, function(id) {
    
    sub_df <- split_df[[as.character(id)]]
    
    # Guard: ID absent from data → NA
    if (is.null(sub_df)) return(NA_real_)
    
    # --- Optional Run Filter -----------------------------------------------
    if (!is.null(run_order)) {
      run_idx <- .get_series_idx(sub_df[[phase_col]], run_order)
      run_idx <- run_idx[!is.na(run_idx)]
      if (length(run_idx) == 0) return(NA_real_)
      sub_df <- sub_df[run_idx, , drop = FALSE]
    }
    
    # --- Matrix Evaluation -------------------------------------------------
    vals <- as.matrix(sub_df[, target_cols, drop = FALSE])
    
    if (type == "row_wise") {
      # Proportion of rows containing at least one event hit
      hits       <- rowSums(vals == event_chr, na.rm = TRUE) > 0
      valid_rows <- rowSums(!is.na(vals)) > 0
      if (sum(valid_rows) == 0) return(NA_real_)
      return(sum(hits[valid_rows]) / sum(valid_rows))
      
    } else {
      # Proportion of non-NA cells containing the event
      denom <- sum(!is.na(vals))
      if (denom == 0) return(NA_real_)
      return(sum(vals == event_chr, na.rm = TRUE) / denom)
    }
    
  }, FUN.VALUE = numeric(1))
  
  return(unname(results))
}
