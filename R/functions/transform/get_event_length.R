
#' Count Event Occurrences per ID Within an Optional Phase Run
#'
#' @description
#' For each requested ID, counts how many times \code{event} appears across
#' one or more \code{target_cols}, optionally restricted to the
#' \code{run_order}-th consecutive run in \code{phase_col}. Two counting
#' modes are available: \code{"any"} counts rows where the event appears in
#' at least one column; \code{"global"} counts total cell-level occurrences
#' across all columns. Output order matches the input order of \code{ids}.
#'
#' @param df          A data frame containing patient-level data.
#' @param id_col      Column name (quoted or unquoted) for the grouping ID.
#' @param phase_col   Column name (quoted or unquoted) for the phase series
#'                    used to locate the run window. Only used when
#'                    \code{run_order} is not \code{NULL}.
#' @param event       The value to count. Coerced to character before
#'                    comparison. Should be passed as a value
#'                    (e.g. \code{"adverse"}), not a column name.
#' @param target_cols Character vector of column names to search for
#'                    \code{event}.
#' @param ids         Vector of length ≥ 1. IDs to look up in \code{id_col}.
#' @param run_order   Optional positive integer. When provided, restricts
#'                    counting to the \code{run_order}-th consecutive run in
#'                    \code{phase_col}. Defaults to \code{NULL} (no restriction).
#' @param type        One of \code{"any"} or \code{"global"}:
#'   \itemize{
#'     \item \code{"any"}    — counts rows where \code{event} appears in at
#'           least one of \code{target_cols}.
#'     \item \code{"global"} — counts total cell-level occurrences of
#'           \code{event} across all \code{target_cols}.
#'   }
#'
#' @return
#' An unnamed numeric vector the same length as \code{ids}. Each element is
#' the event count for that ID. Returns \code{NA_real_} for IDs not found in
#' \code{df} or where \code{run_order} exceeds the available runs in
#' \code{phase_col}.
#'
#' @details
#' Processing pipeline:
#' \enumerate{
#'   \item \strong{Symbol capture} — \code{id_col}, \code{phase_col}, and
#'         \code{event} accept both quoted strings and bare (unquoted) names.
#'   \item \strong{Validation} — delegates to \code{.check_get_event_flag_args()}.
#'   \item \strong{Pre-split} — \code{df} is split once by \code{id_col}
#'         before iterating, avoiding repeated full-frame subsetting per ID.
#'   \item \strong{Run filter} — when \code{run_order} is provided,
#'         \code{.get_series_idx()} restricts rows to the matching phase window.
#'   \item \strong{Matrix evaluation} — \code{target_cols} are coerced to a
#'         matrix for vectorised \code{== event} comparison.
#'   \item \strong{Counting} — \code{"any"} uses \code{rowSums > 0};
#'         \code{"global"} uses \code{sum()} over all cells.
#' }
#'
#' @examples
#' df <- data.frame(
#'   id    = c("A","A","A","B","B","B"),
#'   phase = c("P1","P1","P2","P1","P1","P2"),
#'   col1  = c("X","E","E","E","X","X"),
#'   col2  = c("X","X","E","X","E","X")
#' )
#'
#' # "any" mode: rows where "E" appears in at least one column
#' get_event_length(df, id_col = "id", phase_col = "phase",
#'                  event = "E", target_cols = c("col1", "col2"),
#'                  ids = c("A", "B"), type = "any")
#' #> [1] 2 2
#'
#' # "global" mode: total cell-level occurrences of "E"
#' get_event_length(df, id_col = "id", phase_col = "phase",
#'                  event = "E", target_cols = c("col1", "col2"),
#'                  ids = c("A", "B"), type = "global")
#' #> [1] 3 2
#'
#' # Restrict to 1st phase run only
#' get_event_length(df, id_col = "id", phase_col = "phase",
#'                  event = "E", target_cols = c("col1", "col2"),
#'                  ids = c("A", "B"), run_order = 1, type = "any")
#' #> [1] 1 1
#'
#' # Unknown ID returns NA
#' get_event_length(df, id_col = "id", phase_col = "phase",
#'                  event = "E", target_cols = c("col1", "col2"),
#'                  ids = c("A", "Z"), type = "any")
#' #> [1] 2 NA
#' 
#' @family Series extraction
#'
#' @seealso
#' \code{\link{get_prop_flag}}, \code{.get_series_idx()},
#' \code{.check_get_event_flag_args()}
#'
#' @export
get_event_length <- function(df, id_col, phase_col, event, target_cols, ids,
                             run_order = NULL, type = c("any", "global")) {
  
  # --- 1. Symbol Capture ---------------------------------------------------
  # Applied to column name arguments and event value — not target_cols
  # (it is already a character vector).
  if (is.symbol(substitute(id_col)))    id_col    <- deparse(substitute(id_col))
  if (is.symbol(substitute(phase_col))) phase_col <- deparse(substitute(phase_col))
  if (is.symbol(substitute(event)))     event     <- deparse(substitute(event))  # fixed: was deparsing undefined 'target_col'
  
  # --- 2. Validation -------------------------------------------------------
  .check_get_event_flag_args(df, id_col, phase_col, event, target_cols,
                             ids, run_order, type)
  
  # --- 3. Pre-split by ID --------------------------------------------------
  # Split once before the loop to avoid repeated full-frame subsetting per ID
  columns <- unique(c(phase_col, target_cols))
  split_df <- split(df[, columns, drop = FALSE], as.character(df[[id_col]]))  # fixed: added drop = FALSE
  
  # --- 4. Count Event per ID -----------------------------------------------
  # vapply() guarantees a numeric(1) result per iteration
  results <- vapply(ids, function(id) {
    
    sub_df <- split_df[[as.character(id)]]
    
    # Guard: ID absent from data → NA
    if (is.null(sub_df)) return(NA_real_)
    
    # --- Optional Run Filter -----------------------------------------------
    # Restrict rows to the run_order-th consecutive run in phase_col
    if (!is.null(run_order)) {
      run_idx <- .get_series_idx(sub_df[[phase_col]], run_order)
      run_idx <- run_idx[!is.na(run_idx)]
      
      # Guard: run_order exceeds available runs → NA
      if (length(run_idx) == 0) return(NA_real_)
      
      sub_df <- sub_df[run_idx, , drop = FALSE]
    }
    
    # --- Matrix Evaluation -------------------------------------------------
    # Coerce target columns to matrix for vectorised == comparison
    vals <- as.matrix(sub_df[, target_cols, drop = FALSE])
    
    if (type == "any") {
      # Count rows where event appears in at least one target column
      hits <- rowSums(vals == event, na.rm = TRUE) > 0
      return(sum(hits))
    } else {
      # Count total cell-level occurrences across all target columns
      return(sum(vals == event, na.rm = TRUE))
    }
    
  }, FUN.VALUE = numeric(1))
  
  return(unname(results))
}
