#' Count, Locate or Summarise Event Occurrences per ID
#'
#' @description
#' A unified event analysis function that, for each requested ID, searches
#' one or more \code{target_cols} for occurrences of \code{event} —
#' optionally restricted to the \code{run_order}-th consecutive run in
#' \code{phase_col} — and returns a summary statistic controlled by
#' \code{type}. Supersedes \code{\link{get_prop_flag}},
#' \code{\link{get_event_prop}}, and \code{\link{get_event_length}}, which
#' are now deprecated special cases of this function. Output order matches
#' the input order of \code{ids}.
#'
#' @param df          A data frame containing patient-level data.
#' @param id_col      Column name (quoted or unquoted) for the grouping ID.
#' @param phase_col   Column name (quoted or unquoted) for the phase series
#'                    used to locate the run window. Only used when
#'                    \code{run_order} is not \code{NULL}.
#' @param event       The value to search for. Coerced to character before
#'                    comparison. Pass as a value (e.g. \code{"adverse"}),
#'                    not a column name.
#' @param target_cols Character vector of column names to search for
#'                    \code{event}.
#' @param ids         Vector of length ≥ 1. IDs to look up in \code{id_col}.
#' @param run_order   Optional positive integer. When provided, restricts
#'                    analysis to the \code{run_order}-th consecutive run in
#'                    \code{phase_col}. Defaults to \code{NULL} (no restriction).
#' @param type        One of \code{"any"}, \code{"global"}, \code{"proportion"},
#'                    \code{"density"}, \code{"consecutive"}, \code{"first"},
#'                    or \code{"last"}:
#'   \itemize{
#'     \item \code{"any"}         — number of rows where \code{event} appears
#'           in at least one of \code{target_cols}.
#'     \item \code{"global"}      — total cell-level occurrences of \code{event}
#'           across all \code{target_cols}.
#'     \item \code{"proportion"}  — fraction of valid rows where \code{event}
#'           appears in at least one column:
#'           \eqn{n_{\text{hit rows}} / n_{\text{valid rows}}}.
#'           Supersedes \code{get_prop_flag()} and
#'           \code{get_event_prop(type = "row_wise")}.
#'     \item \code{"density"}     — fraction of non-\code{NA} cells containing
#'           \code{event} across all \code{target_cols}:
#'           \eqn{n_{\text{event cells}} / n_{\text{non-NA cells}}}.
#'           Supersedes \code{get_event_prop(type = "point_wise")}.
#'     \item \code{"consecutive"} — length of the longest consecutive streak
#'           of rows where \code{event} appears in at least one column.
#'     \item \code{"first"}       — row index (within the window) of the first
#'           occurrence of \code{event}.
#'     \item \code{"last"}        — row index (within the window) of the last
#'           occurrence of \code{event}.
#'   }
#'
#' @return
#' An unnamed numeric vector the same length as \code{ids}. The unit depends
#' on \code{type}: a count, a proportion in \code{[0, 1]}, or a row index.
#' Returns \code{NA_real_} for IDs not found in \code{df}, where
#' \code{run_order} exceeds the available runs, or where \code{event} is
#' never found (\code{"first"} / \code{"last"} only).
#'
#' @details
#' Processing pipeline:
#' \enumerate{
#'   \item \strong{Symbol capture} — \code{id_col}, \code{phase_col}, and
#'         \code{event} accept both quoted strings and bare (unquoted) names.
#'   \item \strong{Validation} — delegates to \code{.check_summarise_event_args()}.
#'   \item \strong{Pre-split} — \code{df} is split once by \code{id_col}
#'         before iterating, avoiding repeated full-frame subsetting per ID.
#'   \item \strong{Run filter} — when \code{run_order} is provided,
#'         \code{.get_series_idx()} restricts rows to the matching phase window.
#'   \item \strong{Matrix evaluation} — \code{target_cols} are coerced to a
#'         matrix; a logical hit vector (\code{rowSums > 0}) is derived for
#'         row-level modes.
#'   \item \strong{Summarisation} — the selected \code{type} is applied via
#'         \code{\link{switch}()}.
#' }
#'
#' @note
#' The distinction between \code{"proportion"} and \code{"density"} matters
#' most when \code{target_cols} has more than one column:
#' \code{"proportion"} is insensitive to how many columns per row contain
#' the event, while \code{"density"} reflects cell-level saturation and
#' grows with the number of matching cells per row.
#'
#' @examples
#' df <- data.frame(
#'   id    = c("A","A","A","A","A","B","B","B","B"),
#'   phase = c("P1","P1","P2","P2","P2","P1","P1","P2","P2"),
#'   col1  = c("X","E","E","X","E","E","X","X","E"),
#'   col2  = c("X","X","E","X","X","X","E","X","X")
#' )
#'
#' # "any": rows with at least one "E"
#' summarise_event(df, id_col = "id", phase_col = "phase",
#'                 event = "E", target_cols = c("col1","col2"),
#'                 ids = c("A","B"), type = "any")
#' #> [1] 3 3
#'
#' # "global": total cell occurrences of "E"
#' summarise_event(df, id_col = "id", phase_col = "phase",
#'                 event = "E", target_cols = c("col1","col2"),
#'                 ids = c("A","B"), type = "global")
#' #> [1] 4 3
#'
#' # "proportion": fraction of rows containing "E"
#' summarise_event(df, id_col = "id", phase_col = "phase",
#'                 event = "E", target_cols = c("col1","col2"),
#'                 ids = c("A","B"), type = "proportion")
#' #> [1] 0.600 0.750
#'
#' # "density": fraction of cells containing "E"
#' summarise_event(df, id_col = "id", phase_col = "phase",
#'                 event = "E", target_cols = c("col1","col2"),
#'                 ids = c("A","B"), type = "density")
#' #> [1] 0.400 0.375
#'
#' # "consecutive": longest consecutive streak of rows with "E"
#' summarise_event(df, id_col = "id", phase_col = "phase",
#'                 event = "E", target_cols = c("col1","col2"),
#'                 ids = c("A","B"), type = "consecutive")
#' #> [1] 2 1
#'
#' # "first": row index of first "E" occurrence
#' summarise_event(df, id_col = "id", phase_col = "phase",
#'                 event = "E", target_cols = c("col1","col2"),
#'                 ids = c("A","B"), type = "first")
#' #> [1] 2 1
#'
#' # "last": row index of last "E" occurrence
#' summarise_event(df, id_col = "id", phase_col = "phase",
#'                 event = "E", target_cols = c("col1","col2"),
#'                 ids = c("A","B"), type = "last")
#' #> [1] 5 4
#' 
#' @family event summarisation
#'
#' @seealso
#' \code{\link{get_prop_flag}} (deprecated),
#' \code{\link{get_event_prop}} (deprecated),
#' \code{\link{get_event_length}} (deprecated),
#' \code{.get_series_idx()}, \code{.check_summarise_event_args()}
#'
#' @export
summarise_event <- function(df, id_col, phase_col, event, target_cols, ids,
                            run_order = NULL,
                            type = c("any", "global", "proportion", "density",
                                     "consecutive", "first", "last")) {
  
  # --- 1. Symbol Capture ---------------------------------------------------
  if (is.symbol(substitute(id_col)))    id_col    <- deparse(substitute(id_col))
  if (is.symbol(substitute(phase_col))) phase_col <- deparse(substitute(phase_col))
  if (is.symbol(substitute(event)))     event     <- deparse(substitute(event))
  
  type <- match.arg(type)
  
  # --- 2. Validation -------------------------------------------------------
  .check_summarise_event_args(df, id_col, phase_col, event, target_cols,
                              ids, run_order, type)
  
  # --- 3. Pre-split by ID --------------------------------------------------
  columns  <- unique(c(phase_col, target_cols))
  split_df <- split(df[, columns, drop = FALSE], as.character(df[[id_col]]))
  
  event_chr <- as.character(event)
  
  # --- 4. Summarise per ID -------------------------------------------------
  results <- vapply(ids, function(id) {
    
    sub_df <- split_df[[as.character(id)]]
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
    hits <- rowSums(vals == event_chr, na.rm = TRUE) > 0
    
    # --- Type Dispatch -----------------------------------------------------
    switch(type,
           
           "any" = {
             sum(hits)
           },
           
           "global" = {
             sum(vals == event_chr, na.rm = TRUE)
           },
           
           "proportion" = {
             # Supersedes get_prop_flag() and get_event_prop(type = "row_wise")
             valid_rows <- rowSums(!is.na(vals)) > 0
             if (sum(valid_rows) == 0) return(NA_real_)
             sum(hits[valid_rows]) / sum(valid_rows)
           },
           
           "density" = {
             # Supersedes get_event_prop(type = "point_wise")
             denom <- sum(!is.na(vals))
             if (denom == 0) return(NA_real_)
             sum(vals == event_chr, na.rm = TRUE) / denom
           },
           
           "consecutive" = {
             if (!any(hits)) return(0)
             runs <- rle(hits)
             max(runs$lengths[runs$values])
           },
           
           "first" = {
             idx <- which(hits)[1]
             if (length(idx) == 0 || is.na(idx)) return(NA_real_)
             as.numeric(idx)
           },
           
           "last" = {
             all_hits <- which(hits)
             if (length(all_hits) == 0) return(NA_real_)
             as.numeric(tail(all_hits, 1))
           }
    )
    
  }, FUN.VALUE = numeric(1))
  
  return(unname(results))
}
