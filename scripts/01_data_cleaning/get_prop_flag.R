
#' Compute the Proportion of a Flag Value Within a Specific Phase Run per ID
#'
#' @description
#' For each requested ID, locates the \code{run_order}-th consecutive run in
#' \code{phase_col}, extracts the corresponding values from \code{target_col},
#' and returns the proportion of those values that equal \code{flag}.
#' Output order matches the input order of \code{ids}.
#'
#' @param df         A data frame containing patient-level data.
#' @param id_col     Column name (quoted or unquoted) for the grouping ID.
#' @param phase_col  Column name (quoted or unquoted) for the phase series used
#'                   to locate the run window via run-length encoding.
#' @param target_col Column name (quoted or unquoted) for the values to evaluate
#'                   against \code{flag}.
#' @param flag       The value to count within the run window. Coerced to
#'                   character before comparison. Should be passed as a value
#'                   (e.g. \code{"adhered"}), not a column name.
#' @param run_order  Positive integer. Which run in \code{phase_col} to evaluate
#'                   (1 = first run, 2 = second run, etc.).
#' @param ids        Vector of length ≥ 1. IDs to look up in \code{id_col}.
#' @param ...        Reserved for future use or passed to internal validators.
#'
#' @return
#' An unnamed numeric vector the same length as \code{ids}. Each element is
#' a proportion in \code{[0, 1]}. Returns \code{NA_real_} for IDs where:
#' \itemize{
#'   \item The ID is not found in \code{df}.
#'   \item \code{run_order} exceeds the number of runs in \code{phase_col}.
#'   \item All \code{target_col} values within the run window are \code{NA}.
#' }
#'
#' @details
#' Processing pipeline per ID:
#' \enumerate{
#'   \item Subsets \code{df} to rows matching the ID.
#'   \item Calls \code{.get_series_idx()} on \code{phase_col} to retrieve the
#'         row indices of the \code{run_order}-th consecutive run.
#'   \item Extracts \code{target_col} values at those indices, dropping \code{NA}s.
#'   \item Returns \code{mean(values == flag)}, i.e. the fraction of non-missing
#'         values in the window that match \code{flag}.
#' }
#'
#' @note
#' \code{flag} is a value, not a column name — do not pass it as a bare
#' (unquoted) symbol. Unlike the other column arguments, symbol capture is
#' intentionally not applied to \code{flag}.
#'
#' @examples
#' df <- data.frame(
#'   id     = c("A","A","A","A","A","B","B","B"),
#'   phase  = c("P1","P1","P2","P2","P2","P1","P1","P2"),
#'   status = c("ok","ok","flag","ok","flag","flag","flag","ok")
#' )
#'
#' # Proportion of "flag" in the 1st phase run for each ID
#' get_prop_flag(df, id_col = "id", phase_col = "phase",
#'               target_col = "status", flag = "flag",
#'               run_order = 1, ids = c("A", "B"))
#' #> [1] 0.0 1.0
#'
#' # 2nd phase run for A has 2/3 "flag" values
#' get_prop_flag(df, id_col = "id", phase_col = "phase",
#'               target_col = "status", flag = "flag",
#'               run_order = 2, ids = c("A", "B"))
#' #> [1] 0.6666667 0.0000000
#'
#' # run_order exceeds available runs → NA
#' get_prop_flag(df, id_col = "id", phase_col = "phase",
#'               target_col = "status", flag = "flag",
#'               run_order = 5, ids = "A")
#' #> [1] NA
#'
#' @seealso
#' \code{.get_series_idx()}, \code{\link{get_matching_series_str}},
#' \code{.check_get_prop_flag_args()}
#'
#' @export
get_prop_flag <- function(df, id_col, phase_col, target_col, flag, run_order, ids, ...) {
  
  # --- 1. Symbol Capture ---------------------------------------------------
  # Applied to column name arguments only — flag is a value, not a column name.
  if (is.symbol(substitute(id_col)))     id_col     <- deparse(substitute(id_col))
  if (is.symbol(substitute(phase_col)))  phase_col  <- deparse(substitute(phase_col))
  if (is.symbol(substitute(target_col))) target_col <- deparse(substitute(target_col))
  
  # --- 2. Validation -------------------------------------------------------
  .check_get_prop_flag_args(df, id_col, phase_col, target_col, flag, run_order, ids)
  
  # --- 3. Subset to Required Columns ---------------------------------------
  tmp_df <- df[, c(id_col, phase_col, target_col), drop = FALSE]
  
  # Coerce flag once outside the loop — avoids redundant conversion per ID
  flag_chr <- as.character(flag)
  
  # --- 4. Compute Proportion per ID ----------------------------------------
  # vapply() guarantees a numeric(1) result per iteration
  proportions <- vapply(ids, function(id) {
    
    sub <- tmp_df[tmp_df[[id_col]] == id, , drop = FALSE]
    
    # Guard: ID not found in df → NA
    if (nrow(sub) == 0) return(NA_real_)
    
    # Locate row indices of the run_order-th run in phase_col
    run_idx <- .get_series_idx(sub[[phase_col]], run_order)
    run_idx <- run_idx[!is.na(run_idx)]
    
    # Guard: run_order exceeds available runs → NA
    if (length(run_idx) == 0) return(NA_real_)
    
    # Extract target values within the run window, dropping NAs
    statuses_run <- sub[[target_col]][run_idx]
    statuses_run <- statuses_run[!is.na(statuses_run)]
    
    # Guard: all values in window are NA → NA
    if (length(statuses_run) == 0) return(NA_real_)
    
    # Proportion of values matching the flag
    mean(statuses_run == flag_chr)
    
  }, FUN.VALUE = numeric(1))
  
  return(unname(proportions))
}