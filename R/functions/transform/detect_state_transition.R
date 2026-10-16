#' Detect State Transitions Across Phase Runs per ID
#'
#' @description
#' For each requested ID, determines whether a state transition occurs in
#' \code{target_col} — specifically, whether the value \code{from} is
#' followed by either any different value (\code{to = NULL}) or a specific
#' target value (\code{to}), optionally restricted to a specific phase run
#' (\code{run_order}). Returns a logical vector indicating whether the
#' transition was detected. Output order matches the input order of \code{ids}.
#'
#' @param df         A data frame containing patient-level data.
#' @param id_col     Column name (quoted or unquoted) for the grouping ID.
#' @param phase_col  Column name (quoted or unquoted) for the phase series
#'                   used to locate the run window when \code{run_order} is
#'                   provided.
#' @param target_col Column name (quoted or unquoted) for the state values
#'                   to evaluate.
#' @param from       Character scalar or vector. The state(s) to transition
#'                   \emph{from}. Pass as a value, not a column name.
#' @param to         Optional character scalar or vector. The specific
#'                   state(s) to transition \emph{to}. When \code{NULL}
#'                   (default), any state that is not \code{from} qualifies
#'                   as a transition target.
#' @param ...        Reserved for future use or passed to internal validators.
#' @param attr_col   Optional column name (quoted or unquoted) for an
#'                   attribute to append to \code{target_col} values as
#'                   \code{"value+attr"}, enabling compound state matching.
#'                   Defaults to \code{NULL}.
#' @param ids        Optional vector of IDs to evaluate. When \code{NULL}
#'                   (default), all unique IDs in \code{id_col} are processed.
#' @param run_order  Optional positive integer. When provided, restricts
#'                   transition detection to the \code{run_order}-th
#'                   consecutive run in \code{phase_col}.
#'                   Defaults to \code{NULL} (no restriction).
#' @param pos        Optional positive integer. Specifies the exact relative position 
#'                   after the first \code{from} state to check for the transition (e.g., 
#'                   \code{pos = 1} checks the immediate next value). When \code{NULL} (default), 
#'                   all subsequent positions after \code{from} are evaluated.
#' @param verbose    Logical. Reserved for future diagnostic output.
#'                   Defaults to \code{FALSE}.
#'
#' @return
#' An unnamed logical vector the same length as \code{ids} (or the number
#' of unique IDs when \code{ids = NULL}):
#' \itemize{
#'   \item \code{TRUE}  — a qualifying transition was detected.
#'   \item \code{FALSE} — no transition detected, or fewer than 2 values
#'         in the window, or \code{from} is the last value.
#'   \item \code{NA}    — ID not found in \code{df}, or \code{run_order}
#'         yields no valid indices.
#' }
#'
#' @details
#' Processing pipeline per ID:
#' \enumerate{
#'   \item \strong{Symbol capture} — \code{id_col}, \code{phase_col},
#'         \code{target_col}, and \code{attr_col} accept quoted or unquoted
#'         names. \code{from} and \code{to} are values — symbol capture is
#'         intentionally not applied.
#'   \item \strong{Validation} — delegates to
#'         \code{.check_detect_state_transition_args()}.
#'   \item \strong{Pre-processing} — \code{target_col} is coerced to
#'         character; \code{NA}s replaced with \code{"MISSING"} sentinel.
#'         When \code{attr_col} is provided, non-missing values are combined
#'         as \code{"value+attr"} for compound state matching.
#'   \item \strong{Pre-split} — both \code{target_col} and \code{phase_col}
#'         are split once by \code{id_col} before iterating.
#'   \item \strong{Run filter} — when \code{run_order} is provided,
#'         \code{.get_series_idx()} restricts the value vector to the
#'         matching phase window.
#'   \item \strong{Transition detection}:
#'     \itemize{
#'       \item Locates the first occurrence of \code{from} in the window.
#'       \item If \code{to = NULL}: returns \code{TRUE} if any subsequent
#'             value is not in \code{from}.
#'       \item If \code{to} is specified: returns \code{TRUE} if any
#'             \code{to} value occurs after the first \code{from}.
#'     }
#' }
#'
#' @note
#' \code{from} and \code{to} are values, not column names — do not pass
#' them as bare (unquoted) symbols. Symbol capture is intentionally skipped
#' for these arguments.
#'
#' @examples
#' df <- data.frame(
#'   id     = c("A","A","A","A","B","B","B","C","C"),
#'   phase  = c("P1","P1","P2","P2","P1","P1","P2","P1","P1"),
#'   status = c("X","X","Y","Z","X","X","X","X","Y")
#' )
#'
#' # Detect any transition away from "X"
#' detect_state_transition(df, id_col = "id", phase_col = "phase",
#'                         target_col = "status", from = "X",
#'                         ids = c("A","B","C"))
#' #> [1]  TRUE FALSE  TRUE
#'
#' # Detect specific transition from "X" to "Z"
#' detect_state_transition(df, id_col = "id", phase_col = "phase",
#'                         target_col = "status", from = "X", to = "Z",
#'                         ids = c("A","B","C"))
#' #> [1]  TRUE FALSE FALSE
#'
#' # Restrict to 1st phase run only
#' detect_state_transition(df, id_col = "id", phase_col = "phase",
#'                         target_col = "status", from = "X",
#'                         run_order = 1, ids = c("A","B","C"))
#' #> [1] FALSE FALSE  TRUE
#'
#' # Unknown ID returns NA
#' detect_state_transition(df, id_col = "id", phase_col = "phase",
#'                         target_col = "status", from = "X",
#'                         ids = c("A", "Z"))
#' #> [1] TRUE NA
#' 
#' @family event summarisation
#'
#' @seealso
#' \code{.check_detect_state_transition_args()}, \code{.get_series_idx()},
#' \code{\link{summarise_event}}, \code{\link{get_series_str}}
#' 
#'
#' @export
detect_state_transition <- function(df, id_col, phase_col, target_col,
                                    from, to   = NULL, ...,
                                    attr_col   = NULL,
                                    ids        = NULL,
                                    run_order  = NULL,
                                    pos        = NULL,
                                    verbose    = FALSE) {
  
  # --- 1. Symbol Capture ---------------------------------------------------
  # Applied to column name arguments only.
  # 'from' and 'to' are values — symbol capture intentionally skipped.
  if (is.symbol(substitute(id_col)))     id_col     <- deparse(substitute(id_col))
  if (is.symbol(substitute(phase_col)))  phase_col  <- deparse(substitute(phase_col))
  if (is.symbol(substitute(target_col))) target_col <- deparse(substitute(target_col))
  if (is.symbol(substitute(attr_col)))   attr_col   <- deparse(substitute(attr_col))
  
  # --- 2. Validation -------------------------------------------------------
  .check_detect_state_transition_args(df, id_col, phase_col, target_col,
                                      from, to, attr_col, ids,
                                      run_order, verbose)
  
  # --- 3. Pre-processing ---------------------------------------------------
  # Coerce to character and sentinel-fill NAs before splitting
  vals   <- as.character(df[[target_col]])
  phases <- as.character(df[[phase_col]])
  vals[is.na(vals)] <- "MISSING"
  
  # Compound state: paste target + attribute as "value+attr" where both present
  if (!is.null(attr_col)) {
    attrs <- as.character(df[[attr_col]])
    attrs[is.na(attrs)] <- "MISSING"
    
    mask       <- attrs != "MISSING" & vals != "MISSING"
    vals[mask] <- paste0(vals[mask], "+", attrs[mask])  # fixed: was attr_col (name) not attrs (values)
  }
  
  # --- 4. Pre-split by ID --------------------------------------------------
  # Split once before the loop to avoid repeated full-frame subsetting per ID
  id_char     <- as.character(df[[id_col]])
  values_list <- split(vals,   id_char)
  phases_list <- split(phases, id_char)
  
  ids_char <- if (is.null(ids)) {
    unique(id_char)
  } else {
    as.character(ids)
  }
  
  # --- 5. Transition Detection per ID --------------------------------------
  results <- vapply(ids_char, function(id) {
    
    # Guard: ID absent from data → NA
    if (!(id %in% names(phases_list))) return(NA)
    
    phases_vec <- phases_list[[id]]
    values_vec <- values_list[[id]]
    
    # --- Optional Run Filter -----------------------------------------------
    if (!is.null(run_order)) {
      run_idx <- .get_series_idx(phases_vec, run_order)
      run_idx <- run_idx[!is.na(run_idx)]   # fixed: was any(!is.na()) guard — allows NA indices through
      
      if (length(run_idx) == 0) return(NA)
      values_vec <- values_vec[run_idx]
    }
    
    # Need at least 2 values to detect a transition
    if (length(values_vec) < 2) return(FALSE)
    
    # Locate the first occurrence of the 'from' state
    from_indices <- which(values_vec %in% from)
    if (length(from_indices) == 0) return(FALSE)
    
    first_from <- from_indices[1]
    
    # Check bounds when 'pos' is specified
    if (!is.null(pos)) {
      # target_idx <- first_from + pos
      target_idx <- pos
      
      # Out of bounds check
      if (target_idx > length(values_vec)) return(FALSE)
      
      target_val <- values_vec[target_idx]
      
      if (is.null(to)) {
        return(!(target_val %in% from))
      } else {
        return(target_val %in% to)
      }
    }
    
    # Default behavior: Search across all elements after 'first_from'
    
    # Edge case: 'from' is the last value — no room for a transition
    if (first_from == length(values_vec)) return(FALSE)
    
    after_from_vec <- values_vec[(first_from + 1):length(values_vec)]
    
    if (is.null(to)) {
      # Any subsequent value that is not 'from' qualifies as a transition
      return(any(!(after_from_vec %in% from)))
    } else {
      # Specific transition: 'to' must occur after the first 'from'
      to_indices <- which(values_vec %in% to)
      return(any(to_indices > first_from))
    }
    
  }, FUN.VALUE = logical(1))
  
  return(unname(results))
}
