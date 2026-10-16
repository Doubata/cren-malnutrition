
#' Extract a Run-Encoded String of Values Aligned to a Reference Series Run
#'
#' @description
#' For each requested ID, locates the \code{run_order}-th consecutive run in a
#' reference column (\code{ref_col}), extracts the corresponding rows from the
#' target column (\code{target_col}), and returns a dash-separated
#' run-length encoded string of those values (e.g. \code{"A-B-A"}).
#' Output order matches the input order of \code{ids}.
#'
#' @param df         A data frame containing the data to query.
#' @param id_col     Character. Name of the column used to identify groups.
#' @param target_col Character. Name of the column whose values are encoded.
#' @param ref_col    Character. Name of the reference column used to locate
#'                   the positional run window.
#' @param attr_col   Optional character. Additional attribute column to include
#'                   in the subset. Defaults to \code{NULL}.
#' @param run_order  Positive integer. Which run in \code{ref_col} to align to
#'                   (1 = first run, 2 = second run, etc.).
#' @param ids        Vector of length ≥ 1. IDs to look up in \code{id_col}.
#'
#' @return
#' An unnamed character vector the same length as \code{ids}. Each element is a
#' dash-separated run-length encoded string of \code{target_col} values within
#' the matched run window (e.g. \code{"A-B-A"}). Returns \code{NA} for IDs not
#' found in \code{df} or where \code{run_order} exceeds the number of runs in
#' \code{ref_col} for that ID.
#'
#' @details
#' Processing pipeline per ID:
#' \enumerate{
#'   \item Validates all arguments via \code{.validate_args_matching_series_str()}.
#'   \item Subsets \code{df} to relevant rows and columns.
#'   \item Splits both \code{ref_col} and \code{target_col} into named lists
#'         keyed by ID via \code{.get_values_by_id()}.
#'   \item Applies \code{\link{rle}()} to the reference vector to identify run
#'         boundaries, then computes start/end row positions for the
#'         \code{run_order}-th run using cumulative lengths.
#'   \item Slices \code{target_col} to that window, applies \code{\link{rle}()}
#'         again, and collapses the result with \code{"-"}.
#' }
#'
#' @examples
#' df <- data.frame(
#'   id     = c("A","A","A","A","A", "B","B","B","B"),
#'   ref    = c("X","X","Y","Y","Y", "X","Y","Y","X"),
#'   target = c("a","b","c","c","d", "a","b","c","a")
#' )
#'
#' # 1st run of ref for A is "X" (rows 1-2) → target "a","b" → "a-b"
#' get_matching_series_str(df, id_col = "id", target_col = "target",
#'                         ref_col = "ref", run_order = 1, ids = "A")
#' #> [1] "a-b"
#'
#' # 2nd run of ref for A is "Y" (rows 3-5) → target "c","c","d" → "c-d"
#' get_matching_series_str(df, id_col = "id", target_col = "target",
#'                         ref_col = "ref", run_order = 2, ids = "A")
#' #> [1] "c-d"
#'
#' # run_order exceeds available runs → NA
#' get_matching_series_str(df, id_col = "id", target_col = "target",
#'                         ref_col = "ref", run_order = 5, ids = "A")
#' #> [1] NA
#' 
#' @family Series extraction
#'
#' @seealso
#' \code{\link{get_series_str}}, \code{.validate_matching_series_str_args()},
#' \code{.get_values_by_id()}, \code{\link{rle}}
#'
#' @export
get_matching_series_str <- function(df, id_col, target_col, ref_col, attr_col = NULL, run_order = NULL, ids) {
  
  # --- 1. Validation -------------------------------------------------------
  if (length(ids) == 0) return(character(0))
  
  # --- 2. Subset & Helper extraction --------------------------------------
  sub_df <- df[df[[id_col]] %in% ids, , drop = FALSE]
  
  referencies_list <- .get_values_by_id(sub_df, ref_col, id_col)
  values_list      <- .get_values_by_id(sub_df, target_col, id_col, attr_col = attr_col)
  
  ids_char <- as.character(ids)
  
  safe_rle <- function(x) {
    if (length(x) == 0) return(list(lengths = integer(0), values = character(0)))
    rle(as.character(x))
  }
  
  # --- 3. Robust Execution ------------------------------------------------
  result <- vapply(ids_char, function(id_char) {
    
    # Key existence checks
    if (!id_char %in% names(referencies_list) || !id_char %in% names(values_list)) {
      return(NA_character_)
    }
    
    ref_vec <- referencies_list[[id_char]]
    val_vec <- values_list[[id_char]]
    
    if (length(ref_vec) == 0 || length(val_vec) == 0) return(NA_character_)
    
    # Handle optional run_order
    if (is.null(run_order)) {
      # Return runs across the entire target vector
      matching_runs <- safe_rle(val_vec)
    } else {
      # Extract bounds for the specific run_order index
      ref_runs <- safe_rle(ref_vec)
      
      # Boundary guard
      if (run_order > length(ref_runs$values) || run_order < 1) return(NA_character_)
      
      lengths <- ref_runs$lengths
      e <- sum(lengths[seq_len(run_order)])
      s <- e - lengths[run_order] + 1
      
      # Bounds safety check on val_vec
      if (e > length(val_vec)) return(NA_character_)
      
      matching_runs <- safe_rle(val_vec[s:e])
    }
    
    paste0(matching_runs$values, collapse = "-")
    
  }, FUN.VALUE = character(1), USE.NAMES = FALSE)
  
  return(result)
}

