
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

get_matching_series_str <- function(df, id_col, target_col, ref_col, attr_col = NULL, run_order, ids){
  
  # --- 1. Validation -------------------------------------------------------
  .validate_matching_series_str_args(df, id_col, target_col, ref_col, attr_col, run_order, ids)
  
  # --- 2. Column Selection -------------------------------------------------
  # Append attr_col to the subset only when provided.
  target_cols <- if (is.null(attr_col)) {
    c(id_col, ref_col, target_col)
  } else {
    c(id_col, ref_col, target_col, attr_col)
  }
  
  # --- 3. Subset -----------------------------------------------------------
  sub_df <- df[df[[id_col]] %in% ids, target_cols]
  
  # --- 4. Split by ID ------------------------------------------------------
  # Two parallel named lists — one for the reference series, one for target.
  referencies_list <- .get_values_by_id(sub_df, ref_col, id_col)
  values_list <- .get_values_by_id(sub_df, target_col, id_col, attr_col)
  
  ids_char <- as.character(ids)
  
  # --- 5. Encode & Align ---------------------------------------------------
  result <- sapply(ids_char, function(id_char) {
    
    # Guard: ID absent from reference data → NA
    if (!(id_char %in% names(referencies_list))) return(NA)
    
    ref_vec <- referencies_list[[id_char]]
    
    # RLE on the reference series to identify run boundaries
    ref_runs <- rle(as.character(ref_vec))
    
    # Guard: requested run does not exist for this ID → NA
    if (run_order > length(ref_runs$values)) return(NA)
    
    # Derive start / end row indices for the run_order-th run
    ends   <- cumsum(ref_runs$lengths)
    starts <- c(1, head(ends, -1) + 1)
    
    # Get the position of a specific group run_order
    s <- starts[run_order]
    e <- ends[run_order]
    
    # Slice target values to the matched window, then RLE-encode and collapse
    val_vec <- values_list[[id_char]]
    matching_runs <- rle(val_vec[s:e])
    paste0(matching_runs$values, collapse = "-")
  })
  
  return(unname(result))
}

