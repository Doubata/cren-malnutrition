
#' Get the Length of a Specific Run in a Series per ID
#'
#' @description
#' For each requested ID, applies run-length encoding to \code{target_col}
#' and returns the length of the \code{run_order}-th consecutive run.
#' Output order matches the input order of \code{ids}.
#'
#' @param df         A data frame containing the data to query.
#' @param id_col     Column name (quoted or unquoted). Name of the column
#'                   used to identify groups.
#' @param target_col Column name (quoted or unquoted). Name of the column
#'                   to run-length encode.
#' @param run_order  Positive integer. Which run to measure
#'                   (1 = first run, 2 = second run, etc.).
#' @param ids        Vector of length ≥ 1. IDs to look up in \code{id_col}.
#'
#' @return
#' An unnamed integer vector the same length as \code{ids}. Each element is
#' the length of the \code{run_order}-th run for that ID. Returns
#' \code{NA_integer_} for IDs not found in \code{df} or where
#' \code{run_order} exceeds the total number of runs.
#'
#' @details
#' Processing pipeline:
#' \enumerate{
#'   \item \strong{Symbol capture} — \code{id_col} and \code{target_col}
#'         accept both quoted strings and bare (unquoted) names via
#'         \code{\link{substitute}()}.
#'   \item \strong{Validation} — delegates to
#'         \code{.check_get_series_length_at_args()}.
#'   \item \strong{Subset} — restricts \code{df} to relevant rows and columns.
#'   \item \strong{Pre-split} — \code{df} is split once by \code{id_col}
#'         via \code{.get_values_by_id()} before iterating, with \code{NA}
#'         values replaced by the sentinel \code{"MISSING"} to prevent
#'         \code{\link{rle}()} from silently dropping or merging \code{NA}
#'         runs.
#'   \item \strong{Run length} — \code{\link{rle}()} is applied per ID;
#'         \code{runs\$lengths[run_order]} is returned as an integer.
#' }
#'
#' @note
#' Uses \code{\link{vapply}()} with \code{FUN.VALUE = integer(1)} to
#' guarantee a typed integer vector output, unlike the character vectors
#' returned by sibling functions such as \code{\link{get_series_str}}.
#'
#' @examples
#' df <- data.frame(
#'   id     = c("A","A","A","A","B","B","B"),
#'   status = c("X","X","Y","Y","Y","X","X")
#' )
#'
#' # 1st run for A is "X" repeated twice → 2
#' get_series_length_at(df, id_col = "id", target_col = "status",
#'                      run_order = 1, ids = "A")
#' #> [1] 2
#'
#' # 2nd run for A is "Y" repeated three times → 3
#' get_series_length_at(df, id_col = "id", target_col = "status",
#'                      run_order = 2, ids = "A")
#' #> [1] 3
#'
#' # run_order exceeds available runs → NA
#' get_series_length_at(df, id_col = "id", target_col = "status",
#'                      run_order = 5, ids = "A")
#' #> [1] NA
#' 
#' @family Series extraction
#'
#' @seealso
#' \code{\link{get_series_str}}, \code{\link{get_matching_series_str}},
#' \code{.validate_args_series_length_at()}, \code{\link{rle}}
#'
#' @export
get_series_length_at <- function(df, id_col, target_col, run_order, ids) {
  
  # --- 1. Symbol Capture ---------------------------------------------------
  if (is.symbol(substitute(id_col)))     id_col     <- deparse(substitute(id_col))
  if (is.symbol(substitute(target_col))) target_col <- deparse(substitute(target_col))
  
  # --- 2. Validation -------------------------------------------------------
  .check_get_series_length_at_args(df, id_col, target_col, run_order, ids)
  
  # --- 3. Subset -----------------------------------------------------------
  sub_df <- df[df[[id_col]] %in% ids, c(id_col, target_col), drop = FALSE]
  
  # --- 4. Pre-split by ID --------------------------------------------------
  # .get_values_by_id() replaces NAs with "MISSING" sentinel before splitting,
  # ensuring rle() treats NA positions as a distinct run rather than dropping them.
  values_list <- .get_values_by_id(sub_df, target_col, id_col)
  
  ids_char <- as.character(ids)
  
  # --- 5. Measure Run Length -----------------------------------------------
  # vapply() enforces integer(1) output per iteration — safer than sapply().
  result <- vapply(ids_char, function(id_char) {
    
    # Guard: ID absent from data → NA
    if (!(id_char %in% names(values_list))) return(NA_integer_)
    
    val_vec <- values_list[[id_char]]
    
    runs <- rle(as.character(val_vec))
    
    # Guard: run_order exceeds number of runs for this ID → NA
    if (run_order > length(runs$lengths)) return(NA_integer_)
    
    as.integer(runs$lengths[run_order])
    
  }, FUN.VALUE = integer(1))
  
  return(unname(result))
}

