
#' Build a Run-Length Encoded String of Values per ID
#'
#' @description
#' For each requested ID, extracts the corresponding values from \code{target_col},
#' compresses consecutive duplicates via run-length encoding, and collapses the
#' resulting sequence into a single dash-separated string (e.g. \code{"A-B-A-C"}).
#' Output order matches the input order of \code{ids}.
#'
#' @param df         A data frame containing the data to query.
#' @param id_col     Character. Name of the column used to identify groups.
#' @param target_col Character. Name of the column whose values are encoded.
#' @param attr_col   Optional character. Name of an additional attribute column
#'                   to include in the subset. Defaults to \code{NULL}.
#' @param ids        Vector of length ≥ 1. IDs to look up in \code{id_col}.
#'
#' @return
#' An unnamed character vector the same length as \code{ids}. Each element is a
#' dash-separated run-length encoded string of \code{target_col} values for that
#' ID (e.g. \code{"A-B-A-C"}). Returns \code{NA} for IDs not found in \code{df},
#' or if \code{.get_values_by_id()} does not return a named list.
#'
#' @details
#' Processing pipeline:
#' \enumerate{
#'   \item Validates all arguments via \code{.validate_args_series_str()}.
#'   \item Subsets \code{df} to relevant rows and columns (including
#'         \code{attr_col} when provided).
#'   \item Splits values into a named list keyed by ID via
#'         \code{.get_values_by_id()}.
#'   \item For each ID, applies \code{\link{rle}()} to collapse consecutive
#'         repeated values, then \code{\link{paste0}()} to join them with
#'         \code{"-"}.
#' }
#'
#' @examples
#' df <- data.frame(
#'   patient_id = c("A","A","A","B","B","B"),
#'   status     = c("X","X","Y","Y","Z","Y")
#' )
#'
#' get_series_str(df, id_col = "patient_id", target_col = "status",
#'                ids = c("A", "B"))
#' #> [1] "X-Y"   "Y-Z-Y"
#'
#' # Unknown ID returns NA
#' get_series_str(df, id_col = "patient_id", target_col = "status",
#'                ids = c("A", "Z"))
#' #> [1] "X-Y" NA
#' 
#' @family Series extraction
#'
#' @seealso
#' \code{\link{get_value_at}}, \code{.validate_series_str_args()},
#' \code{.get_values_by_id()}, \code{\link{rle}}
#'
#' @export

get_series_str <- function(df, id_col, target_col, attr_col = NULL, ids){
  
  # --- 1. Symbol Capture ---------------------------------------------------
  if (is.symbol(substitute(id_col)))     id_col     <- deparse(substitute(id_col))
  if (is.symbol(substitute(target_col))) target_col <- deparse(substitute(target_col))
  if (is.symbol(substitute(attr_col)))   attr_col   <- deparse(substitute(attr_col))
  
  # --- 2. Validation -------------------------------------------------------
  .validate_series_str_args(df, id_col, target_col, attr_col, ids)
  
  # --- 3. Column Selection -------------------------------------------------
  # Always include id_col and target_col; append attr_col when provided.
  target_cols <- if (is.null(attr_col)) {
    c(id_col, target_col)
  } else {
    c(id_col, target_col, attr_col)
  }
  
  # --- 4. Subset -----------------------------------------------------------
  # Restrict to relevant rows and columns before splitting (faster on large df).
  sub_df <- df[df[[id_col]] %in% ids, target_cols]
  
  # --- 5. Split by ID ------------------------------------------------------
  # Returns a named list: list("id1" = c(val1, val2, ...), ...)
  values_list <- .get_values_by_id(sub_df, target_col, id_col, attr_col)
  
  # --- 6. Encode & Collapse ------------------------------------------------
  if(is.list(values_list) && !is.null(names(values_list))){
    
    ids_char <- as.character(ids)
    
    result <- sapply(ids_char, function(id_char) {
      # Guard: ID absent from data → NA
      if (!(id_char %in% names(values_list))) return(NA)
      
      values_vec <- values_list[[id_char]]
      
      # rle() strips consecutive duplicates; $values holds the unique run sequence
      values_runs <- rle(as.character(values_vec))$values
      
      # Collapse run sequence into a single dash-separated string e.g. "A-B-A-C"
      paste0(values_runs, collapse = "-")
    })
    
    return(unname(result))
  }else{
    # .get_values_by_id() did not return a valid named list
    return(NA)
  }
}

