
#' Retrieve a Value at a Specific Row Index for Given IDs
#'
#' @description
#' Filters a data frame by a set of IDs, groups rows by those IDs, and extracts
#' the value at a specified positional index within each group. Output order
#' matches the input order of `ids`. Missing IDs or out-of-bounds indices
#' return `NA` gracefully.
#'
#' @param df         A data frame containing the data to query.
#' @param id_col     A character string. The name of the column in `df` used
#'                   to identify groups (e.g., `"patient_id"`).
#' @param target_col A character string. The name of the column from which
#'                   values are extracted (e.g., `"score"`).
#' @param row_index  A positive integer. The positional index (1-based) of the
#'                   value to retrieve within each ID group.
#' @param ...        Reserved for future use or passed to internal validators.
#' @param ids        A vector of IDs to look up. Must match values found in
#'                   `id_col`. Can be numeric or character.
#'
#' @return
#' An unnamed vector of the same length as `ids`, containing the extracted
#' values at position `row_index` for each ID group. Returns `NA` for any ID
#' not found in `df` or where `row_index` exceeds the group's row count.
#'
#' @details
#' Internally, the function:
#' \enumerate{
#'   \item Validates all arguments via \code{.validate_args_value_at()}.
#'   \item Subsets \code{df} to rows whose \code{id_col} value is in \code{ids}.
#'   \item Splits the \code{target_col} values into a named list, keyed by ID.
#'   \item Iterates over \code{ids} (preserving input order) to extract the
#'         value at \code{row_index} from each group vector.
#' }
#' IDs are coerced to character internally for consistent list indexing, so
#' numeric and character IDs are handled uniformly.
#'
#' @examples
#' df <- data.frame(
#'   patient_id = c("A", "A", "B", "B", "C"),
#'   score      = c(10, 20, 30, 40, 50)
#' )
#'
#' # Retrieve the 2nd score for patients A and B
#' get_value_at(df, id_col = "patient_id", target_col = "score",
#'              row_index = 2, ids = c("A", "B"))
#' #> [1] 20 40
#'
#' # ID "D" does not exist — returns NA
#' get_value_at(df, id_col = "patient_id", target_col = "score",
#'              row_index = 1, ids = c("A", "D"))
#' #> [1] 10 NA
#'
#' # row_index out of bounds for "C" (only 1 row) — returns NA
#' get_value_at(df, id_col = "patient_id", target_col = "score",
#'              row_index = 2, ids = c("A", "C"))
#' #> [1] 20 NA
#'
#' @seealso \code{.validate_value_at_args()} for argument validation logic.
#' 
#' @family Series extraction
#'
#' @export

get_value_at <- function(df, id_col, target_col, row_index,..., ids){
  
  # --- 1. Validation -------------------------------------------------------
  .validate_value_at_args(df, id_col, target_col, row_index, ids)
  
  # --- 2. Subset -----------------------------------------------------------
  # Keep only rows whose id_col value appears in the requested ids.
  # This reduces memory usage before splitting.
  sub_df <- df[df[[id_col]] %in% ids, c(id_col, target_col)]
  
  # --- 3. Grouping ---------------------------------------------------------
  # split() partitions the target column into a named list:
  values_list <- split(sub_df[[target_col]], as.character(sub_df[[id_col]]))
  
  # --- 4. ID Coercion ------------------------------------------------------
  # Convert ids to character to align with the character keys in values_list.
  ids_char <- as.character(ids)
  
  # --- 5. Extraction -------------------------------------------------------
  # Iterate over ids in input order so the output vector mirrors ids exactly.
  result <- sapply(ids_char, function(id_char) {
    # Check if the ID exists in the list
    if (!(id_char %in% names(values_list))) {
      return(NA) 
    }
    
    # Check if ID exists in df and has enough rows
    group_vec <- values_list[[id_char]]
    
    if (!is.null(group_vec) && length(group_vec) >= row_index) {
      return(group_vec[row_index])
    } else {
      # Returns NA if ID is missing or index is out of bounds
      return(NA) 
    }
  })
  
  # --- 6. Return -----------------------------------------------------------
  # Strip sapply's automatic names so callers receive a clean vector.
  return(unname(result))
}
