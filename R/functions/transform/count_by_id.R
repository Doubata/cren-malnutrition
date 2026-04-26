#' Count Values per ID, Optionally Filtered by Criteria
#'
#' @description
#' For each requested ID, counts either all values in \code{target_col} or —
#' when \code{criteria} is provided — only those rows where \code{target_col}
#' or \code{attr_col} matches any value in \code{criteria}.
#' Output order matches the input order of \code{ids}.
#'
#' @param df         A data frame containing the data to query.
#' @param id_col     Character. Name of the column used to identify groups.
#' @param target_col Character. Name of the column whose values are counted.
#' @param attr_col   Optional character. Name of an additional attribute column
#'                   also matched against \code{criteria}. Defaults to \code{NULL}.
#' @param ids        Vector of length ≥ 1. IDs to look up in \code{id_col}.
#' @param ...        Reserved for future use or passed to internal validators.
#' @param criteria   Optional vector. Values to match against. When \code{NULL}
#'                   (default), all rows for the ID are counted. When provided,
#'                   counts rows where \code{target_col} OR \code{attr_col}
#'                   matches any element of \code{criteria}.
#'
#' @return
#' An unnamed numeric vector the same length as \code{ids}. Each element is the
#' count for that ID. Returns \code{NA} for IDs not found in \code{df}.
#'
#' @details
#' Processing pipeline per ID:
#' \enumerate{
#'   \item Validates all arguments via \code{.check_args_count_by_id()}.
#'   \item Subsets \code{df} to relevant rows and columns (including
#'         \code{attr_col} when provided).
#'   \item Splits \code{target_col} (and optionally \code{attr_col}) into
#'         named lists keyed by ID via \code{\link{split}()}.
#'   \item For each ID, applies one of three counting strategies:
#'     \itemize{
#'       \item No \code{criteria} → \code{length(values_vec)} (total row count).
#'       \item \code{criteria}, no \code{attr_col} → rows where
#'             \code{target_col \%in\% criteria}.
#'       \item \code{criteria} + \code{attr_col} → rows where
#'             \code{target_col \%in\% criteria} OR \code{attr_col \%in\% criteria}.
#'     }
#' }
#'
#' @examples
#' df <- data.frame(
#'   id     = c("A","A","A","B","B","B"),
#'   status = c("X","Y","X","X","X","Z"),
#'   flag   = c("p","p","q","p","q","q")
#' )
#'
#' # Count all rows per ID
#' count_by_id(df, id_col = "id", target_col = "status", ids = c("A", "B"))
#' #> [1] 3 3
#'
#' # Count only rows where status is "X"
#' count_by_id(df, id_col = "id", target_col = "status",
#'             ids = c("A", "B"), criteria = "X")
#' #> [1] 2 2
#'
#' # Count rows where status OR flag matches criteria
#' count_by_id(df, id_col = "id", target_col = "status", attr_col = "flag",
#'             ids = c("A", "B"), criteria = "q")
#' #> [1] 1 2
#'
#' # Unknown ID returns NA
#' count_by_id(df, id_col = "id", target_col = "status",
#'             ids = c("A", "Z"))
#' #> [1] 3 NA
#'
#' @seealso
#' \code{.check_count_by_id_args()}, \code{\link{get_series_str}},
#' \code{\link{get_series_length_at}}
#'
#' @export
count_by_id <- function(df, id_col, target_col, attr_col = NULL, ids,..., criteria = NULL){
  
  # --- 1. Validation -------------------------------------------------------
  .check_count_by_id_args(df, id_col, target_col, attr_col, ids, criteria)
  
  # --- 2. Column Selection -------------------------------------------------
  # Append attr_col to the subset only when provided.
  target_cols <- if (is.null(attr_col)) {
    c(id_col, target_col)
  } else {
    c(id_col, target_col, attr_col)
  }
  
  # --- 3. Subset -----------------------------------------------------------
  sub_df <- df[df[[id_col]] %in% ids, target_cols]
  
  # --- 4. Split by ID ------------------------------------------------------
  id_factor <- as.character(sub_df[[id_col]])
  values_list <- split(sub_df[[target_col]], id_factor)
  
  # Split attr_col in parallel only when needed
  if(!is.null(attr_col)){
    attributes_list <- split(sub_df[[attr_col]], id_factor)
  }
  
  ids_char <- as.character(ids)
  
  # --- 5. Count ------------------------------------------------------------
  results <- sapply(ids_char, function(id_char){
    
    # Guard: ID absent from data → NA
    if (!(id_char %in% names(values_list))) return(NA)
    
    values_vec <- values_list[[id_char]] # Extract vector
    
    if(!is.null(criteria)){
      if(is.null(attr_col)){
        # Strategy 1: match target_col only
        n <- sum(values_vec %in% criteria)
      }else{
        # Strategy 2: match target_col OR attr_col
        attributes_vec <- attributes_list[[id_char]]
        n <- sum(values_vec %in% criteria | attributes_vec %in% criteria)
      }
      return(n)
    }else{
      # Strategy 3: no filter — count all rows for this ID
      return(length(values_vec))
    }
  })
  
  return(unname(results))
}
