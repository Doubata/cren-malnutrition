# =============================================================================
# outcome_resolution.R — classify an outcome as "categorical" or "numeric"
#
# Resolution order:
#   1. Explicit outcome_type argument (always wins)
#   2. Heuristic on the data, with a guard against integer-coded categoricals
#      (common in DHIS2/ODK exports) — warns rather than guessing silently
# =============================================================================

.resolve_outcome_type <- function(data, outcome, outcome_type = NULL,
                                   numeric_threshold = 5) {

  if (is.null(outcome)) return(NULL)

  if (!is.null(outcome_type)) {
    if (!outcome_type %in% c("categorical", "numeric")) {
      stop("outcome_type must be 'categorical' or 'numeric'", call. = FALSE)
    }
    return(outcome_type)
  }

  vec <- data[[outcome]]
  if (!is.numeric(vec)) return("categorical")

  n_unique <- dplyr::n_distinct(vec, na.rm = TRUE)
  if (n_unique <= numeric_threshold) {
    warning(
      "'", outcome, "' is numeric with only ", n_unique,
      " unique values -- treating as categorical. ",
      "Pass outcome_type explicitly to override.", call. = FALSE
    )
    return("categorical")
  }

  "numeric"
}
