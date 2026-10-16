# =============================================================================
# variable_type_detection.R — classify each variable as a chart_type
# ("categorical" / "continuous" / ...) so a mixed variable list can be routed
# to the right panel builder automatically.
#
# Resolution order (same pattern as outcome_resolution.R):
#   1. Explicit type_map[[var]] override (always wins)
#   2. Class-based heuristic: factor/character/logical/labelled -> categorical,
#      numeric -> continuous, guarded against low-cardinality integer codes
#   3. Unclassifiable class -> hard error (never silently guessed), pointing
#      the user to type_map
# =============================================================================

.detect_variable_type <- function(data, var, type_map = NULL, numeric_threshold = 5, quiet = TRUE) {

  if (!is.null(type_map) && var %in% names(type_map) && !is.na(type_map[[var]])) {
    return(type_map[[var]])
  }

  vec <- data[[var]]

  if (is.factor(vec) || is.character(vec) || is.logical(vec) ||
      inherits(vec, "haven_labelled")) {
    return("categorical")
  }

  if (is.numeric(vec)) {
    n_unique <- dplyr::n_distinct(vec, na.rm = TRUE)
    if (n_unique <= numeric_threshold) {
      if(!quiet){
        warning(
          "'", var, "' is numeric with only ", n_unique,
          " unique values -- treating as categorical. ",
          "Pass type_map = c(", var, " = \"continuous\") to override.", call. = FALSE
        )
      }
      return("categorical")
    }
    return("continuous")
  }

  stop(
    "Cannot auto-detect type for '", var, "' (class: ",
    paste(class(vec), collapse = "/"), "). Provide it via type_map, e.g. ",
    "type_map = c(", var, " = \"categorical\").", call. = FALSE
  )
}

# Named character vector: var -> detected/overridden chart_type
detect_variable_types <- function(data, vars, type_map = NULL, numeric_threshold = 5) {
  purrr::map_chr(
    vars, .detect_variable_type,
    data = data, type_map = type_map, numeric_threshold = numeric_threshold
  ) |>
    rlang::set_names(vars)
}

# Human-readable preview — run this BEFORE building 24+ plots to catch
# misclassification early, since a wrong type silently changes the
# statistical test and geom used for that variable.
describe_variable_types <- function(data, vars, type_map = NULL, numeric_threshold = 5) {
  types <- detect_variable_types(data, vars, type_map, numeric_threshold)
  tibble::tibble(
    var         = names(types),
    chart_type  = unname(types),
    class       = purrr::map_chr(vars, ~ paste(class(data[[.x]]), collapse = "/")),
    n_unique    = purrr::map_int(vars, ~ dplyr::n_distinct(data[[.x]], na.rm = TRUE)),
    overridden  = vars %in% names(type_map)
  )
}
