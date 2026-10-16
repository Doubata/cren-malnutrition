# =============================================================================
# plot_catalog.R — build a named list of plots, then a filterable tibble
# catalog with metadata. This is the object you tar_read() into an Rmd.
# =============================================================================

make_plot_list <- function(data, vars, outcome = NULL, chart_type = "categorical",
                           outcome_type = NULL, geom = NULL, label_map = NULL, ...) {
  
  if (length(vars) == 0) {
    warning("No active variables for this iteration.", call. = FALSE)
    return(invisible(NULL))
  }
  
  builder <- chart_builders[[chart_type]]
  if (is.null(builder)) {
    stop("Unknown chart_type: '", chart_type, "'. Available: ",
         paste(names(chart_builders), collapse = ", "), call. = FALSE)
  }
  
  resolved_outcome_type <- .resolve_outcome_type(data, outcome, outcome_type)
  outcome_label <- if (!is.null(outcome)) .safe_var_label(data, outcome, label_map) else NULL
  labels <- .safe_var_label(data, vars, label_map)
  
  purrr::pmap(
    list(name = vars, label = labels),
    function(name, label) builder(data, name, label, outcome, resolved_outcome_type,
                                  outcome_label = outcome_label, geom = geom, ...)
  ) |>
    rlang::set_names(vars)
}

# label_map / description_map / subgroup_map are typically produced by
# codebook_to_label_map() / codebook_to_description_map() / codebook_to_subgroup_map()
# in variable_codebook.R -- a curated codebook label always wins over
# whatever (possibly absent) labelled::var_label() attribute the data has.
plot_catalog <- function(data, vars, outcome = NULL, chart_type = "categorical",
                         outcome_type = NULL, domain_map = NULL, subgroup_map = NULL,
                         label_map = NULL, description_map = NULL, geom = NULL, ...) {
  
  plots <- make_plot_list(data, vars, outcome, chart_type, outcome_type,
                          geom = geom, label_map = label_map, ...)
  if (is.null(plots)) return(invisible(NULL))
  
  tibble::tibble(
    var         = names(plots),
    label       = .safe_var_label(data, vars, label_map),
    description = if (!is.null(description_map)) unname(description_map[vars]) else NA_character_,
    domain      = if (!is.null(domain_map)) unname(domain_map[vars]) else NA_character_,
    subgroup    = if (!is.null(subgroup_map)) unname(subgroup_map[vars]) else NA_character_,
    chart_type  = chart_type,
    geom        = geom %||% "default",
    plot        = plots
  )
}

# Backward-compatible convenience wrapper for the original categorical-only use case
categorical_plot_catalog <- function(data, cat_vars, outcome = NULL, domain_map = NULL, geom = NULL) {
  plot_catalog(data, cat_vars, outcome, chart_type = "categorical", domain_map = domain_map, geom = geom)
}
