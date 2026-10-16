# =============================================================================
# auto_catalog.R — build a catalog from a MIXED variable list, auto-detecting
# each variable's chart_type and dispatching to the matching panel builder.
# Delegates to plot_catalog() per detected type group; does not duplicate
# any panel/geom logic.
# =============================================================================

auto_plot_catalog <- function(data, vars, outcome = NULL, outcome_type = NULL,
                               domain_map = NULL, subgroup_map = NULL,
                               label_map = NULL, description_map = NULL,
                               type_map = NULL, geom_map = NULL,
                               numeric_threshold = 5, ...) {

  detected <- detect_variable_types(data, vars, type_map, numeric_threshold)

  unknown_types <- setdiff(unique(detected), names(chart_builders))
  if (length(unknown_types) > 0) {
    stop(
      "Detected type(s) with no registered chart builder: ",
      paste(unknown_types, collapse = ", "), ". Registered builders: ",
      paste(names(chart_builders), collapse = ", "), call. = FALSE
    )
  }

  type_groups <- split(vars, detected)

  catalogs <- purrr::imap(type_groups, function(group_vars, chart_type) {
    geom <- if (!is.null(geom_map)) geom_map[[chart_type]] else NULL
    plot_catalog(
      data, group_vars, outcome = outcome, chart_type = chart_type,
      outcome_type = outcome_type, domain_map = domain_map, subgroup_map = subgroup_map,
      label_map = label_map, description_map = description_map, geom = geom, ...
    )
  })

  dplyr::bind_rows(catalogs) |>
    dplyr::arrange(match(var, vars))   # restore original vars order, not alphabetical-by-type
}
