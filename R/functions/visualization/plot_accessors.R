# =============================================================================
# plot_accessors.R — query a catalog and compose plots into grids.
# Agnostic to chart_type / geom: operates on plain ggplot objects.
# =============================================================================

pull_plots <- function(catalog, vars = NULL, domain = NULL, subgroup = NULL, chart_type = NULL) {

  out <- catalog
  if (!is.null(vars))       out <- dplyr::filter(out, var %in% .env$vars)
  if (!is.null(domain))     out <- dplyr::filter(out, domain %in% .env$domain)
  if (!is.null(subgroup))   out <- dplyr::filter(out, subgroup %in% .env$subgroup)
  if (!is.null(chart_type)) out <- dplyr::filter(out, chart_type %in% .env$chart_type)

  if (nrow(out) == 0) {
    warning("No plots matched the given filter(s).", call. = FALSE)
    return(list())
  }

  rlang::set_names(out$plot, out$var)
}

compose_plot_grid <- function(plot_list, ncol = 3, title = NULL) {

  if (length(plot_list) == 0) {
    warning("No plots to compose.", call. = FALSE)
    return(invisible(NULL))
  }

  wrap_plots(plot_list, ncol = ncol) +
    plot_layout(guides = "collect") +
    plot_annotation(
      title = title,
      theme = theme(
        plot.title = element_text(face = "bold", size = 13),
        legend.position = "bottom"
      )
    )
}
