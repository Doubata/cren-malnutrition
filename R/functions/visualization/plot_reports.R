# =============================================================================
# plot_reports.R — top-level entry points: paginated report grids, and
# exporting individual plots to disk for non-R consumers.
# =============================================================================

plot_categorical_bars <- function(data, cat_vars, outcome = NULL, ncol = 3,
                                   per_page = 9, outcome_type = NULL,
                                   domain_map = NULL, geom = NULL,
                                   save_dir = NULL, prefix = "cat_bars") {

  catalog <- plot_catalog(data, cat_vars, outcome, chart_type = "categorical",
                           outcome_type = outcome_type, domain_map = domain_map, geom = geom)
  if (is.null(catalog)) return(invisible(NULL))

  batches <- split(seq_len(nrow(catalog)), ceiling(seq_len(nrow(catalog)) / per_page))

  purrr::imap(batches, function(idx, batch_num) {

    combined <- compose_plot_grid(
      catalog$plot[idx],
      ncol  = ncol,
      title = paste0("Distribution of Categorical Variables - Baseline (",
                      batch_num, "/", length(batches), ")")
    )

    if (!is.null(save_dir)) {
      nrow <- ceiling(length(idx) / ncol)
      ggsave(
        file.path(save_dir, paste0(prefix, "_", batch_num, ".png")),
        combined, width = ncol * 5, height = nrow * 3.2, dpi = 300, limitsize = FALSE
      )
    }

    combined
  })
}

export_categorical_plots <- function(catalog, dir, width = 5, height = 3.5, dpi = 300) {

  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)

  purrr::pwalk(catalog, function(var, plot, ...) {
    ggsave(file.path(dir, paste0(var, ".png")), plot, width = width, height = height, dpi = dpi)
  })

  invisible(catalog)
}
