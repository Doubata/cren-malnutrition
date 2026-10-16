# =============================================================================
# render_plot_grid.R — render a plot grid at a height computed from its OWN
# row count, sidestepping R Markdown's fixed per-chunk fig.height. Use this
# in place of print(compose_plot_grid(...)) inside any loop over domains/
# subgroups, where different sections have different numbers of panels.
# =============================================================================

# panel_width / panel_height are per-panel inches -- matches the convention
# already used in plot_reports.R's ggsave() calls (ncol * 5, nrow * 3.2), so
# grids look consistent whether they're saved to disk or embedded in a report.

render_plot_grid <- function(plots, ncol = 3, title = NULL, id = NULL,
                             output_dir = here::here("output", "figures"),
                             panel_width = 5, panel_height = 3.2, dpi = 200) {
  
  if (length(plots) == 0) return(invisible(NULL))
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  n_row <- ceiling(length(plots) / ncol)
  grid  <- compose_plot_grid(plots, ncol = ncol, title = title)
  if (is.null(grid)) return(invisible(NULL))
  
  safe_id  <- if (!is.null(id)) gsub("[^A-Za-z0-9_-]+", "_", id) else paste0("grid_", as.integer(Sys.time()))
  out_path <- file.path(output_dir, paste0(safe_id, ".png"))
  
  ggplot2::ggsave(out_path, grid,
                  width  = ncol * panel_width,
                  height = n_row * panel_height,
                  dpi = dpi, limitsize = FALSE)
  
  # cat(sprintf("\n\n![](%s)\n\n", out_path))
  
  # forward-slash-normalize the path either way -- LaTeX interprets a
  # backslash as an escape character, so a Windows-style path would break
  # compilation if one ever appears here.
  out_path_norm <- gsub("\\\\", "/", out_path)
  
  if (knitr::is_latex_output()) {
    cat(sprintf("\n\n\\begin{center}\\includegraphics[width=\\linewidth]{%s}\\end{center}\n\n",
                out_path_norm))
  } else {
    cat(sprintf("\n\n![](%s)\n\n", out_path_norm))
  }
  
}