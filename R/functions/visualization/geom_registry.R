# =============================================================================
# geom_registry.R — swappable rendering, keyed by statistical SHAPE (not
# chart_type), so a family like distribution_by_group is reusable across
# multiple panel builders (categorical-vs-numeric AND continuous-vs-categorical).
#
# NOTE: this file defines the registry and its base entries. Do not add
# entries directly elsewhere — if a new geom variant belongs to an existing
# family, add it here; if it needs a new family, add the family here too.
# =============================================================================

geom_registry <- list(

  proportion = list(
    default  = "bar",
    bar      = function(mapping, dodge = position_identity(), ...)
      geom_col(mapping, position = dodge, width = 0.6, alpha = 0.85, ...),
    lollipop = function(mapping, dodge = position_identity(), ...)
      list(
        geom_segment(mapping, aes(xend = after_stat(x), yend = 0),
                     position = dodge, linewidth = 0.6, ...),
        geom_point(mapping, position = dodge, size = 2.5, ...)
      )
  ),

  distribution_by_group = list(
    default = "boxplot",
    boxplot = function(mapping, ...)
      geom_boxplot(mapping, alpha = 0.85, width = 0.6, outlier.size = 0.8, na.rm = TRUE, ...),
    violin  = function(mapping, ...)
      geom_violin(mapping, alpha = 0.85, trim = FALSE, na.rm = TRUE, ...)
  ),

  univariate_distribution = list(
    default   = "histogram",
    histogram = function(mapping, ...)
      geom_histogram(mapping, bins = 30, alpha = 0.85, fill = "#66C2A5", ...),
    density   = function(mapping, ...)
      geom_density(mapping, alpha = 0.6, fill = "#66C2A5", ...)
  ),

  bivariate_numeric = list(
    default = "scatter",
    scatter = function(mapping, ...)
      list(
        geom_point(mapping, alpha = 0.4, size = 1.2, color = "#66C2A5", ...),
        geom_smooth(mapping, method = "loess", se = FALSE, color = "grey30", linewidth = 0.6)
      ),
    hexbin  = function(mapping, ...) geom_hex(mapping, bins = 25, ...)
  )
)

.resolve_geom <- function(family, geom = NULL) {
  choices <- geom_registry[[family]]
  if (is.null(choices)) {
    stop("Unknown geom family: '", family, "'. Available: ",
         paste(names(geom_registry), collapse = ", "), call. = FALSE)
  }

  geom_name <- geom %||% choices$default
  fn <- choices[[geom_name]]
  if (is.null(fn)) {
    stop("Unknown geom '", geom_name, "' for family '", family,
         "'. Available: ", paste(setdiff(names(choices), "default"), collapse = ", "),
         call. = FALSE)
  }
  fn
}

register_geom <- function(family, name, fn) {
  if (is.null(geom_registry[[family]])) {
    geom_registry[[family]] <<- list(default = name)
  }
  geom_registry[[family]][[name]] <<- fn
  invisible(NULL)
}
