# =============================================================================
# panel_continuous.R — panel builder for continuous/numeric variables.
#
# Branching convention (shared across all panel_*.R files):
#   has_outcome == FALSE                    -> unstratified distribution
#   has_outcome, outcome_type == "numeric"     -> bivariate numeric comparison
#   has_outcome, outcome_type == "categorical" -> grouped comparison + test
# =============================================================================

.continuous_panel <- function(data, var, label, outcome, outcome_type, outcome_label = NULL, geom = NULL, ...) {
  
  has_outcome <- !is.null(outcome)
  
  if (has_outcome && outcome_type == "categorical") {
    
    # Numeric variable across outcome groups -> boxplot/violin + Kruskal-Wallis
    kw    <- suppressWarnings(kruskal.test(data[[var]] ~ data[[outcome]]))
    p_lab <- if (kw$p.value < 0.001) "KW p < 0.001" else
      paste0("KW p = ", round(kw$p.value, 3))
    
    render_geom <- .resolve_geom("distribution_by_group", geom)
    
    p <- ggplot(data, aes(x = factor(.data[[outcome]]), y = .data[[var]], fill = factor(.data[[outcome]]))) +
      render_geom(aes(fill = factor(.data[[outcome]]))) +
      scale_fill_brewer(palette = "Set2") +
      labs(x = outcome_label %||% outcome, fill = outcome_label %||% outcome)
    
  } else if (has_outcome && outcome_type == "numeric") {
    
    # Numeric variable vs numeric outcome -> scatter/hexbin + Spearman correlation
    ct    <- suppressWarnings(cor.test(data[[var]], data[[outcome]], method = "spearman"))
    p_lab <- paste0("rho = ", round(ct$estimate, 2), ", p = ",
                    if (ct$p.value < 0.001) "< 0.001" else round(ct$p.value, 3))
    
    render_geom <- .resolve_geom("bivariate_numeric", geom)
    
    p <- ggplot(data, aes(x = .data[[var]], y = .data[[outcome]])) +
      render_geom(aes(x = .data[[var]], y = .data[[outcome]])) +
      labs(x = NULL, y = outcome_label %||% outcome)
    
  } else {
    
    # Unstratified: distribution only
    p_lab <- NULL
    render_geom <- .resolve_geom("univariate_distribution", geom)
    
    p <- ggplot(data, aes(x = .data[[var]])) +
      render_geom(aes(x = .data[[var]])) +
      labs(x = NULL, y = "Count")
  }
  
  p +
    labs(title = label, subtitle = p_lab) +
    theme_minimal(base_size = 10) +
    theme(legend.position = if (has_outcome && outcome_type == "categorical") "bottom" else "none",
          plot.title    = element_text(face = "bold", size = 10),
          plot.subtitle = element_text(size = 8, color = "grey40"))
}

register_chart_builder("continuous", .continuous_panel)