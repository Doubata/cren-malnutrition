# =============================================================================
# panel_categorical.R — panel builder for categorical variables.
#
# Branching convention (shared across all panel_*.R files):
#   has_outcome == FALSE                    -> unstratified distribution
#   has_outcome, outcome_type == "numeric"     -> grouped numeric comparison
#   has_outcome, outcome_type == "categorical" -> grouped comparison + test
# =============================================================================

.categorical_panel <- function(data, var, label, outcome, outcome_type, outcome_label = NULL, geom = NULL, ...) {
  
  has_outcome <- !is.null(outcome)
  
  if (has_outcome && outcome_type == "categorical") {
    
    # Column %: distribution of the variable within each outcome class
    tbl   <- table(data[[var]], data[[outcome]])
    chi   <- suppressWarnings(chisq.test(tbl, simulate.p.value = TRUE))
    p_lab <- if (chi$p.value < 0.001) "X2 p < 0.001" else
      paste0("X2 p = ", round(chi$p.value, 3))
    
    prop_df <- data |>
      count(.data[[var]], .data[[outcome]]) |>
      group_by(.data[[outcome]]) |>
      mutate(prop = n / sum(n)) |>
      ungroup() |>
      rename(Category = 1, Outcome = 2)
    
    dodge       <- position_dodge2(width = 0.7, preserve = "single")
    render_geom <- .resolve_geom("proportion", geom)
    
    p <- ggplot(prop_df, aes(x = reorder(Category, prop), y = prop, fill = factor(Outcome))) +
      render_geom(aes(fill = factor(Outcome)), dodge) +
      geom_text(aes(label = scales::percent(prop, accuracy = 1)),
                position = dodge, vjust = -0.4, size = 3, color = "grey30") +
      scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1), expand = expansion(mult = c(0.02, 0.05))) +
      scale_fill_brewer(palette = "Set2") +
      labs(y = NULL, fill = outcome_label %||% outcome)
    
  } else if (has_outcome && outcome_type == "numeric") {
    
    # Categorical predictor vs numeric outcome -> boxplot/violin + Kruskal-Wallis
    kw    <- suppressWarnings(kruskal.test(data[[outcome]] ~ data[[var]]))
    p_lab <- if (kw$p.value < 0.001) "KW p < 0.001" else
      paste0("KW p = ", round(kw$p.value, 3))
    
    render_geom <- .resolve_geom("distribution_by_group", geom)
    
    p <- ggplot(data, aes(x = reorder(.data[[var]], .data[[outcome]], median, na.rm = TRUE),
                          y = .data[[outcome]], fill = .data[[var]])) +
      render_geom(aes(fill = .data[[var]])) +
      scale_fill_brewer(palette = "Set2") +
      labs(y = outcome_label %||% outcome, fill = NULL) +
      theme(legend.position = "none")
    
  } else {
    
    # Unstratified: overall proportion per category
    p_lab <- NULL
    prop_df <- data |> count(.data[[var]]) |> mutate(prop = n / sum(n)) |> rename(Category = 1)
    
    render_geom <- .resolve_geom("proportion", geom)
    
    p <- ggplot(prop_df, aes(x = reorder(Category, prop), y = prop, fill = "All patients")) +
      render_geom(aes(fill = "All patients")) +
      geom_text(aes(label = scales::percent(prop, accuracy = 1)),
                vjust = -0.4, size = 3, color = "grey30") +
      scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1), expand = expansion(mult = c(0.02, 0.05))) +
      scale_fill_brewer(palette = "Set2") +
      labs(y = NULL, fill = NULL)
  }
  
  p +
    coord_flip() +
    scale_x_discrete(labels = scales::label_wrap(20)) +
    labs(title = label, subtitle = p_lab, x = NULL) +
    theme_minimal(base_size = 10) +
    theme(legend.position = if (has_outcome && outcome_type == "categorical") "bottom" else "none",
          plot.title    = element_text(face = "bold", size = 10),
          plot.subtitle = element_text(size = 8, color = "grey40"),
          axis.text.y   = element_text(size = 8))
}

register_chart_builder("categorical", .categorical_panel)