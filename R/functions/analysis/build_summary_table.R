
build_summary_table <- function(data, X, y, fisher_vars = NULL){
  
  # Build base test config: NULL defaults to standard gtsummary auto-tests for all X
  test_cfg      <- if (!is.null(fisher_vars)) list(any_of(fisher_vars) ~ "fisher.test") else NULL
  test_args_cfg <- if (!is.null(fisher_vars)) list(any_of(fisher_vars) ~ list(simulate.p.value = TRUE)) else NULL
  
  summary_table <- data |>
    
    tbl_summary(
      by             = all_of(y),
      type           = list(where(is.numeric) ~ "continuous2"),
      include        = all_of(X),
      statistic      = list(all_categorical() ~ "{n} ({p}%)",
                            all_continuous() ~ c("{median} ({p25} ; {p75})", "{min} ; {max}")),
      missing        = "ifany",
      missing_text   = "Missing values",
      
      digits         = list(
        all_categorical() ~ c(0, 1),
        all_continuous() ~ c(0, 1, 0)
      )
    ) |>
    add_overall(last = TRUE) |>
    add_p(
      test = test_cfg,
      test.args = test_args_cfg,
      pvalue_fun = ~gtsummary::style_pvalue(.x, digits = 3)
    ) |>
    add_difference()  |>
    add_q() |>
    add_significance_stars(hide_p = F, hide_se = T, hide_ci = T) |>
    # add_overall(last = TRUE) |>
    modify_spanning_header(all_stat_cols() ~ "Non-response to treatment in Phase") |>
    bold_p() |>
    bold_labels() |>
    italicize_levels()
    
  return(summary_table)
}
