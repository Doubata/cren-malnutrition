
build_univariate_table <- function(data, X, y){
  
  univariate_tbl <- data |>
    
    tbl_uvregression(
      y = !!as.symbol(y),
      include = all_of(X),
      method = glm,
      method.args = list(family = binomial),
      exponentiate = TRUE,
    ) |>
    add_n() |>
    add_nevent(location = "level") |>
    add_global_p(
      test = "car::Anova",
      type = "III",
      test.statistic = "LR",
      pvalue_fun = ~style_pvalue(.x, digits = 3)
    ) |>
    modify_column_hide("stat_n") |>
    sort_p() |>
    bold_p() |>
    bold_labels() |>
    italicize_levels() |>
    modify_header(label = "**Predictor**")
    # modify_caption(caption)
  
  return(univariate_tbl)
}
