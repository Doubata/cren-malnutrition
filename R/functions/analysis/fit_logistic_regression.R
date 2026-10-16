
fit_logistic_regression_model <- function(data, X, y){
  
  data.narm <- e6a59sOC1P1 %>% tidyr::drop_na(all_of(c(y,X)))
  
  glm_model <- glm(!!as.symbol(y)~X, data = data.narm, family = binomial)
  
  ### Ajustement du modele logistique binaire
  glm_model_final <- step(glm_model)
  
  # reg2.e6a59sOC1P1 %>%
  #   tbl_regression(exponentiate = TRUE) %>%
  #   add_significance_stars() %>%
  #   modify_column_unhide(c("ci", "p.value")) %>%
  #   add_nevent(location = "level") %>%
  #   bold_labels()
  # car::Anova(reg2.e6a59sOC1P1)
  
  return(glm_model_final)
}


