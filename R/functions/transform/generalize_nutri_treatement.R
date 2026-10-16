
generalize_nutri_treatement <- function(x){
  # Pre-clean string for robust matching
  clean_nutri_treat_series = clean_treat_series(x)
  
  nutri_treat = case_when(
    # Complete missing / empty after cleaning
    clean_nutri_treat_series == "" | is.na(clean_nutri_treat_series) ~ NA_character_,
    
    # Exact Single Formula Types (allowing +RUTF)
    clean_nutri_treat_series %in% c("F75", "F75+RUTF", "F75F", "F75F+RUTF")   ~ clean_nutri_treat_series,
    # clean_nutri_treat_series %in% c("F75F", "F75F+RUTF") ~ clean_nutri_treat_series,
    clean_nutri_treat_series %in% c("F100", "F100+RUTF") ~ "F100",
    clean_nutri_treat_series == "F100d"                  ~ "F100d",
    
    # Direct Transitions
    clean_nutri_treat_series %in% c("F75-F75F", "F75-F75F+RUTF") ~ "F75-F75F",
    clean_nutri_treat_series %in% c("F75F-F75", "F75F-F75+RUTF") ~ "F75F-F75",
    clean_nutri_treat_series %in% c("F75-F100", "F75F-F100") ~ "F75/F75F-F100",
    clean_nutri_treat_series %in% c("F75+RUTF-F100", "F75F+RUTF-F100") ~ "F75/F75F+RUTF-F100",
    
    # Complex Sequences matching target map
    str_detect(clean_nutri_treat_series, "^F75\\+RUTF-F75F") ~ "F75+RUTF-F75F",
    str_detect(clean_nutri_treat_series, "^F75F\\+RUTF-F75")  ~ "F75F+RUTF-F75",
    
    # Catch-all
    TRUE ~ "Others"
  )
  
  nutri_treat = factor(
    nutri_treat,
    levels = c("F75", "F75F", "F75-F75F", "F75F-F75",
               "F75+RUTF", "F75F+RUTF", "F75+RUTF-F75F", "F75F+RUTF-F75",
               "F75/F75F-F100", "F75/F75F+RUTF-F100", "F100", "F100d", "Others")
  )
  
  return(nutri_treat)
}