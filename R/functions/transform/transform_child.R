# --- Prepare & Harmonize Child Data  ---
# Purpose: Sorting, Extract the data of Nutritional Indicators, joins, reshaping, derived datasets

transform_child <- function(child_df, monitoring_df){
  
  child_final <- child_df %>%
    
    arrange(id) %>%
    
    # Age groups and status definition ------------------------------------------
  mutate(
    
    # Create the primary age group
    age_group = case_when(
      ageMois < 6                   ~ "[0;6[",
      ageMois >= 6  & ageMois  < 25 ~ "[6;25[",
      ageMois >= 25 & ageMois <= 59 ~ "[25;59]",
      ageMois > 59                  ~ "]59;plus[",
    ),
    
    # Create subsets dynamically based on the first variable
    age_under_6     = if_else(ageMois <= 59, age_group, NA_character_),
    age_range_6_59  = if_else(ageMois >= 6 & ageMois <= 59, age_group, NA_character_),
    
    # Character to Yes/No Factor
    is_breastfed   = case_when(allait == "oui" ~ TRUE, allait == "non" ~ FALSE, TRUE ~ NA),
    is_vaccinated  = case_when(vaccAJ == "oui" ~ TRUE, vaccAJ == "non" ~ FALSE, TRUE ~ NA),
  ) %>%
    
    # Formatting for factors  ---------------------------------------------------
  # Turn every logical column into a factor
  mutate(
    across(where(is.logical), ~ factor(.x, levels = c(FALSE, TRUE))),
    across(starts_with("age_"), ~ factor(.x, levels = c("[0;6[", "[6;25[", "[25;59]", "]59;plus[")))
  )

  return(child_final)
}

