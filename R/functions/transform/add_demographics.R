
add_demographics <- function(profile_data){
  
  profile_data_enriched <- profile_data |>
    
    # Age groups and status definition ------------------------------------------
  mutate(
    
    age_month = ageMois,
    
    # Create the primary age group
    age_group = case_when(
      age_month < 6                   ~ "[0;6[",
      age_month >= 6  & age_month  < 25 ~ "[6;25[",
      age_month >= 25 & age_month <= 59 ~ "[25;59]",
      age_month > 59                  ~ "]59;plus["
    ),
    
    # Create subsets dynamically based on the first variable
    age_under_6     = if_else(age_month < 6, age_group, NA_character_),
    age_range_6_59  = if_else(age_month >= 6 & age_month <= 59, age_group, NA_character_)
  ) |>
    select(-ageMois)
  
  return(profile_data_enriched)
}
    