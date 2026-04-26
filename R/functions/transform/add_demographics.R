add_demographics <- function(child_df){
  
  df_enriched <- child_df %>%
    
    # Age groups and status definition ------------------------------------------
  mutate(
    
    locality = case_when(zoneResidence == "urbaine" ~ "Urban", zoneResidence == "rurale" ~ "Rural"),
    
    # Create the primary age group
    age_group = case_when(
      ageMois < 6                   ~ "[0;6[",
      ageMois >= 6  & ageMois  < 25 ~ "[6;25[",
      ageMois >= 25 & ageMois <= 59 ~ "[25;59]",
      ageMois > 59                  ~ "]59;plus["
    ),
    
    # Create subsets dynamically based on the first variable
    age_under_6     = if_else(ageMois <= 59, age_group, NA_character_),
    age_range_6_59  = if_else(ageMois >= 6 & ageMois <= 59, age_group, NA_character_)
  )
  
  return(df_enriched)
}
    