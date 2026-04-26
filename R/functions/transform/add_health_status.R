add_health_status <- function(child_df, morbidity_df){
  
  df_enriched <- child_df %>%
    
    left_join(morbidity_df , by = "id") %>%
    
    mutate(
      
      # --- Breastfeeding Status --------------------------------------------
      is_breastfed = case_when(
        allait == "oui" ~ TRUE,
        allait == "non" ~ FALSE,
        TRUE            ~ NA
      ),
      
      # . Sequence of Phases during the stay ####
      phase_series = get_series_str(
        df         = morbidity_df,
        id_col     = "id",
        target_col = "phase",
        ids        = id
      ),
      
      # Edema status
      has_edema = (coalesce(edema, 0) >= 1),
      
      is_vaccinated  = case_when(vaccAJ == "oui" ~ TRUE, vaccAJ == "non" ~ FALSE, TRUE ~ NA),
      
      # Handle composite variables
      morbidite      = rowSums(across(c(gastroenteriteAigue, deshydratation, choc, pneumopathieViraleBacterienne,
                                        paluGraveAnemique, paluGraveNeurologique, autrePaluGrave, 
                                        candidoseDigestive, tuberculose, VIH)), na.rm = TRUE),
      
      has_morbidite  = morbidite >= 1,
      
      severe_malaria = rowSums(across(c(paluGraveAnemique, paluGraveNeurologique, autrePaluGrave)), na.rm = TRUE),
      
      has_severe_malaria   = severe_malaria >= 1,
      
      has_gastroenteritis  = (gastroenteriteAigue  == 1 | deshydratation == 1),
      
      has_severe_anaemia   = (paluGraveAnemique    == 1 | autreAnemie    == 1),
      
      # Dynamic logical conversion of specific columns and converts 1 to TRUE, 0 to FALSE
      across(c(choc, pneumopathieViraleBacterienne, paluGraveAnemique,paluGraveNeurologique,
               autrePaluGrave, autreAnemie, candidoseDigestive, tuberculose, VIH),
             ~ .x == 1, .names = "{CONFIG$morbidity_columns_map[.col]}"),
    )
    
  return(df_enriched)
}

