
add_health_status <- function(profile_data, follow_up_data, morbidity_data){
  
  profile_data_enriched <- profile_data |>
    
    left_join(morbidity_data , by = "id") |>
    
    mutate(
      
      # Sequence of Phases during the stay ####
      phase_series = get_series_str(
        df         = follow_up_data,
        id_col     = "id",
        target_col = "phase",
        ids        = id
      ),
      
      # Edema status
      has_edema = (coalesce(edema, 0) >= 1),
      
      # Handle composite variables
      morbidite      = rowSums(across(c(gastroenteriteAigue, deshydratation, choc, pneumopathieViraleBacterienne,
                                        paluGraveAnemique, paluGraveNeurologique, autrePaluGrave, 
                                        candidoseDigestive, tuberculose, VIH)), na.rm = TRUE),
      
      has_morbidite  = morbidite >= 1,
      
      severe_malaria = rowSums(across(c(paluGraveAnemique, paluGraveNeurologique, autrePaluGrave)), na.rm = TRUE),
      
      has_severe_malaria   = severe_malaria >= 1,
      
      has_gastroenteritis  = (gastroenteriteAigue  == 1 | deshydratation == 1),
      
      has_severe_anaemia   = autreAnemie    == 1,
      
      # Dynamic logical conversion of specific columns and converts 1 to TRUE, 0 to FALSE
      across(c(choc, pneumopathieViraleBacterienne, paluGraveAnemique,paluGraveNeurologique,
               autrePaluGrave, autreAnemie, candidoseDigestive, tuberculose, VIH),
             ~ .x == 1, .names = "{CONFIG$morbidity_columns_map[.col]}"),
    )
    
  return(profile_data_enriched)
}

