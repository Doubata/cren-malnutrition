
add_comorbidity_estimates <- function(comorbidity_data){
  
  comorbidity_map <- c(
    "choc"                          = "has_shock",
    "pneumopathieViraleBacterienne" = "has_pneumonia",
    "paluGraveAnemique"             = "has_severe_malaria_anemia",
    "paluGraveNeurologique"         = "has_neuro_malaria",
    "autrePaluGrave"                = "has_other_severe_malaria",
    "autreAnemie"                   = "has_other_anemia",
    "candidoseDigestive"            = "has_digestive_candidiasis",
    "tuberculose"                   = "has_tuberculosis",
    "VIH"                           = "has_hiv"
  )
  
  comorbidity_data_enriched <- comorbidity_data %>%
    
    # Re-coding of morbidity variables ------------------------------------------
  mutate(
    # Handle composite variables
    morbidite      = rowSums(across(c(gastroenteriteAigue, deshydratation, choc, pneumopathieViraleBacterienne,
                                      paluGraveAnemique, paluGraveNeurologique, autrePaluGrave, 
                                      candidoseDigestive, tuberculose, VIH)), na.rm = TRUE),
    
    severe_malaria = rowSums(across(c(paluGraveAnemique, paluGraveNeurologique, autrePaluGrave)), na.rm = TRUE),
    
    # Dynamic logical conversion of specific columns and converts 1 to TRUE, 0 to FALSE
    across(c(choc, pneumopathieViraleBacterienne, paluGraveAnemique,paluGraveNeurologique,
             autrePaluGrave, autreAnemie, candidoseDigestive, tuberculose, VIH),
           ~ .x == 1, .names = "{comorbidity_map[.col]}"),
    
    # Handle the OR logic for combined categories
    has_morbidite        = morbidite             >= 1,
    has_severe_malaria   = severe_malaria        >= 1,
    has_gastroenteritis  = (gastroenteriteAigue  == 1 | deshydratation == 1),
    has_severe_anaemia   = (paluGraveAnemique    == 1 | autreAnemie    == 1)
  ) %>%
    
    return(comorbidity_data_enriched)
}


