
set_c659_noOed_labels <- function(df){
  
  df <- set_variable_labels(
    df,
    # Variables de controle
    age_group = "Age group",
    sexe = "Sex",
    zoneResidence = "Residence",
    vaccAJ = "Vaccination for age",
    tAdm = "Admission statut",
    length_p1 = "Length of stay Phase 1",
    death_p1 = "Death on phase 1",
    death_day1_p1 = "Death on the first day of phase 1",
    
    # Variables independantes
    is_breastfed = "Breast feeding",
    prop_whz_error_p1 = "WFH Zscore Determining Error [%]",
    nutri_treat_p1 = "Therapeutic food Phase 1",
    
    #
    prop_f75x_freq_less_p1 = "F75/F75F Frequency Inadequacy [%]",
    prop_f75x_freq_more_p1 = "F75/F75F Frequency Excess [%]",
    
    prop_f75x_qty_less_p1 = "F75/F75F Quantity Inadequacy [%]",
    prop_f75x_qty_more_p1 = "F75/F75F Quantity Excess [%]",
    
    prop_f75x_adh_less_p1 = "F75/F75F Intake Inadequacy [%]",
    prop_f75x_adh_more_p1 = "F75/F75F Intake Excess [%]",
    
    prop_familymeals_p1 = "Consumption of familial foods [%]",
    prop_food_intolerance_p1  = "Food intolerance [%]",
    
    # Comorbidite
    morbidite = "Morbidity",
    has_gastroenteritis = "Gastroenteritis",
    has_shock = "Shock",
    has_pneumonia = "Pneumonia",
    has_severe_malaria_anemia = "Severe anemic form of malaria",
    has_neuro_malaria = "Neurological form of malaria",
    has_other_severe_malaria = "Other forms of malaria",
    has_severe_malaria = "Severe malaria (anemic and/or Neurological and/or Other)",
    has_other_anemia = "Anemia",
    has_severe_anaemia = "Anemia and/or Severe anemic form of malaria",
    has_digestive_candidiasis = "Digestive candidiasis",
    has_tuberculosis = "Tuberculosis",
    has_hiv = "VIH",
    
    nutritional_nonresponse_p1 = "Non-response to nutritional treatement in Phase 1"
  )

  return(df)
}