set_labels <- function(data, phase = "1") {
  
  # Base variables applied to both phases
  data <- labelled::set_variable_labels(
    data,
    # Control variables
    age_group = "Age group",
    sexe = "Sex",
    locality = "Locality",
    vaccAJ = "Vaccination status",
    tAdm = "Type admission",
    is_breastfed = "Breastfeeding status",
    
    # Comorbidities
    morbidite = "Comorbidity",
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
    
    .strict = FALSE # Prevents errors if any variables above are missing
  )
  
  if (phase == "1") {
    data <- labelled::set_variable_labels(
      data,
      phase_length = "Phase length (Days)",
      f75_to_f75f = "F75 to F75F transition",
      edema_incident = "Incident edema",
      length_tubefeeding = "Tube feeding duration",
      
      nutri_treat_p1 = "Nutritional treatment (P1)",
      prop_whz_error_p1 = "WHZ error proportion (P1)",
      is_death_in_phase = "Death in phase",
      death_day1_in_phase = "Death on day 1 (P1)",
      
      # Independent variables
      prop_f75x_freq_less_p1 = "F75 frequency below target (P1)",
      prop_f75x_freq_more_p1 = "F75 frequency above target (P1)",
      
      prop_f75x_qty_less_p1 = "F75 quantity below target (P1)",
      prop_f75x_qty_more_p1 = "F75 quantity above target (P1)",
      
      prop_f75x_adh_less_p1 = "F75 adherence below target (P1)",
      prop_f75x_adh_more_p1 = "F75 adherence above target (P1)",
      
      prop_familymeals_p1 = "Family meals proportion (P1)",
      prop_food_intolerance_p1 = "Food intolerance proportion (P1)",
      nonresponse_p1 = "Non-response to treatment in Phase 1",
      
      .strict = FALSE
    )
    
  } else {
    data <- labelled::set_variable_labels(
      data,
      length_pt = "Length of stay Phase T",
      nutri_treat_pt = "Therapeutic food Phase T",
      death_pt = "Death on phase T",
      
      prop_f75x_freq_less_pt = "F75 frequency below target (PT)",
      prop_f75x_freq_more_pt = "F75 frequency above target (PT)",
      
      prop_f75x_qty_less_pt = "F75 quantity below target (PT)",
      prop_f75x_qty_more_pt = "F75 quantity above target (PT)",
      
      prop_f75x_adh_less_pt = "F75 adherence below target (PT)",
      prop_f75x_adh_more_pt = "F75 adherence above target (PT)",
      
      prop_familymeals_pt = "Family meals proportion (PT)",
      prop_food_intolerance_pt = "Food intolerance proportion (PT)",
      nonresponse_pt = "Non-response to treatment in Phase T",
      
      .strict = FALSE
    )
  }
  
  return(data)
}