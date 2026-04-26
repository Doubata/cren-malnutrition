source(here::here("scripts", "01_data_cleaning", "01_shared_transform.R"))

child_e6a59sO <- child_clean %>%
  
  # ---- Filter  ----
  
# by age range
filter(!is.na(age_range_6_59)) %>%
  
  # by edema status (handling NAs explicitly)
  filter(has_edema == FALSE) %>%
  
  # by complex physical and Z-score criteria
  filter(weight >= 3.5, whz %in% c("-3", "-4") | (muac == "pb" & muac_val < 115)) %>%
  
  # by rows where phase starts with "1"
  filter(str_starts(phase_series, "1")) %>%
  
  #  ---- Compute Indicators ----
mutate(

  # . Length of stay [days] in phase 1  ####
  length_p1 = get_series_length_at(
    df         = monitoring_clean, 
    id_col     = "id", 
    target_col= "phase",
    run_order  = 1, 
    ids        = id
  ),
  
  f75_to_f75f_p1 = detect_state_transition(
    df         = monitoring_clean, 
    id_col     = "id", 
    phase_col  = "phase", 
    target_col = "typeAlim",
    from       = "F75", 
    to         = "F75F",
    ids        = id,
    run_order  = 1
  ),
  
  # . Appearance of oedema in phase 1  ####
  edema_incident_p1 = detect_state_transition(
    df         = monitoring_clean, 
    id_col     = "id", 
    phase_col  = "phase", 
    target_col = "oedemes",
    from       = c("MISSING", NA, 0),
    ids        = id,
    run_order  = 1
  ),
  
  # . Duration [days] of tube feeding in phase 1  ####
  length_tubefeeding_p1 = summarise_event(
    df          = monitoring_clean, 
    id_col      = "id", 
    phase_col   = "phase", 
    event       = "sonde",
    target_cols = c("repasH1", "repasH2", "repasH3", "repasH4", "repasH5",
                    "repasH6", "repasH7", "repasH8", "repasH9", "repasH10"),
    ids         = id,
    run_order   = 1, 
    type        = "any"
  ),
  
  # . Death in phase 1 ####
  death_p1 = (phaseDeces %in% "1"),
  # . Death in the first day of phase 1  ####
  death_day1_p1 = (death_p1 == TRUE & length_p1 == 1),
  
  # . Failure Nutritional Treatment Phase 1 ####
  has_failed_p1 = case_when(
    # If at least one is TRUE
    if_any(c(f75_to_f75f_p1, edema_incident_p1, death_p1, death_day1_p1), isTRUE) | 
      length_p1 > 4 | length_tubefeeding_p1 > 4 ~ TRUE,
    
    # If all are NA
    if_all(c(f75_to_f75f_p1, edema_incident_p1, death_p1, death_day1_p1, 
             length_p1, length_tubefeeding_p1), is.na) ~ NA,
    
    # Otherwise
    .default = FALSE
  ),
  
  # . Nutritional treatment during phase 1 ####
  nutri_treat_series = get_matching_series_str(
    df        = monitoring_clean, 
    id_col    = "id",
    target_col   = "typeAlim", 
    ref_col   = "phase", 
    attr_col  = "ATPE",
    run_order = 1, 
    ids       = id
  ),
  
  # . Generalization Of Nutritional treatment during phase 1 ####
  nutri_treat_p1 = case_when(
    # NA: Exactly MISSING
    nutri_treat_series == "MISSING" ~ NA_character_,
    
    # F75 Category: Only F75 and MISSING allowed
    # Regex: Start and end with any combo of F75 and MISSING separated by hyphens
    str_detect(nutri_treat_series, "^(F75|MISSING)(-(F75|MISSING))*$") ~ "F75",
    
    # F75F Category: Only F75F and MISSING allowed
    # Regex: Start and end with any combo of F75F and MISSING
    str_detect(nutri_treat_series, "^(F75F|MISSING)(-(F75F|MISSING))*$") ~ "F75F",
    
    # F75-F75F Category:
    # Starts with F75-F75F (or F75-MISSING-F75F) AND no F100/F100d
    (str_starts(nutri_treat_series, "F75-F75F") | nutri_treat_series == "F75-MISSING-F75F") & 
      !str_detect(nutri_treat_series, "F100") ~ "F75-F75F",
    
    # F75F-F75 Category:
    # Starts with F75F-F75 (or F75F-MISSING-F75) AND no F100/F100d
    (str_starts(nutri_treat_series, "F75F-F75") | nutri_treat_series == "F75F-MISSING-F75") & 
      !str_detect(nutri_treat_series, "F100") ~ "F75F-F75",
    
    # Default
    TRUE ~ "Others"
  ),
  
  prop_whz_error_p1 = summarise_event(
    df          = monitoring_clean,
    id_col      = "id",
    phase_col   = "phase",
    event       = "Not Equal",
    target_cols = "whz_validation",
    ids         = id,
    run_order   = 1,
    type        = "proportion"
  ),
  
  prop_f75x_freq_less_p1 = summarise_event(
    df          = monitoring_clean,
    id_col      = "id",
    phase_col   = "phase",
    event       = "less",
    target_cols = "f75x_freq_validation",
    ids         = id,
    run_order   = 1,
    type        = "proportion"
  ),
  
  # . Proportion Frequency F75/F75F more  ####
  prop_f75x_freq_more_p1 = summarise_event(
    df          = monitoring_clean,
    id_col      = "id",
    phase_col   = "phase",
    event       = "more",
    target_cols = "f75x_freq_validation",
    ids         = id,
    run_order   = 1, 
    type        = "proportion"
  ),
    
  # . Proportion Quantity F75/F75F less  ####
  prop_f75x_qty_less_p1 = summarise_event(
    df          = monitoring_clean, 
    id_col      = "id", 
    phase_col   = "phase",
    event       = "less",
    target_cols = "f75x_qty_validation",
    ids         = id,
    run_order   = 1,
    type        = "proportion"
  ),
    
  # . Proportion Quantity F75/F75F more  ####
  prop_f75x_qty_more_p1 = summarise_event(
    df          = monitoring_clean, 
    id_col      = "id", 
    phase_col   = "phase",
    event       = "more",
    target_cols = "f75x_qty_validation",
    ids         = id,
    run_order   = 1, 
    type        = "proportion"
  ),
    
  # . Proportion Adherence F75/F75F less  ####
  prop_f75x_adh_less_p1 = summarise_event(
    df          = monitoring_clean, 
    id_col      = "id", 
    phase_col   = "phase",
    event       = "less", 
    target_cols = "f75x_adh_validation",
    ids         = id,
    run_order   = 1, 
    type        = "proportion"
  ),
    
  # . Proportion Adherence F75/F75F more  ####
  prop_f75x_adh_more_p1 = summarise_event(
    df          = monitoring_clean, 
    id_col      = "id", 
    phase_col   = "phase",
    event       = "more", 
    target_cols = "f75x_adh_validation",
    ids         = id,
    run_order   = 1, 
    type        = "proportion"
  ),
  
  # . Proportion of Consumption of family meals in phase 1  ####
  prop_familymeals_p1 = summarise_event(
    df          = monitoring_clean,
    id_col      = "id",
    phase_col   = "phase",
    event       = "oui",
    target_cols = "plaFamilCorrige",
    ids         = id,
    run_order   = 1,
    type        = "proportion"
  ),
  
  prop_food_intolerance_p1 = summarise_event(
    df          = monitoring_clean,
    id_col      = "id",
    phase_col   = "phase",
    event       = "TRUE",
    target_cols = "food_intolerance",
    ids         = id,
    run_order   = 1,
    type        = "proportion"
  )
  
)

###############################
##  Phase de stabilisation  ###
###############################
child_e6a59sO <- set_variable_labels(
  child_e6a59sO,
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
  prop_whz_error_p1 = "WFH Zscore Determining Error [%]", # To be completed
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
  has_hiv = "VIH"
)

