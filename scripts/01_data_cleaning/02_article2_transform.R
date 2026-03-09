source(here::here("scripts", "00_setup.R"))
source(here::here("scripts", "01_data_cleaning", "01_shared_transform.R"))

child_e6a59sO <- child_clean %>%
  
  # ---- Filter  ----
  
# by age range
filter(!is.na(age_range_6_59)) %>%
  
  # by edema status (handling NAs explicitly)
  filter(has_edema == FALSE) %>%
  
  # by complex physical and Z-score criteria
  filter(weight >= 3.5, whz %in% c("inf-3", "inf-4", "-4") | (muac == "pb" & muac_val < 115)) %>%
  
  # by rows where phase starts with "1"
  filter(str_starts(phase_series, "1")) %>%
  
  #  ---- Compute Indicators ----
mutate(

  # . Length of stay [days] in phase 1  ####
  length_p1 = get_series_length_at(
    df        = monitoring_clean, 
    id_col    = "id", 
    val_col   = "phase",
    run_order = 1, 
    ids       = id
  ),
  
  # . Change in nutritional treatment [F75 to F75F] in phase 1  ####
  f75_to_f75f_p1 = detect_changes(
    df        = monitoring_clean, 
    id_col    = "id", 
    phase_col = "phase", 
    val_col   = "typeAlim",
    from      = c("F75"), 
    to        = c("F75F"), 
    run_order = 1, 
    ids       = id
  ),
  
  # . Appearance of oedema in phase 1  ####
  edema_incident_p1 = detect_changes(
    df        = monitoring_clean, 
    id_col    = "id", 
    phase_col = "phase", 
    val_col   = "oedemes",
    from      = c("MISSING", NA, 0), 
    run_order = 1,
    ids       = id
  ),
  
  # . Duration [days] of tube feeding in phase 1  ####
  length_tubefeeding_p1 = get_event_length(
    df        = monitoring_clean, 
    id_col    = "id", 
    phase_col = "phase", 
    event     = "sonde",
    col_names = c("repasH1", "repasH2", "repasH3", "repasH4", "repasH5",
                  "repasH6", "repasH7", "repasH8", "repasH9", "repasH10"),
    run_order = 1, 
    func      = "any", 
    ids       = id
  ),
  
  # . Duration [days] of tube feeding in phase 1  ####
  length_tubefeeding_p1 = get_event_length(
    df        = monitoring_clean, 
    id_col    = "id", 
    phase_col = "phase", 
    event     = "sonde",
    col_names = c("repasH1", "repasH2", "repasH3", "repasH4", "repasH5",
                  "repasH6", "repasH7", "repasH8", "repasH9", "repasH10"),
    run_order = 1, 
    func      = "any", 
    ids       = id
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
  nutri_treat_series = extract_matching_series_str(
    df        = monitoring_clean, 
    id_col    = "id",
    val_col   = "typeAlim", 
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
  
  # . Proportion Frequency F75/F75F less ####
  prop_f75x_freq_less_p1  = get_flag_proportion(
    df             = monitoring_clean,
    id_col         = "id",
    phase_col      = "phase",
    validation_col = "f75x_freq_validation",
    flag           = "less",
    run_order      = 1, 
    ids            = id
  ),
    
  # . Proportion Frequency F75/F75F more  ####
  prop_f75x_freq_more_p1  = get_flag_proportion(
    df             = monitoring_clean,
    id_col         = "id",
    phase_col      = "phase",
    validation_col = "f75x_freq_validation",
    flag           = "more",
    run_order      = 1, 
    ids            = id
  ),
    
  # . Proportion Quantity F75/F75F less  ####
  prop_f75x_qty_less_p1  = get_flag_proportion(
    df             = monitoring_clean, 
    id_col         = "id", 
    phase_col      = "phase",
    validation_col = "f75x_qty_validation",
    flag           = "less",
    run_order      = 1,
    ids            = id
    ),
    
  # . Proportion Quantity F75/F75F more  ####
  prop_f75x_qty_more_p1  = get_flag_proportion(
    df             = monitoring_clean, 
    id_col         = "id", 
    phase_col      = "phase",
    validation_col = "f75x_qty_validation",
    flag           = "more", 
    run_order      = 1, 
    ids            = id
  ),
    
  # . Proportion Adherence F75/F75F less  ####
  prop_f75x_adh_less_p1  = get_flag_proportion(
    df             = monitoring_clean, 
    id_col         = "id", 
    phase_col      = "phase",
    validation_col = "f75x_adh_validation",
    flag           = "less", 
    run_order      = 1, 
    ids            = id
  ),
    
  # . Proportion Adherence F75/F75F more  ####
  prop_f75x_adh_more_p1  = get_flag_proportion(
    df             = monitoring_clean, 
    id_col         = "id", 
    phase_col      = "phase",
    validation_col = "f75x_adh_validation",
    flag           = "more", 
    run_order      = 1, 
    ids            = id
  )
  
)

# summary(child_e6a59sO$has_failed_p1)
# glimpse(child_e6a59sO)



# library(gtsummary)
# library(labelled)

################################
###  Phase de stabilisation  ###
################################
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
  zscoreError = "Determining WFH Zscore", # To be completed
  nutri_treat_p1 = "Therapeutic food Phase 1",

  #
  prop_f75x_freq_less_p1 = "F75/F75F Frequency Inadequacy [%]",
  prop_f75x_freq_more_p1 = "F75/F75F Frequency Excess [%]",
  
  prop_f75x_qty_less_p1 = "F75/F75F Quantity Inadequacy [%]",
  prop_f75x_qty_more_p1 = "F75/F75F Quantity Excess [%]",
  
  prop_f75x_adh_less_p1 = "F75/F75F Intake Inadequacy [%]",
  prop_f75x_adh_more_p1 = "F75/F75F Intake Excess [%]",

  # plaFamilPropOui = "Consumption of familial foods",
  # plaFamilPropOui.2 = "Consumption of familial foods",
  # mauvTolerancePropOui = "Poor tolerance",
  
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

