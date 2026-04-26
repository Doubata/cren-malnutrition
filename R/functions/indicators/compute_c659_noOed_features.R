compute_c659_noOed_features <- function(child_df, monitoring_df){
  
  child_df %>%
    #  ---- Compute Indicators ----
  mutate(
    
    # . Length of stay [days] in phase 1  ####
    phase_length = get_series_length_at(
      df         = monitoring_df, 
      id_col     = "id", 
      target_col = "phase",
      run_order  = 1, 
      ids        = id
    ),
    
    f75_to_f75f = detect_state_transition(
      df         = monitoring_df, 
      id_col     = "id", 
      phase_col  = "phase", 
      target_col = "typeAlim",
      from       = "F75", 
      to         = "F75F",
      ids        = id,
      run_order  = 1
    ),
    
    # . Appearance of oedema in phase 1  ####
    edema_incident = detect_state_transition(
      df         = monitoring_df, 
      id_col     = "id", 
      phase_col  = "phase", 
      target_col = "oedemes",
      from       = c("MISSING", NA, 0),
      ids        = id,
      run_order  = 1
    ),
    
    # . Duration [days] of tube feeding in phase 1  ####
    length_tubefeeding = summarise_event(
      df          = monitoring_df, 
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
    is_death_in_phase = (phaseDeces %in% "1"),
    # . Death in the first day of phase 1  ####
    death_day1_in_phase = (is_death_in_phase == TRUE & phase_length == 1),
    
    # . Failure Nutritional Treatment Phase 1 ####
    fail_nutri_phase1 = case_when(
      # If at least one is TRUE
      if_any(c(f75_to_f75f, edema_incident, is_death_in_phase, death_day1_in_phase), isTRUE) | 
        phase_length > 4 | length_tubefeeding > 4 ~ TRUE,
      
      # If all are NA
      if_all(c(f75_to_f75f, edema_incident, is_death_in_phase, death_day1_in_phase, 
               phase_length, length_tubefeeding), is.na) ~ NA,
      
      # Otherwise
      .default = FALSE
    ),
    
    # . Nutritional treatment during phase 1 ####
    nutri_treat_series = get_matching_series_str(
      df        = monitoring_df, 
      id_col    = "id",
      target_col   = "typeAlim", 
      ref_col   = "phase", 
      attr_col  = "ATPE",
      run_order = 1, 
      ids       = id
    ),
    
    # . Generalization Of Nutritional treatment during phase 1 ####
    nutri_treat_phase1 = case_when(
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
    
    # whz_validation = case_when(is.na(whz) | is.na(whz_who) ~ NA, whz == whz_who ~ "Equal", TRUE ~ "Not Equal"),
    
    prop_whz_error = summarise_event(
      df          = monitoring_df,
      id_col      = "id",
      phase_col   = "phase",
      event       = "Not Equal",
      target_cols = "whz_validation",
      ids         = id,
      run_order   = 1,
      type        = "proportion"
    ),
    
    prop_f75x_freq_less = summarise_event(
      df          = monitoring_df,
      id_col      = "id",
      phase_col   = "phase",
      event       = "less",
      target_cols = "f75x_freq_validation",
      ids         = id,
      run_order   = 1,
      type        = "proportion"
    ),
    
    # . Proportion Frequency F75/F75F more  ####
    prop_f75x_freq_more = summarise_event(
      df          = monitoring_df,
      id_col      = "id",
      phase_col   = "phase",
      event       = "more",
      target_cols = "f75x_freq_validation",
      ids         = id,
      run_order   = 1, 
      type        = "proportion"
    ),
    
    # . Proportion Quantity F75/F75F less  ####
    prop_f75x_qty_less = summarise_event(
      df          = monitoring_df, 
      id_col      = "id", 
      phase_col   = "phase",
      event       = "less",
      target_cols = "f75x_qty_validation",
      ids         = id,
      run_order   = 1,
      type        = "proportion"
    ),
    
    # . Proportion Quantity F75/F75F more  ####
    prop_f75x_qty_more = summarise_event(
      df          = monitoring_df, 
      id_col      = "id", 
      phase_col   = "phase",
      event       = "more",
      target_cols = "f75x_qty_validation",
      ids         = id,
      run_order   = 1, 
      type        = "proportion"
    ),
    
    # . Proportion Adherence F75/F75F less  ####
    prop_f75x_adh_less = summarise_event(
      df          = monitoring_df, 
      id_col      = "id", 
      phase_col   = "phase",
      event       = "less", 
      target_cols = "f75x_adh_validation",
      ids         = id,
      run_order   = 1, 
      type        = "proportion"
    ),
    
    # . Proportion Adherence F75/F75F more  ####
    prop_f75x_adh_more = summarise_event(
      df          = monitoring_df, 
      id_col      = "id", 
      phase_col   = "phase",
      event       = "more", 
      target_cols = "f75x_adh_validation",
      ids         = id,
      run_order   = 1, 
      type        = "proportion"
    ),
    
    # . Proportion of Consumption of family meals in phase 1  ####
    prop_familymeals = summarise_event(
      df          = monitoring_df,
      id_col      = "id",
      phase_col   = "phase",
      event       = "oui",
      target_cols = "plaFamilCorrige",
      ids         = id,
      run_order   = 1,
      type        = "proportion"
    ),
    
    prop_food_intolerance = summarise_event(
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
  
  return(df)
}
