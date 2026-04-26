flag_nutritional_nonresponse <- function(child_df, monitoring_df){
  
  df_enriched <- child_df %>%
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
    nutritional_nonresponse_p1 = case_when(
      # If at least one is TRUE
      if_any(c(f75_to_f75f, edema_incident, is_death_in_phase, death_day1_in_phase), isTRUE) | 
        phase_length > 4 | length_tubefeeding > 4 ~ TRUE,
      
      # If all are NA
      if_all(c(f75_to_f75f, edema_incident, is_death_in_phase, death_day1_in_phase, 
               phase_length, length_tubefeeding), is.na) ~ NA,
      
      # Otherwise
      .default = FALSE
    )
    
    
  )
  
    return(df_enriched)
}