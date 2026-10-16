flag_nonresponse <- function(profile_data, follow_up_data, cohort, phase=NULL){
  
  if(cohort == "under6"){
    
    data_enriched <- profile_data |>
      #  ---- Compute Indicators ----
    mutate(
      
      weight_gain_g_kg_j_under6 = calculate_weight_gain(
        df         = follow_up_data, 
        id_col     = "id", 
        weight_col = "poids", 
        phase_col  = "phase",
        # run_order  = 2,
        ids        = id
      ),
      
      # . Persistent edema in day7  ####
      edema_persistent_day7_under6 = detect_state_transition(
        df         = follow_up_data, 
        id_col     = "id", 
        phase_col  = "phase", 
        target_col = "oedemes",
        from       = c("1", "2", "3", "8"),
        to         = c("1", "2", "3", "8"),
        ids        = id,
        # run_order  = 1,
        pos        = 7
      ),
      
      # . Death in phase 1 ####
      is_death_under6 = (discharge_type == "Death"),
      
      # . Failure Nutritional Treatment Phase 1 ####
      nonresponse_under6 = case_when(
        # 1. Check for TRUEs, ignoring NA values so they don't block valid data
        if_any(c(edema_persistent_day7_under6, is_death_under6), ~ .x %in% TRUE) | 
          (weight_gain_g_kg_j_under6 < 5 & !is.na(weight_gain_g_kg_j_under6)) ~ TRUE,
        
        # If all are NA
        if_all(c(weight_gain_g_kg_j_under6, edema_persistent_day7_under6, is_death_under6), is.na) ~ NA,
        
        # Otherwise
        .default = FALSE
      )
      
    )
    
  }else{
    
    # 1. Base Indicators common to all cohorts in Phase 1
    if(phase=="1"){
      
      data_enriched <- profile_data |>
        #  ---- Compute Indicators ----
      mutate(
        
        is_p1 = case_when(
          str_starts(phase_series, "1") ~ TRUE,
          .default = FALSE
        ),
        
        # . Length of stay [days] in phase 1  ####
        length_p1 = get_series_length_at(
          df         = follow_up_data, 
          id_col     = "id", 
          target_col = "phase",
          run_order  = 1, 
          ids        = id
        ),
        
        f75_to_f75f_p1 = detect_state_transition(
          df         = follow_up_data, 
          id_col     = "id", 
          phase_col  = "phase", 
          target_col = "typeAlim",
          from       = "F75", 
          to         = "F75F",
          ids        = id,
          run_order  = 1
        ),
        
        # . Duration [days] of tube feeding in phase 1  ####
        length_tubefeeding_p1 = summarise_event(
          df          = follow_up_data, 
          id_col      = "id", 
          phase_col   = "phase", 
          event       = "sonde",
          target_cols = c("repasH1", "repasH2", "repasH3", "repasH4", "repasH5",
                          "repasH6", "repasH7", "repasH8", "repasH9", "repasH10"),
          ids         = id,
          run_order   = 1, 
          type        = "any"
        ),
        
        discharge_type_p1 = case_when(
          phase_series == "1" ~ discharge_type,
          .default = NA
        ),
        
        # . Death in phase 1 ####
        is_death_p1 = (discharge_type_p1 == "Death"),
        
        # . Death in the first day of phase 1  ####
        death_day1_p1 = (is_death_p1 == TRUE & length_p1 == 1)
        
      )
      
      # Cohort-specific flags for Phase 1
      if(cohort == "c659_noedema"){
        data_enriched <- data_enriched |>
          #  ---- Compute Indicators ----
        mutate(
          
          # . Appearance of oedema in phase 1  ####
          edema_incident_p1 = detect_state_transition(
            df         = follow_up_data, 
            id_col     = "id", 
            phase_col  = "phase", 
            target_col = "oedemes",
            from       = c("MISSING", NA, 0),
            ids        = id,
            run_order  = 1
          ),
          
          # . Failure Nutritional Treatment Phase 1 ####
          nonresponse_p1 = case_when(
            # 1. Check for TRUEs, ignoring NA values so they don't block valid data
            if_any(c(f75_to_f75f_p1, edema_incident_p1, is_death_p1), ~ .x %in% TRUE) | 
              (length_p1 > 4 & !is.na(length_p1)) | 
              (length_tubefeeding_p1 > 4 & !is.na(length_tubefeeding_p1)) ~ TRUE,
            
            # If all are NA
            if_all(c(f75_to_f75f_p1, edema_incident_p1, is_death_p1, 
                     length_p1, length_tubefeeding_p1), is.na) ~ NA,
            
            # Otherwise
            .default = FALSE
          )
          
        )
      }else{
        
        data_enriched <- data_enriched |>
          #  ---- Compute Indicators ----
        mutate(
          
          # . Persistent edema in day5 in phase 1  ####
          edema_persistent_day5_p1 = detect_state_transition(
            df         = follow_up_data, 
            id_col     = "id", 
            phase_col  = "phase", 
            target_col = "oedemes",
            from       = c("1", "2", "3", "8"),
            to         = c("1", "2", "3", "8"),
            ids        = id,
            run_order  = 1,
            pos        = 5
          ),
          
          # . Failure Nutritional Treatment Phase 1 ####
          nonresponse_p1 = case_when(
            # 1. Check for TRUEs, ignoring NA values so they don't block valid data
            if_any(c(f75_to_f75f_p1, edema_persistent_day5_p1, is_death_p1), ~ .x %in% TRUE) | 
              (length_tubefeeding_p1 > 4 & !is.na(length_tubefeeding_p1)) ~ TRUE,
            
            # If all are NA
            if_all(c(f75_to_f75f_p1, edema_persistent_day5_p1, is_death_p1, 
                     length_tubefeeding_p1), is.na) ~ NA,
            
            # Otherwise
            .default = FALSE
          )
          
        )
        
      }
      
    }else{
      
      # 2. Phase T Logic
      data_enriched <- profile_data |>
        #  ---- Compute Indicators ----
      mutate(
        
        # . Length of stay [days] in phase T  ####
        length_pt = get_series_length_at(
          df         = follow_up_data, 
          id_col     = "id", 
          target_col = "phase",
          run_order  = 2, 
          ids        = id
        ),
        
        f75_RUTF_to_f100_pt = detect_state_transition(
          df         = follow_up_data, 
          id_col     = "id", 
          phase_col  = "phase", 
          target_col = "typeAlim",
          from       = c("F75", "F75F"), 
          to         = "F100",
          attr_col   = "rutf",
          ids        = id,
          run_order  = 2
        ),
        
        pt_to_p1 = case_when(
          str_starts(phase_series, "1-T-1") ~ TRUE,
          .default = FALSE
        ),
        
        discharge_type_pt = case_when(
          phase_series == "1-T" ~ discharge_type,
          .default = NA
        ),
        
        # . Death in phase t ####
        is_death_pt = (discharge_type_pt == "Death")
        
      )
      
      if(cohort == "c659_noedema"){
        
        data_enriched <- data_enriched |>
          #  ---- Compute Indicators ----
        mutate(
          
          # . Appearance of oedema in phase T  ####
          edema_incident_pt = detect_state_transition(
            df         = follow_up_data, 
            id_col     = "id", 
            phase_col  = "phase", 
            target_col = "oedemes",
            from       = c("MISSING", NA, 0),
            ids        = id,
            run_order  = 2
          ),
          
          weight_gain_g_kg_j_pt = calculate_weight_gain(
            df         = follow_up_data, 
            id_col     = "id", 
            weight_col = "poids", 
            phase_col  = "phase", 
            ids        = id, 
            run_order  = 2
          ),
          
          # . Failure Nutritional Treatment Phase 1 ####
          nonresponse_pt = case_when(
            # 1. Check for TRUEs, ignoring NA values so they don't block valid data
            if_any(c(f75_RUTF_to_f100_pt, edema_incident_pt, pt_to_p1, is_death_pt), ~ .x %in% TRUE) |
              (weight_gain_g_kg_j_pt < 5 & !is.na(weight_gain_g_kg_j_pt)) ~ TRUE,
            
            # If all are NA
            if_all(c(f75_RUTF_to_f100_pt, edema_incident_pt, pt_to_p1, is_death_pt,
                     weight_gain_g_kg_j_pt), is.na) ~ NA,
            
            # Otherwise
            .default = FALSE
          )
        )
        
      }else{
        
        data_enriched <- data_enriched |>
          #  ---- Compute Indicators ----
        mutate(
          
          # . Persistent edema in day11 phase 1  ####
          edema_persistent_day11_pt = detect_state_transition(
            df         = follow_up_data, 
            id_col     = "id", 
            phase_col  = "phase", 
            target_col = "oedemes",
            from       = c("1", "2", "3", "8"),
            to         = c("1", "2", "3", "8"),
            ids        = id,
            run_order  = 1,
            pos        = 11
          ),
          
          # . Failure Nutritional Treatment Phase 1 ####
          nonresponse_pt = case_when(
            # 1. Check for TRUEs, ignoring NA values so they don't block valid data
            if_any(c(f75_RUTF_to_f100_pt, edema_persistent_day11_pt, pt_to_p1, is_death_pt), ~ .x %in% TRUE) ~ TRUE,
            
            # If all are NA
            if_all(c(f75_RUTF_to_f100_pt, edema_persistent_day11_pt, pt_to_p1, is_death_pt), is.na) ~ NA,
            
            # Otherwise
            .default = FALSE
          )
        )
        
      }
    }
    
  }
  
  return(data_enriched)
}