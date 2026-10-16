 
add_anthropometrics <- function(profile_data, follow_up_data){
  
  profile_data_enriched <- profile_data |>
    
    mutate(
      # Set the first value of the earliest day ---------------------------------
      height           = get_value_at(follow_up_data, "id", "taille",   1, ids = profile_data$id),
      weight           = get_value_at(follow_up_data, "id", "poids",    1, ids = profile_data$id),
      whz              = get_value_at(follow_up_data, "id", "ptZscore", 1, ids = profile_data$id),
      muac             = get_value_at(follow_up_data, "id", "pbOrPc",   1, ids = profile_data$id),
      muac_val         = get_value_at(follow_up_data, "id", "pbOrPcV",  1, ids = profile_data$id),
      edema            = get_value_at(follow_up_data, "id", "oedemes",  1, ids = profile_data$id),
      admission_phase  = get_value_at(follow_up_data, "id", "phase",    1, ids = profile_data$id),
      # Edema status (Using T/F)
      has_edema = (coalesce(edema, 0) >= 1),
      
      prop_whz_error_p1 = summarise_event(
        df          = follow_up_data,
        id_col      = "id",
        phase_col   = "phase",
        event       = "Not Equal",
        target_cols = "whz_validation",
        ids         = id,
        run_order   = 1,
        type        = "proportion"
      ),
    )
  
  return(profile_data_enriched)
}
