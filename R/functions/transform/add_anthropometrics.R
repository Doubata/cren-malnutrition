
add_anthropometrics <- function(child_df, monitoring_df){
  
  df_enriched <- child_df %>%
    
    mutate(
      # Set the first value of the earliest day ---------------------------------
      height           = get_value_at(monitoring_df, "id", "taille",   1, ids = child_df$id),
      weight           = get_value_at(monitoring_df, "id", "poids",    1, ids = child_df$id),
      whz              = get_value_at(monitoring_df, "id", "ptZscore", 1, ids = child_df$id),
      muac             = get_value_at(monitoring_df, "id", "pbOrPc",   1, ids = child_df$id),
      muac_val         = get_value_at(monitoring_df, "id", "pbOrPcV",  1, ids = child_df$id),
      edema            = get_value_at(monitoring_df, "id", "oedemes",  1, ids = child_df$id),
      admission_phase  = get_value_at(monitoring_df, "id", "phase",    1, ids = child_df$id),
      who_whz          = get_value_at(monitoring_df, "id", "who_whz",  1, ids = child_df$id),

      prop_whz_error = summarise_event(
        df          = monitoring_df,
        id_col      = "id",
        phase_col   = "phase",
        event       = "Not Equal",
        target_cols = "whz_validation",
        ids         = id,
        run_order   = 1,
        type        = "proportion"
      )
      
    )
  
  return(df_enriched)
}
