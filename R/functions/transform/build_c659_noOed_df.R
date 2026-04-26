build_c659_noOed_df <- function(child_df) {
  
  # ---- Filter  ----
  c659_noOed_df <- child_df %>%
    # by age range
    filter(!is.na(age_range_6_59)) %>%
    
    # by edema status (handling NAs explicitly)
    filter(has_edema == FALSE) %>%
    
    # by complex physical and Z-score criteria
    filter(weight >= 3.5, whz %in% c("-3", "-4") | (muac == "pb" & muac_val < 115)) %>%
    
    # by rows where phase starts with "1"
    filter(str_starts(phase_series, "1")) %>%
    
    mutate(
      is_phase_1 = str_starts(phase_series, "1"),
      is_phase_T = str_detect(phase_series, "^1T")
    )
  
  return(c659_noOed_df)
}