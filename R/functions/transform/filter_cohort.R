
filter_cohort <- function(data, cohort=NULL, phase=NULL) {
  
  if(is.null(cohort) | !cohort %in% c("c659_noedema", "c659_edema", "under6")) return(data)
  
  if(cohort == "c659_noedema"){
    
    if(phase == "1"){
      # ---- Filter  ----
      data <- data |>
        # by age range
        filter(!is.na(age_range_6_59)) |>
        
        # by edema status (handling NAs explicitly)
        filter(has_edema == FALSE) |>
        
        # by complex physical and Z-score criteria
        filter(weight >= 3.5, whz %in% c("-3", "-4") | (muac == "pb" & muac_val < 115)) |>
        
        # by rows where phase starts with "1"
        filter(str_starts(phase_series, "1"))
    }else{
      # ---- Filter  ----
      data <- data |>
        # by rows where phase starts with "1T"
        dplyr::filter(str_starts(phase_series, "1-T"))
    }
    
  }
  
  if(cohort == "c659_edema"){
    
    if(phase == "1"){
      # ---- Filter  ----
      data <- data |>
        
        # by age range
        filter(!is.na(age_range_6_59)) |>
        
        # by edema status (handling NAs explicitly)
        filter(has_edema == TRUE) |>
        
        # by rows where phase starts with "1"
        filter(str_starts(phase_series, "1"))
    }else{
      # ---- Filter  ----
      data <- data |>
        # by rows where phase starts with "1T"
        dplyr::filter(str_starts(phase_series, "1-T"))
    }
    
  }
  
  if(cohort == "under6"){
    # ---- Filter  ----
    data <- data |>
      filter(!is.na(age_under_6) | weight < 3.5)
    
  }
  
  return(data)
}
