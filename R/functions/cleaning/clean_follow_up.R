
clean_follow_up <- function(follow_up_data){
  
  follow_up_data_cleaned <- follow_up_data %>%
    
    filter(
      !is.na(id),
      !is.na(phase),
      !(is.na(typeAlim) & plaFamilCorrige == "Non")
    ) %>%
    
    mutate(
      ptZscore = dplyr::recode(ptZscore, "inf-4" = "-4", "inf-3" = "-3", "inf-2" = "-2"),
      
      # Convert the date column separately
      dateVis = as.Date(dateVis, format = "%d/%m/%Y"),
      
      rutf = case_when(
        ATPE > 0 ~ "RUTF",
        .default = NA
      )
    ) %>%
    
    # Ordering
    arrange(id, dateVis)
  
  return(follow_up_data_cleaned)
}