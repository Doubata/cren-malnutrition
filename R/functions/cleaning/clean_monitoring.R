
clean_monitoring <- function(monitoring_df){
  
  monitoring_clean <- monitoring_df %>%
    
    filter(
      !is.na(id),
      !is.na(phase),
      !(is.na(typeAlim) & plaFamilCorrige == "Non")
    ) %>%
    
    mutate(
      # Convert numeric columns at once
      across(c(poids, taille, pbOrPcV, ATPE), as.numeric),
      
      ptZscore = recode(ptZscore, "inf-4" = "-4", "inf-3" = "-3", "inf-2" = "-2"),
      
      # Convert the date column separately
      dateVis = as.Date(dateVis, format = "%d/%m/%Y")
    ) %>%
    
    # Ordering
    arrange(id, dateVis)
  
  return(monitoring_clean)
}