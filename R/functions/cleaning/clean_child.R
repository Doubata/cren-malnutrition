
clean_child <- function(child_df){
  
  df_clean <- child_df %>%
    
    filter(!is.na(id)) %>%

    arrange(id)
  
  return(df)
}