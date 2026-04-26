# library(gtsummary)
# library(flextable)
# library(officer)
# library(here)

compute_summary(df, X, y, title){
  
  # 1. Create your gtsummary table
  summary_table <- df %>%
    
    tbl_summary(
      
      by = y,
      
      type = all_continuous() ~ "continuous2",
      
      include = X,
      
      statistic = list(all_categorical() ~ "{p}% ({n}/{N})",
                       all_continuous() ~ c("{N_nonmiss}", "{median} ({p25} ; {p75})", "{min} ; {max}")),
      
      digits = list(all_categorical() ~ c(1, 0, 0), all_continuous() ~ c(0, 1, 0))) %>%
    
    add_overall(last = TRUE) %>%
    
    add_p(pvalue_fun = ~ style_pvalue(.x, digits = 2)) %>%
    
    bold_labels() %>%
    
    modify_spanning_header(
      update = all_stat_cols() ~ title
    )
  
  # 1. Capture the name of the dataframe passed to the function
  df_name <- deparse(substitute(data))
  file_name <- paste0(df_name, "_", label, ".docx")
  
  # 2. Convert gtsummary to a flextable object
  
  # 3. Save to Word using here::here()
  df_name <- name(df)
  doc <- read_docx() %>%
    body_add_flextable(value = flex_table) %>%
    print(target = here::here("output","tables","summary_table.docx"))
  
  
  return(descriptive_table)
}

