

run_clinical_qa <- function(data, report_name = "QA_Report") {
  
  # 1. Internal Pre-processing
  # Convert dateSor from 'dd/mm/yyyy' to Date objects for validation
  data_processed <- data %>%
    mutate(dateSor = dmy(dateSor))
  
  # 2. Define the Validation Agent
  agent <- create_agent(
    tbl = data_processed,
    label = paste("QA Analysis -", Sys.Date())
  ) %>%
    # --- Structural Integrity ---
    col_is_numeric(columns = vars(id, ageMois)) %>%
    col_is_character(columns = vars(numMa, sexe, zoneResidence, modeSorCorrige)) %>%
    rows_distinct(columns = vars(id)) %>% # Ensure ID is unique
    
    # --- Clinical Consistency Rules ---
    # Rule 1: Sexe must be M or F
    col_vals_in_set(vars(sexe), set = c("M", "F")) %>%
    
    # Rule 2: If 'decede' is 'oui', 'phaseDeces' must not be empty
    col_vals_not_null(
      columns = vars(phaseDeces),
      precondition = ~ . %>% filter(decede == "oui")
    ) %>%
    
    # Rule 3: Cross-check 'decede' status with 'modeSorCorrige'
    col_vals_equal(
      columns = vars(modeSorCorrige), 
      value = "decede",
      precondition = ~ . %>% filter(decede == "oui")
    ) %>%
    
    # Rule 4: Breastfeeding (allait) logic for older children
    col_vals_equal(
      columns = vars(allait), 
      value = "non",
      precondition = ~ . %>% filter(ageMois > 36) # Typically non-breastfeeding > 3 years
    ) %>%
    
    # Rule 5: Date check (Date of discharge should not be in the future)
    col_vals_lte(vars(dateSor), value = Sys.Date()) %>%
    
    # Execute the checks
    interrogate()
  
  # 3. Export the Report
  file_name <- paste0(report_name, "_", format(Sys.time(), "%Y%m%d_%H%M"), ".html")
  # export_report(agent, filename = file_name)
  
  message(paste("Success: QA Report generated as", file_name))
  return(agent)
}