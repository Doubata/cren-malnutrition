
calculate_weight_gain <- function(df, id_col, weight_col, phase_col, ids = NULL, run_order = NULL) {
  
  # Keep numeric columns numeric
  weights <- as.numeric(df[[weight_col]])
  phases  <- as.character(df[[phase_col]])
  id_char <- as.character(df[[id_col]])
  
  # Split data by ID
  weights_list <- split(weights, id_char)
  phases_list  <- split(phases, id_char)
  
  ids_char <- if (is.null(ids)) unique(id_char) else as.character(ids)
  
  results <- vapply(ids_char, function(id) {
    if (!(id %in% names(phases_list))) return(NA_real_)
    
    phases_vec  <- phases_list[[id]]
    weights_vec <- weights_list[[id]]
    
    # Apply optional run filter
    if (!is.null(run_order)) {
      run_idx <- .get_series_idx(phases_vec, run_order)
      run_idx <- run_idx[!is.na(run_idx)]
      if (length(run_idx) == 0) return(NA_real_)
      weights_vec <- weights_vec[run_idx]
    }
    
    len <- length(weights_vec)
    
    # Guard: insufficient length or completely empty vector
    if (len < 3 || all(is.na(weights_vec))) return(NA_real_)
    
    min_weight <- min(weights_vec)
    max_weight <- max(weights_vec)
    
    round(((max_weight - min_weight) * 1000) / (min_weight*len), 2)
    
  }, FUN.VALUE = numeric(1))
  
  return(unname(results))
}
