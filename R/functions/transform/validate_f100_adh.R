
validate_f100_adh <- function(data, weight_col, food_col, qty_col, meal_cols, ...,
                              type_food = "F100", who_f100_data) {
  
  # # 1. Safe Symbol/String Extraction
  # get_col_str <- function(arg, env = parent.frame()) {
  #   expr <- substitute(arg, env)
  #   if (is.character(expr)) return(expr)
  #   if (is.name(expr)) return(as.character(expr))
  #   if (is.character(arg)) return(arg)
  #   stop("Column name must be a string or symbol.")
  # }
  # 
  # weight_col <- get_col_str(weight_col)
  # food_col   <- get_col_str(food_col)
  # qty_col    <- get_col_str(qty_col)
  
  # Ensure meal_cols is handled cleanly as a character vector
  if (!is.character(meal_cols)) {
    meal_cols <- as.character(substitute(meal_cols))[-1]
  }
  
  # 2. Vector Extraction
  weight <- data[[weight_col]]
  food   <- data[[food_col]]
  qty    <- data[[qty_col]]
  
  results <- rep(NA_character_, nrow(data))
  
  # 3. Food Filter
  valid_food <- !is.na(food) & food %in% type_food
  if (!any(valid_food)) return(results)
  
  # 4. Internal Adherence Helper
  process_group <- function(idx, lkp_data) {
    if (length(idx) == 0) return(character(0))
    
    # Fast matrix extraction and character coercion
    intake_matrix <- as.matrix(data[idx, meal_cols, drop = FALSE])
    storage.mode(intake_matrix) <- "character"
    
    # Calculate daily intakes
    daily_intakes <- .get_daily_intake_batch(qty[idx], intake_matrix)
    
    # Weight lookup
    w_idx   <- .lookup_who_idx(weight[idx], lkp_data[["weight_kg"]])
    valid_w <- !is.na(w_idx)
    
    res <- rep(NA_character_, length(idx))
    
    if (any(valid_w)) {
      curr_intakes <- daily_intakes[valid_w]
      curr_max     <- lkp_data[["max_ml_per_day"]][w_idx[valid_w]]
      curr_min     <- lkp_data[["min_ml_per_day"]][w_idx[valid_w]]
      
      # Explicit NA-safe categorisation
      sub_res <- rep(NA_character_, length(curr_intakes))
      
      valid_calc <- !is.na(curr_intakes) & !is.na(curr_max) & !is.na(curr_min)
      
      sub_res[valid_calc & curr_intakes > curr_max] <- "more"
      sub_res[valid_calc & curr_intakes < curr_min] <- "less"
      sub_res[valid_calc & curr_intakes >= curr_min & curr_intakes <= curr_max] <- "within"
      
      res[valid_w] <- sub_res
    }
    return(res)
  }
  
  # 5. Assign Results
  valid_indices <- which(valid_food)
  results[valid_indices] <- process_group(valid_indices, who_f100_data)
  
  return(results)
}