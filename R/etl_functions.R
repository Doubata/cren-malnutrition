source(here::here("scripts", "00_setup.R"))
source(here::here("R", "etl_functions_validators.R"))

################################
##### Get WHO Weight index #####
################################

.get_series_idx <- function(value_col, run_order){
  # Handle empty weights or all-NA inputs
  if (length(value_col) == 0) return(NA)
  
  results <- NA
  # Get the runs in ref_vec
  runs <- rle(as.character(value_col))
  
  if(run_order > length(runs$values)){
    return(NA)
  }else{
    # Get positions / indices of each group
    # Cumulative sum gives end positions
    ends   <- cumsum(runs$lengths)
    starts <- c(1, head(ends, -1) + 1)
    # Get the position of a specific group run_order
    s <- starts[run_order]
    e <- ends[run_order]
    results <- c(s:e)
  }
  
  return(results)
}

#############################################################
##### Get the value of a row index of a group of vector #####
#############################################################

get_value_at <- function(df, id_col, val_col, row_index,..., ids){
  # Call the gatekeeper
  .check_args_value_at(df, id_col, val_col, row_index, ids)
  
  # Filter the dfframe to only include the rows with our IDs
  sub_df <- df[df[[id_col]] %in% ids, ]
  
  # Grouping and Extraction
  # split() creates a list where each element is a vector of values for that ID
  values_list <- split(sub_df[[val_col]], as.character(sub_df[[id_col]]))
  
  # Convert ID to character for list indexing
  ids_char <- as.character(ids)
  
  # Iterate 'ids' to ensure the output order matches the input order
  # Handle IDs that might be missing from the dfframe gracefully
  result <- sapply(ids_char, function(id_char) {
    # Check if the ID exists in the list
    if (!(id_char %in% names(values_list))) {
      return(NA) 
    }
    
    # Check if ID exists in df and has enough rows
    group_vec <- values_list[[id_char]]
    
    if (!is.null(group_vec) && length(group_vec) >= row_index) {
      return(group_vec[row_index])
    } else {
      # Returns NA if ID is missing or index is out of bounds
      return(NA) 
    }
  })
  
  # Return as a clean, unnamed vector
  return(unname(result))
}

#############################################################
##### Get list of values or values + attribute by index #####
#############################################################

.get_values_by_id <- function(df, val_col, id_col, ..., attr_col = NULL){
  # Format the target column
  df[[val_col]] <- as.character(df[[val_col]])
  df[is.na(df[[val_col]]), val_col] <- "MISSING"
  
  id_factor <- as.character(df[[id_col]])
  if(is.null(attr_col)){
    # Split the target column by the ID column
    values_list <- split(df[[val_col]], id_factor)
  }else{
    # Format the attribute column if exist
    df[[attr_col]] <- as.character(df[[attr_col]])
    df[is.na(df[[attr_col]]), attr_col] <- "MISSING"
    
    # Create a new column that combine target column with attribute column
    df[[new_val_col]] <- ifelse(df[[attr_col]] == "MISSING",
                                    df[[val_col]],
                                    paste0(df[[val_col]], "+", df[[attr_col]]))
    
    # Split the new column by the ID column AND update values_list
    values_list <- split(df[[new_val_col]], id_factor)
  }
  
  return(values_list)
}

# get_series_val <- function(df, id_col, val_col,..., attr_col = NULL, ids){
#   
#   
# }

#################################################################
##### Get a string of a sequence of series of unique values #####
#################################################################

get_series_str <- function(df, id_col, val_col,..., attr_col = NULL, ids){
  # Call the gatekeeper
  .check_args_series_str(df, id_col, val_col, attr_col, ids)
  
  if(is.null(attr_col)){
    target_cols <- c(id_col, val_col)
  }else{
    target_cols <- c(id_col, val_col, attr_col)
  }
  
  # Extract specific rows with our IDs AND specific columns
  # # Filter df to make splitting faster
  sub_df <- df[df[[id_col]] %in% ids, target_cols]
  
  # Split the target column by the ID column
  values_list <- .get_values_by_id(sub_df, val_col, id_col, attr_col)
  
  if(is.list(values_list) && !is.null(names(values_list))){
    # Convert ID to character for list indexing
    ids_char <- as.character(ids)
    
    # Iterate 'ids' to ensure the output order matches the input order
    # Handle IDs that might be missing from the dfframe gracefully
    result <- sapply(ids_char, function(id_char) {
      # Check if the ID exists in the list
      if (!(id_char %in% names(values_list))) {
        return(NA) 
      }
      
      values_vec <- values_list[[id_char]]
      # if(!is.null(attr_col)) attr_vec <- attr_list[id_str]
      
      # Get the runs
      values_runs <- rle(as.character(values_vec))$values
      # Build a string of values
      runs_str <- paste0(values_runs, collapse = "-")
      
      return(runs_str)
    })
    
    return(unname(result))
  }else{
    return(NA)
  }
}

########################################################################
##### Extract a series that mirrors the position of another series #####
########################################################################

extract_matching_series_str <- function(df, id_col, val_col, ref_col,..., attr_col = NULL, run_order, ids){
  # call the gatekeeper
  .check_args_matching_series_str(df, id_col, val_col, ref_col, attr_col, run_order, ids)
  
  if(is.null(attr_col)){
    target_cols <- c(id_col, ref_col, val_col)
  }else{
    target_cols <- c(id_col, ref_col, val_col, attr_col)
  }
  
  # Filter df to make splitting faster
  sub_df <- df[df[[id_col]] %in% ids, target_cols]
  
  # Split the target column by the ID column
  referencies_list <- .get_values_by_id(sub_df, ref_col, id_col)
  values_list <- .get_values_by_id(sub_df, val_col, id_col, attr_col)
  
  # Convert ID to character for list indexing
  ids_char <- as.character(ids)
  
  # Iterate 'ids' to ensure the output order matches the input order
  result <- sapply(ids_char, function(id_char) {
    # Check if the ID exists in the list
    if (!(id_char %in% names(referencies_list))) {
      return(NA) 
    }
    
    # Get id_str references
    ref_vec <- referencies_list[[id_char]]
    
    # Get the runs in ref_vec
    ref_runs <- rle(as.character(ref_vec))
    
    if(run_order > length(ref_runs$values)){
      return(NA)
    }else{
      # Get positions / indices of each group
      # Cumulative sum gives end positions
      ends   <- cumsum(ref_runs$lengths)
      starts <- c(1, head(ends, -1) + 1)
      # Get the position of a specific group run_order
      s <- starts[run_order]
      e <- ends[run_order]
      
      # Get id_char values
      val_vec <- values_list[[id_char]]
      
      matching_values <- val_vec[s:e]
      matching_values_run <- rle(matching_values) # <-- Upgraded to get runs instead of unique values
      runs_str <- paste0(matching_values_run$values, collapse = "-")
      
      return(runs_str)
    }
  })
  return(unname(result))
}

###########################################################################
##### Get the length of a series at a specific position of a sequence #####
###########################################################################

get_series_length_at <- function(df, id_col, val_col,..., run_order, ids){
  # Call the gatekeeper
  .check_args_series_length_at(df, id_col, val_col, run_order, ids)
  
  # Filter df to make splitting faster
  sub_df <- df[df[[id_col]] %in% ids, c(id_col, val_col), drop = FALSE]
  
  # sub_df[is.na(sub_df[[val_col]]), val_col] <- "MISSING"
  
  # Split the target column by the ID column
  values_list <- .get_values_by_id(sub_df, val_col, id_col)
  
  # Convert ID to character for list indexing
  ids_char <- as.character(ids)
  
  # Iterate 'ids' to ensure the output order matches the input order
  result <- vapply(ids_char, function(id_char) {
    # Check if the ID exists in the list
    if (!(id_char %in% names(values_list))) return(NA_integer_) 
    
    val_vec <- values_list[[id_char]]
    
    # Handle NA values for rle: replace with a placeholder that won't match others
    val_vec[is.na(val_vec)] <- "MISSING"
    
    # Calculate run lengths
    runs <- rle(as.character(val_vec))
    
    if(run_order > length(runs$lengths)){
      
      return(NA_integer_)
    }else{
      
      return(as.integer(runs$lengths[run_order]))
    }
  }, FUN.VALUE = integer(1))
  
  return(unname(result))
}

####################################################
##### Get the number of days of breast feeding #####
####################################################

count_occurrences <- function(df, id_col, val_col,...,attr_col = NULL, criteria = NULL, ids){
  # Call the gatekeeper
  .check_args_count_occurrences(df, id_col, val_col, attr_col, criteria, ids)
  
  # Filter the dataframe
  if(is.null(attr_col)){
    target_cols <- c(id_col, val_col)
  }else{
    target_cols <- c(id_col, val_col, attr_col)
  }
  
  # Filter df to make splitting faster
  sub_df <- df[df[[id_col]] %in% ids, target_cols]
  
  # Split the target column by the ID column
  id_factor <- as.character(sub_df[[id_col]])
  values_list <- split(sub_df[[val_col]], id_factor)
  if(!is.null(attr_col)){
    attributes_list <- split(sub_df[[attr_col]], id_factor)
  }
  
  # Convert ID to character for list indexing
  ids_char <- as.character(ids)
  
  # Iterate 'ids' to ensure the output order matches the input order
  results <- sapply(ids_char, function(id_char){
    # check if the ID exists in the list
    if (!(id_char %in% names(values_list))) {
      return(NA) 
    }
    
    values_vec <- values_list[[id_char]] # Extract vector
    
    if(!is.null(criteria)){
      if(is.null(attr_col)){
        n <- sum(values_vec %in% criteria)
      }else{
        attributes_vec <- attributes_list[[id_char]]
        n <- sum(values_vec %in% criteria | attributes_vec %in% criteria)
      }
      return(n)
    }else{
      return(length(values_vec))
    }
  })
  
  return(unname(results))
}

################################
##### Get WHO Weight index #####
################################

.get_who_lookup_idx <- function(vals, weights){
  # Handle empty weights or all-NA inputs
  if (length(weights) == 0) return(rep(NA, length(vals)))
  
  # Ensure weights are sorted (required for findInterval)
  sorted_weights <- sort(round(weights, 3))
  
  # Subtracting a tiny amount (1e-8) ensures that if val == weight, 
  # it stays in the current interval, then we +1 to get the ceiling.
  idx <- findInterval(round(vals, 3) - 1e-8, sorted_weights) + 1
  
  # If idx > length(weights) or weight < min(weights)
  # findInterval returns 0, so idx becomes 1 so de must mark these as NA 
  out_of_bounds <- is.na(vals) | vals < min(weights) | vals > max(weights)
  idx[out_of_bounds] <- NA
  
  return(idx)
}

###################################################
##### Extract WHO F75x "frequency"/"quantity" #####
###################################################

extract_who_f75x <- function(df, weight_col, food_col, oedema_col, val_col,...,
                             who_df, who_weight_col, who_oedema_col, x12_col, x8_col, x6_col,
                             type_food = c("F75", "F75F") , measure_type = c("frequency", "quantity")){
  
  # 1. Capture Symbols (Entry Point)
  if (is.symbol(substitute(weight_col)))        weight_col        <- deparse(substitute(weight_col))
  if (is.symbol(substitute(food_col)))          food_col          <- deparse(substitute(food_col))
  if (is.symbol(substitute(oedema_col)))        oedema_col        <- deparse(substitute(oedema_col))
  if (is.symbol(substitute(val_col)))           val_col           <- deparse(substitute(val_col))

  if (is.symbol(substitute(who_weight_col)))    who_weight_col    <- deparse(substitute(who_weight_col))
  if (is.symbol(substitute(who_oedema_col)))    who_oedema_col    <- deparse(substitute(who_oedema_col))
  if (is.symbol(substitute(x12_col)))           x12_col           <- deparse(substitute(x12_col))
  if (is.symbol(substitute(x8_col)))            x8_col            <- deparse(substitute(x8_col))
  if (is.symbol(substitute(x6_col)))            x6_col            <- deparse(substitute(x6_col))
  
  # Call the Gatekeeper
  .check_args_who_f75x(df, weight_col , food_col , oedema_col , val_col,
                       who_df, who_weight_col, who_oedema_col,
                       x12_col, x8_col, x6_col, type_food, measure_type)
  
  # Define standard WHO frequencies mapping to the columns
  freq <- c(12, 8, 6)
  
  # Split WHO DF based on oedema labels used in your lookup table
  oedema_df <- who_df[who_df[[who_oedema_col]] == "oui", ]
  no_oedema_df <- who_df[who_df[[who_oedema_col]] == "non", ]
  
  # Initialization
  n <- nrow(df)
  results <- rep(NA, n)
  
  # Filter for correct food types
  valid_food <- df[[food_col]] %in% type_food
  
  # Define Oedema logic: 0 and NA are "No Oedema"
  # This creates a logical vector where TRUE is the Oedema group
  has_oedema_logical <- !is.na(df[[oedema_col]]) & df[[oedema_col]] != 0
  
  # Split indices
  idx_no_oed <- which(valid_food & !has_oedema_logical)
  idx_oed    <- which(valid_food & has_oedema_logical)
  
  # Internal processing helper
  process_group <- function(target_rows, lookup_table) {
    if (length(target_rows) == 0) return(NULL)
    
    # 1. Get row indices from the lookup table (using the vectorized ceiling helper)
    w_idx <- .get_who_lookup_idx(df[[weight_col]][target_rows], lookup_table[[who_weight_col]])
    
    # 2. Build the lookup matrix (x12, x8, x6 columns)
    # We use na.omit/handling because w_idx might contain NAs from out-of-bounds weights
    val_matrix <- cbind(lookup_table[[x12_col]][w_idx], 
                        lookup_table[[x8_col]][w_idx], 
                        lookup_table[[x6_col]][w_idx])
    
    input_vals <- df[[val_col]][target_rows]
    
    if (measure_type == "frequency") {
      # Logic: Find which column matches the provided quantity, return the frequency
      # Using Matrix Algebra
      matches <- max.col(val_matrix == input_vals, ties.method = "first")
      matches[rowSums(val_matrix == input_vals, na.rm = TRUE) == 0] <- NA
      return(freq[matches])
      
    } else { # measure_type == "quantity"
      # Logic: Match the provided frequency against the freq vector, return quantity
      freq_idx <- match(input_vals, freq)
      # Matrix indexing: [row_indices, col_indices]
      return(val_matrix[cbind(seq_along(target_rows), freq_idx)])
    }
  }
  # Assign values back to the original order
  results[idx_no_oed] <- process_group(idx_no_oed, no_oedema_df)
  results[idx_oed]    <- process_group(idx_oed, oedema_df)
  
  return(unname(results))
}

########################################
##### Validate F75x quantity error #####
########################################

validate_f75x_qty_intake <- function(df, weight_col, food_col, oedema_col, freq_col, qty_col,
                                 who_df, who_weight_col, who_oedema_col, max_col, min_col,
                                 type_food = c("F75", "F75F"), ...){
  
  # 1. Split WHO DF based on oedema labels
  oedema_df <- who_df[who_df[[who_oedema_col]] == "oui", ]
  no_oedema_df <- who_df[who_df[[who_oedema_col]] == "non", ]
  
  # 2. Initialization
  n <- nrow(df)
  results <- rep(NA_character_, n) # Use character NA for consistency
  
  # 3. Filtering Logic
  valid_food <- df[[food_col]] %in% type_food
  
  # Treat NA as 0 (No Oedema) for the logic check
  oedema_val <- df[[oedema_col]]
  oedema_val[is.na(oedema_val)] <- 0
  has_oedema_logical <- oedema_val != 0
  
  # Split indices
  idx_no_oed <- which(valid_food & !has_oedema_logical)
  idx_oed    <- which(valid_food & has_oedema_logical)
  
  # 4. Internal processing helper
  process_group <- function(target_rows, lookup_table) {
    if (length(target_rows) == 0) return(character(0))
    
    # Get row indices from the lookup table
    w_idx <- .get_who_lookup_idx(df[[weight_col]][target_rows], lookup_table[[who_weight_col]])
    
    # Build the lookup matrix
    limit_matrix <- cbind(max = lookup_table[[max_col]][w_idx], 
                          min = lookup_table[[min_col]][w_idx])
    
    # Calculate total intake (Frequency * Quantity)
    input_vals <- df[[freq_col]][target_rows] * df[[qty_col]][target_rows]
    
    # Vectorized Categorization
    res <- rep("within", length(input_vals))
    
    # logical indexing for categorization (ignoring NAs in matrix)
    res[input_vals > limit_matrix[, "max"] & !is.na(limit_matrix[, "max"])] <- "more"
    res[input_vals < limit_matrix[, "min"] & !is.na(limit_matrix[, "min"])] <- "less"
    
    # Apply NAs where lookup failed
    res[is.na(w_idx)] <- NA_character_
    
    return(res)
  }
  
  # 5. Assign values back
  if(length(idx_no_oed) > 0) results[idx_no_oed] <- process_group(idx_no_oed, no_oedema_df)
  if(length(idx_oed) > 0)    results[idx_oed]    <- process_group(idx_oed, oedema_df)
  
  return(results)
}

##################################
# Helper for validation function
##################################

.get_daily_intake_batch <- function(food_qty, meals_matrix) {
  # 1. Validation: Ensure food_qty length matches matrix rows
  if (length(food_qty) != nrow(meals_matrix)) {
    stop("Mismatch: 'food_qty' must have the same length as the number of rows in 'meals_matrix'.")
  }
  
  # 2. Define the lookup table
  multipliers <- c(
    "1"     = 1, 
    "3_4"   = 0.75, 
    "1_2"   = 0.5, 
    "1_4"   = 0.25,
    "refus" = 0, 
    "sonde" = 1, 
    "vomit" = 0
  )
  
  # 3. Vectorized Map
  # multipliers[meals_matrix] flattens the matrix into a vector for lookup
  weights_vec <- multipliers[meals_matrix]
  
  # 4. Reconstruct the matrix shape
  weights_matrix <- matrix(weights_vec, 
                           nrow = nrow(meals_matrix), 
                           ncol = ncol(meals_matrix))
  
  # 5. Row-wise Multiplication and Summation
  # intake_matrix[i, ] = weights_matrix[i, ] * food_qty[i]
  intake_matrix <- weights_matrix * food_qty
  
  # Calculate sums
  results <- rowSums(intake_matrix, na.rm = TRUE)
  
  # Identify rows that were entirely NA (or invalid keys)
  # If a row has zero non-NA values, set the result to NA
  all_na_rows <- rowSums(!is.na(weights_matrix)) == 0
  results[all_na_rows] <- NA
  
  return(results)
}

###################################
##### Validate F75x Adherence #####
###################################

validate_f75x_adherence <- function(df, weight_col, food_col, oedema_col, freq_col, qty_col, meal_cols,...,
                                    who_df, who_weight_col, who_oedema_col, max_col, min_col,
                                    type_food = c("F75", "F75F")){
  
  # 1. Initialize result vector
  results <- rep(NA_character_, nrow(df))

  # 2. Identify the rows that actually need checking (valid food types)
  # This prevents processing rows we don't care about
  valid_idx <- which(df[[food_col]] %in% type_food)
  if (length(valid_idx) == 0) return(results)

  # 3. Get unique categories present in the data for the grouping column
  # We use the oedema column as the "key" to match against the lookup table

  oedema_val <- df[[oedema_col]]
  internal_oedema <- rep("oui", nrow(df))
  internal_oedema[oedema_val %in% c(0, "0", "non", "no", NA)] <- "non"
  
  categories <- unique(internal_oedema[valid_idx])

  # 4. Iterate through each category dynamically
  for (cat in categories) {

    # Identify rows in df that match this category
    target_rows <- valid_idx[df[[oedema_col]][valid_idx] == cat]
    if (length(target_rows) == 0) next

    # Filter the lookup table for the matching category
    lookup_sub <- who_df[who_df[[who_oedema_col]] == cat, ]
    if (nrow(lookup_sub) == 0) next
    
    # Defensive conversion: ensure strings, not factors
    intake_matrix <- df[target_rows, meal_cols, drop = FALSE]
    intake_matrix <- sapply(intake_matrix, as.character)
    if(!is.matrix(intake_matrix)) intake_matrix <- matrix(intake_matrix, nrow = length(target_rows))
    
    daily_intakes <- .get_daily_intake_batch(df[[qty_col]][target_rows], intake_matrix)
    
    # Get row indices from the lookup table (using the vectorized ceiling helper)
    w_idx <- .get_who_lookup_idx(df[[weight_col]][target_rows], lookup_sub[[who_weight_col]])
    
    # Handle weights out of WHO range (where w_idx might be NA)
    valid_w <- !is.na(w_idx)
    
    res <- rep(NA_character_, length(target_rows))
    
    # Only compare where weight lookup was successful
    if (any(valid_w)) {
      curr_intakes <- daily_intakes[valid_w]
      curr_max <- lookup_sub[[max_col]][w_idx[valid_w]]
      curr_min <- lookup_sub[[min_col]][w_idx[valid_w]]
      
      sub_res <- rep("within", length(curr_intakes))
      sub_res[curr_intakes > curr_max] <- "more"
      sub_res[curr_intakes < curr_min] <- "less"
      
      res[valid_w] <- sub_res
    }

    # Apply results back to the original index positions
    results[target_rows] <- res
  }

  return(results)
}

###################################################################
##### Validate Flag (less, more, equal) proportion  Adherence #####
###################################################################

get_flag_proportion <- function(df, id_col, phase_col, validation_col, flag, run_order, ids, ...) {
  
  # 2. Add the results to a temporary copy of the dataframe
  temp_df <- df[,c(id_col, phase_col, validation_col)]
  
  # 3. Calculate proportion for each ID in the provided 'ids' vector
  proportions <- vapply(ids, function(current_id) {
    # Subset rows for this ID
    id_rows <- temp_df[temp_df[[id_col]] == current_id, ]
    
    if (nrow(id_rows) == 0) return(NA_real_)
    
    # Get run index
    run_idx <- .get_series_idx(id_rows[[phase_col]], run_order)
    
    # Keep valid run positions
    run_idx <- run_idx[!is.na(run_idx)]
    
    if (length(run_idx) == 0) return(NA_real_)
    
    statuses_run <- id_rows[[validation_col]][run_idx]
    
    # Remove NA values
    statuses_run <- statuses_run[!is.na(statuses_run)]
    
    if (length(statuses_run) == 0) return(NA_real_)
    
    # Proportion of the flag
    mean(statuses_run == flag)
    
  }, FUN.VALUE = numeric(1))
  
  return(unname(proportions))
}

####################################################
##### Extract WHO F100x "frequency"/"quantity" #####
####################################################

extract_who_f100 <- function(df, weight_col, food_col, val_col,...,
                             who_df, who_weight_col, min_col, max_col,
                             measure_type = c("frequency", "quantity")){
  
  # Call the Gatekeeper
  .check_args_who_f100(df, weight_col , food_col , val_col,
                       who_df, who_weight_col, min_col, max_col,
                       measure_type)
  
  # Initialization
  n <- nrow(df)
  results <- rep(NA, n)
  
  # 1. Identify valid rows
  is_f100 <- !is.na(df[[food_col]]) & df[[food_col]] == "F100"
  
  # Split indices
  idx_f100 <- which(is_f100)
  target_rows <- df[[val_col]][idx_f100]
  
  if (length(target_rows) == 0) return(results)
  
  # Get row indices from who_df
  w_idx <- .get_who_lookup_idx(df[[weight_col]][idx_f100], who_df[[who_weight_col]])
  
  results[idx_f100] <- if (measure_type == "frequency") {
    # Logic: Find which row matches 6, return the frequency
    rep(6, length(target_rows))
    
    } else { # measure_type == "quantity"
      # Extract relevant bounds for these specific indices
      current_mins <- who_df[[min_col]][w_idx]
      current_maxs <- who_df[[max_col]][w_idx]
      
      # If value <= min, use min. Otherwise keep value.
      qty <- pmax(target_rows, current_mins, na.rm = TRUE)
      # If value > min, use max
      ifelse(qty > current_mins, current_maxs, qty)
    }
  
  return(unname(results))
}

########################################
##### Validate F100 quantity error #####
########################################

validate_f100_qty_intake <- function(df, weight_col, food_col, freq_col, qty_col,
                                     who_df, who_weight_col, max_col, min_col,
                                     type_food, ...){
  

  # 2. Initialization
  n <- nrow(df)
  results <- rep(NA_character_, n) # Use character NA for consistency
  
  # 3. Filtering Logic
  valid_food <- df[[food_col]] %in% type_food
  target_rows <- which(valid_food)
  if (length(target_rows) == 0) return(character(0))
    
  # Get row indices from the lookup table
  w_idx <- .get_who_lookup_idx(df[[weight_col]][target_rows], who_df[[who_weight_col]])
  
  # Build the lookup matrix
  limit_matrix <- cbind(max = who_df[[max_col]][w_idx], 
                        min = who_df[[min_col]][w_idx])
  
  # Calculate total intake (Frequency * Quantity)
  qty_intakes <- df[[freq_col]][target_rows] * df[[qty_col]][target_rows]
  
  # Vectorized Categorization
  res <- rep("within", length(qty_intakes))
  
  # logical indexing for categorization (ignoring NAs in matrix)
  res[qty_intakes > limit_matrix[, "max"] & !is.na(limit_matrix[, "max"])] <- "more"
  res[qty_intakes < limit_matrix[, "min"] & !is.na(limit_matrix[, "min"])] <- "less"
  
  # Apply NAs where lookup failed
  res[is.na(w_idx)] <- NA_character_
  
  # 5. Assign values back
  results[target_rows] <- res
  
  return(results)
}

####################################################
##### Extract WHO F100d "frequency"/"quantity" #####
####################################################

 # Get who indices
.get_who_f100d_lookup_idx <- function(values, weight_mins, weight_maxs){
  # 1. Safety check: Handle empty values or all-NA inputs
  if (length(values) == 0) return(rep(NA_integer_, length(values)))
  
  # findInterval returns 0, so idx becomes 1 so de must mark these as NA 
  idx <- findInterval(values, weight_mins)
  
  # Remove zeros (below min)
  idx[idx == 0] <- NA_integer_
  
  # Validate against upper bounds
  valid <- !is.na(idx) & values <= weight_maxs[idx]
  idx[!valid] <- NA_integer_
  
  return(idx)
}

# Extract f100d frequencies
extract_who_f100d_freq <- function(df, food_col){
  # Initialization
  n <- nrow(df)
  results <- rep(NA, n)
  
  # 1. Identify valid rows
  is_f100d <- !is.na(df[[food_col]]) & df[[food_col]] == "F100d"
  if (!any(is_f100d)) return(results)
  
  results[is_f100d] <- 8
}

# Extract f100d quantity
extract_who_f100d_qty <- function(df, weight_col, food_col, bf_col,...,
                                  who_bf_df, who_nbf_df, weight_min_col, weight_max_col,
                                  meal_col, acute_meal_col, reah_meal_col){
  
  # Call the Gatekeeper
  .check_args_who_f100d(df, weight_col, food_col, bf_col,
                        who_bf_df, who_nbf_df, weight_min_col, weight_max_col,
                        meal_col, acute_meal_col, reah_meal_col)
  
  # Initialization
  n <- nrow(df)
  results <- rep(NA, n)
  
  # 1. Identify valid rows
  is_f100d <- !is.na(df[[food_col]]) & df[[food_col]] == "F100d"
  if (!any(is_f100d)) return(results)
  
  # We split the processing into Breastfed vs Not Breastfed
  is_bf <- is_f100d & (df[[bf_col]] %in% c("oui", NA))
  is_not_bf <- is_f100d & !(df[[bf_col]] %in% c("oui", NA))
  
  # Process Breastfed
  if (any(is_bf)) {
    weights_bf <- df[[weight_col]][is_bf]
    # l_idx <- get_lookup_idx(weights_bf, who_bf_df)
    l_idx <- .get_who_f100d_lookup_idx(weights_bf,
                                       who_bf_df[[weight_min_col]],
                                       who_bf_df[[weight_max_col]])
    
    results[is_bf] <- who_bf_df[[meal_col]][l_idx]
  }
  
  # Process Not Breastfed
  if (any(is_not_bf)) {
    weights_nbf <- df[[weight_col]][is_not_bf]
    # l_idx <- get_lookup_idx(weights_nbf, who_nbf_df)
    l_idx <- .get_who_f100d_lookup_idx(weights_nbf, 
                                       who_nbf_df[[weight_min_col]],
                                       who_nbf_df[[weight_max_col]])
    
    # Logic: Acute (indices 1-7)
    # is_acute <- l_idx >= 1 & l_idx <= 7
    is_acute <- !is.na(l_idx) & l_idx <= 7
    
    # Get values
    vals <- rep(NA_real_, length(l_idx))
    # vals[is_acute] <- who_nbf_df[[acute_meal_col]][l_idx[is_acute]] * 6 # This is for Adherence formula
    vals[is_acute] <- who_nbf_df[[acute_meal_col]][l_idx[is_acute]]
    # vals[!is_acute] <- who_nbf_df[[reah_meal_col]][l_idx[!is_acute]] * 8 # This is for Ahderence formula
    vals[!is_acute] <- who_nbf_df[[reah_meal_col]][l_idx[!is_acute]]
    
    results[is_not_bf] <- vals
  }

  return(unname(results))
}

#########################################
##### Extract WHO Plumpy "quantity" #####
#########################################

extract_who_plumpy <- function(df, weight_col, plumpy_col,...,
                               who_plumpy_df, who_weight_col, nsachet_col){
  
  # Call the Gatekeeper
  .check_args_who_plumpy(df, weight_col, plumpy_col,
                         who_plumpy_df, who_weight_col, nsachet_col)
  # Initialization
  n <- nrow(df)
  results <- rep(NA_real_, n)
  
  # 1. Identify valid rows
  is_plumpy <- !is.na(df[[plumpy_col]]) & (df[[plumpy_col]] > 0)
  if (!any(is_plumpy)) return(results)
  
  if (any(is_plumpy)) {
    weights_plumpy <- df[[weight_col]][is_plumpy]
    
    # Get the indices from who_plumpy_df
    l_idx <- .get_who_lookup_idx(weights_plumpy, who_plumpy_df[[who_weight_col]])
    
    # Create a container for the results of the plumpy rows
    vals <- rep(NA_real_, length(l_idx))
    
    # Identify which rows actually found a match (non-NA)
    has_match <- !is.na(l_idx)
    
    # Only assign where a match exists
    vals[has_match] <- who_plumpy_df[[nsachet_col]][has_match]
    results[is_plumpy] <- vals
  }
  
  return(unname(results))
}


#########################################
##### Extract WHO zscore "quantity" #####
#########################################

extract_who_zscore <- function(df, weight_col, height_col, sex_col,...,
                               who_zscore_df, who_sex_col, who_height_col,
                               who_3z_col, who_less2z_col, who_2z_col, who_1z_col, who_median_col){
  
  # 1. Capture Symbols
  if (is.symbol(substitute(weight_col)))  weight_col  <- deparse(substitute(weight_col))
  if (is.symbol(substitute(height_col)))  height_col  <- deparse(substitute(height_col))
  if (is.symbol(substitute(sex_col)))     sex_col     <- deparse(substitute(sex_col))

  if (is.symbol(substitute(who_sex_col)))     who_sex_col      <- deparse(substitute(who_sex_col))
  if (is.symbol(substitute(who_height_col)))  who_height_col   <- deparse(substitute(who_height_col))
  if (is.symbol(substitute(who_3z_col)))      who_3z_col       <- deparse(substitute(who_3z_col))
  if (is.symbol(substitute(who_less2z_col)))  who_less2z_col   <- deparse(substitute(who_less2z_col))
  if (is.symbol(substitute(who_2z_col)))      who_2z_col       <- deparse(substitute(who_2z_col))
  if (is.symbol(substitute(who_1z_col)))      who_1z_col       <- deparse(substitute(who_1z_col))
  if (is.symbol(substitute(who_median_col)))  who_median_col   <- deparse(substitute(who_median_col))
  
  # Call the Gatekeeper
  .check_args_who_zscore(df, weight_col, height_col, sex_col,
                         who_zscore_df, who_sex_col, who_height_col,
                         who_3z_col, who_less2z_col, who_2z_col, who_1z_col, who_median_col)
  
  # Initialization
  n <- nrow(df)
  results <- rep(NA, n)
  
  # 1. Identify valid rows
  is_valid <- !is.na(df[[weight_col]]) & !is.na(df[[height_col]]) & !is.na(df[[sex_col]])
  if (!any(is_valid)) return(results)
  
  # We split the processing into Masculin vs Female
  idx_male <- is_valid & df[[sex_col]] == "M"
  idx_female <- is_valid & df[[sex_col]] != "M"
  
  # Split WHO DF based on oedema labels used in your lookup table
  male_df <- who_zscore_df[who_zscore_df[[who_sex_col]] == "M", ]
  female_df <- who_zscore_df[who_zscore_df[[who_sex_col]] != "M", ]
  
  # Internal helper to process each group
  classify_zscore <- function(target_rows, lookup_table){
    if (length(target_rows) == 0) return(NULL)
    
    # 1. Get row indices from the lookup table (using the vectorized ceiling helper)
    w_idx <- .get_who_lookup_idx(df[[height_col]][target_rows], lookup_table[[who_height_col]])
    
    # 2. Build the lookup matrix (x12, x8, x6 columns)
    # We use na.omit/handling because w_idx might contain NAs from out-of-bounds weights
    val_matrix <- cbind(lookup_table[[who_3z_col]][w_idx],
                        lookup_table[[who_less2z_col]][w_idx],
                        lookup_table[[who_2z_col]][w_idx],
                        lookup_table[[who_1z_col]][w_idx],
                        lookup_table[[who_median_col]][w_idx])
    
    input_vals <- df[[weight_col]][target_rows]
    
    # 3. Compare input_vals (vector) to val_matrix (matrix)
    logical_test <- input_vals >= val_matrix
    
    # 4. Sum the rows, treating NAs as FALSE (they didn't pass the threshold)
    # If a threshold is NA, the child can't pass it.
    thresholds_met <- rowSums(logical_test, na.rm = TRUE)
    
    # 5. Final Mapping
    labels <- c("inf-3", "-3", "inf-2", "-2", "-1", "Mediane")
    
    # 6. Map the sums (0-5) to labels (indices 1-6)
    z_categories <- labels[thresholds_met + 1]
    
    # 7. Safety Check: Handle cases where the input was NA 
    # or where the threshold row was entirely empty
    all_thresholds_missing <- rowSums(!is.na(val_matrix)) == 0
    z_categories[is.na(input_vals) | all_thresholds_missing] <- NA
    
    return(z_categories)
  }
  
  # Assign values back to the original order
  results[idx_male] <- classify_zscore(idx_male, male_df)
  results[idx_female] <- classify_zscore(idx_female, female_df)
  
  return(unname(results))
}


##### ***************************************** #####
##### Inclusion criteria functions for analyses #####
##### ***************************************** #####

#############################################################
##### Track state transition across phases of group IDs #####
#############################################################

detect_changes <- function(df, id_col, phase_col, val_col,..., attr_col = NULL, from, to = NULL, run_order = NULL, ids){
  # Call the gatekeeper
  
  # 1. Pre-process values (Vectorized)
  phases <- as.character(df[[phase_col]])
  vals <- as.character(df[[val_col]])
  vals[is.na(vals)] <- "MISSING"
  
  # 2. Attribute Logic (Vectorized)
  if (!is.null(attr_col)) {
    attrs <- as.character(df[[attr_col]])
    attrs[is.na(attrs)] <- "MISSING"
    
    # Only paste where we have valid data; use logic to avoid unnecessary string ops
    mask <- attrs != "MISSING" & vals != "MISSING"
    vals[mask] <- paste0(vals[mask], "+", attr_col)
    # Note: If you meant the value in attr_col, use attrs[mask] instead of attr_col
  }
  
  # Split the target column by the ID column
  id_factor <- as.character(df[[id_col]])
  values_list <- split(vals, id_factor)
  phases_list <- split(phases, id_factor)
  
  # Convert ID to character for list indexing
  ids_char <- as.character(ids)
  
  # Iterate 'ids' to ensure the output order matches the input order
  results <- vapply(ids_char, function(id_char){
    # check if the ID exists in the list
    if (!(id_char %in% names(phases_list))) return(NA)
    
    # Extract vector
    phases_vec <- phases_list[[id_char]]
    values_vec <- values_list[[id_char]]
    
    # If a run_order is provided
    if(!is.null(run_order)){
      run_idx <- .get_series_idx(phases_vec, run_order)
      if(any(!is.na(run_idx))) values_vec <- values_vec[run_idx]
    }
    
    # Change detection logic
    if(length(values_vec) >= 2){
      
      # Find first occurrence of the 'from' state
      from_indices <- which(values_vec %in% from)
      if(length(from_indices) == 0) return(FALSE)
      
      first_from <- from_indices[1]
      
      # FIX: Edge case where 'from' is the last element
      if (first_from == length(values_vec)) return(FALSE)
      
      # Logic for 'to' state
      if(is.null(to)){
        # Look for the first state that isn't 'from' AFTER the first 'from'
        after_from_vec <- values_vec[(first_from + 1):length(values_vec)]
        
        return(any(!(after_from_vec %in% from)))
      } else {
        # Look for the first specific 'to' state AFTER the first 'from'
        to_indices <- which(values_vec %in% to)

        # Returns TRUE if any 'to' occurs after the first 'from'
        return(any(to_indices > first_from))
      }

    }else{
      return(FALSE)
    }
  }, FUN.VALUE = logical(1))
  
  return(unname(results))
}


###########################################################
##### Get the length/number of times an event occured #####
###########################################################

get_event_length <- function(df, id_col, phase_col, event,...,
                             col_names = c(), run_order, func = c("any", "sum"), ids) {
  
  # Split once the whole dataframe to avoid repeated subsetting
  split_df <- split(df, as.character(df[[id_col]]))
  
  # Iterate 'ids' to ensure the output order matches the input order
  results <- vapply(ids, function(id) {
    
    # Check if the ID exists in our data
    sub_df <- split_df[[as.character(id)]]
    # Return NA if the ID is missing from the data
    if (is.null(sub_df)) return(NA_real_)
    
    # Apply run_order filter if provided
    if (!is.null(run_order)) {
      run_idx <- .get_series_idx(sub_df[[phase_col]], run_order)
      if(any(!is.na(run_idx))) sub_df <- sub_df[run_idx, ]
    }
    
    # Extract columns and handle logical checks
    vals <- sub_df[, col_names, drop = FALSE]
    # Perform calculation based on 'func'
    if (func == "any") {
      # Count rows where the event appears at least once
      sum(apply(vals == event, 1, any), na.rm = TRUE)
    } else {
      # Sum total occurrences across all selected columns
      sum(vals == event, na.rm = TRUE)
    }
    
  }, numeric(1))
  
  return(unname(results))
}


#################################################
##### Calculate weight gain who2003 formula #####
#################################################

# calculate_weight_gain_who2003 <- function(df, id_col, phase_col, event,...,
#                                           col_names = c(), run_order, func = c("any", "sum"), ids){
#   
#   
# }






