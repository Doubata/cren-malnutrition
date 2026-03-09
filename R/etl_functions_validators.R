# library(checkmate)
source(here::here("scripts", "00_setup.R"))


###########################################################
##### Helper to validate a dataframe with its columns #####
###########################################################

.check_df_columns <- function(data, col_requirements, coll) {
  if (!test_data_frame(data)) return(NULL)
  
  for (arg_name in names(col_requirements)) {
    req <- col_requirements[[arg_name]]
    col_val <- req$value
    expected_type <- req$type
    is_required <- isTRUE(req$required)
    var_label <- paste0(arg_name, "_col")
    
    # Handle NULL (Optional Argument)
    if (is.null(col_val)) {
      if (is_required) {
        coll$push(paste0("Variable '", var_label, "' is mandatory and cannot be NULL."))
      }
      next 
    }
    
    # A. Validate the argument is a string (the column name)
    assert_string(col_val, min.chars = 1, .var.name = var_label, add = coll)
    
    if (test_string(col_val)) {
      # B. Check existence in dataframe
      assert_names(names(data), must.include = col_val, add = coll)
      
      if (col_val %in% names(data)) {
        # C. Type & Value Dispatch
        column_data <- data[[col_val]]
        
        switch(expected_type,
               "numeric"   = assert_numeric(column_data, .var.name = col_val, add = coll),
               "character" = assert_character(column_data, .var.name = col_val, add = coll),
               # Hybrid Type Check
               "numeric_or_character" = {
                 if (!test_numeric(column_data) && !test_character(column_data)) {
                   coll$push(paste0("Variable '", col_val, "' must be numeric or character, but is ", class(column_data)[1]))
                 }
               },
               "choice"    = assert_subset(column_data, choices = req$choices, 
                                           empty.ok = FALSE, .var.name = col_val, add = coll),
               stop(paste("Unknown validation type:", expected_type))
        )
      }
    }
  }
}

#################################################
##### Validate get_value_at function inputs #####
#################################################

.check_args_value_at <- function(df, id_col, val_col, row_index, ids){
  coll <- makeAssertCollection()
  
  # 1. Structure Checks
  assert_data_frame(df, min.rows = 1, add = coll)
  
  # 2. Main Dataframe: Individual Argument & Data Checks
  df_reqs <- list(
    weight  = list(value = id_col,    type = "numeric",               required = TRUE),
    value   = list(value = val_col,   type = "numeric_or_character",  required = TRUE)
  )
  .check_df_columns(df, df_reqs, coll)
  
  # 4. Check if row_index is a single positive integer
  assert_count(row_index, positive = TRUE)
  
  # 3. Check ids
  assert_vector(ids, min.len = 1, add = coll)
  
  # 5. Report all errors at once if any occurred
  reportAssertions(coll)
}

###################################################
##### Validate get_series_str function inputs #####
###################################################

.check_args_series_str <- function(df, id_col, val_col, attr_col, ids){
  coll <- makeAssertCollection()
  
  # 1. Structure Checks
  assert_data_frame(df, min.rows = 1, add = coll)
  
  # 2. Main Dataframe: Individual Argument & Data Checks
  df_reqs <- list(
    weight  = list(value = id_col,    type = "numeric",    required = TRUE),
    value   = list(value = val_col,   type = "character",  required = TRUE),
    attr    = list(value = attr_col,  type = "numeric",    required = FALSE)
  )
  .check_df_columns(df, df_reqs, coll)
  
  # 3. Check ids
  assert_vector(ids, min.len = 1, add = coll)
  
  # 4. Report all errors at once if any occurred
  reportAssertions(coll)
}

#######################################################
##### Validate extract_matching_series_str inputs #####
#######################################################

.check_args_matching_series_str <- function(df, id_col, val_col, ref_col, attr_col = NULL, run_order, ids){
  coll <- makeAssertCollection()

  # 1. Structure Checks
  assert_data_frame(df, min.rows = 1, add = coll)
  
  # 2. Main Dataframe: Individual Argument & Data Checks
  df_reqs <- list(
    weight  = list(value = id_col,    type = "numeric",    required = TRUE),
    value   = list(value = val_col,   type = "character",  required = TRUE),
    ref     = list(value = ref_col,   type = "character",  required = TRUE),
    attr    = list(value = attr_col,  type = "numeric",    required = FALSE)
  )
  .check_df_columns(df, df_reqs, coll)
  
  # 3. Check run_order value
  assert_count(run_order, positive = TRUE, add = coll)
  
  # 4. Check ids
  assert_vector(ids, min.len = 1, add = coll)
  
  # 5. Report all errors at once if any occurred
  reportAssertions(coll)
}

###################################################
##### Validate get_series_str function inputs #####
###################################################

.check_args_series_length_at <- function(df, id_col, val_col, run_order, ids){
  coll <- makeAssertCollection()
  
  # 1. Structure Checks
  assert_data_frame(df, min.rows = 1, add = coll)
  
  # 2. Main Dataframe: Individual Argument & Data Checks
  df_reqs <- list(
    weight  = list(value = id_col,    type = "numeric",   required = TRUE),
    value   = list(value = val_col,   type = "character", required = TRUE)
  )
  .check_df_columns(df, df_reqs, coll)
  
  # 3. Check if run_order is a single positive integer
  assert_count(run_order, positive = TRUE, add = coll)
  
  # 4. Check ids
  assert_vector(ids, min.len = 1, add = coll)
  
  # 5. Report all errors at once if any occurred
  reportAssertions(coll)
}

#############################################
##### Validate count_occurrences inputs #####
#############################################

.check_args_count_occurrences <- function(df, id_col, val_col, attr_col = NULL, criteria = NULL, ids){
  coll <- makeAssertCollection()
  
  # 1. Structure Checks
  assert_data_frame(df, min.rows = 1, add = coll)
  
  # 2. Main Dataframe: Individual Argument & Data Checks
  df_reqs <- list(
    weight  = list(value = id_col,    type = "numeric",   required = TRUE),
    value   = list(value = val_col,   type = "character", required = TRUE),
    attr    = list(value = attr_col,  type = "character", required = FALSE)
  )
  .check_df_columns(df, df_reqs, coll)
  
  # 3. Adding constraints (length and missing values)
  assert_atomic_vector(criteria, min.len = 1, add = coll)
  
  # 4. Check ids
  assert_vector(ids, min.len = 1, add = coll)
  
  reportAssertions(coll)
}

############################################
##### Validate extract_who_f75x inputs #####
############################################

.check_args_who_f75x <- function(df, weight_col, food_col, oedema_col, val_col,
                                 who_df, who_weight_col, who_oedema_col, x12_col, x8_col, x6_col,
                                 type_food, measure_type) {
  
  coll <- makeAssertCollection()
  
  # 1. Structure Checks
  assert_data_frame(df, min.rows = 1, add = coll)
  assert_data_frame(who_df, min.rows = 1, add = coll)
  
  # 2. Main Dataframe: Individual Argument & Data Checks
  df_reqs <- list(
    weight  = list(value = weight_col,  type = "numeric",   required = TRUE),
    food    = list(value = food_col,    type = "character", required = TRUE),
    oedema  = list(value = oedema_col,  type = "numeric",   required = TRUE),
    value   = list(value = val_col,     type = "numeric",   required = TRUE)
  )
  .check_df_columns(df, df_reqs, coll)
  
  # 3. WHO Reference: Individual Argument & Data Checks
  who_reqs <- list(
    who_weight  = list(value = who_weight_col, type = "numeric",   required = TRUE),
    who_oedema  = list(value = who_oedema_col, type = "character", required = TRUE),
    x12         = list(value = x12_col,        type = "numeric",   required = TRUE),
    x8          = list(value = x8_col,         type = "numeric",   required = TRUE),
    x6          = list(value = x6_col,         type = "numeric",   required = TRUE)
  )
  .check_df_columns(who_df, who_reqs, coll)
  
  # 3. Choices
  assert_subset(type_food, choices = c("F75", "F75F"), empty.ok = FALSE, add = coll)
  assert_subset(measure_type, choices = c("frequency", "quantity"), empty.ok = FALSE, add = coll)
  
  reportAssertions(coll)
}

############################################
##### Validate extract_who_f75x inputs #####
############################################

.check_args_who_f100 <- function(df, weight_col , food_col , val_col,
                                 who_df, who_weight_col, min_col, max_col,
                                 measure_type){
  
  coll <- makeAssertCollection()
  
  # 1. Structure Checks
  assert_data_frame(df, min.rows = 1, add = coll)
  assert_data_frame(who_df, min.rows = 1, add = coll)
  
  # 2. Main Dataframe: Individual Argument & Data Checks
  df_reqs <- list(
    weight  = list(value = weight_col,  type = "numeric",   required = TRUE),
    food    = list(value = food_col,    type = "character", required = TRUE),
    value   = list(value = val_col,     type = "numeric",   required = TRUE)
  )
  .check_df_columns(df, df_reqs, coll)
  
  # 3. WHO Reference: Individual Argument & Data Checks
  who_reqs <- list(
    who_weight = list(value = who_weight_col, type = "numeric",  required = TRUE),
    min        = list(value = min_col,    type = "numeric",      required = TRUE),
    max        = list(value = max_col,    type = "numeric",      required = TRUE)
  )
  .check_df_columns(who_df, who_reqs, coll)
  
  # 5. Choices
  assert_subset(measure_type, choices = c("frequency", "quantity"), empty.ok = FALSE, add = coll)
  
  reportAssertions(coll)
}

############################################
##### Validate extract_who_f75x inputs #####
############################################

.check_args_who_f100d <- function(df, weight_col, food_col, bf_col,
                                  who_bf_df, who_nbf_df, weight_min_col, weight_max_col,
                                  meal_col, acute_meal_col, reah_meal_col){
  
  coll <- makeAssertCollection()
  
  # 1. Structure Checks
  assert_data_frame(df, min.rows = 1, add = coll)
  assert_data_frame(who_bf_df, min.rows = 1, add = coll)
  assert_data_frame(who_nbf_df, min.rows = 1, add = coll)
  
  # 2. Main Dataframe: Individual Argument & Data Checks
  df_reqs <- list(
    weight  = list(value = weight_col, type = "numeric",   required = TRUE),
    food    = list(value = food_col,   type = "character", required = TRUE),
    bf      = list(value = bf_col,     type = "character", required = TRUE)
  )
  .check_df_columns(df, df_reqs, coll)
  
  # 3. WHO Reference: Individual Argument & Data Checks
  
  # who_bf_df
  who_bf_reqs <- list(
    weight_min  = list(value = weight_min_col, type = "numeric",  required = TRUE),
    weight_max  = list(value = weight_max_col, type = "numeric",  required = TRUE),
    meal        = list(value = meal_col,       type = "numeric",  required = TRUE)
  )
  .check_df_columns(who_bf_df, who_bf_reqs, coll)
  
  # who_nbf_df
  who_nbf_reqs <- list(
    weight_min  = list(value = weight_min_col, type = "numeric",  required = TRUE),
    weight_max  = list(value = weight_max_col, type = "numeric",  required = TRUE),
    acute_meal  = list(value = acute_meal_col, type = "numeric",  required = TRUE),
    reah_meal   = list(value = reah_meal_col,  type = "numeric",  required = TRUE)
  )
  .check_df_columns(who_nbf_df, who_nbf_reqs, coll)
  
  reportAssertions(coll)
}

##############################################
##### Validate extract_who_plumpy inputs #####
##############################################

.check_args_who_plumpy <- function(df, weight_col, plumpy_col,
                                   who_plumpy_df, who_weight_col, nsachet_col){
  
  coll <- makeAssertCollection()
  
  # 1. Structure Checks
  assert_data_frame(df, min.rows = 1, add = coll)
  assert_data_frame(who_plumpy_df, min.rows = 1, add = coll)
  
  # 2. Main Dataframe Requirements
  df_reqs <- list(
    weight    = list(value = weight_col,  type = "numeric",  required = TRUE),
    plumpy    = list(value = plumpy_col,  type = "numeric",  required = TRUE)
  )
  .check_df_columns(df, df_reqs, coll)
  
  # 3. WHO Reference: Individual Argument & Data Checks
  # who_plumpy_df
  who_reqs <- list(
    who_weight = list(value = who_weight_col, type = "numeric",  required = TRUE),
    nsachet    = list(value = nsachet_col,    type = "numeric",  required = TRUE)
  )
  .check_df_columns(who_plumpy_df, who_reqs, coll)
  
  reportAssertions(coll)
}

##############################################
##### Validate extract_who_zscore inputs #####
##############################################

.check_args_who_zscore <- function(df, weight_col, height_col, sex_col,
                                   who_zscore_df, who_sex_col, who_height_col,
                                   who_3z_col, who_less2z_col, who_2z_col, who_1z_col, who_median_col) {
  
  coll <- makeAssertCollection()
  
  # 1. Structure Checks
  assert_data_frame(df, min.rows = 1, add = coll)
  assert_data_frame(who_zscore_df, min.rows = 1, add = coll)
  
  # 2. Main Dataframe Requirements
  df_reqs <- list(
    weight    = list(value = weight_col,    type = "numeric",   required = TRUE),
    height    = list(value = height_col,    type = "numeric",   required = TRUE),
    sex       = list(value = sex_col,       type = "character", required = TRUE)
    # New: This column must only contain "F75", "F75F", or both.
    # food_type = list(value = food_type_col, type = "choice", choices = c("F75", "F75F"))
  )
  
  .check_df_columns(df, df_reqs, coll)
  
  # 3. WHO Reference Requirements
  who_reqs <- list(
    who_sex     = list(value = who_sex_col,     type = "character",   required = TRUE),
    who_height  = list(value = who_height_col,  type = "numeric",     required = TRUE),
    who_3z      = list(value = who_3z_col,      type = "numeric",     required = TRUE),
    who_less2z  = list(value = who_less2z_col,  type = "numeric",     required = TRUE),
    who_2z      = list(value = who_2z_col,      type = "numeric",     required = TRUE),
    who_1z      = list(value = who_1z_col,      type = "numeric",     required = TRUE),
    who_median  = list(value = who_median_col,  type = "numeric",     required = TRUE)
  )
  
  .check_df_columns(who_zscore_df, who_reqs, coll)
  
  reportAssertions(coll)
}







