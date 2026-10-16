
validate_f100d_adh <- function(df, weight_col, food_col, bf_col, qty_col, meal_cols,
                               ...,
                               type_food = "F100d",
                               who_f100d_bf_data, 
                               who_f100d_nbf_data) {
  
  # --- 1. Symbol/String Normalization -----------------------------------------
  get_col_str <- function(col_expr) {
    expr <- substitute(col_expr)
    if (is.character(expr)) return(expr)
    if (is.name(expr)) return(as.character(expr))
    return(as.character(col_expr))
  }
  
  weight_col <- get_col_str(weight_col)
  food_col   <- get_col_str(food_col)
  bf_col     <- get_col_str(bf_col)
  qty_col    <- get_col_str(qty_col)
  
  # --- 2. Vector Extraction ---------------------------------------------------
  weight <- df[[weight_col]]
  food   <- df[[food_col]]
  bf     <- df[[bf_col]]
  qty    <- df[[qty_col]]
  
  # --- 3. Initialisation ------------------------------------------------------
  results <- rep(NA_character_, nrow(df))
  
  # --- 4. Food Filter ---------------------------------------------------------
  is_f100d <- !is.na(food) & food %in% type_food
  if (!any(is_f100d)) return(results)
  
  # --- 5. Helper: Core Intake Evaluator ---------------------------------------
  # Vectorized comparison against upper/lower target bounds
  evaluate_intake <- function(sub_df_idx, lkp_w_idx, max_vec, min_vec) {
    if (length(sub_df_idx) == 0) return(character(0))
    
    # Coerce intake matrix efficiently
    intake_matrix <- as.matrix(df[sub_df_idx, meal_cols, drop = FALSE])
    storage.mode(intake_matrix) <- "character"
    
    # Aggregate meal records to daily intake
    daily_intakes <- .get_daily_intake_batch(qty[sub_df_idx], intake_matrix)
    
    valid_w <- !is.na(lkp_w_idx)
    res <- rep(NA_character_, length(sub_df_idx))
    
    if (any(valid_w)) {
      curr_intakes <- daily_intakes[valid_w]
      curr_max     <- max_vec[lkp_w_idx[valid_w]]
      curr_min     <- min_vec[lkp_w_idx[valid_w]]
      
      sub_res <- rep("within", length(curr_intakes))
      sub_res[curr_intakes > curr_max & !is.na(curr_max)] <- "more"
      sub_res[curr_intakes < curr_min & !is.na(curr_min)] <- "less"
      
      # Catch cases where intake calculation yields NA
      sub_res[is.na(curr_intakes)] <- NA_character_
      
      res[valid_w] <- sub_res
    }
    return(res)
  }
  
  # --- 6. Group 1: Breastfed Patients ----------------------------------------
  # "oui" or NA in bf_col -> Breastfed group
  is_bf <- is_f100d & (is.na(bf) | bf == "oui")
  
  if (any(is_bf)) {
    idx_bf <- which(is_bf)
    w_idx  <- .lookup_who_f100d_idx(
      weight[idx_bf],
      who_f100d_bf_data[["weight_kg_lower"]],
      who_f100d_bf_data[["weight_kg_upper"]]
    )
    
    results[idx_bf] <- evaluate_intake(
      sub_df_idx = idx_bf,
      lkp_w_idx  = w_idx,
      max_vec    = who_f100d_bf_data[["max_ml_per_day"]],
      min_vec    = who_f100d_bf_data[["min_ml_per_day"]]
    )
  }
  
  # --- 7. Group 2: Non-Breastfed Patients ------------------------------------
  is_not_bf <- is_f100d & !is.na(bf) & bf != "oui"
  
  if (any(is_not_bf)) {
    idx_nbf <- which(is_not_bf)
    w_idx   <- .lookup_who_f100d_idx(
      weight[idx_nbf],
      who_f100d_nbf_data[["weight_kg_lower"]],
      who_f100d_nbf_data[["weight_kg_upper"]]
    )
    
    # Phase Split: Indices 1-7 indicate acute/severe phase
    is_acute <- !is.na(w_idx) & w_idx <= 7
    is_reha  <- !is.na(w_idx) & w_idx > 7
    
    # --- 7a. Acute / Severe Phase ---
    if (any(is_acute)) {
      acute_idx   <- idx_nbf[is_acute]
      acute_w_idx <- w_idx[is_acute]
      
      results[acute_idx] <- evaluate_intake(
        sub_df_idx = acute_idx,
        lkp_w_idx  = acute_w_idx,
        max_vec    = who_f100d_nbf_data[["max_ml_severe_phase"]],
        min_vec    = who_f100d_nbf_data[["min_ml_severe_phase"]]
      )
    }
    
    # --- 7b. Rehabilitation Phase ---
    if (any(is_reha)) {
      reha_idx   <- idx_nbf[is_reha]
      reha_w_idx <- w_idx[is_reha]
      
      results[reha_idx] <- evaluate_intake(
        sub_df_idx = reha_idx,
        lkp_w_idx  = reha_w_idx,
        max_vec    = who_f100d_nbf_data[["max_ml_reha_phase"]],
        min_vec    = who_f100d_nbf_data[["min_ml_reha_phase"]]
      )
    }
  }
  
  return(results)
}
