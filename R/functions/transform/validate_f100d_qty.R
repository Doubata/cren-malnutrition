
validate_f100d_qty <- function(df, weight_col, food_col, bf_col, qty_col, ...,
                               who_f100d_bf_data, who_f100d_nbf_data) {
  
  # --- 1. Robust Symbol/String Normalization ----------------------------------
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
  
  results <- rep(NA_character_, nrow(df))
  
  # --- 3. Base Filtering ------------------------------------------------------
  is_f100d <- !is.na(food) & food == "F100d"
  if (!any(is_f100d)) return(results)
  
  # --- Helper: Vectorized Range Categorization --------------------------------
  categorize_qty <- function(prescribed, target_limits) {
    res <- rep("within", length(prescribed))
    res[prescribed > target_limits & !is.na(target_limits)] <- "more"
    res[prescribed < target_limits & !is.na(target_limits)] <- "less"
    res[is.na(prescribed) | is.na(target_limits)] <- NA_character_
    return(res)
  }
  
  # --- 4. Breastfed Group Processing ------------------------------------------
  # "oui" or NA -> Breastfed
  is_bf <- is_f100d & (is.na(bf) | bf == "oui")
  
  if (any(is_bf)) {
    w_idx <- .lookup_who_f100d_idx(
      weight[is_bf],
      who_f100d_bf_data[["weight_kg_lower"]],
      who_f100d_bf_data[["weight_kg_upper"]]
    )
    
    limits <- who_f100d_bf_data[["ml_per_feed"]][w_idx]
    results[is_bf] <- categorize_qty(qty[is_bf], limits)
  }
  
  # --- 5. Non-Breastfed Group Processing --------------------------------------
  is_not_bf <- is_f100d & !is.na(bf) & bf != "oui"
  
  if (any(is_not_bf)) {
    w_idx <- .lookup_who_f100d_idx(
      weight[is_not_bf],
      who_f100d_nbf_data[["weight_kg_lower"]],
      who_f100d_nbf_data[["weight_kg_upper"]]
    )
    
    # Phase distinction based on weight index threshold
    is_acute <- !is.na(w_idx) & w_idx <= 7
    
    # Global indices within `df` corresponding to Non-BF rows
    nbf_indices <- which(is_not_bf)
    
    # --- 5a. Acute / Severe Phase (w_idx <= 7) ---
    if (any(is_acute)) {
      acute_df_idx <- nbf_indices[is_acute]
      acute_w_idx  <- w_idx[is_acute]
      
      limits_acute <- who_f100d_nbf_data[["ml_severe_phase"]][acute_w_idx]
      results[acute_df_idx] <- categorize_qty(qty[acute_df_idx], limits_acute)
    }
    
    # --- 5b. Rehabilitation Phase (w_idx > 7) ---
    is_reha <- !is.na(w_idx) & w_idx > 7
    if (any(is_reha)) {
      reha_df_idx <- nbf_indices[is_reha]
      reha_w_idx  <- w_idx[is_reha]
      
      limits_reha <- who_f100d_nbf_data[["ml_reha_phase"]][reha_w_idx]
      results[reha_df_idx] <- categorize_qty(qty[reha_df_idx], limits_reha)
    }
  }
  
  return(results)
}