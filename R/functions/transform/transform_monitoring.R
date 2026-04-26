# --- Prepare & Harmonize Monitoring Data  ---
# Purpose: Sorting, Extract the data of Nutritional Indicators, joins, reshaping, derived datasets

transform_monitoring <- function(monitoring_df, child_df){
  
  monitoring_final <- monitoring_df %>%

    # Enrich with Child Data ----------------------------------------------------
  left_join(child_df |> select(id, sexe, ageMois, allait), by = "id") %>%
    
    # Ordering
    arrange(id, dateVis) %>%
    
    # Nutritional Analysis ------------------------------------------------------
  mutate(
    
    who_f75x_freq = assign_who_f75x(
      ., "poids", "typeAlim", "oedemes", "mlRepas",
      type_food = c("F75", "F75F"),
      measure_type = "frequency",
      f75_lkp_df = WHO_F75_std,
      f75_oed3_lkp_df = WHO_F75_oed3_std
    ),
    
    who_f75x_qty  = assign_who_f75x(
      ., "poids", "typeAlim", "oedemes", "nRepJr",
      type_food = c("F75", "F75F") ,
      measure_type = "quantity",
      f75_lkp_df = WHO_F75_std,
      f75_oed3_lkp_df = WHO_F75_oed3_std
    ),
    
    f75x_cond = typeAlim %in% c("F75", "F75F"),
    f75x_comp = sign(nRepJr - who_f75x_freq),
    
    f75x_freq_validation = ifelse(f75x_cond, c("less","equal","more")[f75x_comp + 2], NA),
    
    f75x_qty_validation = validate_f75x_qty(
      ., "poids", "typeAlim", "oedemes", "nRepJr", "mlRepas",
      type_food     = c("F75", "F75F"),
      f75_lkp_df = WHO_F75_std,
      f75_oed3_lkp_df = WHO_F75_oed3_std
    ),
    
    f75x_adh_validation = validate_f75x_adh(
      ., "poids", "typeAlim", "oedemes", "mlRepas",
      meal_cols  = c("repasH1", "repasH2", "repasH3", "repasH4", "repasH5",
                     "repasH6", "repasH7", "repasH8", "repasH9", "repasH10"),
      type_food  = c("F75", "F75F"),
      f75_lkp_df = WHO_F75_std,
      f75_oed3_lkp_df = WHO_F75_oed3_std
    ),
    
    who_f100_freq = assign_who_f100(
      ., "poids", "typeAlim", "mlRepas",
      measure_type = "frequency",
      f100_lkp_df  = WHO_F100_std
    ),
    
    who_f100_qty = assign_who_f100(
      ., "poids", "typeAlim", "mlRepas",
      measure_type = "quantity",
      f100_lkp_df  = WHO_F100_std
    ),
    
    f100_qty_validation = validate_f100_qty(
      ., "poids", "typeAlim", "nRepJr", "mlRepas",
      f100_lkp_df = WHO_F100_std
    ),
    
    who_f100d_freq = assign_who_f100d_freq(., "typeAlim"),
    
    who_f100d_qty  = assign_who_f100d_qty(
      ., "poids", "typeAlim", "allait",
      f100d_bf_lkp_df  = WHO_F100d_bf,
      f100d_nbf_lkp_df = WHO_F100d_nbf
    ),
    
    who_plumpy = assign_who_rutf(
      ., "poids", "ATPE", 
      rutf_lkp_df = WHO_RUTF_std
    ),
    
    who_whz = assign_who_whz(
      ., "poids", "taille", "sexe", "ageMois",
      c("-4", "-3", "-2", "-1", "Mediane"),
      wfh_boys02_lkp_df = WHO_WFH_boys02_std,
      wfh_boys25_lkp_df = WHO_WFH_boys25_std,
      wfh_girls02_lkp_df = WHO_WFH_girls02_std,
      wfh_girls25_lkp_df = WHO_WFH_girls25_std
    ),
    
    whz_validation = case_when(is.na(whz) | is.na(who_whz) ~ NA, whz == who_whz ~ "Equal", TRUE ~ "Not Equal"),
    
    food_intolerance = vomissCorrige >= 1 | selles >= 4
  )
  
    
    # select(-c("id", "dateVis", "taille", "poids", "ptZscore", "pbOrPc",
    #           "pbOrPcV", "oedemes", "phase", "nRepJr", "typeAlim",
    #           "mlRepas", "repasH1", "repasH2", "repasH3", "repasH4",
    #           "repasH5", "repasH6", "repasH7", "repasH8", "repasH9",
    #           "repasH10", "ATPE", "bouillie", "bouillieCorrige",
    #           "plaFamil", "plaFamilCorrige", "aPlats", "selles",
    #           "vomiss", "vomissCorrige", "index"))
  
  return(monitoring_final)
}

