# --- Prepare & Harmonize Monitoring Data  ---
# Purpose: Sorting, Extract the data of Nutritional Indicators, joins, reshaping, derived datasets

add_who_estimates <- function(follow_up_data, profile_data, who_std_tables){
  
  follow_up_data_enriched <- follow_up_data %>%
    
    # Enrich with Child Data ----------------------------------------------------
  left_join(profile_data |> select(id, sexe, ageMois, allait), by = "id") %>%
    
    # Ordering
    arrange(id, dateVis) %>%
    
    # Nutritional Analysis ------------------------------------------------------
  mutate(
    
    who_f75x_freq = assign_who_f75x(
      ., "poids", "typeAlim", "oedemes", "mlRepas",
      type_food = c("F75", "F75F"),
      measure_type = "frequency",
      who_f75_data = who_std_tables[["F75"]],
      who_f75_oed3_data = who_std_tables[["F75_oed3"]]
    ),
    
    who_f75x_qty  = assign_who_f75x(
      ., "poids", "typeAlim", "oedemes", "nRepJr",
      type_food = c("F75", "F75F") ,
      measure_type = "quantity",
      who_f75_data = who_std_tables[["F75"]],
      who_f75_oed3_data = who_std_tables[["F75_oed3"]]
    ),
    
    f75x_cond = typeAlim %in% c("F75", "F75F"),
    f75x_comp = sign(nRepJr - who_f75x_freq),
    f75x_freq_validation = ifelse(f75x_cond, c("less","equal","more")[f75x_comp + 2], NA),
    
    f75x_qty_validation = validate_f75x_qty(
      ., "poids", "typeAlim", "oedemes", "nRepJr", "mlRepas",
      type_food     = c("F75", "F75F"),
      who_f75_data = who_std_tables[["F75"]],
      who_f75_oed3_data = who_std_tables[["F75_oed3"]]
    ),
    
    f75x_adh_validation = validate_f75x_adh(
      ., "poids", "typeAlim", "oedemes", "mlRepas",
      meal_cols  = c("repasH1", "repasH2", "repasH3", "repasH4", "repasH5",
                     "repasH6", "repasH7", "repasH8", "repasH9", "repasH10"),
      type_food  = c("F75", "F75F"),
      who_f75_data = who_std_tables[["F75"]],
      who_f75_oed3_data = who_std_tables[["F75_oed3"]]
    ),
    
    who_f100_freq = assign_who_f100(
      ., "poids", "typeAlim", "mlRepas",
      measure_type = "frequency",
      who_f100_data  = who_std_tables[["F100"]]
    ),
    
    f100_cond = typeAlim %in% "F100",
    f100_comp = sign(nRepJr - who_f100_freq),
    f100_freq_validation = ifelse(f100_cond, c("less","equal","more")[f100_comp + 2], NA),
    
    who_f100_qty = assign_who_f100(
      ., "poids", "typeAlim", "mlRepas",
      measure_type = "quantity",
      who_f100_data  = who_std_tables[["F100"]]
    ),
    
    f100_qty_validation = validate_f100_qty(
      ., "poids", "typeAlim", "nRepJr", "mlRepas",
      who_f100_data = who_std_tables[["F100"]]
    ),
    
    f100_adh_validation = validate_f100_adh(
      ., "poids", "typeAlim", "mlRepas",
      meal_cols  = c("repasH1", "repasH2", "repasH3", "repasH4", "repasH5",
                     "repasH6", "repasH7", "repasH8", "repasH9", "repasH10"),
      who_f100_data = who_std_tables[["F100"]]
    ),
    
    who_f100d_freq = assign_who_f100d_freq(., "typeAlim"),
    
    f100d_cond = typeAlim %in% "F100d",
    f100d_comp = sign(nRepJr - who_f100d_freq),
    f100d_freq_validation = ifelse(f100d_cond, c("less","equal","more")[f100d_comp + 2], NA),
    
    who_f100d_qty  = assign_who_f100d_qty(
      ., "poids", "typeAlim", "allait",
      who_f100d_bf_data  = who_std_tables[["F100d_bf"]],
      who_f100d_nbf_data = who_std_tables[["F100d_nbf"]]
    ),
    
    f100d_qty_validation = validate_f100d_qty(
      ., "poids", "typeAlim", "oedemes", "mlRepas",
      type_food = "F100d",
      who_f100d_bf_data = who_std_tables[["F100d_bf"]],
      who_f100d_nbf_data = who_std_tables[["F100d_nbf"]]
    ),
    
    f100d_adh_validation = validate_f100d_adh(
      ., "poids", "typeAlim", "mlRepas", "oedemes", "mlRepas",
      meal_cols  = c("repasH1", "repasH2", "repasH3", "repasH4", "repasH5",
                     "repasH6", "repasH7", "repasH8", "repasH9", "repasH10"),
      who_f100d_bf_data = who_std_tables[["F100d_bf"]],
      who_f100d_nbf_data = who_std_tables[["F100d_nbf"]]
    ),
    
    who_rutf = assign_who_rutf(
      ., "poids", "ATPE", 
      rutf_lkp_df = who_std_tables[["RUTF"]]
    ),
    
    rutf_cond = !is.na(typeAlim),
    rutf_comp = sign(ATPE - who_rutf),
    rutf_validation = ifelse(rutf_cond, c("less","equal","more")[rutf_comp + 2], NA),
    
    who_whz = assign_who_whz(
      ., "poids", "taille", "sexe", "ageMois",
      c("-4", "-3", "-2", "-1", "Mediane"),
      wfh_boys02_lkp_df = who_std_tables[["WFH_b02"]],
      wfh_boys25_lkp_df = who_std_tables[["WFH_b25"]],
      wfh_girls02_lkp_df = who_std_tables[["WFH_g02"]],
      wfh_girls25_lkp_df = who_std_tables[["WFH_g25"]]
    ),
    
    whz_validation = case_when(is.na(ptZscore) | is.na(who_whz) ~ NA, ptZscore == who_whz ~ "Equal", TRUE ~ "Not Equal"),
    
    food_intolerance = vomissCorrige >= 1 | selles >= 4
  )
  
  return(follow_up_data_enriched)
}

