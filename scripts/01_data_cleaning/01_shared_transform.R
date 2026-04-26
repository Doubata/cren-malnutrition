source(here::here("_init.R"))
source(here::here("scripts", "00_import_data.R", "01_data_cleaning"))

# --- Step 1: Prepare Monitoring Data and Merge Child Demographics ---
# Purpose: Type conversion, sorting, and joining for WHO calculations

# Load and Clean --------------------------------------------------------------
monitoring_clean <- monitoring_df %>%
  # 1. Type Conversions
  mutate(
    # Convert numeric columns at once
    across(c(poids, taille, pbOrPcV, ATPE), as.numeric),
    
    ptZscore = recode(ptZscore, "inf-4" = "-4", "inf-3" = "-3", "inf-2" = "-2"),
    
    # Convert the date column separately
    dateVis = as.Date(dateVis, format = "%d/%m/%Y")
    
  ) %>%
  
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
  
  whz_validation = case_when(is.na(ptZscore) | is.na(who_whz) ~ NA, ptZscore == who_whz ~ "Equal", TRUE ~ "Not Equal"),
  
  food_intolerance = vomissCorrige >= 1 | selles >= 4
)

# --- Step 2: Prepare & Harmonize Child and Morbidity Data  ---
# Purpose: Sorting, Extract the data of Nutritional Indicators and joining

morbidity_map <- c(
  "choc"                          = "has_shock",
  "pneumopathieViraleBacterienne" = "has_pneumonia",
  "paluGraveAnemique"             = "has_severe_malaria_anemia",
  "paluGraveNeurologique"         = "has_neuro_malaria",
  "autrePaluGrave"                = "has_other_severe_malaria",
  "autreAnemie"                   = "has_other_anemia",
  "candidoseDigestive"            = "has_digestive_candidiasis",
  "tuberculose"                   = "has_tuberculosis",
  "VIH"                           = "has_hiv"
)

child_clean <- child_df %>%
  arrange(id) %>%
  
  # Age groups and status definition ------------------------------------------
mutate(
  
  # . Sequence of Phases during the stay ####
  phase_series = get_series_str(
    df         = monitoring_clean,
    id_col     = "id",
    target_col = "phase",
    ids        = id
  ),
  
  # . Create the primary age group
  age_group = case_when(
    ageMois < 6                   ~ "[0;6[",
    ageMois >= 6  & ageMois  < 25 ~ "[6;25[",
    ageMois >= 25 & ageMois <= 59 ~ "[25;59]",
    ageMois > 59                  ~ "]59;plus[",
    TRUE                          ~ NA_character_
  ),
  
  # Create subsets dynamically based on the first variable
  age_under_6     = if_else(ageMois <= 59, age_group, NA_character_),
  age_range_6_59  = if_else(ageMois >= 6 & ageMois <= 59, age_group, NA_character_),
  
  # Character to Yes/No Factor
  is_breastfed   = case_when(allait == "oui" ~ TRUE, allait == "non" ~ FALSE, TRUE ~ NA),
  is_vaccinated  = case_when(vaccAJ == "oui" ~ TRUE, vaccAJ == "non" ~ FALSE, TRUE ~ NA),
  
) %>%
  
  # Nutritional Indicators ----------------------------------------------------
mutate(
  # Set the first value of the earliest day ---------------------------------
  height           = get_value_at(monitoring_df, "id", "taille",   1, ids = child_df$id),
  weight           = get_value_at(monitoring_df, "id", "poids",    1, ids = child_df$id),
  whz              = get_value_at(monitoring_df, "id", "ptZscore", 1, ids = child_df$id),
  muac             = get_value_at(monitoring_df, "id", "pbOrPc",   1, ids = child_df$id),
  muac_val         = get_value_at(monitoring_df, "id", "pbOrPcV",  1, ids = child_df$id),
  edema            = get_value_at(monitoring_df, "id", "oedemes",  1, ids = child_df$id),
  admission_phase  = get_value_at(monitoring_df, "id", "phase",    1, ids = child_df$id),
  
  # Edema status (Using T/F)
  # Using coalesce to treat NA as 0/False
  has_edema = (coalesce(edema, 0) >= 1),
) %>%
  
  # Enrich with Morbidity Data ------------------------------------------------
left_join(morbidity_df , by = "id") %>%
  
  # Re-coding of morbidity variables ------------------------------------------
mutate(
  # Handle composite variables
  morbidite      = rowSums(across(c(gastroenteriteAigue, deshydratation, choc, pneumopathieViraleBacterienne,
                                    paluGraveAnemique, paluGraveNeurologique, autrePaluGrave, 
                                    candidoseDigestive, tuberculose, VIH)), na.rm = TRUE),
  
  severe_malaria = rowSums(across(c(paluGraveAnemique, paluGraveNeurologique, autrePaluGrave)), na.rm = TRUE),
  
  # Dynamic logical conversion of specific columns and converts 1 to TRUE, 0 to FALSE
  across(c(choc, pneumopathieViraleBacterienne, paluGraveAnemique,paluGraveNeurologique,
           autrePaluGrave, autreAnemie, candidoseDigestive, tuberculose, VIH),
         ~ .x == 1, .names = "{morbidity_map[.col]}"),
  
  # Handle the OR logic for combined categories
  has_morbidite        = morbidite             >= 1,
  has_severe_malaria   = severe_malaria        >= 1,
  has_gastroenteritis  = (gastroenteriteAigue  == 1 | deshydratation == 1),
  has_severe_anaemia   = (paluGraveAnemique    == 1 | autreAnemie    == 1)
) %>%
  
  # Formatting for factors  ---------------------------------------------------
# Turn every logical column into a factor
mutate(
  across(where(is.logical), ~ factor(.x, levels = c(FALSE, TRUE))),
  across(starts_with("age_"), ~ factor(.x, levels = c("[0;6[", "[6;25[", "[25;59]", "]59;plus[")))
)

