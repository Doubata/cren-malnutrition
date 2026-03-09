source(here::here("scripts", "00_setup.R"))
source(here::here("scripts", "01_data_cleaning", "00_import_data.R"))
source(here::here("R", "etl_functions.R"))

# --- Step 1: Prepare Monitoring Data and Merge Child Demographics ---
# Purpose: Type conversion, sorting, and joining for WHO calculations

# Load and Clean --------------------------------------------------------------
monitoring_clean <- monitoring_df %>%
  # 1. Type Conversions
  mutate(
    # Convert numeric columns at once
    across(c(poids, taille, pbOrPcV, ATPE), as.numeric),
    
    # Convert the date column separately
    dateVis = as.Date(dateVis, format = "%d/%m/%Y")
    
  ) %>%
  
  # Enrich with Child Data ----------------------------------------------------
  left_join(child_df |> select(id, sexe, allait), by = "id") %>%

  # Ordering
  arrange(id, dateVis) %>%
  
  # Nutritional Analysis ------------------------------------------------------
mutate(
  
  who_f75x_freq = extract_who_f75x(
    ., "poids", "typeAlim", "oedemes", "mlRepas",
    who_df = who_f75_df,
    who_weight_col = "poids",
    who_oedema_col = "oedemes",
    x12_col = "x12repas",
    x8_col = "x8repas",
    x6_col = "x6repas",
    type_food = c("F75", "F75F"),
    measure_type = "frequency"
  ),
  
  who_f75x_qty  = extract_who_f75x(
    ., "poids", "typeAlim", "oedemes", "nRepJr",
    who_df = who_f75_df,
    who_weight_col = "poids",
    who_oedema_col = "oedemes",
    x12_col = "x12repas",
    x8_col = "x8repas",
    x6_col = "x6repas",
    type_food = c("F75", "F75F") ,
    measure_type = "quantity"
  ),
  
  f75x_cond     = typeAlim %in% c("F75", "F75F"),
  f75x_comp     = sign(nRepJr - who_f75x_freq),
  
  f75x_freq_validation = ifelse(f75x_cond, c("less","equal","more")[f75x_comp + 2], NA),
  
  f75x_qty_validation  = validate_f75x_qty_intake(
    ., "poids", "typeAlim", "oedemes", "nRepJr", "mlRepas",
    who_df = who_f75_df,
    who_weight_col = "poids",
    who_oedema_col = "oedemes",
    max_col = "max",
    min_col = "min",
    type_food = c("F75", "F75F")
  ),
  
  f75x_adh_validation  = validate_f75x_qty_intake(
    ., "poids", "typeAlim", "oedemes", "nRepJr", "mlRepas",
    meal_cols = c("repasH1", "repasH2", "repasH3", "repasH4", "repasH5",
                  "repasH6", "repasH7", "repasH8", "repasH9", "repasH10"),
    who_df = who_f75_df,
    who_weight_col = "poids",
    who_oedema_col = "oedemes",
    max_col = "max",
    min_col = "min",
    type_food = c("F75", "F75F")
  ),
  
  who_f100_freq        = extract_who_f100(
    ., "poids", "typeAlim", "mlRepas",
    who_df = who_f100_df,
    who_weight_col = "poids",
    min_col = "min",
    max_col = "max",
    measure_type = "frequency"
  ),
  
  who_f100_qty         = extract_who_f100(
    ., "poids", "typeAlim", "mlRepas",
    who_df = who_f100_df,
    who_weight_col = "poids",
    min_col = "min",
    max_col = "max",
    measure_type = "quantity"
  ),
  
  who_f100_validation  = validate_f100_qty_intake(
    ., "poids", "typeAlim", "nRepJr", "mlRepas",
    who_df = who_f100_df,
    who_weight_col = "poids",
    min_col = "min",
    max_col = "max",
    type_food = c("F100")
  ),
  
  who_f100d_freq  = extract_who_f100d_freq(., "typeAlim"),
  
  who_f100d_qty   = extract_who_f100d_qty(
    ., "poids", "typeAlim", "allait",
    who_bf_df = who_f100d_bf_df,
    who_nbf_df = who_f100d_nbf_df,
    weight_min_col = "poidsMin",
    weight_max_col = "poidsMax",
    meal_col = "mlRepas", 
    acute_meal_col = "mlRepasPhaseAig",
    reah_meal_col = "mlRepasPhaseReha"
  ),
  
  who_plumpy = extract_who_plumpy(
    ., "poids", "ATPE", 
    who_plumpy_df = who_plumpynut_df,
    who_weight_col = "poids",
    nsachet_col = "nSachet"
  ),
  
  who_zscore = extract_who_zscore(
    ., "poids", "taille", "sexe", 
    who_zscore_df = who_ptz_df,
    who_sex_col = "sexe",
    who_height_col = "taille",
    who_3z_col = "moins3Z",
    who_less2z_col = "infMoins2Z",
    who_2z_col = "moins2Z",
    who_1z_col = "moins1Z",
    who_median_col = "mediane"
  )

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
    df        = monitoring_clean,
    id_col    = "id",
    val_col   = "phase", 
    run_order = 1, 
    ids       = id
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
  age_under_6       = if_else(ageMois <= 59, age_group, NA_character_),
  age_range_6_59    = if_else(ageMois >= 6 & ageMois <= 59, age_group, NA_character_),
  
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
  has_edema    = (coalesce(edema, 0) >= 1),
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
  across(starts_with("age_"),
         ~ factor(.x, levels = c("[0;6[", "[6;25[", "[25;59]", "]59;plus[")))
)

# table(monitoring_df$vomissCorrige)
# 
# table(monitoring_df$vomiss)
