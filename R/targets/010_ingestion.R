# ==============================================================================
# 01_ingestion.R
# PURPOSE : Load all raw data sources into the pipeline
# OUTPUTS : child_raw | monitoring_raw | morbidity_raw | who_std_raw (bundle)
#           + individual WHO tables (auto-generated)
# NAMING  : <subject>_raw  — untouched data straight from disk
# ==============================================================================

targets_ingestion <- list(
  
  # ----------------------------------------------------------------------------
  # CLINICAL DATA  (primary study tables)
  # ----------------------------------------------------------------------------
  
  tar_target(
    child_raw,
    readr::read_csv2(
      get_raw_data_path("fiche_10072023.csv"),
      locale     = locale(encoding = "UTF-8"),
      col_select = c(
        "id", "numMa", "zoneResidence", "modeAdm",
        "tAdm", "tAdmCorrige", "rechuCorrige",
        "sexe", "ageMois", "allait", "vaccAJ", "dateSor",
        "modeSorCorrige", "decede", "phaseDeces"
      ),
      guess_max      = 1000,
      show_col_types = FALSE
    ),
    description = "Raw child admission & discharge records"
  ),
  
  tar_target(
    monitoring_raw,
    readr::read_csv2(
      get_raw_data_path("suivi_11092023.csv"),
      locale     = locale(encoding = "UTF-8"),
      col_select = c(
        "id", "dateVis", "taille", "poids", "ptZscore",
        "pbOrPc", "pbOrPcV", "oedemes", "phase", "nRepJr",
        "typeAlim", "mlRepas",
        "repasH1", "repasH2", "repasH3", "repasH4", "repasH5",
        "repasH6", "repasH7", "repasH8", "repasH9", "repasH10",
        "ATPE", "bouillieCorrige",
        "plaFamil", "plaFamilCorrige", "aPlats",
        "selles", "vomiss", "vomissCorrige", "index"
      ),
      guess_max      = 2000,
      show_col_types = FALSE
    ),
    description = "Raw follow-up visit records (anthropometry + feeding)"
  ),
  
  tar_target(
    morbidity_raw,
    readr::read_csv2(
      get_raw_data_path("comorbidite_25052023.csv"),
      locale     = locale(encoding = "UTF-8"),
      col_select = c(
        "id",
        "gastroenteriteAigue", "deshydratation", "choc",
        "pneumopathieViraleBacterienne",
        "paluGraveAnemique", "paluGraveNeurologique", "autrePaluGrave",
        "autreAnemie", "candidoseDigestive", "tuberculose", "VIH"
      ),
      guess_max      = 1000,
      show_col_types = FALSE
    ),
    description = "Raw comorbidity / morbidity records"
  ),
  
  tar_target(
    codebook_file,
    readr::read_csv2(
      get_raw_data_path("variable_codebook.csv"),
      locale = locale(encoding = "UTF-8"),
      show_col_types = FALSE,
      na = c("", "NA")
    )
  ),
  
  tar_target(
    domain_registry_file,
    readr::read_csv2(
      get_raw_data_path("domains.csv"),
      locale = locale(encoding = "UTF-8"),
      show_col_types = FALSE,
      na = c("", "NA")
    )
  ),
  
  tar_target(
    subgroup_registry_file,
    readr::read_csv2(
      get_raw_data_path("domain_subgroups.csv"),
      locale = locale(encoding = "UTF-8"),
      show_col_types = FALSE,
      na = c("", "NA")
    )
  ),
  
  tar_target(
    codebook,
    load_variable_codebook(codebook_file)
  ),
  
  tar_target(
    domain_registry,
    load_domain_registry(domain_registry_file)
  ),
  
  tar_target(
    subgroup_registry,
    load_domain_subgroup_registry(subgroup_registry_file)
  ),
  
  # ----------------------------------------------------------------------------
  # WHO REFERENCE TABLES  (auto-generated — no repetition)
  # Files: WHO_F75_std, WHO_F75_oed3_std, WHO_F100_std,
  #        WHO_F100d_bf, WHO_F100d_nbf, WHO_RUTF_std,
  #        WHO_WFH_boys_0_2, WHO_WFH_boys_2_5,
  #        WHO_WFH_girls_0_2, WHO_WFH_girls_2_5
  # ----------------------------------------------------------------------------
  who_tables = local({
    
    who_manifest <- tibble::tribble(
      ~name,                 ~file,
      "WHO_F75_std",         "WHO_F75_std.csv",
      "WHO_F75_oed3_std",    "WHO_F75_oed3_std.csv",
      "WHO_F100_std",        "WHO_F100_std.csv",
      "WHO_F100d_bf",        "WHO_F100d_bf.csv",
      "WHO_F100d_nbf",       "WHO_F100d_nbf.csv",
      "WHO_RUTF_std",        "WHO_RUTF_std.csv",
      "WHO_WFH_boys02_std",  "WHO_WFH_boys_0_2_z_std.csv",
      "WHO_WFH_boys25_std",  "WHO_WFH_boys_2_5_z_std.csv",
      "WHO_WFH_girls02_std", "WHO_WFH_girls_0_2_z_std.csv",
      "WHO_WFH_girls25_std", "WHO_WFH_girls_2_5_z_std.csv"
    )
    
    purrr::map(
      seq_len(nrow(who_manifest)),
      ~ tar_target_raw(
        name    = who_manifest$name[[.x]],
        command = substitute(
          readr::read_csv2(
            get_raw_data_path(f),
            locale         = locale(encoding = "UTF-8"),
            show_col_types = FALSE
          ),
          list(f = who_manifest$file[[.x]])
        )
      )
    ) |> purrr::set_names(who_manifest$name)
    
  }),
  
  # ----------------------------------------------------------------------------
  # WHO BUNDLE  — collapses 10 tables into one named-list node
  # Access: who_std_raw$F75, who_std_raw$WFH_b02, etc.
  # Benefit: one node in tar_visnetwork() instead of 10 converging arrows
  # ----------------------------------------------------------------------------
  
  tar_target(
    who_std_raw,
    list(
      F75       = WHO_F75_std,
      F75_oed3  = WHO_F75_oed3_std,
      F100      = WHO_F100_std,
      F100d_bf  = WHO_F100d_bf,
      F100d_nbf = WHO_F100d_nbf,
      RUTF      = WHO_RUTF_std,
      WFH_b02   = WHO_WFH_boys02_std,
      WFH_b25   = WHO_WFH_boys25_std,
      WFH_g02   = WHO_WFH_girls02_std,
      WFH_g25   = WHO_WFH_girls25_std
    ),
    description = "Named-list bundle of all WHO reference standard tables"
  )
  
)

