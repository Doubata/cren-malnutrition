
list(
  
  # =========================
  # CHILD DATA
  # =========================
  tar_target(
    child_df,
    read_csv_utf8(
      get_path("fiche_10072023.csv"),
      cols = c("id", "numMa", "zoneResidence", "modeAdm", "tAdm", "tAdmCorrige",
               "rechuCorrige", "sexe", "ageMois", "allait", "vaccAJ", "dateSor",
               "modeSorCorrige", "decede", "phaseDeces")
    )
  ),
  
  # =========================
  # MONITORING
  # =========================
  tar_target(
    monitoring_df,
    read_csv_utf8(
      get_path("suivi_11092023.csv"),
      cols = c("id", "dateVis", "taille", "poids", "ptZscore", "pbOrPc",
               "pbOrPcV", "oedemes", "phase", "nRepJr", "typeAlim",
               "mlRepas", "repasH1", "repasH2", "repasH3", "repasH4",
               "repasH5", "repasH6", "repasH7", "repasH8", "repasH9",
               "repasH10", "ATPE", "bouillie", "bouillieCorrige",
               "plaFamil", "plaFamilCorrige", "aPlats", "selles",
               "vomiss", "vomissCorrige", "index"),
      guess_max = 5000
    )
  ),
  
  # =========================
  # MORBIDITY
  # =========================
  tar_target(
    morbidity_df,
    read_csv_utf8(
      get_path("comorbidite_25052023.csv"),
      cols = c("gastroenteriteAigue", "deshydratation", "choc",
               "pneumopathieViraleBacterienne", "paluGraveAnemique",
               "paluGraveNeurologique", "autrePaluGrave", "autreAnemie",
               "candidoseDigestive", "tuberculose", "VIH", "id")
    )
  ),
  
  # =========================
  # WHO STANDARD TABLES
  # =========================
  tar_target(WHO_F75_std,      read_csv_utf8(get_path("WHO_F75_std.csv"))),
  tar_target(WHO_F75_oed3_std, read_csv_utf8(get_path("WHO_F75_oed3_std.csv"))),
  tar_target(WHO_F100_std,     read_csv_utf8(get_path("WHO_F100_std.csv"))),
  tar_target(WHO_F100d_bf,     read_csv_utf8(get_path("WHO_F100d_bf.csv"))),
  tar_target(WHO_F100d_nbf,    read_csv_utf8(get_path("WHO_F100d_nbf.csv"))),
  tar_target(WHO_RUTF_std,     read_csv_utf8(get_path("WHO_RUTF_std.csv"))),
  
  tar_target(WHO_WFH_boys02_std,  read_csv_utf8(get_path("WHO_WFH_boys_0_2_z_std.csv"))),
  tar_target(WHO_WFH_boys25_std,  read_csv_utf8(get_path("WHO_WFH_boys_2_5_z_std.csv"))),
  tar_target(WHO_WFH_girls02_std, read_csv_utf8(get_path("WHO_WFH_girls_0_2_z_std.csv"))),
  tar_target(WHO_WFH_girls25_std, read_csv_utf8(get_path("WHO_WFH_girls_2_5_z_std.csv")))
  
)
