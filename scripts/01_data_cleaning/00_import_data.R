
# Import WHO Refrence Tables DF  -----------------------------------------------
WHO_F75_std         <- read_csv2(here::here("data", "raw", "WHO_F75_std.csv"), locale = locale(encoding = "UTF-8"))
WHO_F75_oed3_std    <- read_csv2(here::here("data", "raw", "WHO_F75_oed3_std.csv"), locale = locale(encoding = "UTF-8"))
WHO_F100_std        <- read_csv2(here::here("data", "raw", "WHO_F100_std.csv"), locale = locale(encoding = "UTF-8"))
WHO_F100d_bf        <- read_csv2(here::here("data", "raw", "WHO_F100d_bf.csv"), locale = locale(encoding = "UTF-8"))
WHO_F100d_nbf       <- read_csv2(here::here("data", "raw", "WHO_F100d_nbf.csv"), locale = locale(encoding = "UTF-8"))
WHO_RUTF_std        <- read_csv2(here::here("data", "raw", "WHO_RUTF_std.csv"), locale = locale(encoding = "UTF-8"))
WHO_WFH_boys02_std  <- read_csv2(here::here("data", "raw", "WHO_WFH_boys_0_2_z_std.csv"), locale = locale(encoding = "UTF-8"))
WHO_WFH_boys25_std  <- read_csv2(here::here("data", "raw", "WHO_WFH_boys_2_5_z_std.csv"), locale = locale(encoding = "UTF-8"))
WHO_WFH_girls02_std <- read_csv2(here::here("data", "raw", "WHO_WFH_girls_0_2_z_std.csv"), locale = locale(encoding = "UTF-8"))
WHO_WFH_girls25_std <- read_csv2(here::here("data", "raw", "WHO_WFH_girls_2_5_z_std.csv"), locale = locale(encoding = "UTF-8"))


# Import Children Demographics DF  ---------------------------------------------
child_cols <- c("id", "numMa", "zoneResidence", "modeAdm", "tAdm", "tAdmCorrige", "rechuCorrige",
                "sexe", "ageMois", "allait", "vaccAJ", "dateSor", "modeSorCorrige", "decede", "phaseDeces")

child_df <- read_csv2(here::here("data", "raw", "fiche_10072023.csv"), locale = locale(encoding = "UTF-8"),
                     col_select = all_of(child_cols))

# Import Monitoring DF  --------------------------------------------------------
monitoring_cols <- c("id", "dateVis", "taille", "poids", "ptZscore", "pbOrPc", "pbOrPcV", "oedemes",
                     "phase", "nRepJr", "typeAlim", "mlRepas", "repasH1", "repasH2", "repasH3", "repasH4",
                     "repasH5", "repasH6", "repasH7", "repasH8", "repasH9", "repasH10", "ATPE", "bouillie",
                     "bouillieCorrige", "plaFamil", "plaFamilCorrige" ,"aPlats", "selles", "vomiss",
                     "vomissCorrige", "index")

monitoring_df <- read_csv2(here::here("data", "raw", "suivi_11092023.csv"),
                           locale = locale(encoding = "UTF-8"),
                           guess_max = 5000,
                           col_select = all_of(monitoring_cols))

# Import Morbidity DF  ---------------------------------------------------------
morbidity_cols <- c("gastroenteriteAigue", "deshydratation", "choc", "pneumopathieViraleBacterienne",
                    "paluGraveAnemique", "paluGraveNeurologique", "autrePaluGrave", "autreAnemie",
                    "candidoseDigestive", "tuberculose", "VIH", "id")

morbidity_df <- read_csv2(here::here("data", "raw", "comorbidite_25052023.csv"), locale = locale(encoding = "UTF-8"),
                          col_select = all_of(morbidity_cols))

