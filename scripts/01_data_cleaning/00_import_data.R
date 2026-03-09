source(here::here("scripts", "00_setup.R"))

# Import WHO Refrence Tables DF  -----------------------------------------------
who_ptz_df       <- read_csv2(here::here("data", "raw", "OMS_ptz.csv"), locale = locale(encoding = "UTF-8"))
who_f75_df       <- read_csv2(here::here("data", "raw", "OMS_f75.csv"), locale = locale(encoding = "UTF-8"))
who_f100_df      <- read_csv2(here::here("data", "raw", "OMS_f100.csv"), locale = locale(encoding = "UTF-8"))
who_f100d_bf_df  <- read_csv2(here::here("data", "raw", "OMS_f100d_allait.csv"), locale = locale(encoding = "UTF-8"))
who_f100d_nbf_df <- read_csv2(here::here("data", "raw", "OMS_f100d_nonAllait.csv"), locale = locale(encoding = "UTF-8"))
who_plumpynut_df <- read_csv2(here::here("data", "raw", "OMS_atpe.csv"), locale = locale(encoding = "UTF-8"))


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

monitoring_df <- read_csv2(here::here("data", "raw", "suivi_11092023.csv"), locale = locale(encoding = "UTF-8"),
                          col_select = all_of(monitoring_cols))

# Import Morbidity DF  ---------------------------------------------------------
morbidity_cols <- c("gastroenteriteAigue", "deshydratation", "choc", "pneumopathieViraleBacterienne",
                    "paluGraveAnemique", "paluGraveNeurologique", "autrePaluGrave", "autreAnemie",
                    "candidoseDigestive", "tuberculose", "VIH", "id")

morbidity_df <- read_csv2(here::here("data", "raw", "comorbidite_25052023.csv"), locale = locale(encoding = "UTF-8"),
                          col_select = all_of(morbidity_cols))

