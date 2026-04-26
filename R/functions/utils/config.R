CONFIG <- list(
  
  paths = list(
    raw = "data/raw"
  ),
  
  morbidity_columns_map = c(
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
  
)

get_path <- function(file) {
  here::here(CONFIG$paths$raw, file)
}
