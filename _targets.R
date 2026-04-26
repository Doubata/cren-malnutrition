library(targets)

source(here::here("scripts", "00_import_data.R"))
source(here::here("R", "config.R"))

tar_option_set(
  packages = c("dplyr", "readr", "purrr", "here")
)

# Load functions
tar_source("R/functions")

# Load pipeline modules
ingestion  <- source("R/targets/ingestion.R")$value
cleaning   <- source("R/targets/cleaning.R")$value
transform  <- source("R/targets/transform.R")$value
analysis   <- source("R/targets/analysis.R")$value
export     <- source("R/targets/export.R")$value

list(
  ingestion,
  cleaning,
  transform,
  analysis,
  export
)

targets::tar_make()
targets::tar_visnetwork()


