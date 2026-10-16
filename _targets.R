# Set your global options
options(
  scipen = 999,                  # avoid scientific notation
  dplyr.summarise.inform = FALSE, # suppress messages from dplyr::summarise()
  mc_doScale_quiet = TRUE
)

# Load core workflow packages
packages <- c("targets", "tarchetypes", "visNetwork", "here", "fs",  "qs2", "smd")
for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}

# Tell targets which packages your background steps need
tar_option_set(
  packages = c(
    "readr", "tibble", "dplyr", "stringr", "purrr", "ggplot2", "sjPlot", "patchwork",
    "lubridate", "janitor", "labelled", "smd", "car", "performance", "dataReporter",
    "checkmate", "gtsummary", "pointblank", "scales", "writexl", "openxlsx",
    "rmarkdown", "knitr", "tinytex", "kableExtra", "flextable"
  ),
  format = "qs"                             # Use 'qs' compression format for storage
  # error = "continue"
)

# Walk through the config directory and source each R file
fs::dir_ls(
  path = here::here("R", "config"), 
  glob = "*.R", 
  recurse = TRUE
) |> 
  lapply(suppressMessages(source))

# Walk through the functions directory and source each R file
fs::dir_ls(
  path = here::here("R", "functions"), 
  glob = "*.R", 
  recurse = TRUE
) |> 
  lapply(source)

# Find all target module files
# target_files <- fs::dir_ls(
#   here::here("R", "targets"),
#   glob = "*.R"
# )

# Read them, execute them, and flatten the unnamed lists into one
# pipeline <- lapply(target_files, function(file) source(file)$value) |>
#   unlist(recursive = FALSE)

tar_source(here::here("R", "targets"))

pipeline <- list(
  
  targets_ingestion,
  targets_cleaning,
  targets_transform,
  
  targets_transform_c659_p1,
  targets_analysis_c659_p1,
  targets_reporting_c659_p1,
  
  targets_transform_c659_pt,
  targets_analysis_c659_pt,
  targets_reporting_c659_pt,
  
  targets_transform_under6,
  targets_analysis_under6,
  targets_reporting_under6
  
)

# Return the complete pipeline
pipeline

# targets::tar_make()
# targets::tar_visnetwork()

# Full error message
# targets::tar_meta(fields = c("name", "error", "warnings")) |>
#   dplyr::filter(!is.na(error))

# Wipe the old metadata and cached objects entirely
# targets::tar_destroy()

# targets::tar_load(cohort_nonresponse_regression_prep_c659_noedema)
# targets::tar_load_globals()

# Returns a data frame of target names and commands
# targets::tar_manifest()

# List all saved targets
# tar_objects()
