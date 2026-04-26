############################################################
# Project:     Nutrition Data Quality Analysis
# Author:      Doubata Dramane Marius Coulibaly
# Date:        2024-12-31
# Description: Data cleaning and analysis of routine health
#              nutrition indicators.
############################################################

# ==============================================================================
# Project: CREN Malnutrition Analysis
# Purpose: Initialize environment, load dependencies, and source functions
# ==============================================================================

if (!require("pacman")) install.packages("pacman")

# --- 1. Load or Install Missing Packages --------------------------------------
pacman::p_load(
  # Project & workflow management
  here,       # reproducible file paths within an R project
  devtools,
  usethis,    # tools to automate setup and management of R projects and packages
  
  # Data import
  readr,      # fast import of rectangular data (CSV, TSV)
  
  # Data manipulation & analysis
  tidyverse,  # data manipulation, transformation, and visualization
  
  # Date and time handling
  lubridate,  # easy handling and manipulation of dates and times
  
  # Data cleaning & metadata
  janitor,    # clean column names and simple data cleaning helpers
  labelled,   # manage labelled variables (useful for surveys and statistical data)
  
  # Functional programming / iteration
  purrr,      # tools for iteration (map, map_df, etc.)
  
  # Data validation & robustness
  checkmate,  # fast argument checking for functions
  pointblank, # data quality validation and QA pipelines
  
  # Formatting for tables and plots
  scales,     # formatting numbers (percentages, commas, currency)
  
  # Data export
  writexl,    # export data frames to Excel
  openxlsx,   # advanced Excel writing with formatting and multiple sheets
  
  targets,
  
  # Documentation
  roxygen2,    # generate documentation for functions and packages
  pkgdown,
  renv
)

# --- 2. Source Custom Functions from /R/functions folder --------------------------------
path_to_functions <- list("R/functions/utils/",
                          "R/functions/import/",
                          "R/functions/cleaning/",
                          "R/functions/validation/",
                          "R/functions/transform/",
                          "R/functions/indicators/")

for (p in path_to_functions) {
  if (dir.exists(p)) {
    
    # Get list of R files
    function_files <- list.files(p, pattern = "\\.[Rr]$", full.names = TRUE)
    
    # Source each file into the Global Environment
    # Using local = FALSE or local = globalenv() ensures functions are actually accessible
    invisible(lapply(function_files, source, local = FALSE))
    
    cli::cli_alert_success("Sourced {length(function_files)} function(s) files from {p}")
    
    
  } else {
    cli::cli_alert_danger("Error: '{p}' directory not found!")
  }
  
}

# --- 3. Global options --------------------------------------------------------
options(
  scipen = 999,                  # avoid scientific notation
  dplyr.summarise.inform = FALSE # suppress messages from dplyr::summarise()
)

# library(gtsummary)
# library(labelled)
