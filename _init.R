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
  fs,
  devtools,
  usethis,    # tools to automate setup and management of R projects and packages
  
  # Data manipulation & analysis
  tidyverse,  # data manipulation, transformation, and visualization
  
  # Date and time handling
  lubridate,  # easy handling and manipulation of dates and times
  
  # Data cleaning & metadata
  janitor,    # clean column names and simple data cleaning helpers
  labelled,   # manage labelled variables (useful for surveys and statistical data)
  
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

# --- 3. Global options --------------------------------------------------------
options(
  scipen = 999,                  # avoid scientific notation
  dplyr.summarise.inform = FALSE # suppress messages from dplyr::summarise()
)


# source(here::here("scripts", "00_import_data.R"))
# source(here::here("R", "config.R"))


