############################################################
# Project:     Nutrition Data Quality Analysis
# Author:      Doubata Dramane Marius Coulibaly
# Date:        2024-12-31
# Description: Data cleaning and analysis of routine health
#              nutrition indicators.
############################################################

if (!require("pacman")) install.packages("pacman")

# Load required packages
pacman::p_load(
  readr,      # fast import of rectangular data (CSV, TSV)
  checkmate,
  usethis,
  DataExplorer,
  tidyverse,  # data manipulation, transformation, and visualization (dplyr, ggplot2, tidyr, etc.)
  pointblank, # data quality validation and QA pipelines
  lubridate,  # easy handling and manipulation of dates and times
  here,       # reproducible project file paths
  janitor,    # clean column names, create frequency tables, data cleaning helpers
  labelled,   # work with labelled data (useful for surveys and imported statistical data)
  scales,     # formatting for plots and tables (percentages, thousands, currency, etc.)
  writexl,    # export data frames to Excel (.xlsx)
  openxlsx,   # advanced Excel writing (formatting, multiple sheets, styling)
  purrr       # functional programming tools for iteration (map functions)
)

options(
  scipen = 999,     # avoid scientific notation
  dplyr.summarise.inform = FALSE
)