# library(DataExplorer)
source(here::here("scripts", "00_setup.R"))
source(here::here("R", "qa_child.R"))
source(here::here("R", "qa_monitoring.R"))

# EDA Child_df ---------------------------------------------------------------
glimpse(child_df)
summary(child_df)
head(child_df)

create_report(
  data = child_df,
  output_file = "EDA_Children.html",
  output_dir = here::here("output", "tables"),
  report_title = "Exploratory Data Analysis: Children with SAM"
)

# QA Child_df ----------------------------------------------------------------
qa_child <- run_clinical_qa(child_df, "Child_Clinical_Data_Validation")
# Save as a self-contained HTML file
export_report(qa_child, filename = here::here("output", "tables", "QA_Report_Child_Clinical_Data.html"))


# EDA monitoring_df ----------------------------------------------------------
glimpse(monitoring_df)
summary(monitoring_df)
head(monitoring_df)

create_report(
  data = monitoring_df,
  output_file = "EDA_Monitoring.html",
  output_dir = here::here("output", "tables"),
  report_title = "Exploratory Data Analysis: Monitoring Data Of Children with SAM"
)

# QA monitoring_df -----------------------------------------------------------
qa_monitoring <- run_monitoring_qa(df         = monitoring_df,
                                   excel_path = here::here("output", "tables", "QA_Report_Routine_Data.xlsx"),
                                   html_path  = here::here("output", "tables", "QA_Report_Routine_Data.html")
                                   )


# Exports monitoring_df ------------------------------------------------------

# write_xlsx(as.data.frame(table(child_df$phase_series)), "output/tables/phases_series_results.xlsx")
# write_xlsx(as.data.frame(table(child_df$food_series)), "output/tables/food_series_results.xlsx")

# write_xlsx(as.data.frame(table(child_e6a59sO_df$phase_series_1)), "output/tables/phases_series_1_e6a59sO.xlsx")
# write_xlsx(as.data.frame(table(child_e6a59sO_df$phase_series_2)), "output/tables/phases_series_2_e6a59sO.xlsx")
# write_xlsx(as.data.frame(table(child_e6a59sO_df$nutri_treat_series)), "output/tables/nutri_treat_series_e6a59sO.xlsx")





