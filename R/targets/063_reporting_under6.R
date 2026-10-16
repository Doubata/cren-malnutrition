
targets_reporting_under6 <- list(
  
  tar_render(
    under6_report_html,
    here::here("reports", "under6_report.Rmd"),
    output_format = "html_document"
  ),
  
  tar_render(
    under6_report_pdf,
    here::here("reports", "under6_report.Rmd"),
    output_format = rmarkdown::pdf_document(latex_engine = "xelatex")
    # output_format = "pdf_document"
  )
  
)