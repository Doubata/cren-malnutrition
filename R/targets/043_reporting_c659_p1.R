
targets_reporting_c659_p1 <- list(
  
  tar_render(
    c659_noedema_p1_report_html,
    here::here("reports", "c659_noedema_p1_report.Rmd"),
    output_format = "html_document"
  ),
  
  tar_render(
    c659_noedema_p1_report_pdf,
    here::here("reports", "c659_noedema_p1_report.Rmd"),
    output_format = rmarkdown::pdf_document(latex_engine = "xelatex")
    # output_format = "pdf_document"
  ),
  
  tar_render(
    c659_edema_p1_report_html,
    here::here("reports", "c659_edema_p1_report.Rmd"),
    output_format = "html_document"
  ),
  
  tar_render(
    c659_edema_p1_report_pdf,
    here::here("reports", "c659_edema_p1_report.Rmd"),
    output_format = rmarkdown::pdf_document(latex_engine = "xelatex")
  )
  
)