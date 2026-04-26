
list(
  tar_target(child_clean,      clean_child(child_df)),
  tar_target(monitoring_clean, clean_monitoring(monitoring_df))
)