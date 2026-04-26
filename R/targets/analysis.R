
list(
  
  tar_target(
    analytical_dataset,
    build_dataset(child_clean, monitoring_clean, morbidity_clean)
  )
  
)