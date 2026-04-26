
list(
  
  tar_target(child_clean,  add_demographics(child_clean)),
  tar_target(child_clean,  add_anthropometrics(child_clean, monitoring_clean)),
  tar_target(child_clean,  add_health_status(child_clean, morbidity_clean)),
  
  tar_target(c659_noOed,   build_c659_noOed_df(child_final)),
  tar_target(c659_noOed,   add_nutrition_estimates(c659_noOed, monitoring_clean, phase = "1", run_idx = 1)),
  tar_target(c659_noOed,   flag_nutritional_nonresponse(c659_noOed, monitoring_clean, run_idx = 1))
  
  
)