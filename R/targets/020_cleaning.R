# ==============================================================================
# 02_cleaning.R
# PURPOSE : Apply source-specific cleaning rules to raw tables
# INPUTS  : child_raw | monitoring_raw
# OUTPUTS : child_clean | monitoring_clean
# NAMING  : <subject>_clean  — validated, typed, renamed columns; no derivations
# NOTE    : morbidity_raw has no dedicated cleaning step; comorbidity flags are
#           derived directly in 03_transform.R (add_comorbidity_estimates)
# ==============================================================================

targets_cleaning <- list(
  
  # Child admission & discharge
  tar_target(
    child_clean,
    clean_children_profile(child_raw),
    description = "Clean child admission records: types, labels, derived age groups"
  ),
  
  # Follow-up visit records
  tar_target(
    monitoring_clean,
    clean_follow_up(monitoring_raw),
    description = "Clean follow-up visits: parse dates, validate anthropometry, recode feeding flags"
  )
  
)

