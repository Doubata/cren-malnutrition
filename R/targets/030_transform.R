# ==============================================================================
# 03_transform.R
# PURPOSE : Build the analytic dataset through progressive enrichment
# INPUTS  : child_clean | monitoring_clean | morbidity_raw | who_std_raw
# OUTPUTS : cohort_c659_noedema_prep  (final prepped cohort, phase 1)
#
# PIPELINE STAGES
# ───────────────────────────────────────────────────────────────
#  [A] REFERENCE ESTIMATES   who_ref_prep | comorbidity_prep
#  [B] COHORT ASSEMBLY       cohort_core_prep
#  [C] COHORT FILTER         cohort_c659_noedema_prep  (C659, no oedema)
#  [D] COHORT ENRICHMENT     cohort_c659_noedema_nutrition_prep
#                            cohort_c659_noedema_nonresponse_prep
# ───────────────────────────────────────────────────────────────
# NAMING : <subject>_prep  — enriched / transformed; not yet labelled for output
# ==============================================================================

targets_transform <- list(
  
  # ----------------------------------------------------------------------------
  # [A]  REFERENCE ESTIMATES
  # These two targets are independent of each other and feed into [B].
  # ----------------------------------------------------------------------------
  
  # WHO growth standard estimates joined onto follow-up visits
  # Receives the who_std_raw bundle (one node) instead of 10 individual tables
  tar_target(
    who_ref_prep,
    add_who_estimates(
      monitoring_clean,
      child_clean,
      who_std_raw          # named list: who_std_raw$F75, $WFH_b02, etc.
    ),
    description = "Join WHO reference z-scores and feeding thresholds onto follow-up visits"
  ),
  
  # Comorbidity flags derived from raw morbidity table
  tar_target(
    comorbidity_prep,
    add_comorbidity_estimates(morbidity_raw),
    description = "Compute comorbidity burden scores and flags from raw morbidity records"
  ),
  
  # ----------------------------------------------------------------------------
  # [B]  COHORT ASSEMBLY
  # Groups demographics → anthropometrics → health status into one sequential
  # chain, producing a single fully-enriched cohort table.
  # Intermediate steps (demographics, anthropometrics) are collapsed into
  # cohort_core_prep so the graph stays readable.
  # ----------------------------------------------------------------------------
  
  tar_target(
    cohort_core_prep,
    {
      # Step 1 — baseline demographics (age group, sex, admission mode, zone)
      cohort <- add_demographics(child_clean)
      
      # Step 2 — anthropometric indices (WHZ, MUAC category, oedema grade)
      cohort <- add_anthropometrics(cohort, who_ref_prep)
      
      # Step 3 — composite health status (comorbidity + vital outcome)
      add_health_status(cohort, who_ref_prep, comorbidity_prep)
    },
    description = "Full cohort: demographics → anthropometrics → health status"
  )
  
)