targets_transform_under6 <- list(
  
  tar_target(
    under6_prep,
    {
      # ----------------------------------------------------------------------------
      # [C]  COHORT FILTER  — children under 6 at admission
      # ----------------------------------------------------------------------------
      # "Subset: children aged under 6 at admission"
      prep <- filter_cohort(cohort_core_prep, cohort="under6")
      
      # ----------------------------------------------------------------------------
      # [D]  COHORT ENRICHMENT  — nutrition estimates + non-response flagging
      # ----------------------------------------------------------------------------
      # "Add phase-1 WHO nutrition targets (F75 volumes, RUTF doses) to under 6 cohort"
      prep <- add_nutrition_estimates(prep, who_ref_prep, cohort="under6")
      
      # "Flag non-response to nutritional treatment in under 6 cohort,
      prep <- flag_nonresponse(prep, who_ref_prep, cohort="under6")
      
      prep <- recode_nutrition_estimates(prep, cohort="under6")
      
      droplevels(prep)
    },
    description = "Flag non-response to nutritional treatment in under 6 cohort,_
      Recode nutritional treatment"
  )
  
)