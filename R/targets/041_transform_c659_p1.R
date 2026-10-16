
targets_transform_c659_p1 <- list(
  # ------------------------------------------------------------------
  # PHASE 1  — one branch per cohort, runs independently
  # Produces: prep_c659_noedema, nutrition_prep_c659_noedema
  #           nonresponse_prep_c659_noedema  (and oedema equivalents)
  # ------------------------------------------------------------------
  tarchetypes::tar_map(
    values = phase1_grid_c659,
    names  = cohort, # only ~c659_noedema/c659_edema used as suffix
    
    tar_target(
      p1_prep,
      {
        # ----------------------------------------------------------------------------
        # [C]  COHORT FILTER  — C659 children without/with oedema at admission
        # ----------------------------------------------------------------------------
        # "Subset: children aged 6 to 59 months with/without edema, at admission"
        prep <- filter_cohort(cohort_data_prep, cohort=cohort, phase="1")
        
        # ----------------------------------------------------------------------------
        # [D]  COHORT ENRICHMENT  — nutrition estimates + non-response flagging
        # ----------------------------------------------------------------------------
        # "Add phase-1 WHO nutrition targets (F75 volumes, RUTF doses) to C659 no-oedema/oedema cohort"
        prep <- add_nutrition_estimates(prep, who_ref_prep, cohort=cohort, phase="1", run_idx = 1)
        
        # "Flag non-response to nutritional treatment in C659 no-oedema/oedema cohort,
        # Recode nutritional treatment in C659 no-oedema/oedema cohort, phase 1"
        prep <- flag_nonresponse(prep, who_ref_prep, cohort=cohort, phase="1")
        
        prep <- recode_nutrition_estimates(prep, cohort=cohort, phase="1")
        
        droplevels(prep)
      },
      description = "Flag non-response to nutritional treatment in C659 no-oedema/oedema cohort,_
      Recode nutritional treatment in C659 no-oedema/oedema cohort, phase 1"
    )
  )
  
)
