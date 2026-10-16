
targets_transform_c659_pt <- list(
  # ------------------------------------------------------------------
  # PHASE T  — downstream of phase 1 labelled data (04_analysis output)
  #            specific indicators computed before re-entering the
  #            nutrition + nonresponse chain
  # ------------------------------------------------------------------
  tarchetypes::tar_map(
    values = phaseT_grid_c659,
    names  = cohort, # only ~c659_noedema/c659_edema used as suffix
    
    tar_target(
      pt_prep,
      {
        # ----------------------------------------------------------------------------
        # [C]  COHORT FILTER  — C659 children without edema in phase T
        # ----------------------------------------------------------------------------
        # "Subset: children aged 6 to 59 months without edema, in phase T"
        prep <- filter_cohort(cohort_data_prep, cohort=cohort, phase="T")
        
        # ----------------------------------------------------------------------------
        # [D]  COHORT ENRICHMENT  — nutrition estimates + non-response flagging
        # ----------------------------------------------------------------------------
        # "Add phase-T WHO nutrition targets (F75 volumes, RUTF doses) to C659 no-oedema/oedema cohort"
        prep <- add_nutrition_estimates(prep, who_ref_prep, cohort=cohort, phase = "T", run_idx = 2)
        
        # "Flag non-response to nutritional treatment in C659 no-oedema/oedema cohort,
        # Recode nutritional treatment in C659 no-oedema/oedema cohort, phase T"
        prep <- flag_nonresponse(prep, who_ref_prep, cohort=cohort, phase="T")
        # cohort_prep <- cohort_prep |>
        #   filter(length_pt > 1)
        
        prep <- recode_nutrition_estimates(prep, cohort=cohort, phase="T")
        
        droplevels(prep)
      }
    )
    
  )
  
)