# ==============================================================================
# 04_analysis.R
# PURPOSE : Produce labelled analytic datasets and descriptive outputs
# INPUTS  : prep_p1_c659_noedema
# OUTPUTS : c659_noedema_p1_data  (labelled, analysis-ready)
#           c659_noedema_p1_summary_tbl  (descriptive summary table)
#
# NAMING  : <subject>_data     — final labelled dataset, ready for modelling/export
#           <subject>_tbl      — summary / tabular output
#           <subject>_plot     — visual output (future)
# ==============================================================================

targets_analysis_c659_p1 <- list(
  
  # Phase 1 outputs — one per cohort
  tarchetypes::tar_map(
    values = phase1_grid_c659,
    names  = cohort, # only ~c659_noedema/c659_edema used as suffix
    
    # ----------------------------------------------------------------------------
    # LABELLED DATASET  — attach variable labels, factor levels, display formats
    # ----------------------------------------------------------------------------
    
    tar_target(
      p1_vars,
      {
        meta <- load_phase_metadata(codebook, paste0(cohort_code, "_p1"))
        validate_phase_metadata(meta, codebook, cohort_data, domain_registry, subgroup_registry)
        meta
      }
    ),
    
    tar_target(
      p1_data,
      {
        # extra <- setdiff(p1_vars$univariate, p1_vars$baseline)
        # vars <- c(p1_vars$baseline, extra)
        cohort_data |>
          # dplyr::select(all_of(vars)) |>
          apply_codebook_labels(codebook)
      },
      description = "C659 no-oedema/edema cohort - labelled and analysis-ready"
    ),
    
    # ----------------------------------------------------------------------------
    # DESCRIPTIVE SUMMARY TABLE
    # ----------------------------------------------------------------------------
    tar_target(
      p1_summary_table,
      {
        X = p1_vars$baseline[p1_vars$baseline != p1_vars$outcome]
        y = p1_vars$outcome
        fisher_vars = c("morbidite", "has_tuberculosis", "prop_food_intolerance_p1")
        build_summary_table(
          p1_data,
          X = X,
          y = y,
          fisher_vars = fisher_vars
        )
      },
      description = "C659 no-oedema/edema cohort - Descriptive summary"
    ),
    
    tar_target(
      p1_catalog,
      auto_plot_catalog(
        p1_data,
        p1_vars$baseline[p1_vars$baseline != p1_vars$outcome],
        outcome      = p1_vars$outcome,
        domain_map   = codebook_to_domain_map(codebook),
        subgroup_map = codebook_to_subgroup_map(codebook),
        label_map    = codebook_to_label_map(codebook),
        type_map     = codebook_to_type_map(codebook),
        # geom_map     = c(categorical = "bar", continuous = "violin")
      )
    ),
    
    tar_target(
      p1_univariate_table,
      {
        build_univariate_table(
          p1_data,
          X = p1_vars$univariate,
          y = p1_vars$outcome
        )
      },
      description = "C659 no-oedema/edema cohort - Univariate regression models summary"
    ),
    
    tar_target(
      p1_glm,
      fit_glm_model(p1_data, p1_vars, codebook, domain_registry, subgroup_registry)
    ),
    
    tar_target(
      p1_glm_table,
      summarize_glm(p1_glm)
    ),
    
    tar_target(
      p1_data_narm,
      {
        data_narm <- p1_data |>
          tidyr::drop_na(all_of(p1_vars$glm))
      }
    ),
    
    # tar_target(
    #   p1_summary_table_final,
    #   {
    #     X = p1_vars$baseline[p1_vars$baseline != p1_vars$outcome]
    #     y = p1_vars$outcome
    #     fisher_vars = c("morbidite", "has_tuberculosis", "prop_food_intolerance_p1")
    #     build_summary_table(
    #       p1_data_narm,
    #       X = X,
    #       y = y,
    #       fisher_vars = fisher_vars
    #     )
    #   },
    #   description = "Final Descriptive summary (Table 1) for C659 no-oedema/oedema cohort, phase 1"
    # ),
    
    tar_target(
      p1_glm_final,
      fit_and_select_glm_model(p1_data_narm, p1_vars, codebook, domain_registry, subgroup_registry)
    ),
    
    tar_target(
      p1_glm_final_table,
      summarize_glm(p1_glm_final)
    )
    
  )
  
)
