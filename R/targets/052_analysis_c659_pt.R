# ==============================================================================
# 
# PURPOSE : Produce labelled analytic datasets and descriptive outputs
# INPUTS  : prep_pt_c659_noedema
# OUTPUTS : <cohort>_pt_data  (labelled, analysis-ready)
#           <cohort>_pt_summary_tbl  (descriptive summary table)
#
# NAMING  : <subject>_data     — final labelled dataset, ready for modelling/export
#           <subject>_tbl      — summary / tabular output
#           <subject>_plot     — visual output (future)
# ==============================================================================

targets_analysis_c659_pt <- list(
  
  # Phase T outputs — one per cohort
  tarchetypes::tar_map(
    values = phaseT_grid_c659,
    names  = cohort, # only ~c659_noedema/c659_edema used as suffix
    
    # ----------------------------------------------------------------------------
    # LABELLED DATASET  — attach variable labels, factor levels, display formats
    # ----------------------------------------------------------------------------
    
    tar_target(
      pt_vars,
      {
        meta <- load_phase_metadata(codebook, paste0(cohort_code, "_pt"))
        validate_phase_metadata(meta, codebook, cohort_data, domain_registry, subgroup_registry)
        meta
      }
    ),
    
    tar_target(
      pt_data,
      {
        # extra <- setdiff(pt_vars$univariate, pt_vars$baseline)
        # vars <- c(pt_vars$baseline, extra)
        cohort_data |>
          # dplyr::select(all_of(vars)) |>
          apply_codebook_labels(codebook)
      },
      description = "C659 no-oedema/edema cohort, phase T — labelled and analysis-ready"
    ),
    
    # ----------------------------------------------------------------------------
    # DESCRIPTIVE SUMMARY TABLE
    # ----------------------------------------------------------------------------
    tar_target(
      pt_summary_table,
      {
        X = pt_vars$baseline[pt_vars$baseline != pt_vars$outcome]
        y = pt_vars$outcome
        fisher_vars = NULL
        build_summary_table(
          pt_data,
          X = X,
          y = y,
          fisher_vars = fisher_vars
        )
      },
      description = "Descriptive summary (Table 1) for C659 no-oedema/oedema cohort, phase T"
    ),
    
    tar_target(
      pt_catalog,
      auto_plot_catalog(
        pt_data,
        pt_vars$baseline[pt_vars$baseline != pt_vars$outcome],
        outcome      = pt_vars$outcome,
        domain_map   = codebook_to_domain_map(codebook),
        subgroup_map = codebook_to_subgroup_map(codebook),
        label_map    = codebook_to_label_map(codebook),
        type_map     = codebook_to_type_map(codebook),
        # geom_map     = c(categorical = "bar", continuous = "violin")
      )
    ),
    
    tar_target(
      pt_univariate_table,
      {
        build_univariate_table(
          pt_data,
          X = pt_vars$univariate,
          y = pt_vars$outcome
        )
      },
      description = "Univariate regression models summary (Table 1) for C659 no-oedema/oedema cohort, phase T"
    ),

    tar_target(
      pt_glm,
      fit_glm_model(pt_data, pt_vars, codebook, domain_registry, subgroup_registry)
    ),

    tar_target(
      pt_glm_table,
      summarize_glm(pt_glm)
    ),

    tar_target(
      pt_data_narm,
      {
        data_narm <- pt_data |>
          tidyr::drop_na(all_of(pt_vars$glm))
      }
    ),

    # tar_target(
    #   pt_summary_table_final,
    #   {
    #     X = pt_vars$baseline[pt_vars$baseline != pt_vars$outcome]
    #     y = pt_vars$outcome
    #     build_summary_table(
    #       pt_data_narm,
    #       X = X,
    #       y = y
    #     )
    #   },
    #   description = "Final Descriptive summary for C659 no-oedema cohort, phase T"
    # ),

    tar_target(
      pt_glm_final,
      fit_and_select_glm_model(pt_data_narm, pt_vars, codebook, domain_registry, subgroup_registry)
    ),

    tar_target(
      pt_glm_final_table,
      summarize_glm(pt_glm_final)
    )
    
  )
  
)
