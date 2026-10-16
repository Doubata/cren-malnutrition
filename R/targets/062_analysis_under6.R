
targets_analysis_under6 <- list(
  
  tar_target(
    under6_vars,
    {
      meta <- load_phase_metadata(codebook, "under6")
      validate_phase_metadata(meta, codebook, under6_prep, domain_registry, subgroup_registry)
      meta
    }
  ),
  
  tar_target(
    under6_data,
    {
      extra <- setdiff(under6_vars$univariate, under6_vars$baseline)
      vars <- c(under6_vars$baseline, extra)
      under6_prep |>
        dplyr::select(all_of(vars)) |>
        apply_codebook_labels(codebook)
    },
    description = "Under 6 cohort, labelled and analysis-ready"
  ),
  
  # ----------------------------------------------------------------------------
  # DESCRIPTIVE SUMMARY TABLE
  # ----------------------------------------------------------------------------
  tar_target(
    under6_summary_table,
    {
      X = under6_vars$baseline[under6_vars$baseline != under6_vars$outcome]
      y = under6_vars$outcome
      fisher_vars = NULL
      build_summary_table(
        under6_data,
        X = X,
        y = y,
        fisher_vars = fisher_vars
      )
    },
    description = "Descriptive summary (Table 1) for under 6 cohort, phase T"
  ),
  
  tar_target(
    under6_catalog,
    auto_plot_catalog(
      under6_data,
      under6_vars$baseline[under6_vars$baseline != under6_vars$outcome],
      outcome      = under6_vars$outcome,
      domain_map   = codebook_to_domain_map(codebook),
      subgroup_map = codebook_to_subgroup_map(codebook),
      label_map    = codebook_to_label_map(codebook),
      type_map     = codebook_to_type_map(codebook),
      # geom_map     = c(categorical = "bar", continuous = "violin")
    )
  ),
  
  tar_target(
    under6_univariate_table,
    {
      build_univariate_table(
        under6_data,
        X = under6_vars$univariate,
        y = under6_vars$outcome
      )
    },
    description = "Univariate regression models summary (Table 1) for under 6 cohort, phase T"
  ),
  
  tar_target(
    under6_glm,
    fit_glm_model(under6_data, under6_vars, codebook, domain_registry, subgroup_registry)
  ),
  
  tar_target(
    under6_glm_table,
    summarize_glm(under6_glm)
  ),
  
  tar_target(
    under6_data_narm,
    {
      data_narm <- under6_data |>
        tidyr::drop_na(all_of(under6_vars$glm))
    }
  ),
  
  # tar_target(
  #   under6_summary_table_final,
  #   {
  #     X = under6_vars$baseline[under6_vars$baseline != under6_vars$outcome]
  #     y = under6_vars$outcome
  #     build_summary_table(
  #       under6_data_narm,
  #       X = X,
  #       y = y
  #     )
  #   },
  #   description = "Final Descriptive summary for C659 no-oedema cohort, phase T"
  # ),
  
  tar_target(
    under6_glm_final,
    fit_and_select_glm_model(under6_data_narm, under6_vars, codebook, domain_registry, subgroup_registry)
  ),
  
  tar_target(
    under6_glm_final_table,
    summarize_glm(under6_glm_final)
  )
  
)