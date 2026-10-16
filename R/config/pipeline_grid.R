
# Phase 1 grid — each row is one cohort entering phase 1
phase1_grid_c659 <- tibble::tribble(
  ~cohort,          ~cohort_data,             ~cohort_code,     ~cohort_data_prep,
  "c659_noedema",   "p1_prep_c659_noedema",   "C659NOEDEMA",    "cohort_core_prep",
  "c659_edema",     "p1_prep_c659_edema",     "C659EDEMA",      "cohort_core_prep"
) |>
  dplyr::mutate(
    # cohort         = rlang::syms(cohort),
    cohort_data      = rlang::syms(cohort_data),
    cohort_data_prep = rlang::syms(cohort_data_prep)
  )


# Phase T grid — each row is one cohort entering phase T
phaseT_grid_c659 <- tibble::tribble(
  ~cohort,          ~cohort_data,             ~cohort_code,    ~cohort_data_prep,
  "c659_noedema",   "pt_prep_c659_noedema",   "C659NOEDEMA",   "p1_data_narm_c659_noedema",
  "c659_edema",     "pt_prep_c659_edema",     "C659EDEMA",     "p1_data_narm_c659_edema"
) |>
  dplyr::mutate(
    # cohort         = rlang::syms(cohort),
    cohort_data      = rlang::syms(cohort_data),
    cohort_data_prep = rlang::syms(cohort_data_prep)
  )

