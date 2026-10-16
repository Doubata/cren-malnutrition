# =============================================================================
# regression_models.R — fit a GLM directly from a phase's metadata list
# (as produced by load_phase_metadata()), keeping the formula construction
# and the codebook-derived variable lists as the single source of truth.
# =============================================================================

# Builds outcome ~ predictor1 + predictor2 + ... from a phase metadata list.
# `role` lets you build a formula from a DIFFERENT role than "regression"
# (e.g. "univariate" for one-variable-at-a-time screening models).
build_model_formula <- function(meta, role = "regression") {
  if (is.null(meta$outcome) || length(meta$outcome) == 0) {
    stop("No outcome variable found in this phase's metadata.", call. = FALSE)
  }
  if (length(meta$outcome) > 1) {
    stop("Expected exactly one outcome variable, found: ",
         paste(meta$outcome, collapse = ", "), call. = FALSE)
  }
  if (is.null(meta[[role]]) || length(meta[[role]]) == 0) {
    stop("No variables found for role '", role, "' in this phase's metadata. ",
         "Available roles: ", paste(names(meta), collapse = ", "), call. = FALSE)
  }

  reformulate(meta[[role]], meta$outcome)
}

# Fits a GLM straight from a phase's metadata. Validates first (reusing
# validate_phase_metadata()) so a variable that's missing from `data` fails
# with a clear codebook-level error instead of glm()'s more opaque one.
fit_glm_model <- function(data, meta, codebook, domain_registry, subgroup_registry = NULL,
                           role = "glm", family = binomial(), ...) {

  validate_phase_metadata(meta, codebook, data, domain_registry, subgroup_registry)

  glm(build_model_formula(meta, role), data = data, family = family, ...)
}


fit_and_select_glm_model <- function(data, meta, codebook, domain_registry, subgroup_registry = NULL,
                                     role = "glm", family = binomial(), ...) {
  
  full_model <- fit_glm_model(data = data, meta = meta, codebook = codebook,
                              domain_registry = domain_registry, subgroup_registry = subgroup_registry,
                              role = role, family = family)
  
  # Run step selection. 
  # CRITICAL: trace = 0 is highly recommended in targets to prevent massive log files
  final_model <- step(full_model, trace = 0)
  
  return(final_model)
}


# One-line labelled odds-ratio table, leveraging apply_codebook_labels()
# already having been run on `data` -- gtsummary reads the var_label
# attribute automatically, so predictor names display as their codebook
# labels with zero extra configuration.
summarize_glm <- function(model, exponentiate = TRUE, ...) {
  
  gtsummary::tbl_regression(model, exponentiate = exponentiate, ...) |>
    add_n() |>
    add_nevent(location = "level") |>
    add_global_p() |>
    add_q() |>
    modify_column_unhide(c("conf.low", "p.value")) |>
    add_significance_stars(hide_p = F, hide_se = T, hide_ci = F) |>
    add_vif() |>
    sort_p() |>
    bold_p(t=0.1, q=T) |>
    bold_labels() |>
    italicize_levels() |>
    modify_header(label = "**Predictor**")
    # modify_caption("GLM table")
  
}
